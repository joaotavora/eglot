;;; kvspec.el --- Koutline view specification
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Oct-95 at 15:17:07
;;
;; Copyright (C) 1995-2017  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;; Koutliner view specs (each viewspec is invoked with its first letter)
;; + means support code has been written already.
;;
;; +      all:     Show all lines of cells and all cells in the outline.
;; +      blank:   Blank lines are on.
;;          b - on
;; +      cutoff:  Show only NUM lines per cell, 0 = all
;;          c - set default cutoff lines
;;          cNUM - set cutoff lines to NUM
;;        descend: Only entries below this entry
;; +      elide:   Ellipses are on (now always true)
;;          e - ellipses on 
;;        filter:  Regexp or filter program to select entries for view,
;;                 off=select non-matching entries
;;        glue:    Freeze any group of entries selected to stay at top of
;;                 window, off=freeze those not-in-group.
;;        include: Include an entry referenced by a link.
;; +      level:   Some levels are hidden.
;;          l - set default level clipping
;;          lNUM - set level clipping to NUM
;;        name:    Display leading names within cells.
;;          m  -  show names
;; +      number:  Cell numbers are on
;;          n  - set default labels
;;          n0 - display idstamp labels
;;          n1 - display alpha labels
;;          n2 - display partial alpha labels
;;          n. - display legal labels
;;          n* - display star labels
;;          n~ - turn off labels
;;        rest:    Only following cells.
;;        synthesize: Use a named generator function to generate entries for
;;                    view. 
;;        view:    Turn koutliner view mode on.  Standard insertion keys then
;;                 can be used for browsing and view setting.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'kview)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (defvar label-sep-len nil)
  (defvar modeline-format nil))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kvspec:current nil
  "String that represents the current view spec.
It is local to each koutline.  Nil value means it has not been set yet.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kvspec:activate (&optional view-spec)
  "Activate optional VIEW-SPEC or existing view spec in the current koutline.
VIEW-SPEC is a string or t, which means recompute the current view spec.  See
<${hyperb:dir}/kotl/EXAMPLE.kotl, 2b17=048> for details on valid view specs."
  (interactive (list (read-string "Set view spec: " kvspec:current)))
  (kotl-mode:is-p)
  (when (equal view-spec "")
    (setq view-spec nil))
  (kvspec:initialize)
  (kvspec:update view-spec)
  (kvspec:update-view))

(defun kvspec:initialize ()
  "Ensure that view spec settings will be local to the current buffer."
  (unless (local-variable-p 'kvspec:current (current-buffer))
    (make-local-variable 'kvspec:current)
    (make-local-variable 'kvspec:string)))

(defun kvspec:levels-to-show (levels-to-keep)
  "Hide all cells in outline at levels deeper than LEVELS-TO-KEEP (a number).
Shows any hidden cells within LEVELS-TO-KEEP.  1 is the first level.  0 means
display all levels of cells."
  (if (null levels-to-keep)
      (setq levels-to-keep
	    (read-from-minibuffer "Show cells down to level (0 = show all levels): "
				  nil nil t)))
  (setq levels-to-keep (prefix-numeric-value levels-to-keep))
  (if (< levels-to-keep 0)
      (error "(kvspec:levels-to-show): Must display at least one level."))
  (kview:map-tree
   (lambda (kview) 
     (if (/= (kcell-view:level) levels-to-keep)
	 (kotl-mode:show-tree)
       (kotl-mode:hide-subtree)
       ;; Move to last cell in hidden subtree, to skip further
       ;; processing of these cells.
       (if (kcell-view:next t)
	   (kcell-view:previous)
	 (goto-char (point-max)))))
   kview t)
  (kview:set-attr kview 'levels-to-show levels-to-keep))

(defun kvspec:show-lines-per-cell (num)
  "Show NUM lines per cell."
  (if (or (not (integerp num)) (< num 0))
      (error "(kvspec:show-lines-per-cell): Invalid lines per cell, `%d'" num))
  (kview:set-attr kview 'lines-to-show num)
  (if (not (zerop num))
      ;; Now show NUM lines in cells.
      (kview:map-tree (lambda (kview)
			(kcell-view:expand (point))
			(kvspec:show-lines-this-cell num)) kview t t)))

(defun kvspec:toggle-blank-lines ()
  "Toggle blank lines between cells on or off."
  (interactive)
  (setq kvspec:current
	(if (string-match "b" kvspec:current)
	    (hypb:replace-match-string "b" kvspec:current "" t)
	  (concat "b" kvspec:current)))
  (kvspec:blank-lines)
  (kvspec:update-modeline))

(defun kvspec:update (view-spec)
  "Update current view spec according to VIEW-SPEC but don't change the view.
VIEW-SPEC is a string or t, which means recompute the current view
spec.  A nil value of VIEW-SPEC updates the modeline viewspec display
to be current but does not recompute the viewspec itself.  See
<${hyperb:dir}/kotl/EXAMPLE.kotl, 3b18=048> for details on valid
view specs." 
  (cond ((stringp view-spec)
	 ;; Use given view-spec after removing extraneous characters.
	 (setq view-spec
	       (hypb:replace-match-string
		"[^.*~0-9abcdefgilnrsv]+" view-spec "" t))
	 (unless (string-match "e" view-spec)
	   ;; Force 'e' elide view spec if not there.
	   (setq view-spec
		 (if (string-match "\\([abcd]+\\)" view-spec)
		     (replace-match "\\1e" t nil view-spec)
		   (concat "e" view-spec))))
	 (setq kvspec:current view-spec))
	((or (eq view-spec t) (null kvspec:current))
	 (setq kvspec:current (kvspec:compute))))
  ;; Update display using current specs.
  (kvspec:update-modeline))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kvspec:blank-lines ()
  "Turn blank lines on or off according to `kvspec:current'."
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
      (if (string-match "b" kvspec:current)
	  ;; On
	  (progn (kview:set-attr kview 'blank-lines t)
		 (kproperty:remove (point-min) (point-max) '(invisible t)))
	;; Off
	(kview:set-attr kview 'blank-lines nil)
	(save-excursion
	  (goto-char (point-max))
	  (while (re-search-backward "[\n\r][\n\r]" nil t)
	    ;; Make blank lines invisible.
	    (kproperty:put (1+ (point)) (min (+ (point) 2) (point-max))
			   '(invisible t)))))
    (set-buffer-modified-p modified-p)))

(defun kvspec:compute ()
  "Compute and return current view spec string."
  (concat

   ;; a - Show all cells and cell lines.
   ;; Never compute this setting (use it only within links) since it will
   ;; expose all carefully hidden outline items if the user forgets to turn
   ;; it off when he resets the view specs.

   ;; b - blank separator lines
   (if (kview:get-attr kview 'blank-lines) "b")

   ;; c - cutoff lines per cell
   (let ((lines (kview:get-attr kview 'lines-to-show)))
     (if (zerop lines)
	 nil
       (concat "c" (int-to-string lines))))

   ;; e - ellipses on
   (if selective-display-ellipses "e")

   ;; l - hide some levels
   (let ((levels (kview:get-attr kview 'levels-to-show)))
     (if (zerop levels)
	 nil
       (concat "l" (int-to-string levels))))

   ;; n - numbering type
   (let ((type (kview:label-type kview)))
     (cond ((eq type 'no) nil)
	   ((eq type kview:default-label-type) "n")
	   (t (concat "n" (char-to-string
			   (car (rassq (kview:label-type kview)
				       kvspec:label-type-alist)))))))))

(defun kvspec:elide ()
  "Turn ellipses display following clipped cells on.  This cannot be turned off."
  (setq selective-display-ellipses t))

(defun kvspec:hide-levels ()
  "Show a set number of cell levels according to `kvspec:current'."
  ;; "l" means use value of kview:default-levels-to-show.
  ;; "l0" means show all levels.
  (let (levels)
    (if (not (string-match "l\\([0-9]+\\)?" kvspec:current))
	;; Don't change the view if no view spec is given but note that
	;; all levels should be shown in the future.
	(kview:set-attr kview 'levels-to-show 0)
      (if (match-beginning 1)
	  (setq levels (string-to-number (match-string 1 kvspec:current)))
	(setq levels kview:default-levels-to-show))
      (kvspec:levels-to-show levels))))

(defun kvspec:lines-to-show ()
  "Show a set number of lines per cell according to `kvspec:current'."
  ;; "c" means use value of kview:default-lines-to-show.
  ;; "c0" means show all lines.
  (cond ((not (string-match "c\\([0-9]+\\)?" kvspec:current))
	 ;; Don't change the view if no view spec is given but note that all
	 ;; lines should be shown in the future.
	 (kview:set-attr kview 'lines-to-show 0))
	((match-beginning 1)
	 (kvspec:show-lines-per-cell 
	  (string-to-number (match-string 1 kvspec:current))))
	(t (kvspec:show-lines-per-cell kview:default-lines-to-show))))

(defun kvspec:numbering ()
  "Set the type of numbering (label) display according to `kvspec:current'."
  (if (not (string-match "n\\([.*~0-2]\\)?" kvspec:current))
      nil
    ;; "n"  means use value of kview:default-label-type.
    ;; "n0" means display idstamps.
    ;; "n1" means display alpha labels.
    ;; "n2" means display partial alpha labels.
    ;; "n." means display legal labels.
    ;; "n*" means star labels.
    ;; "n~" means no labels.
    (let (spec type)
      (if (match-beginning 1)
	  (setq spec (string-to-char
		      (substring kvspec:current
				 (match-beginning 1) (match-end 1)))
		type (cdr (assq spec kvspec:label-type-alist)))
	(setq type kview:default-label-type))
      (kview:set-label-type kview type))))

(defun kvspec:show-lines-this-cell (num)
  "Assume the current cell is fully expanded and collapse to show NUM lines within it.
If NUM is greater than the number of lines available, the cell remains fully expanded."
  ;; Use free variable label-sep-len bound in kview:map-* for speed.
  (let ((start (goto-char (kcell-view:start (point) label-sep-len)))
	(end (kcell-view:end-contents)))
    ;; Hide all but num lines of the cell.
    (and (> num 0) (search-forward "\n" end t num)
	 (outline-flag-region (1- (point)) end t))))

(defun kvspec:update-modeline ()
  "Setup or update display of the current kview spec in the modeline."
  (if (stringp kvspec:current)
      (setq kvspec:string (format kvspec:string-format kvspec:current)))
  (if (memq 'kvspec:string mode-line-format)
      nil
    (setq mode-line-format (copy-sequence mode-line-format))
    (let ((elt (or (memq 'mode-line-buffer-identification mode-line-format)
		   (memq 'modeline-buffer-identification
			 mode-line-format))))
      (if elt
	  (setcdr elt (cons 'kvspec:string (cdr elt)))
	;;
	;; XEmacs 19.14 introduced extents into the modeline that we
	;; must work around.  Assume any XEmacs is at least that new.
	(if (featurep 'xemacs)
	    (let ((mf modeline-format)
		  elt)
	      (while mf
		(setq elt (car mf))
		(if (and (consp elt) (eq (cdr elt) 'modeline-buffer-identification))
		    (progn (setcdr mf (cons 'kvspec:string (cdr mf)))
			   (setq mf nil)))
		(setq mf (cdr mf)))))))))

(defun kvspec:update-view ()
  "Update view according to current setting of local `kvspec:current' variable."
  (let ((modified-p (buffer-modified-p))
	(buffer-read-only))
    (save-excursion

      (if (string-match "a" kvspec:current)
	  (kotl-mode:show-all))

      (kvspec:blank-lines) ;; b

      ;; This must come before kvspec:lines-to-show or else it could show
      ;; lines that should be hidden.
      (kvspec:hide-levels) ;; l

      (kvspec:lines-to-show) ;; c

      (if (string-match "d" kvspec:current)
	  nil)

      (kvspec:elide) ;; e

      (if (string-match "f" kvspec:current)
	  nil)

      (if (string-match "g" kvspec:current)
	  nil)

      (if (string-match "i" kvspec:current)
	  nil)

      (if (string-match "r" kvspec:current)
	  nil)

      (if (string-match "s" kvspec:current)
	  nil)

      ;; Do this last since it can trigger an error if partial alpha is
      ;; selected.
      (kvspec:numbering) ;; n

      )
    (set-buffer-modified-p modified-p)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst kvspec:label-type-alist
  '((?0 . id)
    (?1 . alpha)
    (?. . legal)
    ;; (?2 . partial-alpha)
    ;; (?* . star)
    ;; (?~ . no)
    )
  "Alist of (view-spec-character . label-type) pairs.")

(defvar kvspec:string ""
  "String displayed in koutline modelines to reflect the current view spec.
It is local to each koutline.  Set this to nil to disable modeline display of
the view spec settings.")

(defvar kvspec:string-format " <|%s>"
  "Format of the kview spec modeline display.
It must contain a `%s' which is replaced with the current set of view spec
characters at run-time.")

(provide 'kvspec)

;;; kvspec.el ends here
