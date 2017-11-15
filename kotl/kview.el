;;; kview.el --- Display handling of koutlines
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    6/30/93
;;
;; Copyright (C) 1993-2017  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Lisp Libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(klabel kfill hypb)))
;; Quiet byte compiler warnings for this free variable.
(eval-when-compile
  (defvar label-sep-len nil))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(set-default 'kview nil)

(defcustom kview:default-blank-lines t
  "*Default setting of whether to show blank lines between koutline cells.
A value of t means show them, nil means don't show them.  Default
value is t."
  :type 'boolean
  :group 'hyperbole-koutliner)

(defvar kview:default-levels-to-show 0
  "Default number of cell levels to show.  0, the default, means all levels.")

(defvar kview:default-lines-to-show 0
  "Default number of lines per cell to show.  0, the default, means all lines.")

(defcustom kview:default-label-min-width 4
  "*Minimum width to which to pad labels in a kotl view.
Labels are padded with spaces on the left.  Default value is 4."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (> value 1) (<= value 99))))
  :group 'hyperbole-koutliner)

(defcustom kview:default-label-separator ". "
  "*Default string of characters to insert between label and contents of a koutline cell.
Default value is \". \"."
  :type 'string
  :group 'hyperbole-koutliner)

(defcustom kview:default-label-type 'alpha
  "*Default label-type to use for new koutlines.  Default value is 'alpha.
It must be one of the following symbols:
  alpha           for `1b3' full alphanumeric labels
  id              for `027' permanent idstamp labels
  legal           for `1.2.3' legal-style labels"
  :type '(choice (const alpha)
		 (const id)
		 (const legal))
  :group 'hyperbole-koutliner)
;;  no              for no labels
;;  partial-alpha   for partial alphanumeric labels, e.g. `2' for node `1a2'
;;  star            for multi-star labeling, e.g. `***'.

(defcustom kview:default-level-indent 3
  "*Default number of spaces to indent each succeeding level in koutlines.
Default value is 3."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (> value 0) (<= value 60))))
  :group 'hyperbole-koutliner)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;
;;; kcell-view - Kcell view-specific functions
;;;

(defun kcell-view:backward (&optional visible-p label-sep-len)
  "Move to start of the prior cell at the same level as the current cell.
With optional VISIBLE-P, consider only visible cells.
Return t unless no such cell."
  (or label-sep-len (setq label-sep-len
			  (kview:label-separator-length kview)))
  (let ((opoint (point))
	(found) (done)
	(curr-indent 0)
	(start-indent (kcell-view:indent nil label-sep-len)))
    (while (and (not (or found done))
		(kcell-view:previous visible-p label-sep-len))
      (if (bobp)
	  (progn (setq done t)
		 (goto-char opoint))
	(setq curr-indent (kcell-view:indent nil label-sep-len))
	(cond ((= curr-indent start-indent)
	       (goto-char (kcell-view:start nil label-sep-len))
	       (setq found t))
	      ((< curr-indent start-indent)
	       ;; Went past start of this tree without a match.
	       (setq done t)
	       (goto-char opoint))
	      ;; else go to prior node
	      )))
    found))

(defun kcell-view:cell (&optional pos)
  "Return kcell at optional POS or point."
  (kproperty:get (kcell-view:plist-point pos) 'kcell))

(defun kcell-view:child (&optional visible-p label-sep-len)
  "Move to start of current cell's child.
With optional VISIBLE-P, consider only visible children.
Return t unless cell has no matching child.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents."
  (let* ((opoint (point))
	 (prev-indent (kcell-view:indent nil label-sep-len))
	 (next (kcell-view:next visible-p label-sep-len)))
    (or label-sep-len (setq label-sep-len
			    (kview:label-separator-length kview)))
    ;; Since kcell-view:next leaves point at the start of a cell, the cell's
    ;; indent is just the current-column of point.
    (if (and next (> (current-column) prev-indent))
	t
      ;; Move back to previous point and return nil.
      (goto-char opoint)
      nil)))

(defun kcell-view:child-p (&optional pos visible-p label-sep-len)
  "Return t if cell at optional POS or point has a child.
With optional VISIBLE-P, consider only visible children.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents."
  (save-excursion
    (if pos (goto-char pos))
    (kcell-view:child visible-p label-sep-len)))

(defun kcell-view:collapse (&optional pos label-sep-len)
  "Collapse cell at optional POS or point to a single line within the current view.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents."
  (save-excursion
    (goto-char (kcell-view:start pos label-sep-len))
    (kview:end-of-actual-line)
    (outline-flag-region (point) (kcell-view:end-contents) t)))

(defun kcell-view:collapsed-p (&optional pos label-sep-len)
  "Return t if cell at optional POS or point is collapsed within the current view.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents.

A cell may be collapsed yet still have some of its text visible.  Use
`kcell-view:invisible-p' to test for invisibility."
  (if (memq 'outline (mapcar (lambda (o) (overlay-get o 'invisible))
			     ;; Allow for empty cell
			     (overlays-in (1- (kcell-view:start pos label-sep-len))
					  (kcell-view:end-contents pos))))
      t))

(defun kcell-view:hide (&optional pos label-sep-len)
  "Make cell at optional POS or point invisible within the current view.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents."
  (kcell-view:expand pos label-sep-len t))

(defun kcell-view:invisible-p (&optional pos label-sep-len)
  "Return t if cell at optional POS or point is entirely invisible within the current view.
Optional LABEL-SEP-LEN is the length of the separation between
a cell's label and the start of its contents.

Any cell that is invisible is also collapsed as indicated by a call to
`kcell-view:collapsed-p'."
  (let ((start (1- (kcell-view:start pos label-sep-len))) ;; Allow for empty cell
	(end (kcell-view:end-contents pos))
	(invisible))
    (if (delq nil (mapcar (lambda (o)
			      (and (eq (overlay-get o 'invisible) 'outline)
				   (>= start (overlay-start o))
				   (<= end (overlay-end o))))
			  (overlays-in start end)))
	t)))

(defun kcell-view:contents (&optional pos)
  "Return contents of cell at optional POS or point."
  (save-excursion
    (if pos (goto-char pos))
    (let ((indent (kcell-view:indent))
	  (start (kcell-view:start))
	  (end (kcell-view:end-contents)))
      ;; Remove indentation from all but first line.
      (hypb:replace-match-string
       (concat "\\([\n\r]\\)" (make-string indent ?\ ))
       (buffer-substring start end) "\\1"))))

(defun kcell-view:create (kview cell level klabel &optional no-fill)
  "Insert into KVIEW at point, CELL at LEVEL (1 = first level) with KLABEL.
Optional NO-FILL non-nil suppresses filling of cell's contents upon insertion
or movement."
  (unless (zerop (kcell:idstamp cell))
    (or no-fill (setq no-fill (kcell:get-attr cell 'no-fill)))
    (let* ((label-min-width (kview:label-min-width kview))
	   (label-fmt (format "%%%ds" label-min-width))
	   (label (cond ((string-equal klabel "")
			 "")
			((and (string-equal klabel "0")
			      (setq klabel (klabel:format klabel))
			      ;; Fall through
			      nil))
			(t (format label-fmt klabel))))
	   (label-separator (if (string-equal klabel "") " "
			      (kview:label-separator kview)))
	   (mult-line-indent (* (1- level) (kview:level-indent kview)))
	   (thru-label (+ mult-line-indent label-min-width
			  (length label-separator)))
	   (old-point (point))
	   (fill-prefix (make-string thru-label ?\ ))
	   contents new-point)
      (if no-fill (kcell:set-attr cell 'no-fill t))
      (insert fill-prefix)
      (setq contents (kview:insert-contents cell nil no-fill fill-prefix))
      ;; Insert lines to separate cell from next.
      (insert (if (or no-fill (equal contents ""))
		  "\n\n" "\n"))
      (unless (kview:get-attr kview 'blank-lines)
	;; Make blank lines invisible.
	(kproperty:put (1- (point)) (min (point) (point-max))
		       '(invisible t)))
      (kfile:narrow-to-kcells)
      (setq new-point (point))
      (goto-char old-point)
      ;; Delete leading spaces used to get fill right in first cell
      ;; line.  Replace it with label.
      (delete-char thru-label)
      (insert (format (format "%%%ds" (- thru-label (length label-separator)))
		      label))
      (setq old-point (point))
      (insert label-separator)
      (goto-char old-point)
      ;; Add cell's attributes to the text property list at point.
      (kproperty:set 'kcell cell)
      (goto-char new-point))))

(defun kcell-view:end (&optional pos)
  "Return end position of cell from optional POS or point.
Includes blank lines following cell contents."
  (or pos (setq pos (point)))
  (save-excursion
    (or (re-search-forward "[\n\r][\n\r]" nil t)
	(point-max))))

(defun kcell-view:end-contents (&optional pos)
  "Return end position of cell contents from optional POS or point.
Excludes blank lines following cell contents."
  (save-excursion
    (if pos (goto-char pos))
    (goto-char (kcell-view:end))
    (skip-chars-backward "\n\r")
    (point)))

(defun kcell-view:expand (&optional pos label-sep-len hide-p)
  "Expand cell at optional POS or point within the current view."
  (save-excursion
    (goto-char (kcell-view:start pos label-sep-len))
    (outline-flag-region (point) (kcell-view:end-contents) hide-p)))

(defun kcell-view:forward (&optional visible-p label-sep-len)
  "Move to start of the following cell at the same level as the current cell.
With optional VISIBLE-P, consider only visible cells.
Return t unless no such cell."
  (or label-sep-len (setq label-sep-len
			  (kview:label-separator-length kview)))
  (let ((opoint (point))
	(found) (done)
	(curr-indent 0)
	(start-indent (kcell-view:indent nil label-sep-len)))
    (while (and (not (or found done))
		(kcell-view:next visible-p label-sep-len))
      (setq curr-indent (kcell-view:indent nil label-sep-len))
      (cond ((= curr-indent start-indent)
	     (goto-char (kcell-view:start nil label-sep-len))
	     (setq found t))
	    ((< curr-indent start-indent)
	     ;; Went past end of this tree without a match.
	     (setq done t)
	     (goto-char opoint))
	    ;; else go to following node
	    ))
    ;; If didn't find a match, return to original point.
    (or found (goto-char opoint))
    found))

(defun kcell-view:get-attr (attribute &optional pos)
  "Return ATTRIBUTE's value for current cell or cell at optional POS."
  (save-excursion
    (if pos (goto-char pos))
    (kcell:get-attr (kcell-view:cell) attribute)))

(defun kcell-view:idstamp (&optional pos)
  "Return idstamp string of cell at optional POS or point."
  (save-excursion
    (if pos (goto-char pos))
    (format "0%s" (or (kcell:idstamp (kcell-view:cell)) ""))))

(defun kcell-view:indent (&optional pos label-sep-len)
  "Return indentation of cell at optional POS or point.
Optional LABEL-SEP-LEN is the view-specific length of the separator between a
cell's label and the start of its contents."
  (+ (save-excursion
       (kcell-view:to-label-end pos)
       (current-column))
     (or label-sep-len (kview:label-separator-length kview))))

(defun kcell-view:label (&optional pos)
  "Return displayed label string of cell at optional POS or point.
If labels are off, return cell's idstamp as a string."
  (save-excursion
    (if pos (goto-char pos))
    (let ((label-type (kview:label-type kview)))
      (if (eq label-type 'no)
	  (kcell-view:idstamp)
	(kcell-view:to-label-end)
	(buffer-substring (point) (progn (skip-chars-backward "^ \t\n\r")
					 (point)))))))

(defun kcell-view:level (&optional pos label-sep-len indent)
  "Return the outline level of the current cell or the one at optional POS.
0 = top cell level, 1 = 1st level of the outline.  Optional
LABEL-SEP-LEN is the number of spaces between a cell label and
the start of its body.  Optional INDENT is the indentation in
characters of the cell whose level is desired."
  (or label-sep-len (setq label-sep-len (kview:label-separator-length kview)))
  (/ (- (or indent (kcell-view:indent pos label-sep-len)) label-sep-len)
     (kview:level-indent kview)))

(defun kcell-view:line (&optional pos)
  "Return contents of cell line at point or optional POS as a string."
  (save-excursion
    (if pos (goto-char pos))
    (if (kview:valid-position-p)
	(buffer-substring (kotl-mode:to-start-of-line) (kotl-mode:to-end-of-line))
      (error "(kcell-view:line): Invalid position, `%d'" (point)))))

(defun kcell-view:lines-visible ()
  "Return the number of lines visible within the current cell."
  ;; Use free variable label-sep-len bound in kview:map-* for speed.
  (if (kcell-view:invisible-p)
      0
    (let* ((start (kcell-view:start nil (kview:label-separator-length kview)))
	   (end (kview:first-invisible-point start)))
      ;; Prevent bounds error with empty cells that have hidden subtrees.
      (max 1 (count-lines start end)))))

(defun kcell-view:next (&optional visible-p label-sep-len)
  "Move to start of next cell within current view.
With optional VISIBLE-P, consider only visible cells.
Return t unless no next cell."
  (if (kcell-view:next-kcell visible-p label-sep-len)
      (progn (goto-char (kcell-view:start nil label-sep-len))
	     t)))

(defun kcell-view:operate (function &optional start end)
  "Invoke FUNCTION with view restricted to current cell contents.
Optional START and END are start and endpoints of cell to use."
  (save-restriction
    (narrow-to-region (or start (kcell-view:start))
		      (or end (kcell-view:end-contents)))
    (funcall function)))

(defun kcell-view:parent (&optional visible-p label-sep-len)
  "Move to start of current cell's parent within current view.
If parent is top cell, move to first cell within view and return 0.
Otherwise, return t unless optional VISIBLE-P is non-nil and the parent cell
is not part of the current view, else nil."
  (or label-sep-len (setq label-sep-len (kview:label-separator-length kview)))
  (let ((opoint (point))
	(parent-level (1- (kcell-view:level nil label-sep-len))))
    (if (= parent-level 0) ;; top cell
	(progn (goto-char (point-min))
	       (goto-char (kcell-view:start nil label-sep-len))
	       0)
      ;; Skip from point back past any siblings
      (while (kcell-view:backward visible-p label-sep-len))
      ;; Move back to parent.
      (if (kcell-view:previous visible-p label-sep-len)
	  t
	;; Move back to previous point and return nil.
	(goto-char opoint)
	nil))))

(defun kcell-view:previous (&optional visible-p label-sep-len)
  "Move to start of previous cell within current view.
With optional VISIBLE-P, consider only visible cells.
Return t unless no previous cell."
  (if (kcell-view:previous-kcell visible-p label-sep-len)
      (progn (goto-char (kcell-view:start nil label-sep-len))
	     t)))

(defun kcell-view:plist (&optional pos)
  "Return attributes associated with cell at optional POS or point."
  (kcell:plist (kcell-view:cell pos)))

(defun kcell-view:plist-point (&optional pos)
  "Return buffer position of attributes associated with cell at optional POS or point."
  (save-excursion (1+ (kcell-view:to-label-end pos))))

(defun kcell-view:to-label-end (&optional pos)
  "Move point from optional POS to the end of the current cell's label (before the label separator) and return point.
If between kcells, move to the previous one.  The current cell may be hidden."
  (if pos (goto-char pos))
  (kview:end-of-actual-line)
  (cond ((null kview)
	 (error "(kcell-view:to-label-end): Invalid kview; try {M-x kotl-mode RET} to fix it."))
	(t
	 (let ((found))
	   (if (not (setq found (kproperty:get (1- (point)) 'kcell)))
	       ;; If not at beginning of cell contents, move there.
	       (goto-char (kproperty:previous-single-change (point) 'kcell)))
	   ;; Then move to the end of the label (prior to label
	   ;; separator) via embedded kcell property.
	   (goto-char (setq found (kproperty:previous-single-change (point) 'kcell)))
	   (if found
	       (point)
	     (error "(kcell-view:to-label-end): Can't find end of current cell's label"))))))

(defun kcell-view:reference (&optional pos relative-dir)
  "Return a reference to the kcell at optional POS or point for use in a link.
The reference is a string of the form, \"<kcell-file, cell-ref>\" where
cell-ref is as described in the documentation for `kcell:ref-to-id'.
Kcell-file is made relative to optional RELATIVE-DIR before it is returned."
  (format "<%s, %s=%s>" (hpath:relative-to buffer-file-name relative-dir)
	  (kcell-view:label pos) (kcell-view:idstamp pos)))

(defun kcell-view:remove-attr (attribute &optional pos)
  "Remove ATTRIBUTE, if any, for current cell or cell at optional POS."
  (interactive "*SAttribute to remove: ")
  (save-excursion
    (if pos (goto-char pos))
    (let ((kcell (kcell:remove-attr (kcell-view:cell) attribute)))
      (if (called-interactively-p 'interactive)
	  (message "Cell <%s> now has no %s attribute."
		   (kcell-view:label) attribute))
      kcell)))

(defun kcell-view:set-attr (attribute value &optional pos)
  "Set ATTRIBUTE's VALUE for current cell or cell at optional POS and return the cell."
  (save-excursion
    (if pos (goto-char pos))
    ;; Returns kcell.
    (kcell:set-attr (kcell-view:cell) attribute value)))

(defun kcell-view:set-cell (kcell)
  "Attach KCELL property to cell at point."
  (save-excursion
    (kcell-view:to-label-end)
    (kproperty:set 'kcell kcell)))

(defun kcell-view:sibling-p (&optional pos visible-p label-sep-len)
  "Return t if cell at optional POS or point has a successor.
With optional VISIBLE-P, consider only visible siblings."
  (save-excursion
    (if pos (goto-char pos))
    (kcell-view:forward visible-p label-sep-len)))

(defun kcell-view:start (&optional pos label-sep-len)
  "Return start position of visible cell contents from optional POS or point."
  (save-excursion
    (+ (kcell-view:to-label-end pos)
       (or label-sep-len (kview:label-separator-length kview)))))

(defun kcell-view:to-visible-label-end (&optional pos)
  "Move point to the end of the current visible cell's label (before the label separator).
If between kcells, move to the previous one."
  (if pos (goto-char pos))
  ;; Ensure point is within a visible part of the current cell, not
  ;; within some collapsed sub-cell.
  (beginning-of-line)
  (end-of-visible-line)
  (if (setq pos (kproperty:previous-single-change (point) 'kcell))
      (goto-char pos))
  ;; Now move to end of label via embedded kcell property.
  (goto-char (kproperty:previous-single-change (point) 'kcell)))

(defun kcell-view:visible-label (&optional pos)
  "Return the first visible label string of cell preceding optional POS or point.
If labels are off, return cell's idstamp as a string."
  (save-excursion
    ;; Next line ensures point is in the root of the current tree if
    ;; the tree is at all hidden.
    (kotl-mode:to-start-of-line)
    (kcell-view:label pos)))

;;;
;;; kview - one view per buffer, multiple views per kotl
;;;

(defun kview:add-cell (klabel level &optional contents prop-list no-fill)
  "Create a new cell with full KLABEL and add it at point at LEVEL within outline.
Optional cell CONTENTS and PROP-LIST may also be given, as well
as NO-FILL which skips filling of any CONTENTS.  Return new cell.
This function does not renumber any other cells.  1 = first
level."
  (let* ((idstamp (if (klabel:idstamp-p klabel)
		      (if (stringp klabel) (string-to-number klabel) klabel)
		    (kview:id-increment kview)))
	 (new-cell (kcell:create contents idstamp prop-list)))
    (kcell-view:create kview new-cell level klabel no-fill)
    new-cell))

(defun kview:beginning-of-actual-line ()
  "Go to the beginning of the current line whether collapsed or not."
  (if (re-search-backward "[\n\r]" nil 'move)
      (forward-char 1)))

(defun kview:buffer (kview)
  "Return kview's buffer or nil if argument is not a kview."
  (if (kview:is-p kview)
      (kview:get-attr kview 'view-buffer)))

(defun kview:char-invisible-p (&optional pos)
  "Return t if the character after point is invisible/hidden, else nil."
  (or pos (setq pos (point)))
  (when (or (kproperty:get pos 'invisible)
	    (delq nil (mapcar (lambda (o) (overlay-get o 'invisible))
			      (overlays-at (or pos (point))))))
    t))

(defun kview:char-visible-p (&optional pos)
  "Return t if the character after point is visible, else nil."
  (or pos (setq pos (point)))
  (and (not (kproperty:get pos 'invisible))
       (not (delq nil (mapcar (lambda (o) (overlay-get o 'invisible))
			      (overlays-at (or pos (point))))))))

(defun kview:create (buffer-name
			 &optional id-counter label-type level-indent
			 label-separator label-min-width blank-lines
			 levels-to-show lines-to-show)
  "Return a new kview for BUFFER-NAME.
Optional ID-COUNTER is the maximum permanent id previously given out in this
outline.  Optional LABEL-TYPE, LEVEL-INDENT, LABEL-SEPARATOR, LABEL-MIN-WIDTH,
BLANK-LINES, LEVELS-TO-SHOW, and LINES-TO-SHOW may also be given, otherwise default values are used.

  See documentation of:
 `kview:default-label-type' for LABEL-TYPE,
 `kview:default-level-indent' for LEVEL-INDENT,
 `kview:default-label-separator' for LABEL-SEPARATOR,
 `kview:default-label-min-width' for LABEL-MIN-WIDTH,
 `kview:default-blank-lines' for BLANK-LINES,
 `kview:default-levels-to-show' for LEVELS-TO-SHOW,
 `kview:default-lines-to-show' for LINES-TO-SHOW."

  (let ((buf (get-buffer buffer-name)))
    (cond ((null buf)
	   (error "(kview:create): No such buffer, `%s'." buffer-name))
	  ((or (null id-counter) (= id-counter 0))
	   (setq id-counter 0))
	  ((not (integerp id-counter))
	   (error "(kview:create): 2nd arg, `%s', must be an integer." id-counter)))
    (set-buffer buf)
    ;; Don't recreate view if it exists.
    (unless (and (boundp 'kview) (kview:is-p kview) (eq (kview:buffer kview) buf))
      (make-local-variable 'kview)
      (setq kview
	    (list 'kview 'plist
		  (list 'view-buffer (current-buffer)
			'top-cell
			(kcell:create-top buffer-file-name id-counter)
			'label-type (or label-type kview:default-label-type)
			'label-min-width (or label-min-width
					     kview:default-label-min-width)
			'label-separator (or label-separator
					     kview:default-label-separator)
			'label-separator-length
			(length (or label-separator
				    kview:default-label-separator))
			'level-indent (or level-indent
					  kview:default-level-indent)
			'blank-lines
			(or blank-lines kview:default-blank-lines)
			'levels-to-show
			(or levels-to-show kview:default-levels-to-show)
			'lines-to-show
			(or lines-to-show kview:default-lines-to-show))))
      (kview:set-functions (or label-type kview:default-label-type)))
    kview))

(defun kview:delete-region (start end)
  "Delete cells between START and END points from current view."
  (delete-region start end))

(defun kview:end-of-actual-line ()
  "Go to the end of the current line whether collapsed or not."
  (if (re-search-forward "[\n\r]" nil 'move)
      (backward-char 1)))

(defun kview:fill-region (start end &optional kcell justify)
  "Fill region between START and END within current view.
With optional KCELL, assume START and END delimit that cell's contents.
With optional JUSTIFY, justify region as well.
Fill-prefix must be a string of spaces the length of this cell's indent, when
this function is called."
  (let ((opoint (set-marker (make-marker) (point)))
	(label-sep-len (kview:label-separator-length kview))
	(continue t)
	prev-point)
    (goto-char start)
    (while continue
      (if (kcell:get-attr (or kcell (kcell-view:cell)) 'no-fill)
	  (setq continue (kcell-view:next nil label-sep-len))
	(fill-paragraph justify t)
	(setq prev-point (point))
	(forward-paragraph)
	(re-search-forward "[^ \t\n\r]" nil t))
      (setq continue (and continue
			  (/= (point) prev-point)
			  (< (point) (min end (point-max))))))
    ;; Return to original point.
    (goto-char opoint)
    (set-marker opoint nil)))

(defun kview:first-invisible-point (&optional pos)
  "Return the first point following optional POS that is followed by an invisible character, else the end point of the cell contents.
Value may be the character immediately after point."
  (or pos (setq pos (point)))
  (let ((end (kcell-view:end-contents pos)))
    (while (and pos (< pos end) (kview:char-visible-p pos))
      (if (kproperty:get pos 'invisible)
	  (setq pos (kproperty:next-single-change pos 'invisible nil end))
	(let ((overlay (car (delq nil (mapcar (lambda (o) (if (overlay-get o 'invisible) o))
					      (overlays-at pos))))))
	  (setq pos (if overlay (overlay-end overlay) (1+ pos))))))
    (or pos end)))

(defun kview:first-visible-point (&optional pos)
  "Return the first point following optional POS that is followed by a visible character, else (point-max).
Value may be the character immediately after point."
  (or pos (setq pos (point)))
  (while (and pos (kview:char-invisible-p pos))
    (if (kproperty:get pos 'invisible)
	(setq pos (kproperty:next-single-change pos 'invisible))
      (let ((overlay (car (delq nil (mapcar (lambda (o) (if (overlay-get o 'invisible) o))
					    (overlays-at pos))))))
	(setq pos (overlay-end overlay)))))
  (or pos (point-max)))

(defun kview:get-cells-status (kview start end)
  "In the current buffer's KVIEW, between START and END, return a coded list of status of each visible cell.
Status is returned as: 0 if all the lines of the cell are visible and
it has no hidden branches; a positive count of the lines displayed in
the cell if it has no hidden branches; otherwise, a negative count of
the lines displayed, since it has hidden branches."
  ;; Process only visible cells and note when one contains invisible
  ;; subcells, indicating its branches are hidden.
  (kview:map-region
   (lambda ()
     (cond ((kcell-view:next-invisible-p (point) label-sep-len)
	    ;; Skip to end of this subtree
	    (prog1 (- (kcell-view:lines-visible))
	      (goto-char (kotl-mode:tree-end t))))
	   ((kcell-view:collapsed-p (point) label-sep-len)
	    (kcell-view:lines-visible))
	   (t 0)))
   kview t start end))

(defun kcell-view:next-invisible-p (&optional pos label-sep-len)
  "Return t if there is a next cell after optional POS or point and it is invisible."
  (save-excursion (and (kcell-view:next nil label-sep-len)
		       (kcell-view:invisible-p (point) label-sep-len))))

(cond (hyperb:emacs-p
       (defun kview:goto-cell-id (id-string)
	 "Move point to start of cell with idstamp ID-STRING and return t, else nil."
	 (let ((cell-id (string-to-number id-string))
	       (opoint (point))
	       pos kcell)
	   (goto-char (point-min))
	   (while (and (setq pos (kproperty:next-single-change (point) 'kcell))
		       (goto-char pos)
		       (or (null (setq kcell (kproperty:get pos 'kcell)))
			   (/= (kcell:idstamp kcell) cell-id))
		       ;; Skip to the end of this kcell property
		       (setq pos (kproperty:next-single-change (point) 'kcell))
		       (goto-char pos)))
	   (if pos
	       (progn
		 (forward-char (kview:label-separator-length kview))
		 t)
	     (goto-char opoint)
	     nil))))
      ;;
      ;; XEmacs
      (t (defun kview:goto-cell-id (id-string)
	   "Move point to start of cell with idstamp ID-STRING and return t, else nil."
	   (let ((cell-id (string-to-number id-string))
		 label-end kcell)
	     (setq label-end
		   (map-extents
		    (lambda (extent unused)
		      (setq kcell (extent-property extent 'kcell))
		      (and kcell (= (kcell:idstamp kcell) cell-id)
			   (extent-end-position extent)))
		    nil nil nil nil nil 'kcell))
	     (if (null label-end)
		 nil
	       (goto-char label-end)
	       t)))))

(defun kview:id-counter (kview)
  "Return the highest current idstamp (an integer) used by KVIEW."
  (kcell:get-attr (kview:get-attr kview 'top-cell) 'id-counter))

(defun kview:id-increment (kview)
  "Return next idstamp (an integer) for KVIEW."
  (let ((counter (1+ (kview:id-counter kview))))
    (kcell:set-attr (kview:get-attr kview 'top-cell) 'id-counter counter)
    counter))

(defun kview:idstamp-to-label (permanent-id)
  "Return relative label for cell with PERMANENT-ID within current kview."
  (save-excursion
    (if (kotl-mode:goto-cell permanent-id)
	(kcell-view:label))))

(defun kview:insert-contents (kcell contents no-fill fill-prefix)
  "Insert KCELL's CONTENTS into view at point and fill resulting paragraphs, unless NO-FILL is non-nil.
FILL-PREFIX is the indentation string for the current cell.
If CONTENTS is nil, get contents from KCELL.  Return contents inserted (this
value may differ from the value passed in.)"
  (let ((start (point))
	end)
    (setq contents (or contents (kcell:contents kcell) ""))
    (insert contents)
    ;;
    ;; Delete any extra newlines at end of cell contents.
    (setq end (point))
    (skip-chars-backward "\n\r")
    (delete-region (point) end)
    (setq end (point))
    ;;
    (save-restriction
      (if no-fill
	  ;; Insert proper indent in all but the first line which has
	  ;; already been indented.
	  (progn
	    (narrow-to-region start end)
	    (goto-char (point-min))
	    (while (re-search-forward "[\n\r]" nil t)
	      (insert fill-prefix))
	    (goto-char (point-max)))
	;;
	;; Filling cell will insert proper indent on all lines.
	(if (equal contents "")
	    nil
	  (goto-char start)
	  (beginning-of-line)
	  (narrow-to-region (point) end)
	  ;; Add fill-prefix to all but paragraph separator lines, so
	  ;; filling is done properly.
	  (while (re-search-forward "[\n\r][^\n\r]" nil t)
	    (forward-char -1) (insert fill-prefix))
	  (kview:fill-region start end kcell)
	  (goto-char (point-min))
	  ;; Now add fill-prefix to paragraph separator lines.
	  (while (re-search-forward "[\n\r][\n\r]" nil t)
	    (forward-char -1) (insert fill-prefix))
	  ;;
	  (goto-char (point-max))))))
  contents)

(defun kview:is-p (object)
  "Is OBJECT a kview?"
  (if (listp object) (eq (car object) 'kview)))

(defun kview:kotl (kview)
  "Return kview's kotl object or nil if argument is not a kview."
  (if (kview:is-p kview)
      (kview:get-attr kview 'kotl)))

(defun kview:label (klabel-function prev-label child-p)
  "Return label string to display for current cell computed from KLABEL-FUNCTION, PREV-LABEL and CHILD-P."
  (funcall klabel-function prev-label child-p))

(defun kview:label-function (kview)
  "Return function which will return display label for current cell in KVIEW.
Function signature is: (func prev-label &optional child-p), where prev-label
is the display label of the cell preceding the current one and child-p is
non-nil if cell is to be the child of the preceding cell."
  (kview:get-attr kview 'label-function))

(defun kview:label-min-width (kview)
  "Return kview's label-min-width setting or nil if argument is not a kview.
See documentation for kview:default-label-min-width."
  (if (kview:is-p kview)
      (kview:get-attr kview 'label-min-width)))

(defun kview:label-separator (kview)
  "Return kview's label-separator setting or nil if argument is not a kview.
See documentation for kview:default-label-separator."
  (if (kview:is-p kview)
      (kview:get-attr kview 'label-separator)))

(defun kview:label-separator-length (kview)
  "Return kview's label-separator length or nil if argument is not a kview.
See documentation for kview:default-label-separator."
  (kview:get-attr kview 'label-separator-length))

(defun kview:label-type (kview)
  "Return kview's label-type setting or nil if argument is not a kview.
See documentation for kview:default-label-type."
  (if (kview:is-p kview)
      (kview:get-attr kview 'label-type)))

(defun kview:level-indent (kview)
  "Return kview's level-indent setting or nil if argument is not a kview.
See documentation for kview:default-level-indent."
  (if (kview:is-p kview)
      (kview:get-attr kview 'level-indent)))

(defun kview:map-branch (func kview &optional first-p visible-p)
  "Applies FUNC to the sibling trees from point forward within KVIEW and returns results as a list.
With optional FIRST-P non-nil, begins with first sibling in current branch.
With optional VISIBLE-P, considers only those sibling cells that are visible
in the view.

FUNC should take one argument, the kview local variable of the current
buffer or some other kview, and should operate upon the cell at point.

`Cell-indent' contains the indentation value of the first cell mapped when
FUNC is called so that it may test against this value.  `Label-sep-len'
contains the label separator length.

See also `kview:map-region', `kview:map-siblings' and `kview:map-tree'."
    (with-current-buffer (kview:buffer kview)
      (save-excursion
	(let ((results)
	      (label-sep-len (kview:label-separator-length kview))
	      cell-indent)
	  (if first-p
	      ;; Move back to first predecessor at same level.
	      (while (kcell-view:backward t label-sep-len)))
	  (setq cell-indent (kcell-view:indent nil label-sep-len))
	  ;; Terminate when no further cells or when reach a cell at an equal
	  ;; or higher level in the kotl than the first cell that we processed.
	  (while (and (setq results (cons (funcall func kview) results))
		      (kcell-view:next visible-p label-sep-len)
		      (> (kcell-view:indent nil label-sep-len) cell-indent)))
	  (nreverse results)))))

(defun kview:map-region (func kview &optional visible-p start end)
  "Applies FUNC to each cell in the region within KVIEW and returns results as a list.
With optional VISIBLE-P, considers only those cells that are visible
in the view.  With optional START and END positions, uses these rather
than the bounds of the active region.

FUNC should take no arguments and should operate upon the cell at point.

`Label-sep-len’ contains the label separator length.

See also `kview:map-tree', `kview:map-branch’, and ‘kview:map-siblings’."
  (with-current-buffer (kview:buffer kview)
    (save-excursion
      (let* ((results)
	     (label-sep-len (kview:label-separator-length kview))
	     (bounds-given (and (or (integerp start) (markerp start))
				(or (integerp end) (markerp end))
				(<= start end)))
	     map-end)
	(and (not bounds-given)
	     (region-active-p)
	     (setq start (region-beginning)
		   map-end (kcell-view:end-contents (region-end))
		   end (progn (goto-char map-end)
			      (if visible-p
				  (kotl-mode:to-visible-position t)
				map-end))
		   bounds-given t))
	(cond (bounds-given
	       (goto-char start)
	       (if visible-p
		   (kotl-mode:to-visible-position)
		 (kotl-mode:to-valid-position))
	       ;; Terminate when no further cells within the region.
	       (while (and (setq results (cons (funcall func) results))
			   (kcell-view:next visible-p label-sep-len)
			   (<= (point) end)))
	       (nreverse results))
	      (t (error "(kview:map-region): No region or invalid start and end positions")))))))

(defun kview:map-siblings (func kview &optional first-p visible-p)
  "Applies FUNC to the sibling cells from point forward within KVIEW and returns results as a list.
With optional FIRST-P non-nil, begins with first sibling in current branch.
With optional VISIBLE-P, considers only those sibling cells that are visible
in the view.

FUNC should take one argument, the kview local variable of the current
buffer or some other kview, and should operate upon the cell at point.

`Cell-indent' contains the indentation value of the first cell mapped when
FUNC is called so that it may test against this value.  `Label-sep-len'
contains the label separator length.

See also `kview:map-branch' and `kview:map-tree'."
    (with-current-buffer (kview:buffer kview)
      (save-excursion
	(let ((results)
	      (label-sep-len (kview:label-separator-length kview))
	      cell-indent)
	  ;; Next line ensures point is in the root of the current tree if
	  ;; the tree is at all hidden.
	  (if visible-p (kotl-mode:to-start-of-line))
	  (if first-p
	      ;; Move back to first predecessor at same level.
	      (while (kcell-view:backward t label-sep-len)))
	  (setq cell-indent (kcell-view:indent nil label-sep-len))
	  ;; Terminate when no further cells at same level.
	  (while (progn (setq results (cons (funcall func kview) results))
			(kcell-view:forward visible-p label-sep-len)))
	  (nreverse results)))))

(defun kview:map-expanded-tree (func kview &optional top-p)
  "Temporarily expands the tree at point, applies FUNC to the tree in the KVIEW and returns results as a list.
This is for a FUNC that requires all cells in the tree be fully visible and
expanded before operating upon it.  If this is not the case, use
`kview:map-tree' instead.  FUNC may not change the number of or the order of
the cells. 

With optional TOP-P non-nil, maps over all of kview's cells.

FUNC should take one argument, the kview with the tree to map, and should
operate upon the cell at point.

`Cell-indent' contains the indentation value of the first cell mapped when
FUNC is called so that it may test against this value.  `Label-sep-len'
contains the label separator length.

See also `kview:map-region', `kview:map-branch' and `kview:map-siblings'."
    (with-current-buffer (kview:buffer kview)
      (save-excursion
	;; Next line ensures point is in the root of the current tree if
	;; the tree is at all hidden.
	(unless top-p (kotl-mode:to-start-of-line))
	(let* ((results)
	       (label-sep-len (kview:label-separator-length kview))
	       (start (set-marker (make-marker) (if top-p (point-min) (point))))
	       (end (set-marker (make-marker) (if top-p (point-max) (save-excursion (kotl-mode:tree-end)))))
	       (collapsed-cells (kview:get-cells-status kview start end))
	       cell-indent)
	  ;;
	  ;; Expand all cells in tree.
	  (outline-flag-region start end nil)
	  ;;
	  (if top-p
	      (progn (goto-char (point-min))
		     (kview:end-of-actual-line)
		     ;; Terminate when no further cells to process.
		     (while (progn (setq results (cons (funcall func kview) results))
				   (kcell-view:next nil label-sep-len))))
	    (setq cell-indent (kcell-view:indent nil label-sep-len))
	    ;; Terminate when no further cells or when reach a cell at an equal
	    ;; or higher level in the kotl than the first cell that we processed.
	    (while (and (setq results (cons (funcall func kview) results))
			(kcell-view:next nil label-sep-len)
			(> (kcell-view:indent nil label-sep-len)
			   cell-indent))))
	  ;;
	  ;; Restore status of temporarily expanded cells.
	  (if (remq 0 collapsed-cells)
	      (kview:set-cells-status kview start end collapsed-cells))
	  ;;
	  ;; Remove markers.
	  (set-marker start nil)
	  (set-marker end nil)
	  (nreverse results)))))

(defun kview:map-tree (func kview &optional top-p visible-p)
  "Applies FUNC to the tree starting at point within KVIEW and returns results as a list.
With optional TOP-P non-nil, maps over all of kview's cells.
With optional VISIBLE-P, considers only those cells that are visible in the
view.

FUNC should take one argument, the kview with the tree to map, and
should operate upon the cell at point.

`Cell-indent' contains the indentation value of the first cell mapped when
FUNC is called so that it may test against this value.  `Label-sep-len'
contains the label separator length.

See also `kview:map-region', `kview:map-branch' and `kview:map-siblings'."
    (with-current-buffer (kview:buffer kview)
      (save-excursion
	(let ((results)
	      (label-sep-len (kview:label-separator-length kview))
	      cell-indent)
	  (if top-p
	      (progn (goto-char (point-min))
		     (kview:end-of-actual-line)
		     ;; Terminate when no further cells to process.
		     (while (progn (setq results (cons (funcall func kview) results))
				   (kcell-view:next visible-p label-sep-len))))
	    ;; Next line ensures point is in the root of the current tree if
	    ;; the tree is at all hidden.
	    (kotl-mode:to-start-of-line)
	    (setq cell-indent (kcell-view:indent nil label-sep-len))
	    ;; Terminate when no further cells or when reach a cell at an equal
	    ;; or higher level in the kotl than the first cell that we processed.
	    (while (and (setq results (cons (funcall func kview) results))
			(kcell-view:next visible-p label-sep-len)
			(> (kcell-view:indent nil label-sep-len)
			   cell-indent))))
	  (nreverse results)))))

(defun kview:move (from-start from-end to-start from-indent to-indent
	           &optional copy-p fill-p)
  "Move tree between FROM-START and FROM-END to TO-START, changing FROM-INDENT to TO-INDENT.
Copy tree if optional COPY-P is non-nil.  Refill cells if optional
FILL-P is non-nil.  Leave point at TO-START."
  (let ((region (buffer-substring from-start from-end))
	(new-start (set-marker (make-marker) to-start))
	(collapsed-cells (kview:get-cells-status kview from-start from-end))
	expr new-end space)

    ;;
    ;; Move or copy tree region to new location.
    (or copy-p (delete-region from-start from-end))
    (goto-char new-start)
    (insert region)
    (setq new-end (point))
    ;;
    ;; Change indentation of tree cells.
    (if (/= from-indent to-indent)
	(save-restriction
	  (narrow-to-region new-start new-end)
	  ;; Expand all cells in the region to move.
	  (outline-flag-region new-start new-end nil)
	  ;;
	  (goto-char (point-min))
	  (if (< from-indent to-indent)
	      ;; Add indent
	      (progn
		(setq expr (make-string (1+ (- to-indent from-indent)) ?\ ))
		(while (re-search-forward "^ " nil t)
		  (replace-match expr t t)
		  (kfill:forward-line 1)))
	    ;; Reduce indent in all but first cell lines.
	    (setq expr (concat "^" (make-string (- from-indent to-indent) ?\ )))
	    (while (re-search-forward expr nil t)
	      (replace-match "" t t)
	      (kfill:forward-line 1))
	    ;; Reduce indent in first cell lines which may have an
	    ;; autonumber or other cell delimiter.
	    (setq space (- from-indent to-indent
			   (kview:label-separator-length kview)
			   1))
	    (unless (zerop space)
	      (setq expr (concat "^" (make-string
				      (- from-indent to-indent
					 (kview:label-separator-length kview)
					 1)
				      ?\ )))
	      (kview:map-tree
	       (lambda (view)
		 (save-excursion
		   (beginning-of-line)
		   (if (looking-at expr)
		       (replace-match "" t t))))
	       kview t)))
	  ;;
	  (if fill-p
	      ;; Refill cells lacking no-fill attribute.
	      (kview:map-tree (lambda (view) (kotl-mode:fill-cell nil t))
			      kview t))))
    ;;
    (goto-char new-start)
    ;;
    ;; Restore status of temporarily expanded cells.
    (if (remq 0 collapsed-cells)
	(kview:set-cells-status kview new-start new-end collapsed-cells))
    ;;
    ;; Delete temporary markers.
    (set-marker new-start nil)))

(defun kview:previous-visible-point (&optional pos)
  "Return the first point preceding optional POS that is followed by a visible character, else (point-min).
Value may be the character immediately after point."
  (or pos (setq pos (point)))
  (setq pos (1- pos))
  (while (and pos (kview:char-invisible-p pos))
    (if (kproperty:get pos 'invisible)
	(progn (setq pos (kproperty:previous-single-change pos 'invisible))
	       (if pos (setq pos (1- pos))))
      (let ((overlay (car (delq nil (mapcar (lambda (o) (if (overlay-get o 'invisible) o))
					    (overlays-at pos))))))
	(setq pos (1- (overlay-start overlay))))))
  (or pos (point-max)))

(defun kview:set-buffer (kview new-buf)
  "Set KVIEW's buffer to NEW-BUF."
  (if (kview:is-p kview)
      (save-excursion
	(let ((buf (kview:buffer kview)))
	  (if buf (set-buffer buf)))
	(kview:set-attr kview 'view-buffer new-buf))
    (error "(kview:set-buffer): Invalid kview argument")))

(defun kview:set-cells-status (kview start end cell-status-list)
  "In the current buffer's KVIEW, between START and END, use CELL-STATUS-LIST to restore each cell's status.
Status is: 0 if all the lines of the cell are visible and it has no
hidden branches; a positive count of the lines displayed in the cell
if it has no hidden branches; otherwise, a negative count of the lines
displayed, since it has hidden branches."
  (let (status)
    ;; All cells should be fully expanded when this is called but this
    ;; call collapses and hides some, so have it process only visible
    ;; cells and skip those we have hidden.
    (kview:map-region
     (lambda ()
       (setq status (car cell-status-list))
       (cond ((or (null status) (zerop status))) ;; ignore
	     ((< status 0)
	      ;; Hide the subtree of this cell and display only
	      ;; (- status) lines.
	      (kvspec:show-lines-this-cell (- status))
	      (if (save-excursion (kcell-view:parent nil label-sep-len))
		  (kotl-mode:hide-subtree)
		(error "(kview:set-cells-status): Invisible cell `%s' is missing its parent cell"
		       (kcell-view:label))))
	     ;; (> status 0)
	     (t (kvspec:show-lines-this-cell status)))
       (setq cell-status-list (cdr cell-status-list)))
     kview t start end)))

(defun kview:set-label-type (kview new-type)
  "Change kview's label display type to NEW-TYPE, updating all displayed labels.
See documentation for variable, kview:default-label-type, for
valid values of NEW-TYPE."
  (interactive (list kview
		     (let ((completion-ignore-case)
			   (label-type (kview:label-type kview))
			   new-type-str)
		       (if (string-equal
			    ""
			    (setq new-type-str
				  (completing-read
				   (format "View label type (current = %s): "
					   label-type)
				   '(("alpha") ("legal") ("id")
				     ;; ("no") ("partial-alpha") ("star")
				     )
				   nil t)))
			   label-type
			 (intern new-type-str)))))
  (if (not (memq new-type '(alpha legal id ;; no partial-alpha star
			    )))
      (error "(kview:set-label-type): Invalid label type, `%s'." new-type))
  (let ((old-label-type (kview:label-type kview)))
    (if (eq old-label-type new-type)
	;; Per kview function definitions might have changed, so reset them.
	(kview:set-functions new-type)
      (klabel-type:set-labels new-type)
      (kview:set-attr kview 'label-type new-type)
      (kview:set-functions new-type)
      (kvspec:update t))))

(defun kview:top-cell (kview)
  "Return kview's invisible top cell with idstamp 0 or nil if argument is not a kview."
  (if (kview:is-p kview)
      (kview:get-attr kview 'top-cell)))

(defun kview:valid-position-p (&optional pos)
  "Return non-nil iff point or optional POS is at a position where editing may occur.
The read-only positions between cells and within cell indentations are invalid."
  (cond ((null pos)
	 (>= (current-column) (kcell-view:indent)))
	((not (integer-or-marker-p pos))
	 (error "(kview:valid-position-p): Argument POS not an integer
or marker, `%s'" pos))
	((or (< pos (point-min)) (> pos (point-max)))
	 (error "(kview:valid-position-p): Invalid POS argument, `%d'"
		pos))
	(t (save-excursion
	     (goto-char pos)
	     (>= (current-column) (kcell-view:indent))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kview:get-attr (obj attribute)
  "Return the value of OBJECT's ATTRIBUTE."
  (car (cdr (memq attribute (car (cdr (memq 'plist obj)))))))

(defun kcell-view:next-kcell (&optional visible-p label-sep-len)
  "Move to the point holding the kcell property within the next cell of the current kview.
With optional VISIBLE-P, consider only visible cells.  Return t
unless no next cell."
  (let* ((opoint (point))
	 (pos opoint))
    (while (and (setq pos (kproperty:next-single-change pos 'kcell))
		(or (not (kproperty:get pos 'kcell))
		    (and visible-p (kcell-view:invisible-p pos label-sep-len)))))
    (if (and pos (/= pos opoint) (kproperty:get pos 'kcell)
	     (if visible-p (not (kcell-view:invisible-p pos label-sep-len)) t))
	(progn (goto-char pos) t))))

(defun kcell-view:previous-kcell (&optional visible-p label-sep-len)
  "Move to the point holding the kcell property within the previous cell of the current kview.
With optional VISIBLE-P, consider only visible cells.  Return t
unless no previous cell."
  (let* ((opoint (point))
	 (pos opoint))
    ;; First skip past the 2 changes for the current kcell property.
    (setq pos (kproperty:previous-single-change pos 'kcell)
	  pos (kproperty:previous-single-change pos 'kcell))
    (while (and pos (setq pos (kproperty:previous-single-change pos 'kcell))
		(or (not (kproperty:get pos 'kcell))
		    (and visible-p (kcell-view:invisible-p pos label-sep-len)))))
    (if (and pos (/= pos opoint) (kproperty:get pos 'kcell)
	     (if visible-p (not (kcell-view:invisible-p pos label-sep-len)) t))
	(progn (goto-char pos) t))))

(defun kview:set-attr (obj attribute value)
  "Set OBJECT's ATTRIBUTE to VALUE and return VALUE."
  (let* ((plist-ptr (cdr (memq 'plist obj)))
	 (plist (car plist-ptr))
	 (attr (memq attribute plist)))
    (if attr
	(setcar (cdr attr) value)
      (setcar plist-ptr
	      (nconc (list attribute value) plist)))
    value))

(defun kview:set-functions (label-type)
  "Setup functions which handle labels of LABEL-TYPE for current view."
  (kview:set-attr kview 'label-function (klabel-type:function label-type))
  (kview:set-attr kview 'label-child (klabel-type:child label-type))
  (kview:set-attr kview 'label-increment (klabel-type:increment label-type))
  (kview:set-attr kview 'label-parent (klabel-type:parent label-type)))

(defun kview:set-label-separator (label-separator &optional set-default-p)
  "Set the LABEL-SEPARATOR (a string) between labels and cell contents for the current kview.
With optional prefix arg SET-DEFAULT-P, the default separator value used for
new outlines is also set to this new value."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list (if (kview:is-p kview)
		    (read-string
		     (format
		      "Change current%s label separator from \"%s\" to: "
		      (if current-prefix-arg " and default" "")
		      (kview:label-separator kview))))
		current-prefix-arg)))

  (barf-if-buffer-read-only)
  (cond ((not (kview:is-p kview))
	 (error "(kview:set-label-separator): This is not a koutline"))
	((not (stringp label-separator))
	 (error "(kview:set-label-separator): Invalid separator, \"%s\""
		label-separator))
	((< (length label-separator) 2)
	 (error "(kview:set-label-separator): Separator must be two or more characters, \"%s\""
		label-separator)))

  (let* ((old-sep-len (kview:label-separator-length kview))
	 (sep-len (length label-separator))
	 (sep-len-increase (- sep-len old-sep-len))
	 (indent)
	 (reindent-function
	  (cond ((zerop sep-len-increase)
		 (lambda ()))
		((> sep-len-increase 0)
		 ;; Increase indent in each cell line.
		 (lambda ()
		   (goto-char (point-min))
		   (setq indent (make-string
				 sep-len-increase ?\ ))
		   (while (re-search-forward "[^\n\r][\n\r] " nil t)
		     (insert indent))))
		(t
		 ;; Decrease indent in each cell line.
		 (lambda ()
		   (goto-char (point-min))
		   (setq indent
			 (concat "[^\n\r][\n\r]"
				 (make-string
				  (- sep-len-increase) ?\ )))
		   (while (re-search-forward indent nil t)
		     (delete-region
		      (+ (match-beginning 0) 2) (match-end 0)))))))
	 pos)
    (save-excursion
      (goto-char (point-min))
      (kproperty:replace-separator pos label-separator old-sep-len)
      ;; Reindent all lines in cells except the first line which has already
      ;; been done.
      (funcall reindent-function))
    (kview:set-attr kview 'label-separator label-separator)
    (kview:set-attr kview 'label-separator-length sep-len)
    (if set-default-p
	(setq kview:default-label-separator label-separator))))

(provide 'kview)

;;; kview.el ends here
