;;; kfile.el --- Save and restore koutlines from files
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    10/31/93
;;
;; Copyright (C) 1993-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(kproperty kmenu kview)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst kfile:version "Kotl-4.0"
  "Version number of persistent data format used for saving koutlines.")

;;; Ensure that outline structure data is hidden from view after a file save.
(add-hook 'after-save-hook #'kfile:narrow-to-kcells)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar kfile:escape-newlines t 
  "Value of print-escape-newlines used by `kfile:print-to-string' function.")

;;; ************************************************************************
;;; Entry Points
;;; ************************************************************************

;;;###autoload
(defun kfile:find (file-name)
  "Find a file FILE-NAME containing a kotl or create one if none exists.
Return the new kview."
  (interactive
   (list (kfile:read-name
	  "Find koutline file: " nil)))
  (let ((existing-file (file-exists-p file-name))
	buffer)
    (and existing-file
	 (not (file-readable-p file-name))
	 (error
	  "(kfile:find): \"%s\" is not readable.  Check permissions."
	  file-name))
    (setq buffer (find-file file-name))
    ;; Finding the file may have already done a kfile:read as invoked through
    ;; kotl-mode via a file local variable setting.  If so, don't read it
    ;; again.
    (unless (kview:is-p kview)
      (kfile:read buffer existing-file))
    (or (eq major-mode 'kotl-mode) (kotl-mode))
    kview))

;;;###autoload
(defun kfile:is-p ()
  "Iff current buffer contains an unformatted or formatted koutline, return file format version string, else nil."
  (let (ver-string)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(condition-case ()
	    (progn
	      (setq ver-string (read (current-buffer)))
	      (and (stringp ver-string) (string-match "^Kotl-" ver-string)
		   ver-string))
	  (error nil))))))

;;;###autoload
(defun kfile:view (file-name)
  "View an existing kotl version-2 file FILE-NAME in a read-only mode."
  (interactive
   (list (kfile:read-name
	  "View koutline file: " t)))
  (let ((existing-file (file-exists-p file-name)))
    (if existing-file
	(if (not (file-readable-p file-name))
	    (error "(kfile:view): \"%s\" is not readable.  Check permissions."
		   file-name))
      (error "(kfile:view): \"%s\" does not exist." file-name))
    (view-file file-name))
    (kfile:narrow-to-kcells)
    (goto-char (point-min)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kfile:create (buffer)
  "Create a new koutline file attached to BUFFER, with a single empty level 1 kotl cell.
Return file's kview."
  (or buffer (setq buffer (current-buffer)))
  (if (not (bufferp buffer))
      (error "(kfile:create): Invalid buffer argument, %s" buffer))
  (set-buffer buffer)
  (if buffer-read-only
      (error "(kfile:create): %s is read-only" buffer))
  (widen)

  (let ((empty-p (zerop (buffer-size)))
	import-from view standard-output)

    (if (not empty-p)
	;; This is a foreign file whose elements must be converted into
	;; koutline cells.
	(progn (setq import-from (kimport:copy-and-set-buffer buffer))
	       (set-buffer buffer)
	       (erase-buffer))) ;; We copied the contents to `import-from'.

    (setq view (kview:create (buffer-name buffer))
	  standard-output (current-buffer))
    (goto-char (point-min))
    (princ ";; -*- Mode: kotl -*- \n")
    (prin1 kfile:version)
    (princ " ;; file-format\n\^_\n")
    ;; Ensure that last cell has two newlines after it so that
    ;; kfile:insert-attributes finds it.
    (goto-char (point-max))
    (princ "\n\n\^_\n")
    (princ "\^_\n;; depth-first kcell attributes\n")
    ;; Ensure that display is narrowed to cell region only.
    (kfile:narrow-to-kcells)
    (goto-char (point-min))
    (if empty-p
	;; This is a new koutline file.  Always need at least one visible
	;; cell within a view. Insert initial empty cell.
	(progn (kview:add-cell "1" 1)
	       ;; Mark view unmodified, so if kill right away, there is no
	       ;; prompt.
	       (set-buffer-modified-p nil)
	       ;; Move to first cell.
	       (goto-char (point-min))
	       (goto-char (kcell-view:start)))
      ;; Import buffer.  Next line is necessary or the importation will fail.
      (delete-region (point-min) (point-max)) 
      ;; Import foreign buffer as koutline cells.
      (kimport:file import-from (current-buffer))
      ;; If import buffer name starts with a space, kill it, as it is no
      ;; longer needed.
      (if (eq ?\ (aref (buffer-name import-from) 0))
	  (kill-buffer import-from)))

    view))

(defun kfile:read (buffer existing-file-p &optional ver-string)
  "Create a new kotl view by reading BUFFER or create an empty view when EXISTING-FILE-P is nil.
Optional VER-STRING is the outline format version number for the BUFFER that
was previously read by calling `kfile:is-p'.

Return the new view."
  (cond ((not (bufferp buffer))
	 (error "(kfile:read): Argument must be a buffer, `%s'." buffer))
	((and (zerop (buffer-size)) (not existing-file-p))
	 (kfile:create buffer))
	((progn
	   (set-buffer buffer)
	   (not (or (stringp ver-string) (setq ver-string (kfile:is-p)))))
	 (error "(kfile:read): `%s' is not a koutline file." buffer))
	((equal ver-string "Kotl-4.0")
	 (kfile:read-v4-or-v3 buffer nil))
	((equal ver-string "Kotl-3.0")
	 (kfile:read-v4-or-v3 buffer t))
	((equal ver-string "Kotl-2.0")
	 (kfile:read-v2 buffer))
	((equal ver-string "Kotl-1.0")
	 (error "(kfile:read): V1 koutlines are no longer supported"))
	(t (error "(kfile:read): `%s' has unknown kotl version, %s."
		  buffer ver-string))))

(defun kfile:read-v2 (buffer)
  "Create a kotl view by reading kotl version-2 BUFFER.  Return the new view."
  (let ((standard-input buffer)
	cell-count label-type label-min-width label-separator
	level-indent cell-data kotl-structure view kcell-list)
    (widen)
    (goto-char (point-min))
    ;; Skip past cell contents here.
    (search-forward "\n\^_" nil t 2)
    ;; Read rest of file data.
    (setq cell-count (read)
	  label-type (read)
	  label-min-width (read)
	  label-separator (read)
	  level-indent (read)
	  cell-data (read)
	  kotl-structure (read))
    ;;
    ;; kcell-list is a depth-first list of kcells to be attached to the cell
    ;; contents within the kview down below.
    (setq kcell-list (kfile:build-structure-v2 kotl-structure cell-data)
	  view (kview:create (buffer-name buffer) cell-count label-type
				 level-indent label-separator label-min-width))
    ;;
    (kfile:narrow-to-kcells)
    (goto-char (point-min))
    ;;
    ;; Add attributes to cells.
    (kfile:insert-attributes-v2 view kcell-list)
    ;;
    ;; Mark view unmodified and move to first cell.
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (goto-char (kcell-view:start))
    view))

(defun kfile:read-v4-or-v3 (buffer v3-flag)
  "Create a koutline view by reading version-4 BUFFER.  Return the new view.
If V3-FLAG is true, read as a version-3 buffer."
  (let ((standard-input buffer)
	cell-count label-type label-min-width label-separator
	level-indent cell-data view)
    (widen)
    (goto-char (point-min))
    ;; Skip past cell contents here.
    (search-forward "\n\^_" nil t 2)
    ;; Read rest of file data.
    (unless v3-flag ;; V3 files did not store viewspecs.
      (kvspec:initialize)
      (setq kvspec:current (read)))
    (setq cell-count (read)
	  label-type (read)
	  label-min-width (read)
	  label-separator (read)
	  level-indent (read)
	  cell-data (read))
    ;;
    (setq view (kview:create (buffer-name buffer) cell-count label-type
			     level-indent label-separator label-min-width))
    ;;
    (kfile:narrow-to-kcells)
    (goto-char (point-min))
    ;;
    ;; Add attributes to cells.
    (kfile:insert-attributes-v3 view cell-data)
    ;;
    ;; Mark view unmodified and move to first cell.
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (goto-char (kcell-view:start))
    view))

(defun kfile:update (&optional visible-only-p)
  "Update kfile internal structure so that view is ready for saving to a file.
Leave outline file expanded with structure data showing unless optional
VISIBLE-ONLY-P is non-nil.  Signal an error if kotl is not attached to a file."
  (let* ((top (kview:top-cell kview))
	 (file (kcell:get-attr top 'file))
	 (label-type (kview:label-type kview))
	 (label-min-width (kview:label-min-width kview))
	 (label-separator (kview:label-separator kview))
	 (level-indent (kview:level-indent kview))
	 ;; If this happens to be non-nil, it is virtually impossible to save
	 ;; a file, so ensure it is nil.
	 (debug-on-error))
    (cond ((null file)
	   (error "(kfile:update): Current outline is not attached to a file."))
	  ((not (file-writable-p file))
	   (error "(kfile:update): File \"%s\" is not writable." file)))
    (let* ((buffer-read-only)
	   (id-counter (kcell:get-attr top 'id-counter))
	   (kcell-data (make-vector (1+ id-counter) nil))
	   (standard-output (current-buffer))
	   (opoint (set-marker (make-marker) (point)))
	   (kcell-num 1)
	   cell)
      ;;
      ;; Prepare cell data for saving.
      (kfile:narrow-to-kcells)
      (kview:map-tree
        (lambda (view)
	  (setq cell (kcell-view:cell))
	  (aset kcell-data
		kcell-num
		(kcell-data:create cell))
	  (setq kcell-num (1+ kcell-num)))
	kview t)
      ;; Save top cell, 0, last since above loop may increment the total
      ;; number of cells counter stored in it, if any invalid cells are
      ;; encountered. 
      (aset kcell-data 0 (kcell-data:create top))
      (setq id-counter (kcell:get-attr top 'id-counter))
      ;;
      (widen)
      (goto-char (point-min))
      (if (search-forward "\n\^_\n" nil t)
	  (delete-region (point-min) (match-end 0)))
      (princ ";; -*- Mode: kotl -*- \n")
      (prin1 kfile:version)
      (princ " ;; file-format\n\^_\n")
      ;; Skip past cells.
      (if (search-forward "\n\^_\n" nil t)
	  ;; Get rid of excess blank lines after last cell.
	  (progn (goto-char (match-beginning 0))
		 (skip-chars-backward "\n")
		 (delete-region (point) (point-max)))
	(goto-char (point-max)))
      ;; Ensure that last cell has two newlines after it so that
      ;; kfile:insert-attributes finds it.
      (princ "\n\n\^_\n")
      (princ (format (concat
		      "%S ;; kvspec:current\n%d ;; id-counter\n"
		      "%S ;; label-type\n%d ;; label-min-width\n"
		      "%S ;; label-separator\n%d ;; level-indent\n")
		     kvspec:current id-counter label-type label-min-width
		     label-separator level-indent))
      (princ "\^_\n;; depth-first kcell attributes\n")
      (kfile:pretty-print kcell-data)
      ;;
      ;; Don't re-narrow buffer by default since this is used in
      ;; write-contents-hooks after save-buffer has widened buffer.  If
      ;; buffer is narrowed here, only the narrowed portion will be saved to
      ;; the file.  Narrow as an option since saving only the portion of the
      ;; file visible in a view may be useful in some situations.
      (if visible-only-p (kfile:narrow-to-kcells))
      ;;
      ;; Return point to its original position as given by the opoint marker.
      (goto-char opoint)
      (set-marker opoint nil)
      nil)))

;;; Next function is adapted from `file-write' of GNU Emacs.
(defun kfile:write (file)
  "Write current outline to FILE."
  (interactive "FWrite outline file: ")
  (if (or (null file) (string-equal file ""))
      (error "(kfile:write): Invalid file name, \"%s\"" file))
  ;; If arg is just a directory, use same file name, but in that directory.
  (if (and (file-directory-p file) buffer-file-name)
      (setq file (concat (file-name-as-directory file)
			 (file-name-nondirectory buffer-file-name))))
  (kcell:set-attr (kview:top-cell kview) 'file file)
  (set-visited-file-name file)
  ;; Set-visited-file-name clears local-write-file-hooks that we use to save
  ;; koutlines properly, so reinitialize local variables.
  (kotl-mode)
  (set-buffer-modified-p t)
  ;; This next line must come before the save-buffer so write-file-functions
  ;; can make use of it.
  (kview:set-buffer kview (current-buffer))
  (save-buffer))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kfile:build-structure-v2 (kotl-structure cell-data)
  "Build cell list from the KOTL-STRUCTURE and its CELL-DATA.
Assumes all arguments are valid.  CELL-DATA is a vector of cell fields read
from a koutline file.

Return list of outline cells in depth first order.  Invisible top cell is not
included in the list."
  (let ((stack) (sibling-p) (cell-list) func cell)
    (mapc
     (lambda (item)
       (setq func (cdr (assoc item
			      (list
			       (cons "\("
				     (lambda ()
				       (setq stack (cons sibling-p stack)
					     sibling-p nil)))
			       (cons "\)" 
				     (lambda ()
				       (setq sibling-p (car stack)
					     stack (cdr stack))))))))
       (cond (func (funcall func))
	     ;; 0th cell was created with kview:create.
	     ((equal item 0) nil)
	     (t (setq cell (kcell-data:to-kcell-v2 (aref cell-data item))
		      cell-list (cons cell cell-list)
		      sibling-p t)
		)))
     kotl-structure)
    (nreverse cell-list)))

(defun kfile:insert-attributes-v2 (kview kcell-list)
  "Set cell attributes within kview for each element in KCELL-LIST.
Assumes all cell contents are already in kview and that no cells are
hidden."
  (let (buffer-read-only)
    (while
	(progn
	  (skip-chars-forward "\n")
	  ;; !!! Won't work if label-type is 'no.
	  ;; Here we search past the cell identifier
	  ;; for the location at which to place cell properties.
	  ;; Be sure not to skip past a period which may terminate the label.
	  (if (re-search-forward "[A-Za-z0-9]\\(\\.?[A-Za-z0-9]\\)*" nil t)
	      (progn
		(kproperty:set 'kcell (car kcell-list))
		(setq kcell-list (cdr kcell-list))))
	  (search-forward "\n\n" nil t)))))

(defun kfile:insert-attributes-v3 (kview kcell-vector)
  "Set cell attributes within kview for each element in KCELL-VECTOR.
Assumes all cell contents are already in kview and that no cells are
hidden."
  (let ((kcell-num 1)
	(buffer-read-only))
    (while
	(progn
	  (skip-chars-forward "\n")
	  ;; !!! Won't work if label-type is 'no.
	  ;; Here we search past the cell identifier
	  ;; for the location at which to place cell properties.
	  ;; Be sure not to skip past a period which may terminate the label.
	  (if (re-search-forward "[A-Za-z0-9]\\(\\.?[A-Za-z0-9]\\)*" nil t)
	      (progn
		(kproperty:set 'kcell
			       (kcell-data:to-kcell-v3
				(aref kcell-vector kcell-num)))
		(setq kcell-num (1+ kcell-num))))
	  (search-forward "\n\n" nil t)))))

(defun kfile:narrow-to-kcells ()
  "Narrow kotl file to kcell section only."
  (interactive)
  (if (kview:is-p kview)
      (let ((start-text) (end-text))
	(save-excursion
	  (widen)
	  (goto-char (point-min))
	  ;; Skip to start of kcells.
	  (if (search-forward "\n\^_" nil t)
	      (setq start-text (1+ (match-end 0))))
	  ;; Skip past end of kcells.
	  (if (and start-text (search-forward "\n\^_" nil t))
	      (setq end-text (1+ (match-beginning 0))))
	  (if (and start-text end-text)
	      (progn (narrow-to-region start-text end-text)
		     (goto-char (point-min)))
	    (error
	     "(kfile:narrow-to-kcells): Cannot find start or end of kcells"))
	  ))))

(defun kfile:print-to-string (object)
  "Return a string containing OBJECT, any Lisp object, in pretty-printed form.
Quoting characters are used when needed to make output that `read' can
handle, whenever this is possible."
  (with-current-buffer (get-buffer-create " kfile:print-to-string")
    (let ((emacs-lisp-mode-hook)
	  (buffer-read-only))
      (erase-buffer)
      (unwind-protect
	  (progn
	    (emacs-lisp-mode)
	    (let ((print-escape-newlines kfile:escape-newlines))
	      (prin1 object (current-buffer)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      ;; (message "%06d" (- (point-max) (point)))
	      (cond
	       ((looking-at "\\s\(")
		(while (looking-at "\\s(")
		  (forward-char 1)))
	       ((and (looking-at "\\(quote[ \t]+\\)\\([^.)]\\)")
		     (> (match-beginning 1) 1)
		     (eq ?\( (char-after (1- (match-beginning 1))))
		     ;; Make sure this is a two-element list.
		     (save-excursion
		       (goto-char (match-beginning 2))
		       (forward-sexp)
		       ;; (looking-at "[ \t]*\)")
		       ;; Avoid mucking with match-data
		       (eq ?\) (char-after (point)))))
		;; -1 gets the paren preceding the quote as well.
		(delete-region (1- (match-beginning 1)) (match-end 1))
		(insert "'")
		(forward-sexp 1)
		(if (looking-at "[ \t]*\)")
		    (delete-region (match-beginning 0) (match-end 0))
		  (error "Malformed quote"))
		(backward-sexp 1))	      
	       ((condition-case ()
		    (prog1 t (down-list 1))
		  (error nil))
		(backward-char 1)
		(skip-chars-backward " \t")
		(delete-region
		 (point)
		 (progn (skip-chars-forward " \t") (point)))
		(if (not (eq ?' (char-after (1- (point)))))
		    (insert ?\n)))
	       ((condition-case ()
		    (prog1 t (up-list 1))
		  (error nil))
		(while (looking-at "\\s)")
		  (forward-char 1))
		(skip-chars-backward " \t")
		(delete-region
		 (point)
		 (progn (skip-chars-forward " \t") (point)))
		(if (not (eq ?' (char-after (1- (point)))))
		    (insert ?\n)))
	       (t (goto-char (point-max)))))
	    (goto-char (point-min))
	    (indent-sexp)
	    (buffer-string))
	(kill-buffer (current-buffer))))))

(defun kfile:pretty-print (object &optional stream)
  "Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)."
  (princ (kfile:print-to-string object) (or stream standard-output)))

(defun kfile:read-name (prompt existing-p)
  "PROMPT for and read a koutline file name.  EXISTING-P means must exist."
  (let ((filename))
    (while (not filename)
      (setq filename (read-file-name prompt nil nil existing-p))
      (if (or (null filename) (equal filename ""))
	  (progn (beep) (setq filename nil))))
    filename))

(provide 'kfile)

;;; kfile.el ends here
