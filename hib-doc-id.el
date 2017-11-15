;;; hib-doc-id.el --- Implicit button type for document id index entries
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    30-Sep-92 at 19:39:59
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This library defines the `doc-id' implicit button type which lets
;;  you create archives of reference documents by topic and then
;;  reference them within any Hyperbole readable file by use of simple
;;  identifiers, like [Emacs-001].
;;
;;  TO USE:
;;
;;   Pressing the Action Key on a doc id such as, [Emacs-001],
;;   displays the online version of the document, if any.  Pressing the
;;   Assist Key on it displays its document index entry.
;;
;;  TO CONFIGURE:
;;
;;   Set the value of `doc-id-indices' before using the `doc-id'
;;   implicit button type defined herein or you will get an error telling you
;;   to do so.  See the documentation for `doc-id-indices'.
;;
;;   You must explicitly load this library in order to use it, since
;;   Hyperbole does not load it by default.
;;
;;   The default setup uses doc ids of the form, [Emacs-001], delimited by
;;   brackets, starting with a topic name, followed by a - and a
;;   multi-digit numeric identifier.
;;
;;   Typically an index entry should have links to all available forms of its
;;   document, e.g. online, printed, source.  Below is a sample index entry form
;;   to use.  The default variable settings herein work with this format.  If
;;   you prefer a different one, you must change all of the variable values.
;;
;;  --------------------------------------------------------------------------
;;  Title:                                                  ID: []
;;  Email-To:
;;  Distribution:     
;;  
;;  Abstract:         
;;                    
;;                    
;;  References:       
;;  
;;  Author:           
;;  Copyright:        
;;  Keywords:         
;;  
;;  Online-Format:    
;;  Online-Loc:       ""
;;  Printed-Format:
;;  Printed-Loc:      Local Library
;;  Printable-Loc:    ""
;;  Source-Format:
;;  Source-Loc:       ""
;;  
;;  Date:             
;;  Version:          
;;  Version-Changes:  
;;  --------------------------------------------------------------------------

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hactypes hypb hyrolo)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar doc-id-start "["
  "String which delimits start of a site-specific document id.")
(defvar doc-id-end   "]"
  "String which delimits end of a site-specific document id.")

(defvar doc-id-index-entry-regexp "^------+[ \t\n\r]+Title:"
  "Regexp which matches start of a site-specific document index entry.")

(defvar doc-id-match
  (lambda (doc-id)
    (concat "ID:[ \t]*\\[" (regexp-quote doc-id) "\\]"))
  "Function which returns regexp which matches only in DOC-ID's index entry.")

(defvar doc-id-p (lambda (str)
		   (and (stringp str)
			(> (length str) 0)
			(eq ?w (char-syntax (aref str 0)))
			(string-match "\\`\\w+-[0-9][0-9][0-9]+\\'" str)))
  "Value is a function with a boolean result that tests whether `str' is a doc id.")

(defvar doc-id-online-regexp "^Online-Loc:[ \t]*\"\\([^\"]+\\)\""
  "Regexp whose 1st grouping matches an implicit button which displays an online document within an index entry.")

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************
  
;;; ========================================================================
;;; Displays a documentation index entry given an ID.
;;; ========================================================================

(defact link-to-doc (doc-id)
  "Displays online version of a document given by DOC-ID (no delimiters), in other window.
If online version of document is not found in `doc-id-indices', an error is
signalled."
  (interactive "sID for document to link to (omit delimiters): ")
  (let ((hyrolo-display-buffer (hypb:help-buf-name "Doc ID"))
	(delim-doc-id (concat doc-id-start doc-id doc-id-end)))
    (cond ((null doc-id-indices)
	   (error "(doc-id-index-entry): You must set the `doc-id-indices' variable first."))
	  ((let ((hyrolo-entry-regexp doc-id-index-entry-regexp))
	     (zerop (hyrolo-grep (funcall doc-id-match doc-id)
			       1 doc-id-indices nil 'no-display)))
	   (error "(doc-id-index-entry): %s not found in document index."
		  delim-doc-id))
	  ;; Matching index entry has been put into `hyrolo-display-buffer'.
	  (t (with-current-buffer hyrolo-display-buffer
	       (goto-char (point-min))
	       (message "Searching for document %s..." delim-doc-id)
	       (if (re-search-forward doc-id-online-regexp nil t)
		   (progn
		     (goto-char (match-beginning 1))
		     (let ((doc-path (buffer-substring
				      (match-beginning 1) (match-end 1)))
			   (ibut (ibut:at-p)))
		       (if ibut
			   (progn (hbut:act ibut)
				  (message "Displaying %s." delim-doc-id))
			 (error
			  "(link-to-doc): %s online location is invalid: \"%s\""
			  delim-doc-id doc-path))))
		 (error "(link-to-doc): %s is unavailable in online form."
			delim-doc-id)))))))

(defib doc-id ()
  "Displays a document from a local document library given its id.
Ids must be delimited by `doc-id-start' and `doc-id-end' and must
match the function stored in `doc-id-p'."
  (and (not (bolp))
       (let* ((id-and-pos (hbut:label-p t doc-id-start doc-id-end t))
	      (id (car id-and-pos)))
	 (if (funcall doc-id-p id)
	     (progn (ibut:label-set id-and-pos)
		    (hact 'link-to-doc id))))))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar doc-id-indices '()
  "List of pathnames in which to search for site-specific document index entries.
Each file must utilize a hyrolo record format, with each record start
delimited by `doc-id-index-entry-regexp'.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun doc-id:help (but)
  "Displays site-specific document index entry given by doc-id BUT, in other window.
Also displays standard Hyperbole help for implicit button BUT."
  (let ((hyrolo-entry-regexp doc-id-index-entry-regexp)
	(hyrolo-display-buffer (hypb:help-buf-name "Doc ID"))
	(doc-id (hbut:key-to-label (hattr:get but 'lbl-key))))
    (cond ((null doc-id-indices)
	   (error "(doc-id-index-entry): You must set the `doc-id-indices' variable first."))
	  ((zerop (hyrolo-grep (funcall doc-id-match doc-id) 1 doc-id-indices))
	   (error
	     "(doc-id-index-entry): No document index entry found for %s%s%s."
		  doc-id-start doc-id doc-id-end)))
    (let* ((report-buf (hypb:help-buf-name))
	   (temp-buffer-show-hook
	    (lambda (buffer)
	      (setq hkey--wconfig (current-window-configuration))
	      (let ((wind (get-buffer-create buffer)))
		(setq minibuffer-scroll-window wind))))
	   (temp-buffer-show-function temp-buffer-show-hook))
      (hbut:report but)
      (with-current-buffer hyrolo-display-buffer
	(setq buffer-read-only nil)
	(goto-char (point-max))
	(insert-buffer-substring report-buf)
	(set-buffer-modified-p nil)
	(setq buffer-read-only nil)
	(goto-char (point-min)))
      (kill-buffer report-buf)
      )))

(provide 'hib-doc-id)

;;; hib-doc-id.el ends here
