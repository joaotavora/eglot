;;; hyrolo-logic.el ---  Logic functions for GNU Hyperbole Rolo files
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    13-Jun-89 at 22:57:33
;;
;; Copyright (C) 1989-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;  INSTALLATION:
;;
;;   See also hyrolo.el.  These functions are separated from hyrolo.el since many
;;   users may never want or need them.  They can be automatically loaded when
;;   desired by adding the following to one of your Emacs init files:
;;
;;    (autoload 'hyrolo-fgrep-logical "hyrolo-logic" "Rolo search with logical operators." t)
;;
;;  FEATURES:
;;
;;   1.  One command, `hyrolo-fgrep-logical' which prompts for a logical search
;;       expression string and displays any matching entries.  A sample expression
;;       might be:
;;        (and (or (not time card) (xor "french balloons" spanish)) teacher pet)
;;
;;       Either double quotes or parentheses may be used to group multiple
;;       words as a single argument.
;;
;;   2.  Logical `hyrolo-and', `hyrolo-or', `hyrolo-not', and `hyrolo-xor' rolo
;;       entry string filter functions. They take any number of string or
;;       boolean arguments and may be nested.  NOTE THAT THESE FUNCTIONS
;;       SHOULD NEVER BE CALLED DIRECTLY UNLESS THE FREE VARIABLES `start'
;;       and `end' ARE BOUND BEFOREHAND.
;;
;;   3.  Logical `hyrolo-r-and', `hyrolo-r-or', `hyrolo-r-not', and `hyrolo-r-xor'
;;       rolo entry regexp filter functions.  They take any number of string or
;;       boolean arguments and may be nested.  NOTE THAT THESE FUNCTIONS
;;       SHOULD NEVER BE CALLED DIRECTLY UNLESS THE FREE VARIABLES `start'
;;       and `end' ARE BOUND BEFOREHAND.
;;
;;  EXAMPLE PROGRAMMATIC USAGE:
;;
;;     (hyrolo-logic (hyrolo-and (hyrolo-not "Tool-And-Die") "secretary"))
;;
;;   would find all non-Tool-And-Die Corp. secretaries in your rolo.
;;
;;   The logical matching routines are not at all optimal, but then most
;;   rolo files are not terribly lengthy either, so results are often
;;   displayed quickly.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hyrolo)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (defvar next-entry-exists nil))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hyrolo-fgrep-logical (expr)
  "Display rolo entries matching EXPR which may contain prefix logical operators.
A complex example of EXPR might be:
  (and (or (not time card) (xor (and french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Match exactly one of `french balloons' or `spanish'
    and
  Match `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument."
  (interactive "sLogical rolo search: ")
  (let* ((case-fold-search t)
	 (total-matches))
    (if (not (string-match "\(\\(and\\|or\\|xor\\|not\\)\\>" expr))
	;; Search string does not contain embedded logic
	;; operators; do a string search instead.
	(setq total-matches (hyrolo-fgrep expr))
      (setq expr (hypb:replace-match-string "\(or " expr "\(| " t))
      (setq expr (hypb:replace-match-string "\(xor " expr "\(@ " t))
      (setq expr (hypb:replace-match-string "\(not " expr "\(! " t))
      (setq expr (hypb:replace-match-string "\(and " expr "\(& " t))
      (setq expr (hypb:replace-match-string
		  "\"\\([^\"]*\\)\"" expr "{\\1}" nil))
      (setq expr (hypb:replace-match-string
		  "\(\\([^@|!&()][^()\"]*\\)\)" expr "{\\1}" nil))
      (let ((saved-expr expr))
	(while
	    (not (equal
		  saved-expr
		  (setq expr (hypb:replace-match-string
			      "\\(\\s-\\)\\([^{}()\" \t\n\r]+\\)\\([^{}()]*[()]\\)"
			      expr "\\1\"\\2\"\\3" nil))))
	  (setq saved-expr expr)))
      (setq expr (hypb:replace-match-string
		  "{\\([^{}]+\\)}" expr "\"\\1\"" nil))
      (setq expr (hypb:replace-match-string "\(| " expr "\(hyrolo-or " t))
      (setq expr (hypb:replace-match-string "\(@ " expr "\(hyrolo-xor " t))
      (setq expr (hypb:replace-match-string "\(! " expr "\(hyrolo-not " t))
      (setq expr (hypb:replace-match-string "\(& " expr "\(hyrolo-and " t))
      (setq expr (format "(hyrolo-logic (quote %s) nil nil t)" expr))
      (setq total-matches (eval (read expr))))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

(defun hyrolo-logic (sexp &optional in-bufs count-only include-sub-entries
		     no-sub-entries-out)
  "Apply SEXP to all entries in optional IN-BUFS, display entries where SEXP is non-nil.
If IN-BUFS is nil, `hyrolo-file-list' is used.  If optional COUNT-ONLY is
non-nil, don't display entries, return count of matching entries only.  If
optional INCLUDE-SUB-ENTRIES flag is non-nil, SEXP will be applied across all
sub-entries at once.  Default is to apply SEXP to each entry and sub-entry
separately.  Entries are displayed with all of their sub-entries unless
INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT flag is non-nil.
SEXP should use the free variables `start' and `end' which contain the limits
of the region on which it should operate.  Returns number of evaluations of
SEXP that matched entries."
  (let ((obuf (current-buffer))
	(display-buf (if count-only
			 nil
		       (prog1 (set-buffer (get-buffer-create hyrolo-display-buffer))
			 (setq buffer-read-only nil)
			 (erase-buffer)))))
    (let ((result
	   (mapcar
	    (lambda (in-bufs)
	      (hyrolo-map-logic sexp in-bufs count-only include-sub-entries
				no-sub-entries-out))
	    (cond ((null in-bufs) hyrolo-file-list)
		  ((listp in-bufs) in-bufs)
		  ((list in-bufs))))))
      (let ((total-matches (apply '+ result)))
	(unless (or count-only (= total-matches 0))
	  (hyrolo-display-matches display-buf))
	total-matches))))

(defun hyrolo-map-logic (sexp hyrolo-buf &optional count-only
			    include-sub-entries no-sub-entries-out)
  "Apply logical SEXP to each entry in HYROLO-BUF and write out matching entries to `hyrolo-display-buffer'.
If optional COUNT-ONLY is non-nil, don't display entries, return count of
matching entries only.  If optional INCLUDE-SUB-ENTRIES flag is non-nil, SEXP
will be applied across all sub-entries at once.  Default is to apply SEXP to
each entry and sub-entry separately.  Entries are displayed with all of their
sub-entries unless INCLUDE-SUB-ENTRIES is nil and optional NO-SUB-ENTRIES-OUT
flag is non-nil.  SEXP should use the free variables `start' and `end' which
contain the limits of the region on which it should operate.  Returns number
of applications of SEXP that matched entries."
  (setq hyrolo-buf (or (get-buffer hyrolo-buf) hyrolo-buf))
  (if (or (bufferp hyrolo-buf)
	  (if (file-exists-p hyrolo-buf)
	      (setq hyrolo-buf (find-file-noselect hyrolo-buf t))))
      (let* ((display-buf (set-buffer (get-buffer-create hyrolo-display-buffer)))
	     (buffer-read-only))
	(let ((hdr-pos) (num-found 0))
	  (set-buffer hyrolo-buf)
	  (save-excursion
	    (save-restriction
	      (widen)
	      (goto-char 1)
	      ;; Ensure no entries in outline mode are hidden.
	      (outline-show-all)
	      (when (re-search-forward hyrolo-hdr-regexp nil t 2)
		(forward-line)
		(setq hdr-pos (cons (point-min) (point))))
	      (let* ((start)
		     (end)
		     (end-entry-hdr)
		     (curr-entry-level))
		(while (re-search-forward hyrolo-entry-regexp nil t)
		  (setq end-entry-hdr (point)
			start (save-excursion (beginning-of-line) (point))
			next-entry-exists nil
			curr-entry-level (buffer-substring start end-entry-hdr)
			end (hyrolo-to-entry-end include-sub-entries curr-entry-level))
		  (let ((result (eval sexp)))
		    (or count-only 
			(and result (= num-found 0) hdr-pos
			     (let* ((src (or (buffer-file-name hyrolo-buf)
					     hyrolo-buf))
				    (src-line
				     (format
				      (concat (if (boundp 'hbut:source-prefix)
						  hbut:source-prefix
						"@loc> ")
					      "%s")
				      (prin1-to-string src))))
			       (set-buffer display-buf)
			       (goto-char (point-max))
			       (if hdr-pos
				   (progn
				     (insert-buffer-substring
				      hyrolo-buf (car hdr-pos) (cdr hdr-pos))
				     (insert src-line "\n\n"))
				 (insert (format hyrolo-hdr-format src-line)))
			       (set-buffer hyrolo-buf))))
		    (if result
			(progn (goto-char end)
			       (setq num-found (1+ num-found)
				     end (if (or include-sub-entries
						 no-sub-entries-out)
					     end
					   (goto-char (hyrolo-to-entry-end
						       t curr-entry-level))))
			       (or count-only
				   (append-to-buffer display-buf start end)))
		      (goto-char end-entry-hdr)))))))
	  (hyrolo-kill-buffer hyrolo-buf)
	  num-found))
    0))

;;
;; INTERNAL FUNCTIONS.
;;

;; Do NOT call the following functions directly.
;; Send them as parts of an expression to `hyrolo-logic'.

(defun hyrolo-not (&rest pat-list)
  "Logical <not> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat))
    (while (and pat-list
		(or (null (setq pat (car pat-list)))
		    (and (stringp pat)
			 (goto-char start)
			 (not (search-forward pat end t)))))
      (setq pat-list (cdr pat-list)))
    (not pat-list)))

(defun hyrolo-or (&rest pat-list)
  "Logical <or> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq t pat-list)
      t
    (let ((pat))
      (while (and pat-list
		  (or (null (setq pat (car pat-list)))
		      (and (stringp pat)
			   (goto-char start)
			   (not (search-forward pat end t)))))
	(setq pat-list (cdr pat-list)))
      (if pat-list t nil))))

(defun hyrolo-xor (&rest pat-list)
  "Logical <xor> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat)
	(matches 0))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (or (eq pat t)
			     (not (goto-char start))
			     (search-forward pat end t))
			 (setq matches (1+ matches)))
		    t)
		(< matches 2))
      (setq pat-list (cdr pat-list)))
    (= matches 1)))

(defun hyrolo-and (&rest pat-list)
  "Logical <and> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (unless (memq nil pat-list)
    (let ((pat))
      (while (and pat-list
		  (setq pat (car pat-list))
		  (or (eq pat t)
		      (not (goto-char start))
		      (search-forward pat end t)))
	(setq pat-list (cdr pat-list)))
      (not pat-list))))

;; Work with regular expression patterns rather than strings

(defun hyrolo-r-not (&rest pat-list)
  "Logical <not> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat))
    (while (and pat-list
		(or (null (setq pat (car pat-list)))
		    (and (stringp path)
			 (goto-char start)
			 (not (re-search-forward pat end t)))))
      (setq pat-list (cdr pat-list)))
    (not pat-list)))

(defun hyrolo-r-or (&rest pat-list)
  "Logical <or> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (if (memq t pat-list)
      t
    (let ((pat))
      (while (and pat-list
		  (or (null (setq pat (car pat-list)))
		      (and (stringp pat)
			   (goto-char start)
			   (not (re-search-forward pat end t)))))
	(setq pat-list (cdr pat-list)))
      (if pat-list t nil))))

(defun hyrolo-r-xor (&rest pat-list)
  "Logical <xor> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (let ((pat)
	(matches 0))
    (while (and pat-list
		(or (not (setq pat (car pat-list)))
		    (and (or (eq pat t)
			     (not (goto-char start))
			     (re-search-forward pat end t))
			 (setq matches (1+ matches)))
		    t)
		(< matches 2))
      (setq pat-list (cdr pat-list)))
    (= matches 1)))

(defun hyrolo-r-and (&rest pat-list)
  "Logical <and> rolo entry filter.  PAT-LIST is a list of pattern elements.
Each element may be t, nil, or a string."
  (unless (memq nil pat-list)
    (let ((pat))
      (while (and pat-list
		  (setq pat (car pat-list))
		  (or (eq pat t)
		      (not (goto-char start))
		      (re-search-forward pat end t)))
	(setq pat-list (cdr pat-list)))
      (not pat-list))))

(provide 'hyrolo-logic)

;;; hyrolo-logic.el ends here
