;;; hyrolo-demo.el --- Code to support DEMO introduction to HyRolo
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     4-Nov-17 at 13:56:47
;;
;; Copyright (C) 2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Code:

;;; ************************************************************************
;;; Requirements
;;; ************************************************************************

(require 'hyrolo-logic)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hyrolo-demo-save-key nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hyrolo-demo-fgrep (string &optional max-matches)
  "Display rolo entries in \"DEMO-ROLO.otl\" matching STRING (or a logical match expression).
Display to a maximum of optional prefix arg MAX-MATCHES.
Each entry is displayed with all of its sub-entries.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Returns number of entries matched.  See also documentation for
the function `hyrolo-demo-fgrep-logical' for documentation on the
logical expression matching."
  (interactive "sFind rolo string (or logical expression): \nP")
  (let ((hyrolo-file-list (list (expand-file-name "DEMO-ROLO.otl" hyperb:dir))))
    (hyrolo-fgrep string max-matches)))

;;;###autoload
(defun hyrolo-demo-fgrep-logical (expr)
  "Display rolo entries in \"DEMO-ROLO.otl\" matching EXPR which may contain prefix logical operators.
A complex example of EXPR might be:
  (and (or (not time card) (xor (french balloons) spanish)) teacher pet)
which means:
  Match neither `time' nor `card'
    or
  Matches exactly one of `french balloons' or `spanish'
    and
  Matches `teacher' and `pet'.

Either double quotes or parentheses may be used to group multiple words as a
single argument."
  (interactive "sLogical rolo search: ")
  (let ((hyrolo-file-list (list (expand-file-name "DEMO-ROLO.otl" hyperb:dir))))
    (hyrolo-fgrep-logical expr)))

(defun hyrolo-demo-quit ()
  "Remove the code in this file."
  (interactive)
  (when hyrolo-demo-save-key
    (global-set-key "\C-x4r" hyrolo-demo-save-key))
  (makunbound 'hyrolo-demo-save-key)
  (fmakunbound 'hyrolo-demo-fgrep)
  (fmakunbound 'hyrolo-demo-fgrep-logical)
  (setq features (delq 'hyrolo-demo features))
  (mapc (lambda (buf) (when (get-buffer buf) (kill-buffer buf)))
	'("*Hyperbole Rolo*" "DEMO-ROLO.otl"))
  (load "hyperbole-autoloads")
  (fmakunbound 'hyrolo-demo-quit)
  (message "HyRolo demo code removed and {C-x 4 r} key binding reset."))

;;; ************************************************************************
;;; Key Bindings
;;; ************************************************************************

(unless (eq (key-binding "\C-x4r") #'hyrolo-demo-fgrep)
  (setq hyrolo-demo-save-key (key-binding "\C-x4r")))

(global-set-key "\C-x4r" 'hyrolo-demo-fgrep)

(provide 'hyrolo-demo)
