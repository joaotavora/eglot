;;; hvar.el --- Variable manipulation routines for GNU Hyperbole
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-Oct-91 at 14:00:24
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'set)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun var:add-and-run-hook (hook hook-function)
  "Adds to HOOK (a symbol ending with -hook) HOOK-FUNCTION and then calls HOOK-FUNCTION in every buffer with the matching major mode based on HOOK's name."
  (add-hook hook hook-function)
  (let* ((hook-name (symbol-name hook))
	 (mode (if (string-match "-hooks?\\'" hook-name)
		   (intern-soft (substring hook-name 0 (match-beginning 0))))))
    (if mode (var:run-hook-in-matching-buffers mode hook-function))))

(defun var:append-all ()
  "Add back all hook values previously added by var:append in this Emacs session.
The ones that were removed by var:remove-all at some point."
  (mapc (lambda (elt) (var:append (car elt) (cdr elt)))
	var::append-list)
  var::append-list)

;;;###autoload
(defun var:append (var-symbol list-to-add)
  "Appends to value held by VAR-SYMBOL, LIST-TO-ADD.  Returns new value.
If VAR-SYMBOL is unbound, it is set to LIST-TO-ADD.
Used to append to hook variables.  Stores all values for later removal.
Does nothing when `inhibit-hyperbole-messaging' is non-nil."
  (if (not (symbolp var-symbol))
      (error "(var:append): First argument, `%s', must be a symbol (not a string)." var-symbol))
  (if (or (null list-to-add) (not (listp list-to-add)))
      (error "(var:append): Second argument, `%s', must be a non-empty list." list-to-add))
  (unless inhibit-hyperbole-messaging
    (let ((val) result)
      (setq result
	    (if (and (boundp var-symbol)
		     (setq val (symbol-value var-symbol))
		     (or (if (symbolp val)
			     (setq val (cons val nil)))
			 (listp val)))
		(progn (if (eq (car val) 'lambda)
			   (setq val (list val)))
		       (set var-symbol (set:union val list-to-add)))
	      (set var-symbol list-to-add)))
      (add-to-list 'var::append-list (cons var-symbol result))
      (symbol-value var-symbol))))

(defun var:remove (var-symbol list-to-remove)
  "Removes from VAR-SYMBOL the functions in LIST-TO-REMOVE.
Used to remove from hook variables."
  (if (not (symbolp var-symbol))
      (error "(var:remove): First argument, `%s', must be a symbol (not a string)." var-symbol))
  (if (or (null list-to-remove) (not (listp list-to-remove)))
      (error "(var:remove): Second argument, `%s', must be a non-empty list." list-to-remove))
  (if (eq (car list-to-remove) 'lambda)
      (setq list-to-remove (list list-to-remove)))
  (mapc (lambda (func) (remove-hook var-symbol func))
	list-to-remove)
  (setq var::append-list (delete (cons var-symbol list-to-remove) var::append-list))
  (symbol-value var-symbol))

(defun var:remove-all ()
  "Remove all hook values added by var:append in this Emacs session from their associated hook variables.
Keeps a copy of these values for future re-use."
  (mapc (lambda (elt) (var:remove (car elt) (cdr elt)))
	var::append-list)
  var::append-list)

(defun var:run-hook-in-matching-buffers (mode hook-function)
  "For a given major MODE (a symbol) call HOOK-FUNCTION in all existing buffers with that major mode.
This is used after a hook is changed to affect buffers that existed before the change was made."
  (mapc (lambda (buf) (with-current-buffer buf (funcall hook-function)))
	(delq nil (mapcar (lambda (buf) (if (eq (buffer-local-value 'major-mode buf) mode)
					    buf))
			  (buffer-list)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar var::append-list nil
  "List of (var-symbol . appended-list) elements saved from this Emacs session.")

(provide 'hvar)


;;; hvar.el ends here
