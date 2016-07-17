;;; hlvar.el --- GNU Hyperbole variables in local variable lists
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     4-Nov-91 at 00:26:06
;;
;; Copyright (C) 1995-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;; This file is a slight variant of that found in "files.el" from GNU Emacs.
;;
;;; Commentary:
;;
;;   Hyperbole uses the colon character extensively in its variable names.
;;   The standard GNU Emacs syntax for local variable setting does not allow
;;   the use of this character, even though it is a valid symbol name
;;   character.  The code here is slightly modified to support local setting of
;;   variables with colons in their names.
;;
;;   Where the standard code allows: var:val
;    This code requires one use:     var: val  (where var may include colons)
;;
;;   So functionality is gained and none is lost, but a slight incompatibility
;;   in protocol is introduced.

;;; Code:
;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hack-local-variables (&optional force)
  "Parse, and bind or evaluate as appropriate, any local variables
for current buffer."
  (if (fboundp 'hack-local-variables-prop-line)
      (hack-local-variables-prop-line))
  ;; Look for "Local variables:" line in last page.
  (save-excursion
    (goto-char (point-max))
    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
    (let (local-start)
      (if (let ((case-fold-search t)
		(ignore nil))
	    (and (search-forward "Local Variables:" nil t)
		 (setq local-start (match-beginning 0))
		 (or (and (not (string-match "^19\\." emacs-version))
			  (not inhibit-local-variables))
		     force
		     (if (string-match "^19\\." emacs-version)
			 (cond ((eq enable-local-variables t) t)
			       ((eq enable-local-variables nil)
				(setq ignore t))))
		     (if ignore
			 nil
		       (save-window-excursion
			 (switch-to-buffer (current-buffer))
			 (save-excursion
			   (beginning-of-line)
			   (set-window-start (selected-window) (point)))
			 (y-or-n-p
			  (format "Set local variables as specified at end of %s? "
				  (file-name-nondirectory
				   buffer-file-name))))))))
	  (let ((continue t)
		prefix prefixlen suffix beg
		(enable-local-eval
		 (if (boundp 'enable-local-eval) enable-local-eval)))
	    ;; The prefix is what comes before "local variables:" in its line.
	    ;; The suffix is what comes after "local variables:" in its line.
	    (skip-chars-forward " \t")
	    (or (eolp)
		(setq suffix (buffer-substring (point)
					       (progn (end-of-line) (point)))))
	    (goto-char local-start)
	    (or (bolp)
		(setq prefix
		      (buffer-substring (point)
					(progn (beginning-of-line) (point)))))

	    (if prefix (setq prefixlen (length prefix)
			     prefix (regexp-quote prefix)))
	    (if suffix (setq suffix (concat (regexp-quote suffix) "$")))
	    (while continue
	      ;; Look at next local variable spec.
	      (if selective-display (re-search-forward "[\n\r]")
		(forward-line 1))
	      ;; Skip the prefix, if any.
	      (if prefix
		  (if (looking-at prefix)
		      (forward-char prefixlen)
		    (error "Local variables entry is missing the prefix")))
	      ;; Find the variable name; strip whitespace.
	      (skip-chars-forward " \t")
	      (setq beg (point))
	      ;;
	      ;; Bob Weiner - changed here to allow colons in var names.
	      ;;
	      (skip-chars-forward "^ \t\n\r")
	      (skip-chars-backward ":")
	      (or (looking-at "[ \t]*:")
		  (error "(hack-local-variables): Missing colon in local variables entry"))
	      ;;
	      ;; Bob Weiner - end changes.
	      ;;
	      (let* ((str (buffer-substring beg (point)))
		     (var (read str))
		     val)
		;; Setting variable named "end" means end of list.
		(if (string-equal (downcase str) "end")
		    (setq continue nil)
		  ;; Otherwise read the variable value.
		  (skip-chars-forward "^:")
		  (forward-char 1)
		  (setq val (read (current-buffer)))
		  (skip-chars-backward "\n\r")
		  (skip-chars-forward " \t")
		  (or (if suffix (looking-at suffix) (eolp))
		      (error "Local variables entry is terminated incorrectly"))
		  ;; Set the variable.  "Variables" mode and eval are funny.
		  (if (fboundp 'hack-one-local-variable)
		      (hack-one-local-variable var val)
		    (cond ((eq var 'mode)
			   (funcall (intern (concat (downcase (symbol-name val))
						    "-mode"))))
			  ((eq var 'eval)
			   (if (string-equal (user-login-name) "root")
			       (message
				"Ignoring `eval:' in file's local variables")
			     (eval val)))
			  (t (make-local-variable var)
			     (set var val))))))))))
    (run-hooks 'hack-local-variables-hook)))



(provide 'hlvar)

;;; hlvar.el ends here
