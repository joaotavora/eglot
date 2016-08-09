;;; hsmail.el --- GNU Hyperbole buttons in mail composer: mail and mh-letter
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     9-May-91 at 04:50:20
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

(require 'sendmail)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar smail:comment nil
  "Default comment form to evaluate and add to outgoing mail and Gnus postings.
Default is nil for no comment.  Set to: '(format \"Comments: GNU Hyperbole mail buttons accepted, v%s.\n\" hyperb:version)
for a comment.")

;;; Used by 'mail-send' in Emacs "sendmail.el".
(if (boundp 'send-mail-function)
    (or (if (listp send-mail-function)
	    (unless (equal (nth 2 send-mail-function) '(smail:widen))
	      (error
	       "(hsmail): Set 'send-mail-function' to a symbol-name, not a list, before load.")))
	(setq send-mail-function `(lambda () (smail:widen) (,send-mail-function))))
  (error "(hsmail): Install an Emacs \"sendmail.el\" which includes 'send-mail-function'."))

(if (fboundp 'mail-prefix-region)
    ;;
    ;; For compatibility with rsw-modified sendmail.el.
    (defvar mail-yank-hook
      ;; Set off original message.
      (lambda () (mail-prefix-region (hypb:mark t) (point)))
      "*Hook to run mail yank preface function.
Expects point and mark to be set to the region to preface.")
  ;;
  ;; Else for compatibility with Supercite and GNU Emacs.
  ;; If you create your own yank hook, set this variable rather than
  ;; `mail-yank-hook' from above.
  (defvar mail-citation-hook nil
    "Hook for modifying a citation just inserted in the mail buffer.
Each hook function can find the citation between (point) and (mark t),
and should leave point and mark around the citation text as modified.
The hook functions can find the header of the cited message
in the variable `mail-citation-header', whether or not this is included
in the cited portion of the message.

If this hook is entirely empty (nil), a default action is taken
instead of no action.")
  (defvar mail-yank-hooks '(mail-indent-citation)
      "*Obsolete hook to run mail yank citation function.  Use mail-citation-hook instead.
Expects point and mark to be set to the region to cite."))

;; For compatibility with Supercite and GNU Emacs.
(defvar mail-yank-prefix "> "
  "*Prefix to insert on lines of yanked message being replied to.
If this is nil, use indentation, as specified by `mail-indentation-spaces'.")

(defvar mail-indentation-spaces 3
  "*Number of spaces to insert at the beginning of each cited line.
Used by `mail-yank-original' via `mail-indent-citation'.")

;;; ************************************************************************
;;; Overloaded functions
;;; ************************************************************************

(defun smail:comment-add (&optional comment-form)
  "Adds a comment to the current outgoing message if Hyperbole has been loaded and `inhibit-hyperbole-messaging' is nil.
Optional COMMENT-FORM is evaluated to obtain the string to add to the
message.  If not given, 'smail:comment' is evaluated by default."
  (if (and (featurep 'hyperbole) (not inhibit-hyperbole-messaging))
      (let ((comment (eval (or comment-form smail:comment))))
	(if comment
	    (save-excursion
	      (goto-char (point-min))
	      (and (or (search-forward mail-header-separator nil t)
		       (if (eq major-mode 'mh-letter-mode)
			   (search-forward "\n--------" nil t)))
		   (not (search-backward comment nil t))
		   (progn (beginning-of-line) (insert comment))))))))

(defun smail:widen ()
  "Widens outgoing mail buffer to include Hyperbole button data."
  (if (fboundp 'mail+narrow) (mail+narrow) (widen)))

;; Redefine this function from Emacs "sendmail.el" to work with supercite.
(defun mail-indent-citation ()
  "Modify text just inserted from a message to be cited.
The inserted text should be the region.
When this function returns, the region is again around the modified text.

Normally, indent each nonblank line `mail-indentation-spaces' spaces.
However, if `mail-yank-prefix' is non-nil, insert that prefix on each line."
  ;; Don't ever remove headers if user uses Supercite package,
  ;; since he can set an option in that package to do
  ;; the removal.
  (unless (hypb:supercite-p)
    (mail-yank-clear-headers (region-beginning) (region-end)))
  (if (null mail-yank-prefix)
      (indent-rigidly (region-beginning) (region-end)
		      mail-indentation-spaces)
    (save-excursion
      (let ((end (set-marker (make-marker) (region-end))))
	(goto-char (region-beginning))
	(while (< (point) end)
	  (insert mail-yank-prefix)
	  (forward-line 1))))))

;; Redefine this function from "sendmail.el" to include Hyperbole button
;; data when yanking in a message and to highlight buttons if possible.
(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in Rmail).
Puts point before the text and mark after.
Applies 'mail-citation-hook', 'mail-yank-hook' or 'mail-yank-hooks'
to text (in decreasing order of precedence).
Just \\[universal-argument] as argument means don't apply hooks
and don't delete any header fields.

If supercite is in use, header fields are never deleted.
Use (setq sc-nuke-mail-headers 'all) to have them removed."
  (interactive "P")
  (if mail-reply-action
      (let ((start (point))
	    (original mail-reply-action)
	    (omark (mark t))
	    opoint)
	(and (consp original) (eq (car original) 'insert-buffer)
	     (setq original (nth 1 original)))
	(unwind-protect
	    (progn
	      (if (consp original)
		  (progn
		    ;; Call yank function, and set the mark if it doesn't.
		    (apply (car original) (cdr original))
		    (if (eq omark (mark t))
			(push-mark (point))))
		;; If the original message is in another window in the same
		;; frame, delete that window to save space.
		(delete-windows-on original t)
		(with-current-buffer original
		  ;; Might be called from newsreader before any
		  ;; Hyperbole mail reader support has been autoloaded.
		  (cond ((fboundp 'rmail:msg-widen) (rmail:msg-widen))
			((eq major-mode 'news-reply-mode) (widen))))
		(setq opoint (point))
		(with-no-warnings
		  ;; We really want this to set mark.
		  (insert-buffer original)
		  ;; If they yank the original text, the encoding of the
		  ;; original message is a better default than
		  ;; the default buffer-file-coding-system.
		  (and (coding-system-equal
			(default-value 'buffer-file-coding-system)
			buffer-file-coding-system)
		       (setq buffer-file-coding-system
			     (coding-system-change-text-conversion
			      buffer-file-coding-system
			      (coding-system-base
			       (with-current-buffer original
				 buffer-file-coding-system))))))
		(set-text-properties (point) (mark t) nil))
		(hmail:msg-narrow)
		(if (fboundp 'hproperty:but-create) (hproperty:but-create))
		(if (consp arg)
		    nil
		  ;; Don't ever remove headers if user uses Supercite package,
		  ;; since he can set an option in that package to do
		  ;; the removal.
		  (or (hypb:supercite-p)
		      (mail-yank-clear-headers
		       start (marker-position (hypb:mark-marker t))))
		  (goto-char start)
		  (let ((mail-indentation-spaces (if arg (prefix-numeric-value arg)
						   mail-indentation-spaces))
			;; Avoid error in Transient Mark mode
			;; on account of mark's being inactive.
			(mark-even-if-inactive t))
		    (cond ((and (boundp 'mail-citation-hook) mail-citation-hook)
			   ;; Bind mail-citation-header to the inserted
			   ;; message's header.
			   (let ((mail-citation-header
				  (buffer-substring-no-properties
				   start
				   (save-excursion
				     (save-restriction
				       (narrow-to-region start (point-max))
				       (goto-char start)
				       (rfc822-goto-eoh)
				       (point))))))
			     (run-hooks 'mail-citation-hook)))
			  ((and (boundp 'mail-yank-hook) mail-yank-hook)
			   (run-hooks 'mail-yank-hook))
			  ((and (boundp 'mail-yank-hooks) mail-yank-hooks)
			   (run-hooks 'mail-yank-hooks))
			  (t (mail-indent-citation))))
		  (goto-char (min (point-max) (hypb:mark t)))
		  (set-mark opoint)
		  (delete-region (point) ; Remove trailing blank lines.
				 (progn (re-search-backward "[^ \t\n\r\f]")
					(end-of-line)
					(point))))
		(unless (eq major-mode 'news-reply-mode)
		  ;; This is like exchange-point-and-mark, but doesn't activate the mark.
		  ;; It is cleaner to avoid activation, even though the command
		  ;; loop would deactivate the mark because we inserted text.
		  (goto-char (prog1 (hypb:mark t)
			       (set-marker (hypb:mark-marker t)
					   (point) (current-buffer))))
		  (if (not (eolp)) (insert ?\n))))
	  (with-current-buffer mail-reply-buffer
	    (hmail:msg-narrow))))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;;; Try to setup comment addition as the first element of these hooks.
(add-hook 'message-setup-hook  #'smail:comment-add)
(add-hook 'mh-letter-mode-hook #'smail:comment-add)

(provide 'hsmail)

;;; hsmail.el ends here
