;;; hrmail.el --- GNU Hyperbole buttons in mail reader: Rmail
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     9-May-91 at 04:22:02
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;; 
;;   Automatically configured for use in "hyperbole.el".
;;   If hsettings loading fails prior to initializing Hyperbole Rmail support,
;;
;;       {M-x Rmail-init RET}
;;
;;   will do it.
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hmail hact rmail rmailsum rmailedit)))
(load "hsmail")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun Rmail-init ()
  "Initializes Hyperbole support for Rmail mail reading."
  (interactive)
  (setq hmail:composer  'mail-mode
	hmail:lister    'rmail-summary-mode
	hmail:modifier  'rmail-edit-mode
	hmail:reader    'rmail-mode)
  (var:append 'rmail-show-message-hook '(hmail:msg-narrow))
  ;;
  ;;
  ;; Setup public abstract interface to Hyperbole defined mail
  ;; reader-specific functions used in "hmail.el".
  ;;
  (rmail:init)
  ;;
  ;; Setup private abstract interface to mail reader-specific functions
  ;; used in "hmail.el".
  ;;
  (defalias 'rmail:get-new       'rmail-get-new-mail)
  (defalias 'rmail:msg-forward   'rmail-forward)
  (defalias 'rmail:summ-msg-to   'rmail-summary-goto-msg)
  (defalias 'rmail:summ-new      'rmail-new-summary)
  (if (called-interactively-p 'interactive)
      (message "Hyperbole RMAIL mail reader support initialized."))
  )

(defun Rmail-msg-hdrs-full (toggled)
  "If TOGGLED is non-nil, toggle full/hidden headers, else show full headers."
  (save-excursion
    (if (or toggled
	    (let ((tog nil))
	      (save-excursion
		(save-restriction
		  (rmail-maybe-set-message-counters)
		  (narrow-to-region (rmail-msgbeg rmail-current-message)
				    (point-max))
		  (let ((buffer-read-only nil))
		    (goto-char (point-min))
		    (forward-line 1)
		    ;; Need to show full header
		    (if (eq (following-char) ?1)
			(setq tog t)))))
	      tog))
	(progn (rmail-toggle-header)
	       (setq toggled t)))
    toggled))

(defun Rmail-msg-narrow ()
  "Narrows mail reader buffer to current message.
This includes Hyperbole button data."
  (let ((beg (rmail-msgbeg rmail-current-message))
	(end (rmail-msgend rmail-current-message)))
    (narrow-to-region beg end)))

(defun Rmail-msg-next ()        (rmail-next-undeleted-message 1))

(defun Rmail-msg-num ()
  "Returns number of Rmail message that point is within."
  (interactive)
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-backward "^\^_" nil t)))
       (if (= opoint (point))
	   (backward-char 1)
	 (setq count (1+ count)))))
    count))

(defun Rmail-msg-prev ()        (rmail-previous-undeleted-message 1))

(defun Rmail-msg-to-p (mail-msg-id mail-file)
  "Sets current buffer to start of msg with MAIL-MSG-ID in MAIL-FILE.
Returns t if successful, else nil."
  (if (not (file-readable-p mail-file))
      nil
    (let ((buf (get-file-buffer mail-file)))
      (cond (buf
	     (switch-to-buffer buf)
	     (or (eq major-mode 'rmail-mode)
		 (rmail mail-file)))
	    (t (rmail mail-file))))
    (widen)
    (goto-char 1)
    (if (re-search-forward (concat rmail:msg-hdr-prefix
				   (regexp-quote mail-msg-id)) nil t)
	;; Found matching msg
	(progn
	  (setq buffer-read-only t)
	  (rmail-show-message (Rmail-msg-num))
	  t))))


(defun Rmail-msg-widen ()
  "Widens buffer to full current message including Hyperbole button data."
  (let ((start (point-min))
	(end (point-max)))
    (unwind-protect
	(save-excursion
	  (widen)
	  (if (re-search-forward "^\^_" nil t)
	      (progn (forward-char -1)
		     (setq end (point)))))
      (narrow-to-region start end))))

(defun Rmail-to ()
  "Sets current buffer to a mail reader buffer."
  (and (eq major-mode 'rmail-summary-mode) (set-buffer rmail-buffer)))

(defalias 'Rmail-Summ-delete        'rmail-summary-delete-forward)

(defalias 'Rmail-Summ-expunge       'rmail-summary-expunge)

(defalias 'Rmail-Summ-goto          'rmail-summary-goto-msg)

(defun Rmail-Summ-to ()
  "Sets current buffer to a mail listing buffer."
  (and (eq major-mode 'rmail-mode) (set-buffer rmail-summary-buffer)))

(defalias 'Rmail-Summ-undelete-all  'rmail-summary-undelete-many)

;;; ************************************************************************
;;; Overloaded functions
;;; ************************************************************************

(if (featurep 'rmail-hyperbole)
    ;; No overloads are necessary, the needed features are built-in.
    nil

;;; else
;;;
;;; Redefine version of this function from "rmailedit.el" to include any
;;; hidden Hyperbole button data when computing message length.
(defun rmail-cease-edit ()
  "Finish editing message; switch back to Rmail proper."
  (interactive)
  ;; Make sure buffer ends with a newline.
  (save-excursion
    (Rmail-msg-widen)
    (goto-char (point-max))
    (if (not (eq (preceding-char) ?\n))
	(insert "\n"))
    ;; Adjust the marker that points to the end of this message.
    (set-marker (aref rmail-message-vector (1+ rmail-current-message))
		(point))
    (hmail:msg-narrow))
  (let ((old rmail-old-text))
    ;; Update the mode line.
    (set-buffer-modified-p (buffer-modified-p))
    (rmail-mode-1)
    (if (and (= (length old) (- (point-max) (point-min)))
	     (string-equal old (buffer-substring (point-min) (point-max))))
	nil
      (setq old nil)
      (rmail-set-attribute "edited" t)
      (if (boundp 'rmail-summary-vector)
	  (progn
	    (aset rmail-summary-vector (1- rmail-current-message) nil)
	    (save-excursion
	      (rmail-widen-to-current-msgbeg
	       (lambda ()
		 (forward-line 2)
		 ;; Delete summary line which may have changed.
		 (if (looking-at "Summary-line: ")
		     (let ((buffer-read-only nil))
		       (delete-region (point)
				      (progn (forward-line 1)
					     (point)))))
		 ;;
		 ;; Update internal copy of subject line, from
		 ;; message subject line, which may have changed.
		 (let ((subject
			(save-excursion
			  (if (re-search-forward "^Subject:" nil t)
			      (progn 
				;; Try to find subject in msg header.
				(re-search-forward "^Subject:" nil t)
				(skip-chars-forward " \t")
				(buffer-substring
				 (point) (progn (re-search-forward
						 "\n[^ \t]" nil t)
						(- (point) 2))))))))
		   (if (and subject
			    (search-forward "\n*** EOOH ***\n" nil t))
		       (let ((buffer-read-only nil))
			 (goto-char (match-beginning 0))
			 (if (re-search-backward "^Subject:" nil t)
			     (delete-region (point)
					    (progn (re-search-forward
						    "\n[^ \t]" nil t)
						   (1- (point)))))
			 (beginning-of-line)
			 (insert "Subject: " subject "\n"))))))

	      ;; Adjust the marker that points to the end of this message
	      ;; since editing may have occured above.
	      (goto-char (point-max))
	      (set-marker (aref rmail-message-vector (1+ rmail-current-message))
			  (point))

	      ;; Update summary line for current message, if it exists in the
	      ;; summary buffer.
	      (rmail-summary-update-line rmail-current-message)

	      ;; This may set the current buffer to the summary buffer.
	      (rmail-show-message))))))
  (setq buffer-read-only t))


;;; Redefine version of this function from "rmail.el" to include any
;;; Hyperbole button data.
(defun rmail-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (if resend
      (call-interactively 'rmail-resend)
    (let* ((forward-buffer (current-buffer))
	   (toggle-header-flag)
	   (mime-version
	    (or (mail-fetch-field "Mime-Version")
		(progn (rmail-toggle-header)
		       (setq toggle-header-flag t)
		       nil)
		(mail-fetch-field "Mime-Version")))
	   (mime-content-type
	    (if mime-version (mail-fetch-field "Content-Type")))
	   (mime-content-encoding
	    (if mime-version (mail-fetch-field "Content-Transfer-Encoding")))
	   (subject (concat "["
			    (let ((from (or (mail-fetch-field "From")
					    (mail-fetch-field ">From"))))
			      (if from
				  (concat (mail-strip-quoted-names from) ": ")
				""))
			    (or (mail-fetch-field "Subject") "")
			    "]")))
      (if toggle-header-flag (rmail-toggle-header))
      (if mime-version (setq subject (concat subject "\nMime-Version: "
					     mime-version
					     (if mime-content-type
						 (concat "\nContent-Type: "
							 mime-content-type))
					     (if mime-content-encoding
						 (concat
						  "\nContent-Transfer-Encoding: "
						  mime-content-encoding)))))
      (save-restriction
	(Rmail-msg-widen)
	;; Turn off the usual actions for initializing the message body
	;; because we want to get only the text from the failure message.
	(let (mail-signature message-setup-hook)
	  ;; If only one window, use it for the mail buffer.
	  ;; Otherwise, use another window for the mail buffer
	  ;; so that the Rmail buffer remains visible
	  ;; and sending the mail will get back to it.
	  (if (funcall (cond ((and (not rmail-mail-new-frame) (one-window-p t))
			      'mail)
			     ((fboundp 'rmail-start-mail)
			      'rmail-start-mail)
			     (t 'mail-other-window))
		       nil nil subject nil nil nil
		       (list (list (lambda (buf msgnum)
				     (with-current-buffer buf
				       (rmail-set-attribute
					"forwarded" t msgnum)))
				   (current-buffer)
				   rmail-current-message)))
	      (save-excursion
		;; Insert after header separator--before signature if any.
		(goto-char (point-min))
		(search-forward-regexp
		 (concat "^" (regexp-quote mail-header-separator)))
		(forward-line 1)
		(insert-buffer-substring forward-buffer)
		(hmail:msg-narrow))))))))

;;; Redefine version of 'rmail-get-new-mail' from "rmail.el" to highlight
;;; Hyperbole buttons when possible.
;;;
(if (boundp 'rmail-get-new-mail-post-hook)
    (add-hook 'rmail-get-new-mail-post-hook
	      (lambda ()
		(if (fboundp 'hproperty:but-create)
		    (progn (widen) (hproperty:but-create)
			   (rmail-show-message)))))
  (hypb:function-overload 'rmail-get-new-mail nil
			  '(if (fboundp 'hproperty:but-create)
			       (progn (widen) (hproperty:but-create)
				      (rmail-show-message)))))

;;; Redefine version of 'rmail-new-summary' from "rmailsum.el" to
;;; highlight Hyperbole buttons when possible.
;;;
(if (boundp 'rmail-summary-create-post-hook)
    (add-hook 'rmail-summary-create-post-hook
	      (lambda ()
		(if (fboundp 'hproperty:but-create)
		    (hproperty:but-create))))
  (hypb:function-overload 'rmail-new-summary nil
			  '(if (fboundp 'hproperty:but-create)
			       (hproperty:but-create))))

;; end not InfoDock
)


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hrmail)

;;; hrmail.el ends here
