;;; hvm.el ---  GNU Hyperbole buttons in mail reader: Vm
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    10-Oct-91 at 01:51:12
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Automatically configured for use in "hyperbole.el".
;;   If hsite loading fails prior to initializing Hyperbole Vm support,
;;
;;       {M-x Vm-init RET}
;;
;;   will do it.
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; VM is not available as a regular Emacs package, so it is somewhat
;; difficult to install properly.  Requiring it below will therefore
;; often trigger an error and prevent Hyperbole from properly
;; loading, so prevent this error with a condition-case around this
;; whole file.

(condition-case nil
(progn

(eval-and-compile
  (require 'hmail)
  (load "hsmail")
  (when (require 'vm nil t)
    (or (and (fboundp 'vm-edit-message) (fboundp 'vm-edit-message-end))
	(load "vm-edit"))
    (vm-session-initialization))) ;; this requires all of vm's modules

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; Current versions of VM define this next variable in "vm-vars.el".  We
;;; define it here for earlier VM versions.
(defvar vm-edit-message-mode nil
  "*Major mode to use when editing messages in VM.")

;;; "hmail.el" procedures will branch improperly if a regular mode, like VM's
;;; default `text-mode', is used for editing.
(setq vm-edit-message-mode 'vm-edit-mode)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun vm-edit-mode ()
  "Major mode for editing vm mail messages.
  Special commands:\\{vm-edit-message-map}
Turning on vm-edit-mode calls the value of the variable vm-edit-message-hook,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
;;  (use-local-map vm-edit-message-map)
  (setq mode-name "VM Edit")
  (setq major-mode 'vm-edit-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
;;  (run-hooks 'vm-edit-message-hook)
  )

;;;###autoload
(defun Vm-init ()
  "Initializes Hyperbole support for Vm mail reading."
  (interactive)
  (setq hmail:composer  'mail-mode
	hmail:lister    'vm-summary-mode
	hmail:modifier  'vm-edit-mode
	hmail:reader    'vm-mode)
  ;;
  ;; Setup public abstract interface to Hyperbole defined mail
  ;; reader-specific functions used in "hmail.el".
  ;;
  (rmail:init)
  ;;
  ;; Setup private abstract interface to mail reader-specific functions
  ;; used in "hmail.el".
  ;;
  (defalias 'rmail:get-new       'vm-get-new-mail)
  (defalias 'rmail:msg-forward   'vm-forward-message)
  (defalias 'rmail:summ-msg-to   'vm-follow-summary-cursor)
  (defalias 'rmail:summ-new      'vm-summarize)
  (if (called-interactively-p 'interactive)
      (message "Hyperbole VM mail reader support initialized."))
  )

(defun Vm-msg-hdrs-full (toggled)
  "If TOGGLED is non-nil, toggle full/hidden headers, else show full headers."
  (save-excursion
    (if (or toggled
	    (let ((exposed (= (point-min)
			      (vm-start-of (car vm-message-pointer)))))
	      (not exposed)))
	(progn (vm-expose-hidden-headers)
	       (setq toggled t)))
    toggled))

(defun Vm-msg-narrow ()
  "Narrows mail reader buffer to current message.
This includes Hyperbole button data."
  (save-excursion
    (vm-select-folder-buffer)
    (narrow-to-region (point-min) (Vm-msg-end))))

(defun Vm-msg-next ()           (vm-next-message 1))

(defun Vm-msg-num ()
  "Returns number of vm mail message that point is within, in physical message order."
  (interactive)
  (let ((count 1)
	(case-fold-search))
    (save-excursion
      (save-restriction
	(widen)
	(while (re-search-backward Vm-msg-start-regexp nil t)
	  (setq count (1+ count)))))
    count))

(defun Vm-msg-prev ()           (vm-previous-message 1))

(defun Vm-msg-to-p (mail-msg-id mail-file)
  "Sets current buffer to start of msg with MAIL-MSG-ID in MAIL-FILE.
Returns t if successful, else nil or signals error."
  (if (not (file-readable-p mail-file))
      nil
    (vm-visit-folder mail-file)
    (widen)
    (goto-char 1)
      (if (let ((case-fold-search))
	    (re-search-forward (concat rmail:msg-hdr-prefix
				       (regexp-quote mail-msg-id)) nil t))
	  ;; Found matching msg
	  (progn
	    (setq buffer-read-only t)
	    (vm-goto-message-at-point)
	    t))))

(defun Vm-msg-widen ()
  "Widens buffer to full current message including Hyperbole button data."
  (save-excursion
    (vm-select-folder-buffer)
    (narrow-to-region (point-min) (Vm-msg-end))))

(defun Vm-to ()
  "Sets current buffer to a mail reader buffer."
  (and (eq major-mode 'vm-summary-mode) (set-buffer vm-mail-buffer)))

(defun Vm-Summ-delete ()
  (vm-follow-summary-cursor)
  (vm-delete-message 1))

(defalias 'Vm-Summ-expunge          'vm-expunge-folder)

(defalias 'Vm-Summ-goto             'vm-follow-summary-cursor)

(defun Vm-Summ-to ()
  "Sets current buffer to a mail listing buffer."
  (and (eq major-mode 'vm-mode) (set-buffer vm-summary-buffer)))

(defun Vm-Summ-undelete-all ()
  (message
   "(Vm-Summ-undelete-all: Vm doesn't have an undelete all msgs function."))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun Vm-msg-end ()
  "Returns end point for current Vm message, including Hyperbole button data.
Has side-effect of widening buffer."
  (save-excursion
    (goto-char (point-min))
    (widen)
    (if (let ((case-fold-search))
	  (re-search-forward Vm-msg-start-regexp nil t))
	(match-beginning 0)
      (point-max))))

;;; Redefine version of this function from "vm-page.el" to hide any
;;; Hyperbole button data whenever a message is displayed in its entirety.
(defun vm-show-current-message ()
  (save-excursion
    (save-excursion
      (goto-char (point-min))
      (hmail:msg-narrow (point-min) (Vm-msg-end)))
    (and vm-honor-page-delimiters
	 (save-excursion
	   (if (search-forward page-delimiter nil t)
	       (progn
		 (goto-char (match-beginning 0))
		 (not (looking-at (regexp-quote hmail:hbdata-sep))))))
	 (progn
	   (if (looking-at page-delimiter)
	       (forward-page 1))
	   (vm-narrow-to-page))))
  ;; don't mark the message as read if the user can't see it!
  (if (vm-get-buffer-window (current-buffer))
      (progn
	(setq vm-system-state 'showing)
	(cond ((vm-new-flag (car vm-message-pointer))
	       (vm-set-new-flag (car vm-message-pointer) nil)))
	(cond ((vm-unread-flag (car vm-message-pointer))
	       (vm-set-unread-flag (car vm-message-pointer) nil)))
	(vm-update-summary-and-mode-line)
	(vm-howl-if-eom))
    (if (fboundp 'hproperty:but-create) (hproperty:but-create))
    (vm-update-summary-and-mode-line)))

;;; Redefine version of this function from "vm-page.el" to treat end of
;;; text (excluding Hyperbole button data) as end of message.
(defun vm-scroll-forward-internal (arg)
  (let ((direction (prefix-numeric-value arg))
	(w (selected-window)))
    (condition-case error-data
	(progn (scroll-up arg) nil)
      (error
       (if (or (and (< direction 0)
		    (> (point-min) (vm-text-of (car vm-message-pointer))))
	       (and (>= direction 0)
		    (/= (point-max)
			(save-restriction
			  (hmail:hbdata-start
			   (point-min)
			   (vm-text-end-of
			    (car vm-message-pointer)))))))
	   (progn
	     (vm-widen-page)
	     (if (>= direction 0)
		 (progn
		   (forward-page 1)
		   (set-window-start w (point))
		   nil )
	       (if (or (bolp)
		       (not (save-excursion
			      (beginning-of-line)
			      (looking-at page-delimiter))))
		   (forward-page -1))
	       (beginning-of-line)
	       (set-window-start w (point))
	       'tryagain))
	 (if (eq (car error-data) 'end-of-buffer)
	     (if vm-auto-next-message
		 'next-message
	       (set-window-point w (point))
	       'end-of-message)))))))

;;; Redefine version of this function from "vm-page.el" (called by
;;; vm-scroll-* functions).  Make it keep Hyperbole button data hidden.
(defun vm-widen-page ()
  (if (or (> (point-min) (vm-text-of (car vm-message-pointer)))
	  (/= (point-max) (vm-text-end-of (car vm-message-pointer))))
      (hmail:msg-narrow (vm-vheaders-of (car vm-message-pointer))
			(if (or (vm-new-flag (car vm-message-pointer))
				(vm-unread-flag (car vm-message-pointer)))
			    (vm-text-of (car vm-message-pointer))
			  (vm-text-end-of (car vm-message-pointer))))))

;;; Redefine version of this function from "vm-edit.el" to hide
;;; Hyperbole button data when insert edited message from temporary buffer.
(hypb:function-overload 'vm-edit-message nil '(hmail:msg-narrow))

;;; Redefine version of this function from "vm-edit.el" to hide
;;; Hyperbole button data when insert edited message from temporary buffer.
(defun vm-edit-message-end ()
  "End the edit of a message and copy the result to its folder."
  (interactive)
  (if (null vm-message-pointer)
      (error "This is not a VM message edit buffer."))
  (if (null (buffer-name (vm-buffer-of (car vm-message-pointer))))
      (error "The folder buffer for this message has been killed."))
  (let ((pos-offset (- (point) (point-min))))
    ;; make sure the message ends with a newline
    (goto-char (point-max))
    (and (/= (preceding-char) ?\n) (insert ?\n))
    ;; munge message separators found in the edited message to
    ;; prevent message from being split into several messages.
    (vm-munge-message-separators (vm-message-type-of (car vm-message-pointer))
				 (point-min) (point-max))
    ;; for From_-with-Content-Length recompute the Content-Length header
    (if (eq (vm-message-type-of (car vm-message-pointer))
	    'From_-with-Content-Length)
	(let ((buffer-read-only nil)
	      length)
	  (goto-char (point-min))
	  ;; first delete all copies of Content-Length
	  (while (and (re-search-forward vm-content-length-search-regexp nil t)
		      (null (match-beginning 1))
		      (progn (goto-char (match-beginning 0))
			     (vm-match-header vm-content-length-header)))
	    (delete-region (vm-matched-header-start) (vm-matched-header-end)))
	  ;; now compute the message body length
	  (goto-char (point-min))
	  (search-forward "\n\n" nil 0)
	  (setq length (- (point-max) (point)))
	  ;; insert the header
	  (goto-char (point-min))
	  (insert vm-content-length-header " " (int-to-string length) "\n")))
    (let ((edit-buf (current-buffer))
	  (mp vm-message-pointer))
      (if (buffer-modified-p)
	  (progn
	    (widen)
	    (with-current-buffer (vm-buffer-of (vm-real-message-of (car mp)))
	      (if (not (memq (vm-real-message-of (car mp)) vm-message-list))
		  (error "The original copy of this message has been expunged."))
	      (vm-save-restriction
	       (widen)
	       (goto-char (vm-headers-of (vm-real-message-of (car mp))))
	       (let ((vm-message-pointer mp)
		     opoint
		     (buffer-read-only nil))
		 (setq opoint (point))
		 (insert-buffer-substring edit-buf)
		 (delete-region
		  (point) (vm-text-end-of (vm-real-message-of (car mp))))
		 (vm-discard-cached-data)
		 (hmail:msg-narrow))
	       (vm-set-edited-flag-of (car mp) t)
	       (vm-set-edit-buffer-of (car mp) nil))
	      (set-buffer (vm-buffer-of (car mp)))
	      (if (eq (vm-real-message-of (car mp))
		      (vm-real-message-of (car vm-message-pointer)))
		  (progn
		    (vm-preview-current-message)
		    ;; Try to position the cursor in the message
		    ;; window close to where it was in the edit
		    ;; window.  This works well for non MIME
		    ;; messages, but the cursor drifts badly for
		    ;; MIME and for refilled messages.
		    (vm-save-buffer-excursion
		     (and vm-presentation-buffer
			  (set-buffer vm-presentation-buffer))
		     (vm-save-restriction
		      (vm-save-buffer-excursion
		       (widen)
		       (let ((osw (selected-window))
			     (new-win (vm-get-visible-buffer-window
				       (current-buffer))))
			 (unwind-protect
			     (if new-win
				 (progn
				   (select-window new-win)
				   (goto-char (vm-headers-of
					       (car vm-message-pointer)))
				   (condition-case nil
				       (forward-char pos-offset)
				     (error nil))))
			   (if (not (eq osw (selected-window)))
			       (select-window osw))))))))
		(vm-update-summary-and-mode-line))))
	(message "No change."))
      (vm-display edit-buf nil '(vm-edit-message-end)
		  '(vm-edit-message-end reading-message startup))
      (set-buffer-modified-p nil)
      (kill-buffer edit-buf))))


;;; Define this function if the VM version in use doesn't have it.
(or (fboundp 'vm-goto-message-at-point)
(defun vm-goto-message-at-point ()
  "In a VM folder buffer, select the message that contains point."
  (cond ((fboundp 'vm-update-search-position)
	 (vm-update-search-position t)
	 ;; vm-show-current-message only adjusts (point-max),
	 ;; it doesn't change (point-min).
	 (narrow-to-region
	  (vm-vheaders-of (car vm-message-pointer))
	  (point-max))
	 (vm-show-current-message)
	 (setq vm-system-state 'reading))
	((fboundp 'vm-isearch-update)
	 (vm-isearch-update)
	 (narrow-to-region
	  (vm-vheaders-of (car vm-message-pointer))
	  (point-max))
	 (vm-show-current-message)
	 (setq vm-system-state 'reading))
	(t (error "vm search code is missing, can't continue"))))
)

;;; Hide any Hyperbole button data when reply to or forward a message.
;;; See "vm-reply.el".
(var:append 'vm-mail-mode-hook '(hmail:msg-narrow))

;;; Redefine this function from "vm-folder.el" called whenever new mail is
;;; incorporated so that it will highlight Hyperbole buttons when possible.
;;  Returns list of new messages if there were any new messages, nil otherwise.
(defun vm-assimilate-new-messages (&optional
				   dont-read-attributes
				   gobble-order
				   labels first-time)
  (let ((tail-cons (vm-last vm-message-list))
	b-list new-messages)
    (save-excursion
      (vm-save-restriction
       (widen)
       (if (fboundp 'hproperty:but-create)
	   (hproperty:but-create))
       (vm-build-message-list)
       (if (or (null tail-cons) (cdr tail-cons))
	   (progn
             (if (not vm-assimilate-new-messages-sorted)
                 (setq vm-ml-sort-keys nil))
	     (if dont-read-attributes
		 (vm-set-default-attributes (cdr tail-cons))
	       (vm-read-attributes (cdr tail-cons)))
	     ;; Yuck.  This has to be done here instead of in the
	     ;; vm function because this needs to be done before
	     ;; any initial thread sort (so that if the thread
	     ;; sort matches the saved order the folder won't be
	     ;; modified) but after the message list is created.
	     ;; Since thread sorting is done here this has to be
	     ;; done here too.
	     (if gobble-order
		 (vm-gobble-message-order))
	     (if (or (vectorp vm-thread-obarray)
		     vm-summary-show-threads)
		 (vm-build-threads (cdr tail-cons))))))
      (setq new-messages (if tail-cons (cdr tail-cons) vm-message-list))
      (vm-set-numbering-redo-start-point new-messages)
      (vm-set-summary-redo-start-point new-messages))
    ;; Only update the folders summary count here if new messages
    ;; have arrived, not when we're reading the folder for the
    ;; first time, and not if we cannot assume that all the arrived
    ;; messages should be considered new.  Use gobble-order as a
    ;; first time indicator along with the new messages being equal
    ;; to the whole message list.
    (if (and new-messages dont-read-attributes
	     (or (not (eq new-messages vm-message-list))
		 (null gobble-order)))
	(vm-modify-folder-totals buffer-file-name 'arrived
				 (length new-messages)))
    ;; copy the new-messages list because sorting might scramble
    ;; it.  Also something the user does when
    ;; vm-arrived-message-hook is run might affect it.
    ;; vm-assimilate-new-messages returns this value so it must
    ;; not be mangled.
    (setq new-messages (copy-sequence new-messages))
    ;; add the labels
    (if (and new-messages labels (boundp 'vm-burst-digest-messages-inherit-labels)
	     vm-burst-digest-messages-inherit-labels)
	(let ((mp new-messages))
	  (while mp
	    (vm-set-labels-of (car mp) (copy-sequence labels))
	    (setq mp (cdr mp)))))
    (if (and new-messages vm-summary-show-threads)
	(progn
	  ;; get numbering and summary of new messages done now
	  ;; so that the sort code only has to worry about the
	  ;; changes it needs to make.
	  (vm-update-summary-and-mode-line)
	  (vm-sort-messages "thread")))
    (if (and new-message
	     (or vm-arrived-message-hook vm-arrived-messages-hook)
	     ;; Run the hooks only if this is not the first
	     ;; time vm-assimilate-new-messages has been called
	     ;; in this folder.
	     (not first-time))
	(let ((new-messages new-messages))
	  ;; seems wise to do this so that if the user runs VM
	  ;; commands here they start with as much of a clean
	  ;; slate as we can provide, given we're currently deep
	  ;; in the guts of VM.
	  (vm-update-summary-and-mode-line)
	  (if vm-arrived-message-hook
	      (while new-messages
		(vm-run-message-hook (car new-messages)
				     'vm-arrived-message-hook)
		(setq new-messages (cdr new-messages))))
	  (run-hooks 'vm-arrived-messages-hook)))
    (if (and new-messages vm-virtual-buffers)
	(save-excursion
	  (setq b-list vm-virtual-buffers)
	  (while b-list
	    ;; buffer might be dead
	    (if (buffer-name (car b-list))
		(let (tail-cons)
		  (set-buffer (car b-list))
		  (setq tail-cons (vm-last vm-message-list))
		  (vm-build-virtual-message-list new-messages)
		  (if (or (null tail-cons) (cdr tail-cons))
		      (progn
                        (if (not vm-assimilate-new-messages-sorted)
                            (setq vm-ml-sort-keys nil))
			(if (vectorp vm-thread-obarray)
			    (vm-build-threads (cdr tail-cons)))
			(vm-set-summary-redo-start-point
			 (or (cdr tail-cons) vm-message-list))
			(vm-set-numbering-redo-start-point
			 (or (cdr tail-cons) vm-message-list))
			(if (null vm-message-pointer)
			    (progn (setq vm-message-pointer vm-message-list
					 vm-need-summary-pointer-update t)
				   (if vm-message-pointer
				       (vm-preview-current-message))))
			(if vm-summary-show-threads
			    (progn
			      (vm-update-summary-and-mode-line)
			      (vm-sort-messages "thread")))))))
	    (setq b-list (cdr b-list)))))
    (if (and new-messages vm-ml-sort-keys)
        (vm-sort-messages vm-ml-sort-keys))
    new-messages ))

;;; Redefine version of `vm-force-mode-line-update' from "vm-folder.el"
;;; to highlight Hyperbole buttons in summary buffers.
(defun vm-force-mode-line-update ()
  "Force a mode line update in all frames."
  (if vm-summary-buffer
      (with-current-buffer vm-summary-buffer
	(if (fboundp 'hproperty:but-create) (hproperty:but-create))))
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update t)
    (with-current-buffer (other-buffer)
      (set-buffer-modified-p (buffer-modified-p)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar Vm-msg-start-regexp "\n\nFrom \\|\n\001\001\001\001"
  "Regular expression that begins a Vm mail message.")

(provide 'hvm)
)
(error nil))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; hvm.el ends here
