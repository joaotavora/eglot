;;; hmh.el ---  GNU Hyperbole buttons in mail reader: Mh
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-May-91 at 17:06:36
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;; 
;;   Automatically configured for use in "hsite.el".
;;   If hsite loading fails prior to initializing Hyperbole Mh support,
;;
;;       {M-x Mh-init RET}
;;
;;   will do it.
;;
;;
;;     Have not yet overloaded 'mh-yank-cur-msg' to yank and hide
;;   button data from mail reader buffer.
;;     Have not yet overloaded 'mh-insert-letter' to highlight buttons
;;   and to merge its button data.
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hmail mh-e)))
(load "hsmail")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun Mh-init ()
  "Initializes Hyperbole support for Mh mail reading."
  (interactive)
  (setq hmail:composer  'mh-letter-mode
	hmail:lister    'mh-folder-mode
	hmail:modifier  'mh-letter-mode
	hmail:reader    'mh-show-mode)
  (var:append 'mh-show-hook '(hmail:msg-narrow Mh-hbut-highlight))
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
  (defalias 'rmail:get-new       'mh-inc-folder)
  (defalias 'rmail:msg-forward   'mh-redistribute)
  (defalias 'rmail:summ-msg-to   'mh-goto-msg)
  (defalias 'rmail:summ-new      'mh-rescan-folder)
  (if (called-interactively-p 'interactive)
      (message "Hyperbole MH mail reader support initialized."))
  )

(defun Mh-hbut-highlight ()
  "Highlight any Hyperbole buttons in buffer for which display support exists."
  (if (fboundp 'hproperty:but-create) (hproperty:but-create)))

(defun Mh-msg-hdrs-full (toggled)
  "If TOGGLED is non-nil, toggle full/hidden headers, else show full headers.
For now, a no-op."
  )

(defun Mh-msg-narrow ()
  "Narrows mail reader buffer to current message.
This includes Hyperbole button data."
  (Mh-msg-widen))

(defun Mh-msg-next ()           (mh-next-undeleted-msg 1))

(defun Mh-msg-num ()
  "Returns number of mail message that point is within."
  (interactive)
  (mh-get-msg-num nil))

(defun Mh-msg-prev ()           (mh-previous-undeleted-msg 1))

(defun Mh-msg-to-p (mail-msg-id mail-file)
  "Sets current buffer to start of msg with MAIL-MSG-ID in MAIL-FILE.
Returns t if successful, else nil."
  (if (not (file-readable-p mail-file))
      nil
    (find-file mail-file)
    (hmail:msg-narrow)
    (goto-char 1)
    t))

(defun Mh-msg-widen ()
  "Widens buffer to full current message including Hyperbole button data."
  (Mh-to) (widen))

(defun Mh-to ()
  "Sets current buffer to a mail reader buffer."
  (and (eq major-mode 'Mh-folder-mode)
       (set-buffer mh-show-buffer)))

(defun Mh-Summ-delete ()        (mh-delete-msg (mh-get-msg-num t)))

(defalias 'Mh-Summ-expunge          'mh-execute-commands)

(defun Mh-Summ-goto ()
  (let ((msg-num (mh-get-msg-num nil)))
    (mh-goto-msg msg-num nil t)
    (mh-show msg-num)))

(defun Mh-Summ-to ()
  "Sets current buffer to a mail listing buffer."
  (let ((summ-buf))
    (save-excursion
      (mapc (lambda (window)
	      (if summ-buf
		  nil
		(set-buffer (window-buffer window))
		(if (eq major-mode 'Mh-folder-mode)
		    (setq summ-buf (current-buffer)))))
	    (hypb:window-list 'no-mini)))
    (if summ-buf (set-buffer summ-buf))))

(defun Mh-Summ-undelete-all ()
  (message
   "(Mh-Summ-undelete-all: I don't think mh-e has an undelete operator."))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************
;;;
;;; Redefine version of this function from mh-e.el to run mh-show-hook at end.
;;; This hook may already be run, depending on the version of mh-e you are
;;; running, but running it twice shouldn't do any harm.  Comment this out if
;;; you know that your mh-e.el already runs the hook.
(hypb:function-overload 'mh-display-msg nil
			'(run-hooks 'mh-show-hook))

;;;
;;; Redefine version of 'mh-regenerate-headers' to highlight Hyperbole
;;; buttons when possible.
;;;
(hypb:function-overload 'mh-regenerate-headers nil
			'(if (fboundp 'hproperty:but-create)
			     (hproperty:but-create)))

;;;
;;; Set 'mh-send-letter' hook to widen to include button data before sending.
;;;
(var:append 'mh-before-send-letter-hook '(widen))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'hmh)


;;; hmh.el ends here
