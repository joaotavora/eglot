;;; hmail.el --- GNU Hyperbole buttons embedded in e-mail messages
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     9-Oct-91 at 18:38:05
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the HY-COPY (Hyperbole) or BR-COPY (OO-Browser) file for license
;; information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   The `hmail' class provides an abstract interface for connecting
;;   GNU Emacs-based mail readers and composers to Hyperbole.  Its
;;   public variables together with supporting classes determine the
;;   mail tools that Hyperbole will support.
;;
;;   The `rmail' and `lmail' classes provide a set of feature names
;;   that Hyperbole packages can call to interface to a user's selected
;;   mail reader.  Eventually, a full abstract calling interface may be
;;   developed.  The public features (the ones above the line of dashes)
;;   must be redefined for any mail reader.  The private features are
;;   used only by a particular mail reader.
;;
;;   The `smail' class is similar; it connects a mail composer for use
;;   with Hyperbole.

;;; Code:
;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hnews:composer  'news-reply-mode
 "Major mode for composing USENET news to be sent with Hyperbole buttons.")
(defvar hnews:lister    'gnus-summary-mode
 "Major mode for listing USENET news header summaries with Hyperbole buttons.")
(defvar hnews:reader    'gnus-article-mode
 "Major mode for reading USENET news with Hyperbole buttons.")

(defcustom hmail:init-function nil
  "*Function (a symbol) to run to initialize Hyperbole support for a mail reader/composer.
Valid values are: nil, Mh-init, Rmail-init or Vm-init."
  :type '(choice (const nil)
		 (const Mh-init)
		 (const Rmail-init)
		 (const Vm-init))
  :group 'hyperbole-commands)

(defvar hmail:composer  'message-mode
 "Major mode for composing mail to be sent with Hyperbole buttons.")
(defvar hmail:lister    nil
 "Major mode for listing mail header summaries with Hyperbole buttons.")
(defvar hmail:modifier  nil
 "Major mode for editing received mail with Hyperbole buttons.")
(defvar hmail:reader    nil
 "Major mode for reading mail with Hyperbole buttons.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; ========================================================================
;;; hmail class - abstract
;;; ========================================================================

(defun hmail:hbdata-start (&optional msg-start msg-end)
  "Returns point immediately before any Hyperbole button data in current msg.
Returns message end point when no button data is found.
Has side-effect of widening buffer. 
Message's displayable part begins at optional MSG-START and ends at or before
MSG-END."
  (widen)
  (or msg-end (setq msg-end (point-max)))
  (save-excursion
    (goto-char msg-end)
    (if (search-backward hmail:hbdata-sep msg-start t) (1- (point)) msg-end)))

(defun hmail:hbdata-to-p ()
  "Moves point to Hyperbole but data start in an e-mail msg.
Returns t if button data is found."
  (and (cond ((memq major-mode (list hmail:reader hmail:modifier))
	      (hmail:msg-narrow) t)
	     ((or (hmail:lister-p) (hnews:lister-p)) t)
	     ((memq major-mode (list hmail:composer hnews:reader
				     hnews:composer))
	      (widen) t))
       (progn
	 (goto-char (point-max))
	 (if (search-backward hmail:hbdata-sep nil t)
	     (progn (forward-line 1) t)))))

(defun hmail:browser-p ()
  "Returns t iff current major mode helps browse received e-mail messages."
  (memq major-mode (list hmail:reader hmail:lister)))

(defun hmail:buffer (&optional buf invisible-flag)
  "Start composing mail with the contents of optional BUF as the message body.
Invisible text is expanded and included in the mail only if INVISIBLE-FLAG is
non-nil.  BUF defaults to the current buffer and may be a buffer or buffer
name."
  (interactive (list (current-buffer) (y-or-n-p "Include invisible text? ")))
  (or buf (setq buf (current-buffer)))
  (if (stringp buf) (setq buf (get-buffer buf)))
  (set-buffer buf)
  (hmail:region (point-min) (point-max) buf invisible-flag))

;;;###autoload
(defun hmail:compose (address expr &optional subject help)
  "Compose mail with ADDRESS and evaluation of EXPR.
Optional SUBJECT and HELP message may also be given."
  (interactive "sDeliver e-mail to: \nSubject: ")
  (require 'hactypes) ;; Needed in case EXPR calls hact.
  (unless (or (stringp help) (stringp subject))
    (setq subject "Be explicit here.  Make a statement or ask a question."))
  (hmail:invoke address nil subject)
  (eval expr)
  (if (re-search-backward "^Subject: " nil t)
      (goto-char (match-end 0)))
  (message (if (stringp help)
	       help
	     "Replace subject, compose message, and then mail.")))

(defun hmail:composing-dir (key-src)
  "If button KEY-SRC is a mail/news composure buffer, returns composure directory, else nil."
  (save-excursion
    (and (bufferp key-src)
	 (progn (set-buffer key-src)
		(or (eq major-mode hmail:composer)
		    (eq major-mode hnews:composer)))
	 default-directory)))

(defun hmail:editor-p ()
  "Returns t iff current major mode edits Hyperbole e-mail/news messages."
  (memq major-mode (list hmail:composer hnews:composer hmail:modifier)))

(defun hmail:init (class-prefix func-suffix-list)
  "Sets up CLASS-PREFIX functions with aliases for FUNC-SUFFIX-LIST.
`hmail:reader' should be set appropriately before this is called."
  (when hmail:reader
    (let* ((reader-name (symbol-name hmail:reader))
	   (reader-prefix (capitalize
			   (substring reader-name
				      0 (string-match "-" reader-name))))
	   hmail-func)
      (mapcar (lambda (func-suffix)
		(setq hmail-func (hypb:replace-match-string
				  "Summ-" func-suffix "" t))
		(defalias (intern (concat class-prefix hmail-func))
		  (intern (concat reader-prefix "-" func-suffix))))
	      func-suffix-list))))

(defun hmail:invoke (&optional address cc subject)
  "Invoke user preferred mail composer: vm-mail, mh-send or mail.
Optional arguments are ADDRESS, CC list and SUBJECT of mail."
  ;; Next 3 lines prevent blank lines between fields due to
  ;; fill-region-as-paragraph within mail-setup.
  (if (equal address "") (setq address nil))
  (if (equal cc "") (setq cc nil))
  (if (equal subject "") (setq subject nil))
  (compose-mail address subject (if cc (list (cons "CC" cc)))))


(defun hmail:lister-p ()
  "Returns t iff current major mode is a Hyperbole e-mail lister mode."
  (eq major-mode hmail:lister))

(defun hnews:lister-p ()
  "Returns t iff current major mode is a Hyperbole news summary lister mode."
  (eq major-mode hnews:lister))

(defun hmail:mode-is-p ()
  "Returns current major mode if a Hyperbole e-mail or news mode, else nil."
  (car (memq major-mode
	     (list hmail:reader hmail:composer hmail:lister hmail:modifier
		   hnews:reader hnews:composer hnews:lister))))

;;;###autoload
(defun hmail:msg-narrow (&optional msg-start msg-end)
  "Narrows buffer to displayable part of current message.
Its displayable part begins at optional MSG-START and ends at or before
MSG-END."
  (if (hmail:reader-p) (rmail:msg-widen))
  (setq msg-start (or msg-start (point-min))
	msg-end (or msg-end (point-max)))
  (narrow-to-region msg-start (hmail:hbdata-start msg-start msg-end)))

(defun hmail:reader-p ()
  "Returns t iff current major mode shows received Hyperbole e-mail messages."
  (memq major-mode (list hmail:reader hmail:modifier)))

(defun hmail:region (start end &optional buf invisible-flag)
  "Start composing mail with region between START and END included in message.
Invisible text is expanded and included in the mail only if INVISIBLE-FLAG is
non-nil.  Optional BUF contains the region and defaults to the current
buffer.  It may be a buffer or buffer name."
  (interactive (list (region-beginning) (region-end) (current-buffer)
		     (y-or-n-p "Include invisible text? ")))
  (or buf (setq buf (current-buffer)))
  (if (stringp buf) (setq buf (get-buffer buf)))
  (let (mail-buf)
    (hmail:invoke)
    (setq mail-buf (current-buffer))
    (save-excursion
      (if (search-forward mail-header-separator nil t)
	  ;; Within header, so move to body
	  (goto-char (point-max)))
      (set-buffer buf)
      (hypb:insert-region mail-buf start end invisible-flag))))

;;; ========================================================================
;;; rmail class - mail reader interface - abstract
;;; ========================================================================

(defun rmail:init ()
  "Initializes Hyperbole abstract mail interface for a particular mail reader.
`hmail:reader' should be set appropriately before this is called."
  (hmail:init "rmail:" '("msg-hdrs-full" "msg-narrow" "msg-num"
			 "msg-prev" "msg-next"
			 "msg-to-p"  ;; 2 args: (mail-msg-id mail-file)
			 "msg-widen" "to"))
  (hmail:init "lmail:" '("Summ-delete" "Summ-expunge" "Summ-goto" "Summ-to"
			 "Summ-undelete-all")))

(defvar rmail:msg-hdr-prefix "\\(^Date: \\|\n\nFrom [^ \n]+ \\)"
  "String header preceding an e-mail received message-id.")

(defun rmail:msg-id-get ()
  "Returns current msg id for an `hmail:reader' buffer as a string, else nil.
Signals error when current mail reader is not supported."
  (let* ((reader (symbol-name hmail:reader))
	 ;; (toggled)
	 )
    (or (fboundp 'rmail:msg-hdrs-full)
	(error "(rmail:msg-id-get): Invalid mail reader: %s" reader))
    (save-excursion
      (unwind-protect
	  (progn
	    ;; (setq toggled (rmail:msg-hdrs-full nil))
	    (goto-char (point-min))
	    (if (re-search-forward (concat rmail:msg-hdr-prefix
					   "\\(.+\\)"))
		;; Found matching msg
		(buffer-substring (match-beginning 2) (match-end 2))))
	;; (rmail:msg-hdrs-full toggled)
	()
	))))

;;; ------------------------------------------------------------------------
;;; Each mail reader-specific Hyperbole support module must also define
;;; the following functions, commonly aliased to existing mail reader
;;; functions within the "-init" function of the Hyperbole module.
;;; See "hrmail.el" for examples.
;;;
;;; rmail:get-new, rmail:msg-forward, rmail:summ-msg-to, rmail:summ-new

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hmail:hbdata-sep "\^Lbd"
  "Text separating e-mail msg from any trailing Hyperbole button data.")

(provide 'hmail)

;;; hmail.el ends here
