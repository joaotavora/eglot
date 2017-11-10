;;; hyrolo.el --- Hierarchical, multi-file, easy-to-use contact management system
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     7-Jun-89 at 22:08:29
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This is Hyperbole's advanced rolo system, HyRolo, for convenient
;;  management of hierarchical, record-oriented information.  Most
;;  often this is used for contact management but it can quickly be
;;  adapted to most any record-oriented lookup task, for fast retrieval.
;;
;;  See all the autoloaded functions herein for interactive commands.
;;  See the Info manual entry "(hyperbole)HyRolo" for usage information.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'custom) ;; For defface.
(require 'hversion)
(require 'hmail)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (unless (require 'bbdb nil t)
    (defvar bbdb-file nil))
  (unless (require 'google-contacts nil t)
    (defvar google-contacts-buffer-name nil))
  (defvar next-entry-exists nil))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defgroup hyperbole-rolo nil
  "Hyperbole Rolo hierarchical contact manager customizations."
  :group 'hyperbole)

(defcustom hyrolo-date-format "%m/%d/%Y"
  "*Format of date string used in Rolo automatic date stamps.  Default is American style.
See documentation of the function `format-time-string' for format options."
  :type 'string
  :group 'hyperbole-rolo)

(defvar hyrolo-display-format-function
  ;; Set trailing newlines to exactly two.
  (lambda (entry)
    (concat (replace-regexp-in-string "[ \t\n\r]+\\'" "" entry nil t) "\n\n"))
  "*Function of one argument, a rolo entry string, which modifies the string for display.")

(defcustom hyrolo-email-format "%s\t\t<%s>"
  "*Format string to use when adding an entry with e-mail addr from a mail msg.
It must contain a %s indicating where to put the entry name and a second
%s indicating where to put the e-mail address."
  :type 'string
  :group 'hyperbole-rolo)

(defcustom hyrolo-google-contacts-flag t
  "*Non-nil means search Google Contacts on each hyrolo query.
The google-contact package must be loaded and a gpg encryption
executable must be found as well (for Oauth security)."
  :type 'boolean
  :group 'hyperbole-rolo)

(defun hyrolo-google-contacts-p ()
  "Return non-nil if `hyrolo-google-contacts-flag' is non-nil and google-contacts package and gpg executables are available for use."
  (and hyrolo-google-contacts-flag
       (featurep 'google-contacts) 
       ;; If no gpg encryption executable, Oauth login to Google will fail.
       (or (executable-find "gpg2") (executable-find "gpg"))))

;;;###autoload
(defun hyrolo-initialize-file-list ()
  "Initialize the list of files to use for HyRolo searches."
  (interactive)
  (let* ((gcontacts (if (hyrolo-google-contacts-p) google-contacts-buffer-name))
	 (ms "c:/_rolo.otl")
	 (unix "~/.rolo.otl")
	 (list (delq nil (if (and (boundp 'bbdb-file) (stringp bbdb-file))
			     (if hyperb:microcruft-os-p
				 (list ms bbdb-file gcontacts)
			       (list  "~/.rolo.otl" bbdb-file gcontacts))
			   (if hyperb:microcruft-os-p (list ms gcontacts) (list unix gcontacts))))))
      (when (called-interactively-p 'interactive)
	(message "HyRolo Search List: %S" list))
      list))

(defvar hyrolo-file-list (hyrolo-initialize-file-list)
  "*List of files containing rolo entries.
The first file should be a user-specific rolo file, typically in the home
directory.

A hyrolo-file consists of:
   (1) an optional header beginning with and ending with a line which matches
       hyrolo-hdr-regexp;
   (2) one or more rolo entries which each begin with
       hyrolo-entry-regexp and may be nested.")
(define-obsolete-variable-alias 'rolo-file-list 'hyrolo-file-list "06.00")

(defcustom hyrolo-highlight-face nil
  "*Face used to highlight rolo search matches."
  :type 'face
  :group 'hyperbole-rolo)
(unless hyrolo-highlight-face
  (setq hyrolo-highlight-face
	(cond (hyperb:emacs-p
	       (if (fboundp 'defface)
		   (defface hyrolo-highlight-face nil
			    "*Face used to highlight rolo search matches."
			    :group 'hyperbole-rolo)))
	      (t (if (fboundp 'defface)
		     (face-name
		      (defface hyrolo-highlight-face nil
			"*Face used to highlight rolo search matches."
			:group 'hyperbole-rolo))))))
  (if (fboundp 'hproperty:set-item-highlight)
      (hproperty:set-item-highlight)))

(defcustom hyrolo-kill-buffers-after-use nil
  "*Non-nil means kill rolo file buffers after searching them for entries.
Only unmodified buffers are killed."
  :type 'boolean
  :group 'hyperbole-rolo)

(defcustom hyrolo-save-buffers-after-use t
  "*Non-nil means save rolo file after an entry is killed."
  :type 'boolean
  :group 'hyperbole-rolo)

;; Insert or update the entry date each time an entry is added or edited.
(add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
(add-hook 'hyrolo-edit-hook #'hyrolo-set-date)

(defvar hyrolo-yank-reformat-function nil
  "*Value is a function of two arguments, START and END, invoked after a hyrolo-yank.
It should reformat the region given by the arguments to some preferred style.
Default value is nil, meaning no reformmating is done.")

;;; ************************************************************************
;;; Commands
;;; ************************************************************************

;;;###autoload
(defun hyrolo-add (name &optional file)
  "Adds a new entry in personal rolo for NAME.
Last name first is best, e.g. \"Smith, John\".
With prefix argument, prompts for optional FILE to add entry within.
NAME may be of the form: parent/child to insert child below a parent
entry which begins with the parent string."
  (interactive
   (progn
     (or (fboundp 'mail-fetch-field) (require 'mail-utils))
     (let* ((lst (hyrolo-name-and-email))
	    (name (car lst))
	    (email (car (cdr lst)))
	    (entry (read-string "Name to add to rolo: "
				(or name email))))
       (list (if (and email name
		      (string-match (concat "\\`" (regexp-quote entry)) name))
		 (format hyrolo-email-format entry email) entry)
	     current-prefix-arg))))
  (if (or (not (stringp name)) (string= name ""))
      (error "(hyrolo-add): Invalid name: `%s'" name))
  (if (and (called-interactively-p 'interactive) file)
      (setq file (completing-read "File to add to: "
				  (mapcar 'list hyrolo-file-list))))
  (if (null file) (setq file (car hyrolo-file-list)))
  (cond ((and file (or (not (stringp file)) (string= file "")))
	 (error "(hyrolo-add): Invalid file: `%s'" file))
	((and (file-exists-p file) (not (file-readable-p file)))
	 (error "(hyrolo-add): File not readable: `%s'" file))
	((not (file-writable-p file))
	 (error "(hyrolo-add): File not writable: `%s'" file)))
  (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
  (if (called-interactively-p 'interactive) (message "Locating insertion point for `%s'..." name))
  (let ((parent "") (level "") end)
    (widen) (goto-char 1)
    (while (string-match "\\`[^\]\[<>{}\"]*/" name)
      (setq end (1- (match-end 0))
	    parent (substring name 0 end)
	    name (substring name (min (1+ end) (length name))))
      (if (re-search-forward
	   (concat "\\(" hyrolo-entry-regexp "\\)[ \t]*" 
		   (regexp-quote parent)) nil t)
	  (setq level (buffer-substring-no-properties (match-beginning 1)
						      (match-end 1)))
	(error "(hyrolo-add): `%s' category not found in \"%s\"."
	       parent file)))
    (narrow-to-region (point)
		      (progn (hyrolo-to-entry-end t level) (point)))
    (let* ((len (length name))
	   (name-level (concat level "*"))
	   (level-len (length name-level))
	   (first-char (aref name 0))
	   (entry "")
	   (entry-spc "")
	   (entry-level)
	   (match)
	   (again t))
      ;; Speed up entry insertion point location if this is a first-level
      ;; entry by moving to an entry with the same (or nearest) first character
      ;; to that of `name'.
      (if (and (= level-len 1)
	       (equal hyrolo-entry-regexp "^\\*+"))
	  (progn (goto-char (point-min))
		 (if (re-search-forward (concat "^\\*[ \t]*"
						(char-to-string first-char))
					nil t)
		     (goto-char (match-beginning 0))
		   (goto-char (point-max))
		   (if (and (> first-char ?0)
			    (re-search-backward
			     (concat "^\\*[ \t]*["
				     (substring
				      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
				      0 (min (- first-char ?0) 62))
				     "]")
			     nil t))
		       (progn (goto-char (match-end 0))
			      (hyrolo-to-entry-end t entry-level)
			      ;; Now at the insertion point, immediately after
			      ;; the last existing entry whose first character
			      ;; is less than that of `name'.  Setting `again'
			      ;; to nil prevents further searching for an
			      ;; insertion point.
			      (setq again nil)))))
	(goto-char (point-min)))

      (while (and again
		  (re-search-forward
		   (concat "\\(" hyrolo-entry-regexp "\\)\\([ \t]*\\)")
		   nil 'end))
	(setq entry-level (buffer-substring-no-properties (match-beginning 1)
							  (match-end 1)))
	(if (/= (length entry-level) level-len)
	    (hyrolo-to-entry-end t entry-level)
	  (setq entry (buffer-substring-no-properties (point) (+ (point) len))
		entry-spc (buffer-substring-no-properties (match-beginning 2)
							  (match-end 2)))
	  (cond ((string< entry name)
		 (hyrolo-to-entry-end t entry-level))
		((string< name entry)
		 (setq again nil) (beginning-of-line))
		(t ;; found existing entry matching name
		 (setq again nil match t)))))
      (setq buffer-read-only nil)
      (if match
	  nil
	(insert (or entry-level (concat level "*"))
		(if (string= entry-spc "") "   " entry-spc)
		name "\n")
	(backward-char 1))
      ;; hyrolo-to-buffer may move point from its desired location, so
      ;; restore it.
      (let ((opoint (point)))
	(widen)
	(hyrolo-to-buffer (current-buffer))
	(goto-char opoint))
      (run-hooks 'hyrolo-add-hook)
      (if (called-interactively-p 'interactive)
	  (message "Edit entry at point.")))))

;;;###autoload
(defun hyrolo-display-matches (&optional display-buf return-to-buffer)
  "Display optional DISPLAY-BUF buffer of previously found rolo matches.
If DISPLAY-BUF is nil, use the value in `hyrolo-display-buffer'.
Second arg RETURN-TO-BUFFER is the buffer to leave point within after the display."
  (interactive)
  (or display-buf (setq display-buf (get-buffer hyrolo-display-buffer)))
  (if display-buf nil
    (error "(hyrolo-display-matches): Search the rolo first."))
  ;; Save current window configuration if rolo match buffer is not
  ;; displayed in one of the windows already.
  (or
   ;; Handle both Emacs V18 and V19 versions of get-buffer-window.
   (condition-case ()
       (get-buffer-window display-buf (selected-frame))
     (error (get-buffer-window display-buf)))
   (setq hyrolo--wconfig (current-window-configuration)))
  (hyrolo-to-buffer display-buf)
  (if (eq major-mode 'hyrolo-mode) nil (hyrolo-mode))
  (setq buffer-read-only nil)
  (if (fboundp 'hproperty:but-create) (hproperty:but-create))
  (hyrolo-shrink-window)
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'hyrolo-display-hook)
  ;; Leave point in match buffer unless a specific RETURN-TO-BUFFER has
  ;; been specified.  Use {q} to quit and restore display.
  (if return-to-buffer (hyrolo-to-buffer return-to-buffer)))

;;;###autoload
(defun hyrolo-edit (&optional name file)
  "Edits a rolo entry given by optional NAME within `hyrolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
With no NAME arg, simply displays FILE or first entry in `hyrolo-file-list' in an
editable mode.  NAME may be of the form: parent/child to edit child below a
parent entry which begins with the parent string."
  (interactive "sEdit rolo entry named: \nP")
  (if (string-equal name "") (setq name nil))
  (and name (not (stringp name))
       (error "(hyrolo-edit): Invalid name: `%s'" name))
  (if (and (called-interactively-p 'interactive) current-prefix-arg)
      (if (= (length hyrolo-file-list) 1)
	  (setq file (car hyrolo-file-list))
	(setq file (completing-read "Entry's File: "
				    (mapcar 'list hyrolo-file-list)))))
  (let ((found-point) (file-list (if file (list file) hyrolo-file-list)))
    (or file (setq file (car file-list)))
    (if (null name)
	(progn (if (not (file-writable-p file))
		   (error "(hyrolo-edit): File not writable: `%s'" file))
	       (find-file-other-window file) (setq buffer-read-only nil))
      (if (setq found-point (hyrolo-to name file-list))
	  (progn
	    (setq file buffer-file-name)
	    (if (file-writable-p file)
		(setq buffer-read-only nil)
	      (message
	       "(hyrolo-edit): Entry found but file not writable: `%s'" file)
	      (beep))
	    (hyrolo-to-buffer (current-buffer)))
	(message "(hyrolo-edit): `%s' not found." name)
	(beep)
	(hyrolo-to-buffer (or (get-file-buffer (car file-list))
			    (find-file-noselect (car file-list))))
	(setq buffer-read-only nil))
      (widen)
      ;; hyrolo-to-buffer may have moved point from its desired location, so
      ;; restore it.
      (if found-point (goto-char found-point))
      (run-hooks 'hyrolo-edit-hook))))

(defun hyrolo-edit-entry ()
  "Edit the source entry of the rolo match buffer entry at point.
Returns entry name if found, else nil."
  (interactive)
  (let ((name (hyrolo-name-at))
	src)
    (if name
	(progn (setq src (hbut:key-src))
	       (cond ((and (boundp 'bbdb-file) (stringp bbdb-file) (equal src (expand-file-name bbdb-file)))
		      ;; For now, can't edit an entry from the bbdb database, signal an error.
		      (error "(hyrolo-edit-entry): BBDB entries are not editable."))
		     ((and (featurep 'google-contacts) (equal src (get-buffer google-contacts-buffer-name)))
		      ;; For now, can't edit an entry from Google Contacts, signal an error.
		      (error "(hyrolo-edit-entry): Google Contacts entries are not editable."))
		     (t (hyrolo-edit name src)
			name)))
      (error "(hyrolo-edit-entry): Move to an entry to edit it."))))

;;;###autoload
(defun hyrolo-fgrep (string &optional max-matches hyrolo-file count-only no-display)
  "Display rolo entries matching STRING (or a logical match expression).
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or `hyrolo-file-list'.  Default is to find all matching entries.
Each entry is displayed with all of its sub-entries.  Optional COUNT-ONLY
non-nil means don't retrieve and don't display matching entries.  Optional
NO-DISPLAY non-nil means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all
matches but omit file headers, negative values mean find up to the inverse of
that number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
`hyrolo-file-list' and the function `hyrolo-fgrep-logical' for documentation on
the logical expression matching."
  (interactive "sFind rolo string (or logical expression): \nP")
  (let ((total-matches
	 (if (string-match "\(\\(and\\|or\\|xor\\|not\\)\\>" string)
	     ;; Search string contains embedded logic operators.
	     (hyrolo-fgrep-logical string)
	   (hyrolo-grep (regexp-quote string) max-matches
		      hyrolo-file count-only no-display))))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-grep (regexp &optional max-matches hyrolo-file-or-bufs count-only no-display)
  "Display rolo entries matching REGEXP.
To a maximum of prefix arg MAX-MATCHES, in buffer(s) from optional HYROLO-FILE-OR-BUFS or
hyrolo-file-list.  Default is to find all matching entries.  Each entry is
displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil means don't
retrieve and don't display matching entries.  Optional NO-DISPLAY non-nil
means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
hyrolo-file-list."
  (interactive "sFind rolo regular expression: \nP")
  (unless (or (integerp max-matches) (memq max-matches '(nil t)))
    (setq max-matches (prefix-numeric-value max-matches)))
  (let ((hyrolo-file-list
	 (cond ((null hyrolo-file-or-bufs) hyrolo-file-list)
	       ((listp hyrolo-file-or-bufs) hyrolo-file-or-bufs)
	       ((list hyrolo-file-or-bufs))))
	(case-fold-search t)
	(display-buf (if count-only
			 nil
		       (set-buffer (get-buffer-create hyrolo-display-buffer))))
	(total-matches 0)
	(num-matched 0)
	(inserting (or (eq max-matches t)
		       (and (integerp max-matches) (< max-matches 0))))
	(file))
    (if count-only nil
      (setq buffer-read-only nil)
      (or inserting (erase-buffer)))
    (while (and (setq file (car hyrolo-file-list))
		(or (not (integerp max-matches))
		    (< total-matches (max max-matches (- max-matches)))))
      (setq hyrolo-file-list (cdr hyrolo-file-list)
	    num-matched (cond ((and (featurep 'bbdb) (equal file bbdb-file))
			       (hyrolo-bbdb-grep-file file regexp max-matches count-only))
			      ((and (hyrolo-google-contacts-p) (equal file google-contacts-buffer-name))
			       (hyrolo-retrieve-google-contacts (regexp-quote regexp))
			       (hyrolo-google-contacts-grep-file file regexp max-matches count-only))
			      (t (hyrolo-grep-file file regexp max-matches count-only)))
	    total-matches (+ total-matches num-matched))
      (if (integerp max-matches)
	  (setq max-matches
		(if (>= max-matches 0)
		    (- max-matches num-matched)
		  (+ max-matches num-matched)))))
    (unless (or count-only no-display inserting (= total-matches 0))
      (hyrolo-display-matches display-buf))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")
		 ))
    total-matches))

(defun hyrolo-isearch (&optional arg)
  "Interactively search forward for the next occurrence of the current match string.
Then add characters to further narrow the search.  With optional prefix ARG non-nil,
search for the current match regular expression rather than string."
  (interactive "P")
  (if arg
      (hyrolo-isearch-regexp)
    (hyrolo-verify)
    (setq unread-command-events
	  (append unread-command-events (map #'list #'identity (regexp-quote hyrolo-match-regexp))))
    (let ((case-fold-search t))
      (isearch-forward))))

(defun hyrolo-isearch-regexp (&optional arg)
  "Interactively search forward for the next occurrence of the current match regexp.
Then add characters to further narrow the search.  With optional prefix ARG non-nil,
search for the current match string rather than regular expression."
  (interactive "P")
  (if arg
      (hyrolo-isearch)
    (hyrolo-isearch-for-regexp hyrolo-match-regexp)))

(defun hyrolo-verify ()
  "Verify point is in a rolo match buffer and a match query has been performed, then execute rest of forms, BODY."
  (cond ((not (stringp hyrolo-match-regexp))
	 (error "(HyRolo): Invoke a rolo search expression first"))
	((not (equal (buffer-name) hyrolo-display-buffer))
	 (error "(HyRolo): Use this command in the %s match buffer"
		hyrolo-display-buffer))))

;;;###autoload
(defun hyrolo-kill (name &optional file)
  "Kills a rolo entry given by NAME within `hyrolo-file-list'.
With prefix argument, prompts for optional FILE to locate entry within.
NAME may be of the form: parent/child to kill child below a parent entry
which begins with the parent string.
Returns t if entry is killed, nil otherwise."
  (interactive "sKill rolo entry named: \nP")
  (if (or (not (stringp name)) (string= name "") (string-match "\\*" name))
      (error "(hyrolo-kill): Invalid name: `%s'" name))
  (if (and (called-interactively-p 'interactive) current-prefix-arg)
      (setq file (completing-read "Entry's File: "
				  (mapcar 'list hyrolo-file-list))))
  (let ((file-list (if file (list file) hyrolo-file-list))
	(killed))
    (or file (setq file (car file-list)))
    (if (hyrolo-to name file-list)
	(progn
	  (setq file buffer-file-name)
	  (if (file-writable-p file)
	      (let ((kill-op
		     (lambda (start level)
		       (kill-region
			start (hyrolo-to-entry-end t level))
		       (setq killed t)
		       (hyrolo-save-buffer)
		       (hyrolo-kill-buffer)))
		    start end level)
		(setq buffer-read-only nil)
		(re-search-backward hyrolo-entry-regexp nil t)
		(setq end (match-end 0))
		(beginning-of-line)
		(setq start (point)
		      level (buffer-substring-no-properties start end))
		(goto-char end)
		(skip-chars-forward " \t")
		(if (called-interactively-p 'interactive)
		    (let ((entry-line (buffer-substring-no-properties
				       (point)
				       (min (+ (point) 60)
					    (progn (end-of-line) (point))))))
		      (if (y-or-n-p (format "Kill `%s...' " entry-line))
			  (progn
			    (funcall kill-op start level)
			    (message "Killed"))
			(message "Aborted")))
		  (funcall kill-op start level)))
	    (message
	     "(hyrolo-kill): Entry found but file not writable: `%s'" file)
	    (beep)))
      (message "(hyrolo-kill): `%s' not found." name)
      (beep))
    killed))

(defun hyrolo-locate ()
  "Interactively search for an entry beginning with a set of search characters."
  (interactive)
  (hyrolo-isearch-for-regexp (concat hyrolo-entry-regexp "[ \t]*")))

(defun hyrolo-mail-to ()
  "Start composing mail addressed to the first e-mail address at or after point."
  (interactive)
  (let ((opoint (point)) button)
    (skip-chars-backward "^ \t\n\r<>")
    (if (and (re-search-forward mail-address-regexp nil t)
	     (goto-char (match-beginning 1))
	     (setq button (ibut:at-p)))
	(hui:hbut-act button)
      (goto-char opoint)
      (beep)
      (message "(hyrolo-mail-to): Invalid buffer or no e-mail address found"))))

(defun hyrolo-next-match ()
  "Move point forward to the start of the next rolo search match."
  (interactive)
  (hyrolo-verify)
  (let ((start (point))
	(case-fold-search t))
    (if (looking-at hyrolo-match-regexp)
	(goto-char (match-end 0)))
    (if (re-search-forward hyrolo-match-regexp nil t)
	(goto-char (match-beginning 0))
      (goto-char start)
      (error
       "(hyrolo-next-match): No following matches for \"%s\"" hyrolo-match-regexp))))

(defun hyrolo-overview (levels-to-show)
  "Show the first line of all levels of rolo matches.
With a prefix argument of LEVELS-TO-SHOW > 0, show the first lines of entries only to that depth."
  (interactive "P")
  (hyrolo-verify)
  ;; Use {t} to display top-level cells only.
  (if (or (null levels-to-show)
	  (if (called-interactively-p 'interactive)
	      (progn (setq levels-to-show (prefix-numeric-value current-prefix-arg))
		     (<= levels-to-show 0))
	    (not (integerp levels-to-show))))
      (setq levels-to-show 100))
  (hyrolo-show-levels levels-to-show))

(defun hyrolo-previous-match ()
  "Move point back to the start of the previous rolo search match."
  (interactive)
  (hyrolo-verify)
  (let ((case-fold-search t))
    (if (re-search-backward hyrolo-match-regexp nil t)
	nil
      (error
       "(hyrolo-previous-match): No prior matches for \"%s\"" hyrolo-match-regexp))))

(defun hyrolo-prompt (keyboard-function dialog-box-function prompt)
  "Use KEYBOARD-FUNCTION or DIALOG-BOX-FUNCTION, if available, to PROMPT for a yes/no answer.
XEmacs only."
  (if (and (fboundp 'device-type) (not (eq (device-type) 'tty))
	   (fboundp 'yes-or-no-p-maybe-dialog-box))
      ;; Graphical XEmacs
      (funcall dialog-box-function prompt)
    (funcall keyboard-function prompt)))

(defun hyrolo-quit ()
  "Quit from the rolo match buffer and restore the prior frame display."
  (interactive)
  (hyrolo-verify)
  (bury-buffer)
  (and hyrolo--wconfig (window-configuration-p hyrolo--wconfig)
       (set-window-configuration hyrolo--wconfig)))

(defun hyrolo-rename (old-file new-file)
  "Prompt user to rename OLD-FILE to NEW-FILE."
  (interactive (if hyperb:microcruft-os-p
		   '("c:/_rolodex.otl" "c:/_rolo.otl")
		 '("~/.rolodex.otl" "~/.rolo.otl")))
  (if (and (equal (car hyrolo-file-list) new-file)
	   (file-readable-p old-file)
	   (progn (beep)
		  (or (hyrolo-prompt
		       'y-or-n-p 'yes-or-no-p-dialog-box
		       (format "(hyrolo-rename): Rename \"%s\" to the new standard \"%s\"? "
			       old-file new-file))
		      ;; Setup to get rolo matches from OLD-FILE.
		      (progn (setq hyrolo-file-list
				   (cons old-file (cdr hyrolo-file-list)))
			     nil))))
      (progn (rename-file old-file new-file 1)
	     ;; Also rename backup file if it exists.
	     (if (file-readable-p (concat old-file "~"))
		 (rename-file (concat old-file "~") (concat new-file "~") 1))
	     (if (get-file-buffer old-file)
		 (with-current-buffer (get-file-buffer old-file)
		   (rename-buffer (file-name-nondirectory new-file))
		   (setq buffer-file-name (expand-file-name new-file))))
	     (message "(HyRolo): Your personal rolo file is now: \"%s\"."
		      new-file))))

;;;###autoload
(defun hyrolo-sort (&optional hyrolo-file)
  "Sorts up to 14 levels of entries in HYROLO-FILE (default is personal rolo).
Assumes entries are delimited by one or more `*'characters.
Returns list of number of groupings at each entry level." 
  (interactive
   (list (let ((default "")
	       (file))
	   (setq file
		 (completing-read
		  (format "Sort rolo file (default %s): "
			  (file-name-nondirectory
			   (setq default
				 (if (and buffer-file-name
					  (memq
					   t (mapcar
					      (lambda (file)
						(equal buffer-file-name
						       (expand-file-name file)))
					      hyrolo-file-list)))
				     buffer-file-name
				   (car hyrolo-file-list)))))
		  (mapcar 'list hyrolo-file-list)))
	   (if (string= file "") default file))))
  (if (or (not hyrolo-file) (equal hyrolo-file ""))
      (setq hyrolo-file (car hyrolo-file-list)))
  (if (not (and (stringp hyrolo-file) (file-readable-p hyrolo-file)))
      (error "(hyrolo-sort): Invalid or unreadable file: %s" hyrolo-file))
  (let ((level-regexp (regexp-quote "**************"))
	(entries-per-level-list)
	(n))
    (while (not (equal level-regexp ""))
      (setq n (hyrolo-sort-level hyrolo-file level-regexp))
      (if (or (/= n 0) entries-per-level-list)
	  (setq entries-per-level-list
		(append (list n) entries-per-level-list)))
      (setq level-regexp (substring level-regexp 0 (- (length level-regexp) 2))))
    entries-per-level-list))

(defun hyrolo-sort-level (hyrolo-file level-regexp &optional max-groupings)
  "Sorts groupings of entries in HYROLO-FILE at hierarchy level LEVEL-REGEXP.
To a maximum of optional MAX-GROUPINGS.  Nil value of MAX-GROUPINGS means all
groupings at the given level.  LEVEL-REGEXP should simply match the text of
any rolo entry of the given level, not the beginning of a line (^); an
example, might be (regexp-quote \"**\") to match level two.  Returns number
of groupings sorted."
  (interactive "sSort rolo file: \nRegexp for level's entries: \nP")
  (let ((sort-fold-case t))
    (hyrolo-map-level
     (lambda (start end) (hyrolo-sort-lines nil start end))
     hyrolo-file
     level-regexp
     max-groupings)))

;; This wraps forward-visible-line, making its ARG optional, making
;; its calling convention match that of forward-line.
(defun hyrolo-forward-visible-line (&optional arg)
  "Move forward by optional ARG lines (default = 1), ignoring currently invisible newlines only.
If ARG is negative, move backward -ARG lines.
If ARG is zero, move to the beginning of the current line."
  (if (null arg) (setq arg 1))
  (forward-visible-line arg))

;; Derived from `sort-lines' in "sort.el" since through Emacs 25.0
;; invisible lines are not grouped with the prior visible line, making
;; rolo entry (or any record) sorts fail.  This next function fixes that.
(defun hyrolo-sort-lines (reverse beg end)
  "Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order."
  (interactive "P\nr")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
	  ((inhibit-field-text-motion t))
	(sort-subr reverse 'hyrolo-forward-visible-line 'end-of-visible-line)))))

;;;###autoload
(defun hyrolo-toggle-datestamps (&optional arg)
  "Toggle whether datestamps are updated when rolo entries are modified.
With optional ARG, turn them on iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       (boundp 'hyrolo-add-hook) (listp hyrolo-add-hook)
	       (memq 'hyrolo-set-date hyrolo-add-hook)))
      (progn (remove-hook 'hyrolo-add-hook 'hyrolo-set-date)
	     (remove-hook 'hyrolo-edit-hook 'hyrolo-set-date)
	     (message "Rolo date stamps are now off."))
    (add-hook 'hyrolo-add-hook  #'hyrolo-set-date)
    (add-hook 'hyrolo-edit-hook #'hyrolo-set-date)
    (message "Rolo date stamps are now on.")))

(defun hyrolo-toggle-narrow-to-entry ()
  "Toggle between display of current entry and display of all matched entries.
Useful when bound to a mouse key."
  (interactive)
  (if (hyrolo-narrowed-p)
      (widen)
    (if (or (looking-at hyrolo-entry-regexp)
	    (re-search-backward hyrolo-entry-regexp nil t))
	(progn (forward-char)
	       (narrow-to-region (1- (point)) (hyrolo-display-to-entry-end)))))
  (hyrolo-shrink-window)
  (goto-char (point-min)))

(defun hyrolo-top-level ()
  "Show only the first line of all top-level rolo matches."
  (interactive)
  (hyrolo-verify)
  (hyrolo-show-levels 1))

;;;###autoload
(defun hyrolo-word (string &optional max-matches hyrolo-file count-only no-display)
  "Display rolo entries with whole word matches for STRING.
To a maximum of optional prefix arg MAX-MATCHES, in file(s) from optional
HYROLO-FILE or hyrolo-file-list.  Default is to find all matching entries.  Each
entry is displayed with all of its sub-entries.  Optional COUNT-ONLY non-nil
means don't retrieve and don't display matching entries.  Optional NO-DISPLAY
non-nil means retrieve entries but don't display.

Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.

Returns number of entries matched.  See also documentation for the variable
hyrolo-file-list."
  (interactive "sFind rolo whole word matches of: \nP")
  (let ((total-matches (hyrolo-grep (format "\\b%s\\b" (regexp-quote string))
				  max-matches
				  hyrolo-file count-only no-display)))
    (if (called-interactively-p 'interactive)
	(message "%s matching entr%s found in the rolo."
		 (if (= total-matches 0) "No" total-matches)
		 (if (= total-matches 1) "y" "ies")))
    total-matches))

;;;###autoload
(defun hyrolo-yank (name &optional regexp-p)
  "Inserts at point the first rolo entry matching NAME.
With optional prefix arg, REGEXP-P, treats NAME as a regular expression instead
of a string."
  (interactive "sInsert rolo entry named: \nP")
  (let ((hyrolo-display-buffer (current-buffer))
	(start (point))
	found)
    (save-excursion
      (setq found (if regexp-p
		      (hyrolo-grep name -1)
		    (hyrolo-grep (regexp-quote name) -1))))
    ;; Let user reformat the region just yanked.
    (if (and (= found 1) (fboundp hyrolo-yank-reformat-function))
	(funcall hyrolo-yank-reformat-function start (point)))
    found))

;;; ************************************************************************
;;; Big Brother Database (BBDB) Integration
;;; ************************************************************************

;;;###autoload
(defun hyrolo-bbdb-fgrep (&optional arg)
  "Fgrep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-bbdb-grep (not arg)))

;;;###autoload
(defun hyrolo-bbdb-grep (&optional arg)
  "Grep over a bbdb database and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> \".bbdb\"
======================================================================
* Jones     Tom                <tj@groovycat.org>
* Sera      Kate               <uptown@singular.net>
* Yako      Maso               <ym@destination.ny>"
  (interactive "P")
  (require 'bbdb)
  (let ((hyrolo-file-list (list bbdb-file))
	(hyrolo-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file)))))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (re-search-forward "^\\*" nil t)
    (beginning-of-line)
    (shell-command-on-region (point) (point-max) "column -s: -t" nil t)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-bbdb-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in bbdb HYROLO-FILE-OR-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  (let ((hyrolo-entry-regexp "^\\[")
	(hyrolo-display-format-function #'hyrolo-bbdb-entry-format)
	;; Kill the bbdb file after use if it is not already in a buffer.
	(hyrolo-kill-buffers-after-use
	 (not (get-file-buffer (expand-file-name bbdb-file)))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

(defun hyrolo-bbdb-entry-format (entry)
  (let ((v (read entry)))
    (format "* %s: %s: <%s>\n" (elt v 1) (elt v 0) (car (elt v 7)))))

;;; ************************************************************************
;;; Google Contacts Integration
;;; ************************************************************************

;;;###autoload
(defun hyrolo-google-contacts-fgrep (&optional arg)
  "Fgrep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do a grep regexp match instead of a string match."
  (interactive "P")
  (hyrolo-google-contacts-grep (not arg)))

;;;###autoload
(defun hyrolo-google-contacts-grep (&optional arg)
  "Grep over a buffer of Google Contacts and format the results as rolo entries.
With optional prefix ARG, do an fgrep string match instead of a regexp match.

Output looks like so:
======================================================================
@loc> <buffer *Google Contacts*>
======================================================================
* Jones     Tom
* Sera      Kate
* Yako      Maso"
  (interactive "P")
  (require 'google-contacts)
  (let ((hyrolo-file-list (list google-contacts-buffer-name))
	;; Kill the google-contacts buffer after use if it is not already in use.
	(hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name))))
    (call-interactively (if arg 'hyrolo-fgrep 'hyrolo-grep))
    (read-only-mode 0)
    (re-search-forward rolo-entry-regexp nil t)
    (beginning-of-line)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun hyrolo-google-contacts-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in google-contacts HYROLO-FILE-OR-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  ;; Kill the google-contacts buffer after use if it is not already in use.
  (let ((hyrolo-kill-buffers-after-use (not (get-buffer google-contacts-buffer-name))))
    (hyrolo-grep-file hyrolo-file-or-buf regexp max-matches count-only)))

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-data (contacts token contact-prefix-string)
  (if (not contacts)
      ;; No contacts, insert a string and return nil
      (insert "No result.")
    (print contacts (get-buffer-create "*contacts-data*"))
    (dolist (contact contacts)
      (let* ((name-value (nth 0 (xml-get-children contact 'gd:name)))
             (fullname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:fullName))))
             (givenname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:givenName))))
             (familyname (xml-node-child-string (nth 0 (xml-get-children name-value 'gd:familyName))))

             (nickname (xml-node-child-string (nth 0 (xml-get-children contact 'gContact:nickname))))
             (birthday (xml-get-attribute-or-nil (nth 0 (xml-get-children contact 'gContact:birthday)) 'when))

             (organization-value (nth 0 (xml-get-children contact 'gd:organization)))
             (organization-name (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgName))))
             (organization-title (xml-node-child-string (nth 0 (xml-get-children organization-value 'gd:orgTitle))))

             (notes (xml-node-child-string (nth 0 (xml-get-children contact 'content))))
             ;; Links
             (links (xml-get-children contact 'link))

             ;; Multiple values
             ;; Format is ((rel-type . data) (rel-type . data) … )
             (events (google-contacts-build-node-list contact 'gContact:event
                                                      (xml-get-attribute (nth 0 (xml-get-children child 'gd:when)) 'startTime)))
             (emails (google-contacts-build-node-list contact 'gd:email
                                                      (xml-get-attribute child 'address)))
             (phones (google-contacts-build-node-list contact 'gd:phoneNumber))
             (websites (google-contacts-build-node-list contact 'gContact:website
                                                        (xml-get-attribute child 'href)))
             (relations (google-contacts-build-node-list contact 'gContact:relation))
             (postal-addresses (google-contacts-build-node-list contact 'gd:structuredPostalAddress
                                                                (xml-node-child-string
                                                                 (nth 0 (xml-get-children child 'gd:formattedAddress)))))
             (instant-messaging (google-contacts-build-node-list contact 'gd:im
                                                                 (cons
                                                                  (xml-node-get-attribute-type child 'protocol)
                                                                  (cdr (assoc 'address (xml-node-attributes child))))))
             (beg (point)))
        (unless (and (string= familyname "") (string= givenname "") (string= nickname ""))
	  (insert contact-prefix-string familyname (if (or (string= familyname "")
							   (string= givenname "")) "" ", ")
		  givenname
		  (if (string= nickname "")
		      ""
		    (format " (%s)" nickname))
		  "\n"))

        (unless (and (string= organization-name "")
                     (string= organization-title ""))
	  (insert (google-contacts-margin-element))
	  (if (string= organization-title "")
	      (insert organization-name "\n")
	    (insert organization-title " @ " organization-name "\n")))

        (hyrolo-google-contacts-insert-generic-list emails "E-mails")
        (hyrolo-google-contacts-insert-generic-list phones "Phones")
        (hyrolo-google-contacts-insert-generic-list postal-addresses "Addresses"
                                             (lambda (address)
                                               (google-contacts-add-margin-to-text (cdr address)
                                                                                   (+ 4 (length (car address))))))
        (hyrolo-google-contacts-insert-generic-list websites "Websites"
                                             (lambda (website) (cdr website)))
        (hyrolo-google-contacts-insert-generic-list events "Events")
        (hyrolo-google-contacts-insert-generic-list relations "Relations"
                                             (lambda (relation)
                                               (widget-create 'link
                                                              :button-prefix "" :button-suffix ""
                                                              :action (lambda (widget &optional _event)
                                                                        (google-contacts (widget-value widget)))
                                                              (cdr relation))
                                               ""))
        (hyrolo-google-contacts-insert-generic-list instant-messaging "Instant messaging"
                                             (lambda (im)
                                               (concat (cddr im) " (" (cadr im) ")")))

        (when birthday
          (insert "\n" (google-contacts-margin-element) "Birthday: " birthday "\n"))

        (unless (string= notes "")
          (insert "\n" (google-contacts-margin-element) "Notes:  "
                  (google-contacts-add-margin-to-text notes 8)
                  "\n"))

        ;; Insert properties
        (put-text-property beg (1+ beg) 'google-contacts t)
        (when emails
          (put-text-property beg (point)
                             'google-contacts-email (concat fullname " <" (cdr (nth 0 emails)) ">")))))
    (goto-char (point-min)))
  ;; Return contacts
  contacts)

;; Derived from google-contacts.el.
(defun hyrolo-google-contacts-insert-generic-list (items title &optional get-value)
  "Insert a text for rendering ITEMS with TITLE.
Use GET-VALUE to get the value from the cdr of the item,
otherwise just put the cdr of item."
  (when items
    (insert "\n" (google-contacts-margin-element) (concat title ":\n"))
    (dolist (item items)
      (insert (google-contacts-margin-element) "  "
              (concat (car item) ":") " "
	      (if get-value
                  (funcall get-value item)
                (cdr item))
              "\n"))))

;; Derived from google-contacts.el.
(defun hyrolo-retrieve-google-contacts (&optional query-string force-refresh)
  (interactive
   (list (read-string "Look for: " (car google-contacts-history)
                      'google-contacts-history)
         current-prefix-arg))
  (let ((buffer (google-contacts-make-buffer))
        (token (google-contacts-oauth-token))
        (google-contacts-expire-time (if force-refresh 0 google-contacts-expire-time))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (setq google-contacts-query-string query-string)
      (hyrolo-google-contacts-insert-data (xml-get-children (google-contacts-data query-string token)
							    'entry)
					  token "* "))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hyrolo-fgrep-file (hyrolo-file-or-buf string &optional max-matches count-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching STRING to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  (hyrolo-grep-file hyrolo-file-or-buf (regexp-quote string) max-matches count-only))

(defun hyrolo-grep-file (hyrolo-file-or-buf regexp &optional max-matches count-only)
  "Retrieve entries in HYROLO-FILE-OR-BUF matching REGEXP to a maximum of optional MAX-MATCHES.
Nil value of MAX-MATCHES means find all matches, t value means find all matches
but omit file headers, negative values mean find up to the inverse of that
number of entries and omit file headers.  Optional COUNT-ONLY non-nil
means don't retrieve matching entries.
Returns number of matching entries found."
  ;;
  ;; Save regexp as last rolo search expression.
  (setq hyrolo-match-regexp regexp)
  ;;
  (let ((new-buf-p) (actual-buf))
    (if (and (or (null max-matches) (eq max-matches t) (integerp max-matches))
	     (or (setq actual-buf (hyrolo-buffer-exists-p hyrolo-file-or-buf))
		 (if (file-exists-p hyrolo-file-or-buf)
		     (setq actual-buf (find-file-noselect hyrolo-file-or-buf t)
			   new-buf-p t))))
	(let ((hdr-pos) (num-found 0) (curr-entry-level)
	      (incl-hdr t) start next-entry-exists)
	  (if max-matches
	      (cond ((eq max-matches t)
		     (setq incl-hdr nil max-matches nil))
		    ((< max-matches 0)
		     (setq incl-hdr nil
			   max-matches (- max-matches)))))
	  (set-buffer actual-buf)
	  (if new-buf-p (setq buffer-read-only t))
	  (save-excursion
	    (save-restriction
	      (widen)
	      ;; Ensure no entries in outline mode are hidden.
	      (if (fboundp #'outline-show-all) (outline-show-all))
	      (goto-char (point-min))
	      (if (re-search-forward hyrolo-hdr-regexp nil t 2)
		  (progn (forward-line)
			 (setq hdr-pos (cons (point-min) (point)))))
	      (re-search-forward hyrolo-entry-regexp nil t)
	      (while (and (or (null max-matches) (< num-found max-matches))
			  (re-search-forward regexp nil t))
		(re-search-backward hyrolo-entry-regexp nil t)
		(setq start (point)
		      next-entry-exists nil)
		(re-search-forward hyrolo-entry-regexp nil t)
		(setq curr-entry-level (buffer-substring-no-properties start (point)))
		(hyrolo-to-entry-end t curr-entry-level)
		(or count-only
		    (if (and (zerop num-found) incl-hdr)
			(let* ((src (or (buffer-file-name actual-buf)
					actual-buf))
			       (src-line
				(format
				 (concat (if (boundp 'hbut:source-prefix)
					     hbut:source-prefix
					   "@loc> ")
					 "%s")
				 (prin1-to-string src))))
			  (set-buffer hyrolo-display-buffer)
			  (goto-char (point-max))
			  (if hdr-pos
			      (progn
				(insert-buffer-substring-no-properties
				 actual-buf (car hdr-pos) (cdr hdr-pos))
				(insert src-line "\n\n"))
			    (insert (format hyrolo-hdr-format src-line)))
			  (set-buffer actual-buf))))
		(setq num-found (1+ num-found))
		(or count-only
		    (hyrolo-add-match hyrolo-display-buffer regexp start (point))))))
	  (hyrolo-kill-buffer actual-buf)
	  num-found)
      0)))

(defun hyrolo-map-level (func hyrolo-file-or-buf level-regexp &optional max-groupings)
  "Perform FUNC on groupings of HYROLO-FILE-OR-BUF entries at level LEVEL-REGEXP,
to a maximum of optional argument MAX-GROUPINGS.  Nil value of MAX-GROUPINGS
means all groupings at the given level.  FUNC should take two arguments, the
start and the end of the region that it should manipulate.  LEVEL-REGEXP
should match the prefix text of any rolo entry of the given level, not the
beginning of a line (^); an example, might be (regexp-quote \"**\") to match
level two.  Returns number of groupings matched."
  (let ((actual-buf))
    (if (and (or (null max-groupings) (< 0 max-groupings))
	     (or (setq actual-buf (hyrolo-buffer-exists-p hyrolo-file-or-buf))
		 (if (file-exists-p hyrolo-file-or-buf)
		     (progn (setq actual-buf (find-file-noselect hyrolo-file-or-buf t))
			    t))))
	(progn
	  (set-buffer actual-buf)
	  (let ((num-found 0)
		(exact-level-regexp (concat "^\\(" level-regexp "\\)[ \t\n\r]"))
		(outline-regexp hyrolo-entry-regexp)
		(buffer-read-only)
		(level-len))
	    (goto-char (point-min))
	    ;; Pass buffer header if it exists
	    (if (re-search-forward hyrolo-hdr-regexp nil t 2)
		(forward-line))
	    (while (and (or (null max-groupings) (< num-found max-groupings))
			(re-search-forward exact-level-regexp nil t))
	      (setq num-found (1+ num-found))
	      (let* ((opoint (prog1 (point) (beginning-of-line)))
		     (grouping-start (point))
		     (start grouping-start)
		     (level-len (or level-len (- (1- opoint) start)))
		     (next-level-len)
		     (next-entry-exists)
		     (grouping-end)
		     (no-subtree))
		(while (and (progn
			      (if (setq next-entry-exists
					(re-search-forward
					 hyrolo-entry-regexp nil t 2))
				  (setq next-level-len
					(- (point)
					   (progn (beginning-of-line)
						  (point)))
					grouping-end
					(< next-level-len level-len)
					no-subtree
					(<= next-level-len level-len))
				(setq grouping-end t no-subtree t)
				(goto-char (point-max)))
			      (let ((end (point)))
				(goto-char start)
				(outline-hide-subtree) ; and hide multiple entry lines
				;; Move to start of next entry at equal
				;; or higher level.
				(setq start
				      (if no-subtree
					  end
					(if (re-search-forward
					     hyrolo-entry-regexp nil t)
					    (progn (beginning-of-line) (point))
					  (point-max))))
				;; Remember last expression in `progn'
				;; must always return non-nil.
				(goto-char start)))
			    (not grouping-end)))
		(let ((end (point)))
		  (goto-char grouping-start)
		  (funcall func grouping-start end)
		  (goto-char end))))
	    (outline-show-all)
	    (hyrolo-kill-buffer actual-buf)
	    num-found))
      0)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hyrolo-add-match (hyrolo-matches-buffer regexp start end)
  "Insert before point in HYROLO-MATCHES-BUFFER an entry matching REGEXP from the current region between START to END."
  (let ((hyrolo-buf (current-buffer))
	(hyrolo-entry (buffer-substring-no-properties start end))
	opoint)
    (set-buffer (get-buffer-create hyrolo-matches-buffer))
    (setq opoint (point))
    (insert (funcall hyrolo-display-format-function hyrolo-entry))
    (hyrolo-highlight-matches regexp opoint (point))
    (set-buffer hyrolo-buf)))

(defun hyrolo-buffer-exists-p (hyrolo-buf)
  "Returns buffer given by HYROLO-BUF or nil.
HYROLO-BUF may be a file-name, buffer-name, or buffer."
  (car (memq (get-buffer (or (and (stringp hyrolo-buf)
				  (get-file-buffer hyrolo-buf))
			     hyrolo-buf))
	     (buffer-list))))

(defun hyrolo-current-date ()
  "Return the current date (a string) in a form used for rolo entry insertion."
  (format-time-string hyrolo-date-format))

(defun hyrolo-display-to-entry-end ()
  "Go to end of current entry, ignoring sub-entries."
  (if (re-search-forward (concat hyrolo-hdr-regexp "\\|"
				 hyrolo-entry-regexp) nil t)
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))


(defun hyrolo-format-name (name-str first last)
  "Reverse order of NAME-STR field given my regexp match field FIRST and LAST."
  (if (match-beginning last)
      (concat (substring name-str (match-beginning last) (match-end last))
	      ", "
	      (substring name-str (match-beginning first) (match-end first)))))

(defun hyrolo-highlight-matches (regexp start end)
  "Highlight matches for REGEXP in region from START to END."
  (if (fboundp 'hproperty:but-add)
      (let ((hproperty:but-emphasize-flag))
	(save-excursion
	  (goto-char start)
	  (while (re-search-forward regexp nil t)
	    (hproperty:but-add (match-beginning 0) (match-end 0)
			       (or hyrolo-highlight-face
				   hproperty:highlight-face)))))))

(defun hyrolo-isearch-for-regexp (regexp)
  "Interactively search forward for the next occurrence of current match regexp.
Then add characters to further narrow the search."
  (hyrolo-verify)
  (if (stringp regexp)
      (setq unread-command-events
	    (append unread-command-events (map #'list #'identity regexp))))
  (let ((case-fold-search t))
    (isearch-forward-regexp)))

(defun hyrolo-kill-buffer (&optional hyrolo-buf)
  "Kills optional HYROLO-BUF if unchanged and `hyrolo-kill-buffers-after-use' is t.
Default is current buffer."
  (or hyrolo-buf (setq hyrolo-buf (current-buffer)))
  (and hyrolo-kill-buffers-after-use (not (buffer-modified-p hyrolo-buf))
       (kill-buffer hyrolo-buf)))

(defun hyrolo-name-and-email ()
  "If point is in a mail message, returns list of (name email-addr) of sender.
Name is returned as `last, first-and-middle'."
  (let ((email) (name) (from))
    (save-window-excursion
      (if (or (hmail:lister-p) (hnews:lister-p))
	  (other-window 1))
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (if (search-forward "\n\n" nil t)
	      (narrow-to-region (point-min) (point)))
	  (setq email (mail-fetch-field "reply-to")
		from  (mail-fetch-field "from")))))
    (if from
	(cond
	 ;; Match: email, email (name), email "name"
	 ((string-match
	   (concat "^\\([^\"<>() \t\n\r\f]+\\)"
		   "\\([ \t]*[(\"][ \t]*\\([^\"()]+\\)[ \t]+"
		   "\\([^\" \t()]+\\)[ \t]*[)\"]\\)?[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 3 4))
	  (or email (setq email (substring from (match-beginning 1)
					   (match-end 1)))))
	 ;; Match: <email>, name <email>, "name" <email>
	 ((string-match
	   (concat "^\\(\"?\\([^\"<>()\n]+\\)[ \t]+"
		   "\\([^\" \t()<>]+\\)\"?[ \t]+\\)?"
		   "<\\([^\"<>() \t\n\r\f]+\\)>[ \t]*$")
	   from)
	  (setq name (hyrolo-format-name from 2 3))
	  (or email (setq email (substring from (match-beginning 4)
					   (match-end 4)))))))
    (if (or name email)
	(list name email))))

(defun hyrolo-name-at ()
  "If point is within an entry in `hyrolo-display-buffer', returns the entry name, else nil."
  (if (string-equal (buffer-name) hyrolo-display-buffer)
      (save-excursion
	(if (or (looking-at hyrolo-entry-regexp)
		(progn (end-of-line)
		       (re-search-backward hyrolo-entry-regexp nil t)))
	    (progn (goto-char (match-end 0))
		   (skip-chars-forward " \t")
		   (if (or (looking-at "[^ \t\n\r]+ ?, ?[^ \t\n\r]+")
			   (looking-at "\\( ?[^ \t\n\r]+\\)+"))
		       (buffer-substring-no-properties (match-beginning 0)
						       (match-end 0))))))))

(defun hyrolo-narrowed-p ()
  (or (/= (point-min) 1) (/= (1+ (buffer-size)) (point-max))))

(defun hyrolo-save-buffer (&optional hyrolo-buf)
  "Saves optional HYROLO-BUF if changed and `hyrolo-save-buffers-after-use' is t.
Default is current buffer.  Used, for example, after a rolo entry is killed."
  (or hyrolo-buf (setq hyrolo-buf (current-buffer)))
  (and hyrolo-save-buffers-after-use (buffer-modified-p hyrolo-buf)
       (set-buffer hyrolo-buf) (save-buffer)))

(defun hyrolo-set-date ()
  "Add a line with the current date at the end of the current rolo entry.
Suitable for use as an entry in `hyrolo-add-hook' and `hyrolo-edit-hook'.
The date format is determined by the setting, `hyrolo-date-format', with
a default of MM/DD/YYYY."
  (save-excursion
    (skip-chars-forward "*")
    (hyrolo-to-entry-end)
    (skip-chars-backward " \t\n\r\f")
    (skip-chars-backward "^\n\r\f")
    (if (looking-at "\\s-+[-0-9./]+\\s-*$") ;; a date
	(progn (delete-region (point) (match-end 0))
	       (insert "\t" (hyrolo-current-date)))
      (end-of-line)
      (insert "\n\t" (hyrolo-current-date)))))

(defun hyrolo-show-levels (num-levels)
  "Show only the first line of up to NUM-LEVELS of rolo matches.  NUM-LEVELS must be 1 or greater."
  (outline-show-all)
  (save-excursion
     (goto-char (point-min))
     (if (not (re-search-forward hyrolo-hdr-regexp nil t 2))
	 (outline-hide-sublevels num-levels)
       (goto-char (point-min))
       (let (start end)
	 (while (re-search-forward hyrolo-hdr-regexp nil t 2)
	   (forward-line)
	   (setq start (point)
		 end (if (re-search-forward hyrolo-hdr-regexp nil t)
			 (progn (beginning-of-line) (point))
		       (goto-char (point-max))))
	   (save-restriction
	     (narrow-to-region start end)
	     (outline-hide-sublevels num-levels)))))
     ;; This pause forces a window redisplay that maximizes the
     ;; entries displayed for any final location of point.
     (goto-char (point-min))
     (sit-for 0.001)))

(defun hyrolo-shrink-window ()
  (let* ((lines (count-lines (point-min) (point-max)))
	 (height (window-height))
	 (window-min-height 2)
	 (desired-shrinkage (1- (min (- height lines)))))
    (and (>= lines 0)
	 (/= desired-shrinkage 0)
	 (> (frame-height) (1+ height))
	 (shrink-window 
	  (if (< desired-shrinkage 0)
	      (max desired-shrinkage (- height (/ (frame-height) 2)))
	    (min desired-shrinkage (- height window-min-height)))))))

(defun hyrolo-to (name &optional file-list)
  "Moves point to entry for NAME within optional FILE-LIST.
`hyrolo-file-list' is used as default when FILE-LIST is nil.
Leaves point immediately after match for NAME within entry.
Switches internal current buffer but does not alter the frame.
Returns point where matching entry begins or nil if not found."
  (or file-list (setq file-list hyrolo-file-list))
  (let ((found) file)
    (while (and (not found) file-list)
      (setq file (car file-list)
	    file-list (cdr file-list))
      (cond ((and file (or (not (stringp file)) (string= file "")))
	     (error "(hyrolo-to): Invalid file: `%s'" file))
	    ((and (file-exists-p file) (not (file-readable-p file)))
	     (error "(hyrolo-to): File not readable: `%s'" file)))
      (set-buffer (or (get-file-buffer file) (find-file-noselect file)))
      (let ((case-fold-search t) (real-name name) (parent "") (level) end)
	(widen) (goto-char 1)
	(while (string-match "\\`[^\]\[<>{}\"]*/" name)
	  (setq end (1- (match-end 0))
		level nil
		parent (substring name 0 end)
		name (substring name (min (1+ end) (length name))))
	  (cond ((progn
		   (while (and (not level) (search-forward parent nil t))
		     (save-excursion
		       (beginning-of-line)
		       (if (looking-at
			    (concat "\\(" hyrolo-entry-regexp "\\)[ \t]*" 
				    (regexp-quote parent)))
			   (setq level (buffer-substring-no-properties (match-beginning 1)
								       (match-end 1))))))
		   level))
		((equal name real-name)) ;; Try next file.
		(t ;; Found parent but not child
		 (setq buffer-read-only nil)
		 (hyrolo-to-buffer (current-buffer))
		 (error "(hyrolo-to): `%s' part of name not found in \"%s\"."
			parent file)))
	  (if level
	      (narrow-to-region (point)
				(save-excursion
				  (hyrolo-to-entry-end t level) (point)))))
	(goto-char (point-min))
	(while (and (search-forward name nil t)
		    (not (save-excursion
			   (beginning-of-line)
			   (setq found
				 (if (looking-at
				      (concat "\\(" hyrolo-entry-regexp
					      "\\)[ \t]*"
					      (regexp-quote name)))
				     (point))))))))
      (or found (hyrolo-kill-buffer))) ;; conditionally kill
    (widen)
    found))

(defun hyrolo-to-buffer (buffer &optional other-window-flag frame)
  "Pop to BUFFER."
  (cond ((featurep 'xemacs)
	 (pop-to-buffer buffer other-window-flag
			;; default is to use selected frame
			(or frame (selected-frame))))
	(t (pop-to-buffer buffer other-window-flag))))

(defun hyrolo-to-entry-end (&optional include-sub-entries curr-entry-level)
  "Moves point to the end of the whole entry that point is within if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is a string whose length is the same as the last found entry
header.  If INCLUDE-SUB-ENTRIES is nil, CURR-ENTRY-LEVEL is not needed.
Returns current point."
  ;; Sets free variable, next-entry-exists, for speed.
  (while (and (setq next-entry-exists
		    (re-search-forward hyrolo-entry-regexp nil t))
	      include-sub-entries
	      (> (- (point) (save-excursion
			      (beginning-of-line)
			      (point)))
		 (length curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))

(defun hyrolo-mode ()
  "Major mode for the rolo match buffer.
Calls the functions given by `hyrolo-mode-hook'.
\\{hyrolo-mode-map}"
  (interactive)
  (setq major-mode 'hyrolo-mode
	mode-name "HyRolo")
  (use-local-map hyrolo-mode-map)
  ;;
  (set-syntax-table hyrolo-mode-syntax-table)
  ;;
  (if (not (fboundp 'outline-minor-mode))
      nil
    (outline-minor-mode 1))
  (run-hooks 'hyrolo-mode-hook))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hyrolo-display-buffer "*Hyperbole Rolo*"
  "Buffer used to display set of last matching rolo entries.")
(define-obsolete-variable-alias 'rolo-display-buffer 'hyrolo-display-buffer "06.00")

(defvar hyrolo-entry-regexp "^\\*+"
  "Regular expression to match the beginning of a rolo entry.
This pattern must match the beginning of the line.  Entries may be nested
through the use of increasingly longer beginning patterns.")
(define-obsolete-variable-alias 'rolo-entry-regexp 'hyrolo-entry-regexp "06.00")

(defconst hyrolo-hdr-format
  (concat
   "======================================================================\n"
   "%s\n"
   "======================================================================\n")
  "Header to insert preceding a file's first rolo entry match when
file has none of its own.  Used with one argument, the file name."
  )

(defconst hyrolo-hdr-regexp "^==="
  "Regular expression to match the first and last lines of rolo file headers.
This header is inserted into hyrolo-display-buffer before any entries from the
file are added.")

(defconst hyrolo-match-regexp nil
  "Last regular expression used to search the rolo.
Nil before a search is done.
String search expressions are converted to regular expressions.")

(defvar hyrolo--wconfig nil
  "Saves frame's window configuration prior to a rolo search.")

(defvar hyrolo-mode-syntax-table nil
  "Syntax table used while in hyrolo match mode.")

(if hyrolo-mode-syntax-table
    ()
  (setq hyrolo-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  ;; Support syntactic selection of delimited e-mail addresses.
  (modify-syntax-entry ?\<  "\(\>" hyrolo-mode-syntax-table)
  (modify-syntax-entry ?\>  "\)\<" hyrolo-mode-syntax-table))

(defvar hyrolo-mode-map nil
  "Keymap for the rolo match buffer.")

(if hyrolo-mode-map
    nil
  (setq hyrolo-mode-map (make-keymap))
  (if (fboundp 'set-keymap-name)
      (set-keymap-name hyrolo-mode-map 'hyrolo-mode-map))
  (suppress-keymap hyrolo-mode-map)
  (define-key hyrolo-mode-map "<"        'beginning-of-buffer)
  (define-key hyrolo-mode-map ">"        'end-of-buffer)
  (define-key hyrolo-mode-map "."        'beginning-of-buffer)
  (define-key hyrolo-mode-map ","        'end-of-buffer)
  (define-key hyrolo-mode-map "?"        'describe-mode)
  (define-key hyrolo-mode-map "\177"     'scroll-down)
  (define-key hyrolo-mode-map " "        'scroll-up)
  (define-key hyrolo-mode-map "a"        'outline-show-all)
  (define-key hyrolo-mode-map "b"        'outline-backward-same-level)
  (define-key hyrolo-mode-map "e"        'hyrolo-edit-entry)
  (define-key hyrolo-mode-map "f"        'outline-forward-same-level)
  (define-key hyrolo-mode-map "h"        'outline-hide-subtree)
  (define-key hyrolo-mode-map "l"        'hyrolo-locate)
  (define-key hyrolo-mode-map "m"        'hyrolo-mail-to)
  (define-key hyrolo-mode-map "n"        'outline-next-visible-heading)
  (define-key hyrolo-mode-map "o"        'hyrolo-overview)
  (define-key hyrolo-mode-map "p"        'outline-previous-visible-heading)
  (define-key hyrolo-mode-map "q"        'hyrolo-quit)
  (define-key hyrolo-mode-map "r"        'hyrolo-previous-match)
  (define-key hyrolo-mode-map "s"        'outline-show-subtree)
  (define-key hyrolo-mode-map "\M-s"     'hyrolo-isearch)
  (define-key hyrolo-mode-map "t"        'hyrolo-top-level)
  (define-key hyrolo-mode-map "\C-i"     'hyrolo-next-match)      ;; {TAB}
  (define-key hyrolo-mode-map "\M-\C-i"  'hyrolo-previous-match)  ;; {M-TAB}
  (define-key hyrolo-mode-map [backtab]  'hyrolo-previous-match)  ;; {Shift-TAB}
  (define-key hyrolo-mode-map "u"        'outline-up-heading)
  )

;; Prompt user to rename old personal rolo file to new name, if necessary.
(or noninteractive (call-interactively 'hyrolo-rename))

(provide 'hyrolo)

;;; hyrolo.el ends here
