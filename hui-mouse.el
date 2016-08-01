;;; hui-mouse.el --- Use key or mouse key for many functions, e.g. GNU Hyperbole menus
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    04-Feb-89
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This code is machine independent.  It works best with a pointing device but
;;  may also be used from a keyboard.  When used with a pointing device it
;;  requires an Emacs command that sets point to the location of the pointing
;;  device's cursor.
;;
;;  If you want to use your shift-middle mouse button to select Hyperbole menu
;;  items and Hyperbole buttons, follow these instructions.
;;
;;  If you plan to use a mouse only with X windows (XEmacs, GNU Emacs
;;  19, or InfoDock), Mac OS X, or NeXTSTEP, and you want to use the
;;  shift-middle and shift-right buttons, you need not do any mouse
;;  configuration.  Your Emacs executable must have been built so as to
;;  include the mouse support files for your window system, however.  These
;;  are in the Emacs "src" directory: for X "x*.c".
;;
;;  To use a different mouse key or a different window system, modify the
;;  mouse key bindings in "hmouse-sh.el".
;;
;; Using the Action Mouse Key to browse through and delete files from
;; Dired listings is exceptionally nice, just as it is when reading mail.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-set-point-command nil
  "*Command that sets point to the mouse cursor position.")

(defun action-key-error ()
  (hypb:error "(Action Key): No action defined for this context; try another location."))
(defun assist-key-error ()
  (hypb:error "(Assist Key): No action defined for this context; try another location."))

(defcustom action-key-default-function #'action-key-error
  "*Function run by the Action Key in an unspecified context.
Set it to #'hyperbole if you want it to display the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-default-function #'assist-key-error
  "*Function run by the Assist Key in an unspecified context.
Set it to #'hkey-summarize if you want it to display a summary of Smart Key behavior."
  :type 'function
  :group 'hyperbole-keys)

(defcustom action-key-eol-function #'smart-scroll-up
  "*Function run by the Action Key at the end of a line.
Set it to #'complete if you want behavior similar to that of standard Emacs."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-eol-function #'smart-scroll-down
  "*Function run by the Assist Key at the end of a line.
Set it to #'complete if you want behavior similar to that of standard Emacs."
  :type 'function
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; Hyperbole context-sensitive keys dispatch table
;;; ************************************************************************

(defvar hkey-value nil
  "Communicates a value between a Smart Key predicate and its actions.")

(defvar hkey-alist
  '(
    ;; Handle Emacs push buttons in buffers
    ((and (fboundp 'button-at) (button-at (point))) .
     ((push-button) . (smart-push-button-help)))
    ;;
    ;; If click in the minibuffer and reading an argument,
    ;; accept argument or give completion help.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (not (eq hargs:reading-p 'hmenu))) .
     ((exit-minibuffer) . (smart-completion-help)))
    ;;
    ;; If reading a Hyperbole menu item or a Hyperbole completion-based
    ;; argument, allow selection of an item at point.
    ((and (> (minibuffer-depth) 0) (setq hkey-value (hargs:at-p))) .
     ((hargs:select-p hkey-value) .
      (hargs:select-p hkey-value 'assist)))
    ;;
    ;; If reading a Hyperbole menu item and nothing is selected, just return.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (eq hargs:reading-p 'hmenu)) .
     ((exit-minibuffer) . (exit-minibuffer)))
    ;;
    ;; The ID-edit package supports rapid killing, copying, yanking and
    ;; display management. It is available only as a part of InfoDock.
    ;; It is not included with Hyperbole.
    ((and (boundp 'id-edit-mode) id-edit-mode
	  (not buffer-read-only)) .
     ((id-edit-yank) . (id-edit-yank)))
    ;;
    ((and (fboundp 'xref--item-at-point) (xref--item-at-point)) .
     ((xref-goto-xref) . (xref-show-location-at-point)))
    ;;
    ((if (eq major-mode 'kotl-mode)
	(and (not (kotl-mode:eobp)) (kotl-mode:eolp))
      (and (not (eobp)) (or (eolp) (and selective-display (eq (following-char) ?\r))))) .
     ((funcall action-key-eol-function) . (funcall assist-key-eol-function)))
    ;;
    ;; The Smart Menu system provides menus within Emacs on a dumb terminal.
    ;; It is a part of InfoDock, but may also be obtained as a separate
    ;; package.  It is not included with Hyperbole.
    ((eq major-mode 'smart-menu-mode) . 
     ((smart-menu-select) . (smart-menu-help)))
    ;;
    ((eq major-mode 'dired-mode) . 
     ((smart-dired) . (smart-dired-assist)))
    ;;
    ;; If on a Hyperbole button, perform action or give help.
    ((hbut:at-p) .
     ((hui:hbut-act 'hbut:current) . (hui:hbut-help 'hbut:current)))
    ;;
    ;; The Smart Menu system is an attractive in-buffer menu system
    ;; that predates Emacs menu systems; it is included in InfoDock.
    ((and (fboundp 'smart-menu-choose-menu)
	  (setq hkey-value (and hkey-always-display-menu
				(smart-menu-choose-menu)))
	  (not (and (get-buffer-window *smart-menu-buffer*)
		    (eq hkey-value *smart-menu-curr*)))) .
		    ((smart-menu hkey-value) .
		     (smart-menu hkey-value)))
    ;;
    ;; View minor mode
    ((if (boundp 'view-minor-mode) view-minor-mode) .
     ((cond ((last-line-p)
	     (view-quit))
	    ((pos-visible-in-window-p (point-max))
	     (goto-char (point-max)))
	    (t (scroll-up))) .
	    (scroll-down)))
    ;;
    ;; Support the OO-Browser when available.  It is a separate Emacs
    ;; package not included with Hyperbole.  Within an OO-Browser
    ;; OOBR-FTR buffer, an *Implementors* listing buffer, or an
    ;; Element signatures listing buffer of the OO-Browser, display
    ;; the associated element.
    ((or (string-equal (buffer-name) "*Implementors*")
	 (string-match "-Elements\\'" (buffer-name))
	 (and (boundp 'br-feature-tags-file)
	      (stringp br-feature-tags-file)
	      (equal br-feature-tags-file buffer-file-name))) .
	      ((smart-element) . (hkey-help)))
    ;;
    ;; View major mode
    ((eq major-mode 'view-mode) .
     ((View-scroll-lines-forward) . (View-scroll-lines-backward)))
    ;;
    ;; Select or select-and-kill a markup pair (e.g. hmtl tags), list,
    ;; array/vector, set, function, comment or string that begins or
    ;; ends at point.  For markup pairs, point must be at the first
    ;; character of the opening or closing tag.
    ((hui-select-at-delimited-thing-p) .
     ((hui-select-delimited-thing) . (progn (hui-select-delimited-thing)
					    (hmouse-kill-region))))
    ;;
    ;; If the prior test failed and point is at the start or end of an
    ;; sexpression, mark it for editing or kill it (assist key).  This
    ;; only handles the special case where point is just after the
    ;; closing delimiter and not at an end-of-line, so this may be
    ;; removed someday.
    ((hui-select-at-delimited-sexp-p) .
     ((hui-select-mark-delimited-sexp) .
      (progn (hui-select-mark-delimited-sexp) (hmouse-kill-region))))
    ;;
    ((eq major-mode 'kotl-mode) . 
     ((kotl-mode:action-key) . (kotl-mode:assist-key)))
    ;;
    ;; Rdb-mode supports direct selection and viewing of in-memory relational
    ;; databases.  Rdb-mode is available as a part of InfoDock.
    ;; It is not included with Hyperbole.
    ((eq major-mode 'rdb-mode) . ((rdb:action-key) . (rdb:assist-key)))
    ;;
    ;; Restore window config and hide help buffer when click at buffer end.
    ((if (= (point) (point-max))
	 (string-match "^\\*Help\\|Help\\*$" (buffer-name))) .
	 ((hkey-help-hide) . (hkey-help-hide)))
    ;;
    ;; Imenu listing in GNU Emacs
    ((smart-imenu-item-at-p) .
     ((smart-imenu-display-item-where (car hkey-value) (cdr hkey-value)) .
      (imenu-choose-buffer-index)))
    ;;
    ;; Function menu listing mode in XEmacs
    ((eq major-mode 'fume-list-mode) .
     ((fume-list-mouse-select current-mouse-event) . (fume-prompt-function-goto)))
    ;;
    ((and (eq major-mode 'c-mode)
	  buffer-file-name (smart-c-at-tag-p)) .
	  ((smart-c) . (smart-c nil 'next-tag)))
    ;;
    ((and (eq major-mode 'c++-mode) buffer-file-name
	  ;; Don't use smart-c++-at-tag-p here since it will prevent #include
	  ;; lines from matching.
	  (smart-c-at-tag-p)) .
	  ((smart-c++) . (smart-c++ nil 'next-tag)))
    ;;
    ((and (eq major-mode 'asm-mode)
	  buffer-file-name (smart-asm-at-tag-p)) .
	  ((smart-asm) . (smart-asm nil 'next-tag)))
    ;;
    ((and (smart-lisp-mode-p) (smart-lisp-at-tag-p)) .
     ((smart-lisp) . (smart-lisp 'show-doc)))
    ;;
    ((and (eq major-mode 'java-mode) buffer-file-name
	  (or (smart-java-at-tag-p)
	      ;; Also handle Java @see cross-references.
	      (looking-at "@see[ \t]+")
	      (save-excursion
		(and (re-search-backward "[@\n\r\f]" nil t)
		     (looking-at "@see[ \t]+"))))) .
		     ((smart-java) . (smart-java nil 'next-tag)))
    ;;
    ((and (memq major-mode '(js2-mode js-mode js3-mode javascript-mode html-mode web-mode))
	  buffer-file-name
	  (smart-javascript-at-tag-p)) .
     ((smart-javascript) . (smart-javascript nil 'next-tag)))
    ;;
    ((and (eq major-mode 'python-mode) buffer-file-name
	  (smart-python-at-tag-p)) .
	  ((smart-python) . (smart-python nil 'next-tag)))
    ;;
    ((and (eq major-mode 'objc-mode) buffer-file-name
	  (smart-objc-at-tag-p)) .
	  ((smart-objc) . (smart-objc nil 'next-tag)))
    ;;
    ((and (memq major-mode '(fortran-mode f90-mode))
	  buffer-file-name (smart-fortran-at-tag-p)) .
	  ((smart-fortran) . (smart-fortran nil 'next-tag)))
    ;;
    ((eq major-mode 'occur-mode) .
     ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'moccur-mode) .
     ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
    ((eq major-mode 'amoccur-mode) .
     ((amoccur-mode-goto-occurrence) . (amoccur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'calendar-mode) .
     ((smart-calendar) . (smart-calendar-assist)))
    ;;
    ((eq major-mode 'unix-apropos-mode) .
     ((smart-apropos) . (smart-apropos-assist)))
    ;;
    ((eq major-mode 'outline-mode) .
     ((smart-outline) . (smart-outline-assist)))
    ;;
    ((eq major-mode 'Info-mode) .
     ((smart-info) .  (smart-info-assist)))
    ;;
    ((if (boundp 'hmail:reader)
	 (or (eq major-mode hmail:reader)
	     (eq major-mode hmail:lister))) .
	     ((smart-hmail) . (smart-hmail-assist)))
    ;;
    ((eq major-mode 'gnus-group-mode)
     (smart-gnus-group) . (smart-gnus-group-assist))
    ;;
    ((eq major-mode 'gnus-summary-mode)
     (smart-gnus-summary) . (smart-gnus-summary-assist))
    ;;
    ((eq major-mode 'gnus-article-mode)
     (smart-gnus-article) . (smart-gnus-article-assist))
    ;;
    ((eq major-mode 'Buffer-menu-mode) .
     ((smart-buffer-menu) . (smart-buffer-menu-assist)))
    ;;
    ((eq major-mode 'ibuffer-mode) .
     ((smart-ibuffer-menu) . (smart-ibuffer-menu-assist)))
    ;;
    ((eq major-mode 'tar-mode) . 
     ((smart-tar) . (smart-tar-assist)))
    ;;
    ;; Follow references in man pages.
    ((setq hkey-value (smart-man-entry-ref)) .
     ((smart-man-display hkey-value) .
      (smart-man-display hkey-value)))
    ;;
    ((eq major-mode 'w3-mode) . 
     ((w3-follow-link) . (w3-goto-last-buffer)))
    ;;
    ((if (boundp 'hyrolo-display-buffer)
	 (equal (buffer-name) hyrolo-display-buffer)) .
	 ((smart-hyrolo) . (smart-hyrolo-assist)))
    ;;
    ;; Gomoku game
    ((eq major-mode 'gomoku-mode) . 
     ((gomoku-human-plays) . (gomoku-human-takes-back)))
    ;;
    ;; Support the OO-Browser when available.  It is a separate Emacs
    ;; package not included with Hyperbole.  Hyperbole supplies a stub
    ;; `br-in-browser' test for use here.
    ((or (br-in-browser) (eq major-mode 'br-mode)) .
     ((smart-br-dispatch) . (smart-br-assist-dispatch)))
    ;;
    ;; Outline minor mode is on and usable.
    ((and (boundp 'outline-minor-mode) outline-minor-mode) .
     ((smart-outline) . (smart-outline-assist)))
    )
  "Alist of predicates and form-conses for Action and Assist Keys.
Each element is: (predicate-form . (action-key-form . assist-key-form)).
When the Action or Assist Key is pressed, the first or second form,
respectively, associated with the first non-nil predicate is evaluated.")

;;; ************************************************************************
;;; driver code
;;; ************************************************************************

;; The following autoload is needed if another subsystem besides
;; Hyperbole uses this mouse handling code.
(autoload 'var:append "hvar" "Append to a list variable." nil)

(require 'hargs)
(require 'hmouse-key)
(defvar hmouse-alist hkey-alist
  "Alist of predicates and form-conses for context-sensitive smart key mouse actions.
When the action-key or the assist-key is pressed, the first or second
form, respectively, associated with the first non-nil predicate is
evaluated.")
(load "hui-window")

;;; ************************************************************************
;;; support code
;;; ************************************************************************

;; The `load' line below loads any local Smart Key function definitions.
;; The public distribution contains none.  You may leave it commented out if
;; you prefer.
;; (load "smart-local" t)

;;; ************************************************************************
;;; Required Init functions
;;; ************************************************************************

(defun first-line-p ()
  "Returns true if point is on the first line of the buffer."
  (save-excursion (beginning-of-line) (bobp)))

(defun last-line-p ()
  "Returns true if point is on the last line of the buffer."
  (save-excursion (end-of-line) (eobp)))

(defun smart-completion-help ()
  "Offer completion help for current minibuffer argument, if any."
  (if (where-is-internal 'minibuffer-completion-help (current-local-map))
      (minibuffer-completion-help)))

(defun smart-symlink-expand (path)
  "Returns referent for possible symbolic link, PATH."
  (if (not (fboundp 'symlink-referent))
      path
    (let ((start 0) (len (length path)) (ref) (part))
      (while (and (< start len) (setq part (string-match "/[^/]*" path start)))
	(setq part (concat ref
			   (substring path start (setq start (match-end 0))))
	      ref (symlink-referent part)))
      ref)))

;;; ************************************************************************
;;; smart-buffer-menu functions
;;; ************************************************************************

(defun smart-buffer-menu ()
  "Uses a single key or mouse key to manipulate buffer-menu entries.

Invoked via a key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on the first column of an entry, the selected buffer is marked for
     display; 
 (2) on the second column of an entry, the selected buffer is marked to be
     saved;
 (3) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on (if
     within the OO-Browser user interface, only the selected buffer is
     displayed);
 (4) on or after the last line in the buffer, all saves and deletes are done."

  (interactive)
  (cond ((last-line-p) (Buffer-menu-execute))
	((bolp) (Buffer-menu-mark))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-save))
	((br-in-browser) (br-buffer-menu-select))
	(t (Buffer-menu-select))))

(defun smart-buffer-menu-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate buffer-menu entries.

Invoked via an assist-key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display and for saving or deletion; 
 (2) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (3) on or after the last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((last-line-p) (progn (list-buffers) (forward-line 3)))
	((bolp) (Buffer-menu-unmark))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-unmark))
	(t (Buffer-menu-delete))))

;;; ************************************************************************
;;; smart-ibuffer-menu functions
;;; ************************************************************************

(defun smart-ibuffer-menu ()
  "Uses a single key or mouse key to manipulate ibuffer entries.

Invoked via a key press when in ibuffer-mode.  It assumes that
its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor there.

If key is pressed:
 (1) on the first or second column of an entry, the selected buffer is
     marked for display;
 (2) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on (if
     within the OO-Browser user interface, only the selected buffer is
     displayed);
 (3) on the first or last line in the buffer, all deletes are done."

  (interactive)
  (cond ((or (first-line-p) (last-line-p))
	 (ibuffer-do-kill-on-deletion-marks))
	((or (bolp) (save-excursion
		      (goto-char (1- (point)))
		      (bolp)))
	 (ibuffer-mark-forward 1))
	((br-in-browser) (br-buffer-menu-select))  
	(t (ibuffer-do-view))))

(defun smart-ibuffer-menu-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate buffer-menu entries.

Invoked via an assist-key press when in ibuffer-mode.  It assumes that
its caller has already checked that the assist-key was pressed in an
appropriate buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display or deletion; 
 (2) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (3) on the first or last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((or (first-line-p) (last-line-p))
	 (ibuffer-unmark-all 0))
	((or (bolp) (save-excursion
		      (goto-char (1- (point)))
		      (bolp)))
	 (ibuffer-unmark-forward 1))
	(t (ibuffer-mark-for-delete 1))))

;;; ************************************************************************
;;; smart-calendar functions
;;; ************************************************************************

(defun smart-calendar ()
  "Uses a single key or mouse key to manipulate the scrolling calendar.

Invoked via a key press when in calendar-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) at the end of the buffer, the calendar is scrolled forward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     backward 3 months;
 (3) on a date, the diary entries for the date, if any, are displayed."

  (interactive)
  (cond ((eobp) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-left-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-right-three-months 1))
	(t (calendar-cursor-to-nearest-date)
	   (view-diary-entries 1))))

(defun smart-calendar-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate the scrolling calendar.

Invoked via an assist-key press when in calendar-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) at the end of the buffer, the calendar is scrolled backward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     forward 3 months;
 (3) anywhere else, all dates with marking diary entries are marked in the
     calendar window."

  (interactive)
  (cond ((eobp) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-right-three-months 1))
	((< (current-column) 5) (calendar-cursor-to-nearest-date)
	 (scroll-calendar-left-three-months 1))
	(t (mark-diary-entries))))


;;; ************************************************************************
;;; smart-dired functions
;;; ************************************************************************

(defun smart-dired ()
  "Uses a single key or mouse key to manipulate directory entries.

Invoked via a key press when in dired-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed for
     editing in the other window;
 (2) on the first line of the buffer, if any deletes are to be
     performed, they are executed after user verification; otherwise, nothing
     is done;
 (3) on or after the last line in the buffer, this dired invocation is quit."

  (interactive)
  (cond ((first-line-p)
	 (if (save-excursion
	       (goto-char (point-min))
	       (re-search-forward "^D" nil t))
	     (cond ;; For Tree-dired compatibility
	      ((fboundp 'dired-do-flagged-delete)
	       (dired-do-flagged-delete))
	      ((fboundp 'dired-do-deletions)
	       (dired-do-deletions))
	      (t (error "(smart-dired): No Dired expunge function.")))))
	((last-line-p)
	 (quit-window))
	(t (hpath:find (dired-get-filename)))))

(defun smart-dired-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate directory entries.

Invoked via an assist-key press when in dired-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on a `~' character, all backup files in the directory are marked for
     deletion;
 (2) on a `#' character, all auto-save files in the directory are marked for
     deletion;
 (3) anywhere else within an entry line, the current entry is marked for
     deletion;
 (4) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (dired-unmark-all-files ?D)
	 (goto-char (point-max)))
	((looking-at "~") (dired-flag-backup-files))
	((looking-at "#") (dired-flag-auto-save-files))
	(t (if (fboundp 'dired-flag-file-deletion)
	       (dired-flag-file-deletion 1)
	     (dired-flag-file-deleted 1)))))

;;; ************************************************************************
;;; smart-gnus functions
;;; ************************************************************************

(defun smart-gnus-group ()
  "Uses a key or mouse key to move through Gnus Newsgroup listings.
Invoked via a key press when in gnus-group-mode.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) if `gnus-topic-mode' is active, and on a topic line, the topic is
     expanded or collapsed as needed;
 (3) to the left of any GNUS-GROUP line, on any of the whitespace, the current
     group is unsubscribed or resubscribed;
 (4) at the end of the GNUS-GROUP buffer, after all lines, checks for new
     news."

  (interactive)
  (cond ((last-line-p) (gnus-group-get-new-news))
	((progn (skip-chars-backward " U") (bolp))
	 (gnus-group-unsubscribe-current-group))
	((gnus-topic-mode-p) (gnus-topic-read-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-group-assist ()
  "Uses an assist-key or assist-mouse key to move through Gnus Newsgroup listings.
Invoked via an assist-key press when in gnus-group-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a GNUS-GROUP line, that newsgroup is read;
 (2) if `gnus-topic-mode' is active, and on a topic line, the topic is
     expanded or collapsed as needed;
 (3) to the left of any GNUS-GROUP line, on any of the whitespace, the user is
     prompted for a group name to subscribe or unsubscribe to;
 (4) at the end of the GNUS-GROUP buffer, after all lines, quits from the
     newsreader."

  (interactive)
  (cond ((last-line-p) (gnus-group-exit))
	((progn (skip-chars-backward " U") (bolp))
	 (call-interactively 'gnus-group-unsubscribe-group))
	((gnus-topic-mode-p) (gnus-topic-read-group))
	(t (gnus-group-read-group nil))))

(defun smart-gnus-summary ()
  "Uses a key or mouse key to move through Gnus News article listings.
Invoked via a key press when in gnus-summary-mode.  It assumes that its caller
has already checked that the key was pressed in an appropriate buffer and has
moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read, marked deleted, and scrolled
     forward;
 (3) at the end of the GNUS-SUMMARY buffer, the next undeleted article
     is read or the next group is entered."

  (interactive)
  (cond ((last-line-p)
	 (if gnus-current-article
	     (progn (goto-char (point-min))
		    (re-search-forward
		      (format "^.[ ]+%d:" gnus-current-article) nil t)
		    (setq this-command 'gnus-summary-next-page)
		    (call-interactively 'gnus-summary-next-page))
	   (goto-char (point-min))
	   (setq this-command 'gnus-summary-first-unread-article)
	   (call-interactively 'gnus-summary-first-unread-article)))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-article-as-unread ?\ )
	 (forward-line 1))
	(t (setq this-command 'gnus-summary-next-page)
	   (call-interactively 'gnus-summary-next-page))))

(defun smart-gnus-summary-assist ()
  "Uses an assist-key or assist-mouse key to move through Gnus News articles.
Invoked via an assist-key press when in gnus-summary-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) to the left of an article number, that article is marked as unread;
 (2) a GNUS-SUMMARY line, that article is read and scrolled backward;
 (3) at the end of the GNUS-SUMMARY buffer, the summary is exited, the user
     is returned to group mode."

  (interactive)
  (cond ((last-line-p)
	 (setq this-command 'gnus-summary-prev-page)
	 (call-interactively 'gnus-summary-exit))
	((save-excursion (skip-chars-backward " D") (bolp))
	 (gnus-summary-mark-article-as-unread ?\ )
	 (forward-line -1))
	(t (setq this-command 'gnus-summary-prev-page)
	   (call-interactively 'gnus-summary-prev-page))))

(defun smart-gnus-article ()
  "Uses a key or mouse key to move through Gnus netnews articles.

Invoked via a key press when in gnus-article-mode.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) the first line or end of an article, the next unread message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled up a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-next-unread-article)
		    (gnus-summary-next-unread-article)
		    (gnus-summary-goto-subject gnus-current-article)
		    )
	   (let ((artic (get-buffer-window gnus-article-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-up))))

(defun smart-gnus-article-assist ()
  "Uses an assist-key or mouse assist-key to move through Gnus netnews articles.

Invoked via an assist-key press when in gnus-article-mode.
It assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed within:
 (1) the first line or end of an article, the previous message is displayed;
 (2) the first line of an Info cross reference, the reference is followed;
 (3) anywhere else, the window is scrolled down a windowful."
  (interactive)
  (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
	 (unwind-protect
	     (progn (set-buffer gnus-summary-buffer)
		    (setq this-command 'gnus-summary-prev-article)
		    (gnus-summary-prev-article nil)
		    (gnus-summary-goto-subject gnus-current-article)
		    )
	   (let ((artic (get-buffer-window gnus-summary-buffer)))
	     (if artic (select-window artic)))))
	((and (not (eolp)) (Info-handle-in-note)))
	(t (smart-scroll-down))))

;;; ************************************************************************
;;; smart-hmail functions
;;; ************************************************************************

(defun smart-hmail ()
  "Uses a key or mouse key to move through e-mail messages and summaries.

Invoked via a key press when in hmail:reader or hmail:lister mode.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the next undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled up a windowful; 
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is displayed in the msg window;
 (5) a msg summary buffer, on or after the last line, the messages marked
     for deletion are expunged."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-next))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-up))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:expunge))
	(t (lmail:goto))))

(defun smart-hmail-assist ()
  "Uses an assist key or mouse key to move through e-mail messages and summaries.

Invoked via an assist key press when in hmail:reader or hmail:lister mode.
It assumes that its caller has already checked that the assist-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.

If assist-key is pressed within:
 (1) a msg buffer, within the first line or at the end of a message,
     the previous undeleted message is displayed;
 (2) a msg buffer within the first line of an Info cross reference, the
     reference is followed;
 (3) anywhere else in a msg buffer, the window is scrolled down a windowful; 
 (4) a msg summary buffer on a header entry, the message corresponding to
     the header is marked as deleted;
 (5) a msg summary buffer, on or after the last line, all messages are
     marked undeleted."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode hmail:reader)
	 (cond ((or (last-line-p) (and (not (eolp)) (first-line-p)))
		(rmail:msg-prev))
	       ((and (not (eolp)) (Info-handle-in-note)))
	       ((smart-scroll-down))))
	;;
	;; Assume are in msg summary buffer
	;;
	((last-line-p) (lmail:undelete-all))
	(t (lmail:delete))))

;;; ************************************************************************
;;; smart-hyrolo functions
;;; ************************************************************************

(defun smart-hyrolo ()
  "In hyrolo match buffer, edits current entry.
Uses one key or mouse key.

Invoked via a key press when in the `hyrolo-display-buffer'.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer."
  (interactive)
  (hyrolo-edit-entry))

(defalias 'smart-hyrolo-assist 'smart-hyrolo)

;;; ************************************************************************
;;; smart-imenu functions
;;; ************************************************************************

(defun smart-imenu-display-item-where (item-name item-pos)
  "Displays an ITEM-NAME defined at ITEM-POS within the current buffer.
Uses the imenu library and the value of `hpath:display-where' to display."
  (hpath:display-buffer (current-buffer))
  (funcall imenu-default-goto-function item-name item-pos))

(defun smart-imenu-item-at-p (&optional variable-flag)
  "If any identifier at point is in the current buffer's imenu, returns its marker position, else nil.
With optional VARIABLE-FLAG non-nil, matches to variable definitions only.

Does nothing unless imenu has been loaded and an index has been
created for the current buffer.  When return value is non-nil, also
sets `hkey-value' to (identifier . identifier-definition-buffer-position)."
  (and (featurep 'imenu) imenu--index-alist
       (setq hkey-value (hargs:find-tag-default))
       (setq hkey-value (cons hkey-value (smart-imenu-item-p hkey-value variable-flag)))
       (cdr hkey-value)))

;; Derived from `imenu' function in the imenu library.
(defun smart-imenu-item-p (index-item &optional variable-flag)
  "If INDEX-ITEM is in the current buffer's imenu, returns its definition marker position, else nil.
If INDEX-ITEM is both a function and a variable, the function
definition is used by default; in such a case, when optional
VARIABLE-FLAG is non-nil, the variable definition is used instead."
  (when (stringp index-item)
      ;; Convert a string to an alist element.
      (if variable-flag
	  ;; Match to variable definitions only.
	  (setq index-item (assoc index-item
				  (cdr (assoc "Variables" (imenu--make-index-alist)))))
	;; First try to match a function definition and if that fails,
	;; then allow variable definitions and other types of matches as well.
	(let ((alist (imenu--make-index-alist)))
	  (setq index-item (or (assoc index-item alist)
			       ;; Does nothing unless the dash Emacs Lisp
			       ;; library is available for the -flatten function.
			       (and (require 'dash nil t)
				    (assoc index-item (-flatten alist)))))))
      (if index-item (cdr index-item))))

;;; ************************************************************************
;;; smart-info functions
;;; ************************************************************************
;;; Autoloaded in "hmouse-info.el".

;;; ************************************************************************
;;; smart-man functions
;;; ************************************************************************

;; "unix-apropos.el" is a publicly available Emacs Lisp package that
;; allows man page browsing from apropos listings.  "superman.el" is a
;; newer, much more complete package that you would probably prefer at
;; this point, but there is no Smart Key apropos support for it.  There
;; is smart key support within the man page buffers it produces, however.
;;

(defun smart-apropos ()
  "Moves through UNIX man apropos listings by using one key or mouse key.

Invoked via a key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled up
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window)
    (unix-apropos-get-man)))

(defun smart-apropos-assist ()
  "Moves through UNIX man apropos listings by using one assist-key or mouse assist-key.

Invoked via an assist-key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled down
     a windowful."

  (interactive)
  (if (last-line-p)
      (scroll-other-window (- 3 (window-height)))
    (unix-apropos-get-man)))

(defun smart-man-display (lisp-form)
  "Evaluates LISP-FORM returned from `smart-man-entry-ref' to display a man page."
  (eval lisp-form))

(defun smart-man-entry-ref ()
  "Returns form which displays referenced manual entry that point is on or nil.
Handles references in sections: NAME, SEE ALSO, or PACKAGES USED.  Also can
display C routine definitions selected in a man page, see
`smart-man-c-routine-ref'.

Man page buffer must either have an attached file or else a `man-path'
local variable containing its pathname."
  (interactive)
  (let ((ref ""))
    (if (not (or (if (string-match "Manual Entry\\|\\*man "
				   (buffer-name (current-buffer)))
		     (progn (and (boundp 'man-path) man-path
				 (setq ref (smart-symlink-expand man-path)))
			    t))
		 (if buffer-file-name
		     (string-match "/man/" (setq ref (smart-symlink-expand
						      buffer-file-name))))))
	(setq ref nil)
      (or (setq ref (or (smart-man-file-ref)
			(smart-man-c-routine-ref)))
	  (save-excursion
	    (let ((opoint (point))
		  (case-fold-search))
	      (and
	       (re-search-backward "^[.A-Z]" nil t)
	       (looking-at
		"\\(\\.SH[ \t]+\\)?\\(SEE ALSO\\|NAME\\|PACKAGES USED\\)")
	       (progn (goto-char opoint)
		      (skip-chars-backward "-_a-zA-Z0-9?.(")
		      (let ((start (point)))
			(skip-chars-forward "-_a-zA-Z0-9?.()")
			(setq ref (buffer-substring start (point)))
			;; Leave only one char within ref parens
			(if ref
			    (if (string-match "(\\(.\\)\\(.+\\))" ref)
				(setq ref (concat (substring ref 0 (match-end 1))
						  "\)"))))
			)))))))
    (cond ((equal ref "") nil)
	  ((stringp ref) (list 'manual-entry ref))
	  (t ref))))

(defun smart-man-c-routine-ref ()
  "Returns form to jump to the definition of the C function whose name is at point, if any, or nil
Valid sections within the man page are: ROUTINES, MACROS or FUNCTIONS.
Uses (smart-tags-file-list) function to determine the tags file from which to
locate the definition."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^\\(FUNCTIONS\\|ROUTINES\\|MACROS\\)[ \t\n\r]")
	   (progn (goto-char opoint)
		  (skip-chars-backward "_~<>:a-zA-Z0-9(")
		  (if (or (looking-at "\\([_~<>:a-zA-Z0-9]+\\)[ \t\n\r]*(")
			  (looking-at "\\([_~<:A-Z][_<>:A-Z0-9]+\\)"))
		      (setq ref (buffer-substring
				 (match-beginning 1) (match-end 1))
			    )))))
    (if ref
	(list 'smart-tags-display ref nil
	      (smart-tags-file-list (and (boundp 'man-path) man-path))))))

(defun smart-man-file-ref ()
  "Returns form to eval to display file whose name point is on, within a FILES man page section.
If not on a file name, returns nil."
  (let ((ref)
	(opoint (point))
	(case-fold-search))
    (save-excursion
      (and (re-search-backward "^[.A-Z]" nil t)
	   (looking-at "^FILES[ \t\n\r]")
	     (progn (goto-char opoint)
		    (skip-chars-backward "^ \t")
		    (if (looking-at "/[^ \t\n\r]+")
			(setq ref (buffer-substring
				   (match-beginning 0) (match-end 0))
			      )))))
    (if ref
	(list (if (br-in-browser)
		  'find-file 'find-file-other-window)
	      ref))))

;;; ************************************************************************
;;; smart-outline functions
;;; ************************************************************************

;; The functions in this section may require InfoDock's version of outline.el
;; for smart-outline-cut to work.

(defvar smart-outline-cut nil
  "Non-nil means outline region was cut and is ready to be pasted at point.")

(eval-after-load "outline"
  #'(mapc (lambda (mode)
	    (add-hook mode (lambda ()
			     (make-local-variable 'smart-outline-cut)
			     ;; Non-nil means outline region was cut
			     ;; and is available to be pasted at point.
			     (setq smart-outline-cut nil))))
	  '(outline-mode-hook outline-minor-mode-hook)))

(defun smart-outline ()
  "Collapses, expands, and moves outline entries.
Invoked via a key press when in outline-mode.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) after an outline heading has been cut via the Action Key, then paste the
     cut heading at point;
 (2) at the end of buffer, show all buffer text 
 (3) at the beginning of a heading line, cut the headings subtree from the
     buffer;
 (4) on a header line but not at the beginning or end, if headings subtree is
     hidden then show it, otherwise hide it;
 (5) anywhere else, scroll up a windowful."

  (interactive)
  (cond (smart-outline-cut
	 (setq smart-outline-cut nil) (yank))
	((eobp) (outline-show-all))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region
	  (point)
	  (or (outline-get-next-sibling)
	      ;; Skip past start of current entry
	      (progn (re-search-forward outline-regexp nil t)
		     (smart-outline-to-entry-end t (outline-level))))))

	((or (eolp) (zerop (save-excursion (beginning-of-line)
					   (outline-level))))
	 (smart-scroll-up))
	;; On an outline header line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (outline-show-subtree))
	(t (outline-hide-subtree))))


(defun smart-outline-assist ()
  "Collapses, expands, and moves outline entries.
Invoked via an assist-key press when in outline-mode.  It assumes that
its caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If assist-key is pressed:
 (1) after an outline heading has been cut via the action-key, allow multiple
     pastes throughout the buffer (last paste should be done with the Action Key,
     not the Assist Key);
 (2) at the end of buffer, hide all bodies in buffer;
 (3) at the beginning of a heading line, cut the current heading (sans
     subtree) from the buffer;
 (4) on a header line but not at the beginning or end, if heading body is
     hidden then show it, otherwise hide it;
 (5) anywhere else, scroll down a windowful."

  (interactive)
  (cond (smart-outline-cut (yank))
	((eobp) (outline-hide-body))
	((and (bolp) (looking-at outline-regexp))
	 (setq smart-outline-cut t)
	 (kill-region (point) 
		      ;; Skip past start of current entry
		      (progn (re-search-forward outline-regexp nil t)
			     (smart-outline-to-entry-end
			      nil (outline-level)))))
	((or (eolp) (zerop (save-excursion (beginning-of-line)
					   (outline-level))))
	 (smart-scroll-down))
	;; On an outline header line but not at the start/end of line.
	((smart-outline-subtree-hidden-p)
	 (outline-show-entry))
	(t (outline-hide-entry))))

(defun smart-outline-to-entry-end
  (&optional include-sub-entries curr-entry-level)
  "Goes to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
CURR-ENTRY-LEVEL is an integer representing the length of the current level
string which matched to `outline-regexp'.  If INCLUDE-SUB-ENTRIES is nil,
CURR-ENTRY-LEVEL is not needed."
  (let (next-entry-exists)
    (while (and (setq next-entry-exists
		      (re-search-forward outline-regexp nil t))
		include-sub-entries
		(save-excursion
		  (beginning-of-line)
		  (> (outline-level)
		     curr-entry-level))))
    (if next-entry-exists
	(progn (beginning-of-line) (point))
      (goto-char (point-max)))))

(defun smart-outline-subtree-hidden-p ()
  "Returns t if at least initial subtree of heading is hidden, else nil."
  (outline-invisible-in-p (point) (or (save-excursion (re-search-forward "[\n\r]" nil t)) (point))))

;;; ************************************************************************
;;; smart-push-button functions
;;; ************************************************************************

;; Emacs push button support
(defun smart-push-button-help (&optional pos)
  "Show help about a push button's action at optional POS or at point in the current buffer."
  (let ((expr (button-get (button-at (or pos (point))) 'action))
	 ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
	 (temp-buffer-show-hook)
	 (temp-buffer-show-function))
    (if (functionp expr) (describe-function expr)
      (with-help-window (print (format "Button's action is: '%s'" expr))))))

;;; ************************************************************************
;;; smart-tar functions
;;; ************************************************************************

(defun smart-tar ()
  "Uses a single key or mouse key to manipulate tar file entries.

Invoked via a key press when in tar-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) within an entry line, the selected file/directory is displayed for
     editing in the other window;
 (2) on or after the last line in the buffer, if any deletes are to be
     performed, they are executed after user verification, otherwise, this
     tar file browser is quit."

  (interactive)
  (cond ((last-line-p)
	 (let (flagged)
	   (save-excursion
	     (goto-char 1)
	     (setq flagged (re-search-forward "^D" nil t)))
	   (if flagged
	       (tar-expunge)
	     (kill-buffer nil))))
	(t (tar-extract-other-window))))

(defun smart-tar-assist ()
  "Uses a single assist-key or mouse assist-key to manipulate tar file entries.

Invoked via an assist-key press when in dired-mode.  It assumes that its
caller has already checked that the assist-key was pressed in an appropriate
buffer and has moved the cursor there.

If assist-key is pressed:
 (1) on an entry line, the current entry is marked for deletion;
 (2) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (tar-unflag (- (count-lines (point-min) (point-max))))
	 (goto-char (point-max)))
	(t (tar-flag-deleted 1))))

(provide 'hui-mouse)

;;; hui-mouse.el ends here
