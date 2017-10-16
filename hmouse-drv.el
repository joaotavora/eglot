;;; hmouse-drv.el --- Smart Key/Mouse driver functions.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    04-Feb-90
;;
;; Copyright (C) 1989-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (defvar assist-flag nil)
  (defvar hkey-action nil)
  (defvar pred-value nil))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hmouse-verify-release-window-flag t
  "When non-nil under the macOS window system, verifies the application of top-most window.
Forces a window system check at a screen position that the top-most
window there is an Emacs frame or treats the location as outside Emacs,
even though an Emacs frame may be below the top-most window.

See function `hmouse-window-at-absolute-pixel-position' for more details.")

(defvar action-key-depressed-flag nil "t while Action Key is depressed.")
(defvar assist-key-depressed-flag nil "t while Assist Key is depressed.")
(defvar action-key-depress-args nil
  "List of mouse event args from most recent depress of the Action Key.")
(defvar assist-key-depress-args nil
  "List of mouse event args from most recent depress of the Assist Key.")

(defvar action-key-release-args nil
  "List of mouse event args from most recent release of the Action Key.")
(defvar assist-key-release-args nil
  "List of mouse event args from most recent release of the Assist Key.")

(defvar action-key-depress-window nil
  "The last window in which the Action Key was depressed or nil.
This is set to nil when the depress is on an inactive minibuffer.")
(defvar assist-key-depress-window nil
  "The last window in which the Assist Key was depressed or nil.
This is set to nil when the depress is on an inactive minibuffer.")
(defvar action-key-release-window nil
  "The last window in which the Action Key was released or nil.")
(defvar assist-key-release-window nil
  "The last window in which the Assist Key was released or nil.")

(defvar action-key-depress-position nil
  "The last screen position at which the Action Key was depressed or nil.")
(defvar assist-key-depress-position nil
  "The last screen position at which the Assist Key was depressed or nil.")

(defvar action-key-depress-prev-point nil
  "Marker at point prior to last Action Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar assist-key-depress-prev-point nil
  "Marker at point prior to last Assist Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar action-key-release-prev-point nil
  "Marker at point prior to last Action Key release.
Note that this may be a buffer different than where the release occurs.")
(defvar assist-key-release-prev-point nil
  "Marker at point prior to last Assist Key release.
Note that this may be a buffer different than where the release occurs.")

(defvar action-key-cancelled nil
  "When non-nil, cancels last Action Key depress.")
(defvar assist-key-cancelled nil
  "When non-nil, cancels last Assist Key depress.")

(defvar action-key-help-flag nil
  "When non-nil, forces display of help for next Action Key release.")
(defvar assist-key-help-flag nil
  "When non-nil, forces display of help for next Assist Key release.")

(defcustom hkey-debug nil
  "If non-nil, displays a message with the context and values from each Smart Key activation.
Default is nil."
  :type 'boolean
  :group 'hyperbole-commands)

(defvar hkey-region nil
  "Used to pass the value of a selected region between a Smart Key depress and release.
This permits the Smart Keys to behave as paste keys.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar action-mouse-key-prev-window nil
  "Window point was in prior to current invocation of `action/assist-mouse-key'.")

(defvar action-mouse-key-prefix-arg nil
  "Prefix argument to pass to `smart-br-cmd-select'.")

(defvar hkey-help-msg "" "Holds last Smart Key help message.")
(defvar hkey--wconfig nil
  "Window configuration within current frame prior to display of a help buffer.")

;;; ************************************************************************
;;; Hyperbole context-sensitive key driver functions
;;; ************************************************************************

;;; Smart Key Depress Functions
(defun action-key-depress (&rest args)
  (interactive)
  (cond (assist-key-depressed-flag
	 (or action-key-help-flag
	     (setq assist-key-help-flag t)))
	((hmouse-save-region)))
  (setq action-key-depress-prev-point (point-marker)
	action-key-depressed-flag t
	action-key-depress-args (hmouse-set-point args)
	action-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	action-key-depress-position (mouse-absolute-pixel-position)
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil)
  (run-hooks 'action-key-depress-hook))

(defun assist-key-depress (&rest args)
  (interactive)
  (cond (action-key-depressed-flag
	 (or assist-key-help-flag
	     (setq action-key-help-flag t)))
	((hmouse-save-region)))
  (setq assist-key-depress-prev-point (point-marker)
	assist-key-depressed-flag t
	assist-key-depress-args (hmouse-set-point args)
	assist-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	assist-key-depress-position (mouse-absolute-pixel-position)
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil)
  (run-hooks 'assist-key-depress-hook))

(defun action-key-depress-emacs (event)
  (interactive "e")
  (action-key-depress event))

(defun assist-key-depress-emacs (event)
  (interactive "e")
  (assist-key-depress event))

;;; Smart Key Release Functions
(defun action-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (apply #'action-mouse-key (hmouse-key-release-args-emacs event)))

(defun assist-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (apply #'assist-mouse-key (hmouse-key-release-args-emacs event)))

(defun action-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute `action-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; action-key-depress command invocation.
  (if action-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq action-key-depressed-flag nil)
	(cond (action-key-cancelled
		(setq action-key-cancelled nil
		      assist-key-depressed-flag nil))
	      (assist-key-depressed-flag
		(hmouse-function nil nil args))
	      ((hkey-mouse-help nil args))
	      (t
	       (run-hooks 'action-key-release-hook)
	       (hmouse-function #'action-key-internal nil args)))
	;; Need to clear these variables so that mouse pasting does
	;; not occur repeatedly from a single region selection.
	(setq hkey-region nil
	      hkey-value nil))))

(defun assist-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute `assist-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; assist-key-depress command invocation.
  (if assist-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq assist-key-depressed-flag nil)
	(cond (assist-key-cancelled
		(setq assist-key-cancelled nil
		      action-key-depressed-flag nil))
	      (action-key-depressed-flag
		(hmouse-function nil t args))
	      ((hkey-mouse-help t args))
	      (t
	       (run-hooks 'assist-key-release-hook)
	       (hmouse-function #'assist-key-internal t args)))
	;; Need to clear this variable so that mouse pasting does
	;; not occur repeatedly from a single region selection.
	(setq hkey-region nil
	      hkey-value nil))))

;;; Smart Key Commands
(defun action-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `action-key-default-function' variable is run.  Return t
unless the `action-key-default-function' variable is not bound to
a valid function."
  (interactive)
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq action-key-depress-prev-point nil
	action-key-depress-args nil
	action-key-depress-window nil
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil)
  (prog1 (action-key-internal)
    (run-hooks 'action-key-depress-hook 'action-key-release-hook)))

(defun action-key-internal ()
  (setq action-key-depressed-flag nil)
  (if action-key-cancelled
      (setq action-key-cancelled nil
	    assist-key-depressed-flag nil))
  (or (hkey-execute nil)
      (when (fboundp action-key-default-function)
	(funcall action-key-default-function)
	t)))

(defun assist-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `assist-key-default-function' variable is run.  Return
non-nil unless `assist-key-default-function' variable is not
bound to a valid function."
  (interactive)
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq assist-key-depress-prev-point nil
	assist-key-depress-args nil
	assist-key-depress-window nil
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil)
  (prog1 (assist-key-internal)
    (run-hooks 'assist-key-depress-hook 'assist-key-release-hook)))

(defun assist-key-internal ()
  (setq assist-key-depressed-flag nil)
  (if assist-key-cancelled
      (setq assist-key-cancelled nil
	    action-key-depressed-flag nil))
  (or (hkey-execute t)
      (when (fboundp assist-key-default-function)
	(funcall assist-key-default-function)
	t)))

(defun hkey-either (arg)
  "Execute `action-key' or with non-nil ARG execute `assist-key'."
  (interactive "P")
  (if arg (assist-key) (action-key)))

;;; ************************************************************************
;;; Public support functions
;;; ************************************************************************

(defun hkey-debug ()
  (message (format "(HyDebug) %sContext: %s; %s: %s; Buf: %s; Mode: %s; MinibufDepth: %s"
		   (cond ((eq pred-value 'hbut:current)
			  (format "ButType: %s; ButLabel: %s; "
				  (hattr:get  'hbut:current 'categ)
				  (hypb:format-quote (hbut:label 'hbut:current))))
			 ((functionp pred-value)
			  (format "Selection Func: %s; " pred-value))
			 (t ""))
		   pred
		   (if assist-flag "Assist" "Action")
		   (hypb:format-quote (format "%s" hkey-action))
		   (current-buffer) major-mode (minibuffer-depth))))

(defun hkey-execute (assist-flag)
  "Evaluate Action Key form (or Assist Key form with ASSIST-FLAG non-nil) for first non-nil predicate from `hkey-alist'.
Non-nil ASSIST-FLAG means evaluate second form, otherwise evaluate first form.
Return non-nil iff a non-nil predicate is found."
  ;; Keep in mind that hkey-alist may be set to hmouse-alist here, with additional predicates.
  (let ((hkey-forms hkey-alist)
	(pred-value) (hkey-action) hkey-form pred)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (if (setq hkey-action (if assist-flag (cdr (cdr hkey-form)) (car (cdr hkey-form)))
		pred (car hkey-form)
		pred-value (eval pred))
	  ;; Conditionally debug after Smart Key release and evaluation
	  ;; of matching predicate but before hkey-action is executed.
	  (progn (if hkey-debug (hkey-debug))
		 (eval hkey-action))
	(setq hkey-forms (cdr hkey-forms))))
    pred-value))

(defun hkey-help (&optional assist-flag)
  "Display help for the Action Key command in current context.
With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
Return non-nil iff associated help documentation is found."
  (interactive "P")
  (let ((hkey-forms hkey-alist)
	hkey-form pred-value call calls cmd-sym doc)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (or (setq pred-value (eval (car hkey-form)))
	  (setq hkey-forms (cdr hkey-forms))))
    (if pred-value
	(setq call (if assist-flag (cdr (cdr hkey-form))
		     (car (cdr hkey-form)))
	      cmd-sym (car call))
      (setq cmd-sym (if assist-flag assist-key-default-function action-key-default-function)
	    call cmd-sym))
    (if (and (consp call) (eq (car call) 'call-interactively))
	(if (consp (cadr call))
	    (setq cmd-sym (if (memq (caadr call) '(function quote))
			      (cadadr call) 
			    (caadr call)))))
    (setq calls (if (and (consp call) (eq (car call) 'or))
		    (mapcar #'car (cdr call))
		  (list cmd-sym)))

    (setq hkey-help-msg
	  (if (and cmd-sym (symbolp cmd-sym))
	      (progn
		(let* ((condition (car hkey-form))
		       (temp-buffer-show-hook
			 (lambda (buf)
			   (set-buffer buf)
			   (help-mode)
			   (let ((owind (selected-window)))
			     (if (br-in-browser)
				 (save-excursion
				   (br-to-view-window)
				   (select-window (previous-window))
				   (display-buffer buf 'other-win))
			       (display-buffer buf 'other-win))
			     (if (or (and (boundp 'help-window-select)
					  help-window-select)
				     (and (boundp 'help-selects-help-window)
					  help-selects-help-window))
				 (select-window (get-buffer-window buf))
			       (select-window owind)))))
		       (temp-buffer-show-function temp-buffer-show-hook))
		  (with-output-to-temp-buffer
		      (hypb:help-buf-name
		       (format "%s Key" (if assist-flag "Assist" "Action")))
		    (princ (format "A click of the %s Key"
				   (if assist-flag "Assist" "Action")))
		    (terpri)
		    (princ "WHEN  ")
		    (princ
		      (or condition
			  "there is no matching context"))
		    (terpri)

		    (mapcar (lambda (c)
			      (if (> (length calls) 1)
				  ;; Is an 'or' set of calls
				  (princ "'OR' "))
			      (princ "CALLS ") (princ (list c))
			      (when (setq doc (documentation c))
				(princ " WHICH")
				(princ (if (string-match "\\`[a-zA-Z]*[a-rt-zA-RT-Z]+s[ [:punct:]]" doc)
					   ":" " WILL:"))
				(terpri) (terpri)
				(princ (replace-regexp-in-string "^" "  " doc))
				(terpri) (terpri)))
			    calls)

		    (when (memq cmd-sym '(hui:hbut-act hui:hbut-help))
		      (princ (format "BUTTON SPECIFICS:\n\n%s\n"
				     (actype:doc 'hbut:current t)))
		      (hattr:report
		       (nthcdr 2 (hattr:list 'hbut:current))))
		    (terpri)
		    ))
		"")
	    (message "No %s Key command for current context."
		     (if assist-flag "Assist" "Action"))))
    doc))

(defun hkey-assist-help ()
  "Display doc associated with Assist Key command in current context.
Return non-nil iff associated documentation is found."
  (interactive)
  (hkey-help 'assist))

;; Overload help-mode quit-window function to support Hyperbole
;; hkey--wconfig window configurations.
(unless (eq (symbol-function #'quit-window) #'hkey-help-hide)
  (defalias 'hkey-quit-window (hypb:function-copy #'quit-window)))

;;;###autoload
(defun hkey-help-hide (&optional kill window)
  "Optionally KILL current buffer (default is bury) and quit WINDOW.
Restore frame to configuration prior to help buffer display.
Point must be in a help buffer.  See `hkey-quit-window' for additional
details."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (window-configuration-p hkey--wconfig)
	(progn (set-window-configuration hkey--wconfig)
	       (if kill (kill-buffer buf)
		 (bury-buffer buf)))
      (hkey-quit-window kill window)))
  (setq hkey--wconfig nil))

(defalias 'quit-window 'hkey-help-hide)

;; Newer versions of Emacs define this variable but older versions,
;; e.g. Emacs 22, do not.  Calls to the `with-help-buffer' macro
;; compiled in Emacs 25 will fail without this, so conditionally
;; define it here.
(unless (boundp 'help-window-point-marker)
  (defvar help-window-point-marker (make-marker)
    "Marker to override default `window-point' in help windows."))

;;;###autoload
(defun hkey-help-show (&optional buffer current-window)
  "Save prior window configuration if BUFFER displays help.  Display BUFFER.

With optional second arg CURRENT-WINDOW non-nil, force display of buffer within
the current window.  By default, it is displayed according to the setting of
`hpath:display-where'."
  (if (bufferp buffer) (setq buffer (buffer-name buffer)))
  (if (null buffer) (setq buffer (buffer-name (current-buffer))))
  (and (stringp buffer)
       (string-match "^\\*Help\\|Help\\*$" buffer)
       (not (memq t (mapcar (lambda (wind)
			      (string-match
			       "^\\*Help\\|Help\\*$"
			       (buffer-name (window-buffer wind))))
			    (hypb:window-list 'no-mini))))
       (setq hkey--wconfig (current-window-configuration)))
  (unwind-protect
      (let* ((buf (get-buffer-create buffer))
	     ;; Help-mode calls with-temp-buffer which invokes one of these hooks
	     ;; which calls hkey-help-show again, so nullify them before
	     ;; displaying the buffer.
	     (temp-buffer-show-hook)
	     (temp-buffer-show-function)
	     (wind (cond (current-window
			  (switch-to-buffer buf)
			  (selected-window))
			 (t (hpath:display-buffer buf)))))
	(when wind
	  (setq minibuffer-scroll-window wind)
	  ;; Don't use help-mode in buffers already set up with a
	  ;; quit-key to bury the buffer, e.g. minibuffer completions,
	  ;; as this will sometimes disable default left mouse key item
	  ;; selection.
	  (unless (or (where-is-internal 'quit-window (current-local-map))
		      (where-is-internal 'hkey-help-hide (current-local-map)))
	    (if (string-match "^\\*Help\\|Help\\*$" (buffer-name))
		(help-mode))
	    (local-set-key "q" #'hkey-help-hide))))
    ;; If in a *Completions* buffer, re-select the window that
    ;; generated the completions.
    (if (buffer-live-p completion-reference-buffer)
	(select-window (get-buffer-window completion-reference-buffer t)))))

(defun hkey-mouse-help (assist-flag args)
  "If a Smart Key help flag is set and the other Smart Key is not down, show help.
Takes two args:  ASSIST-FLAG should be non-nil iff command applies to the Assist Key.
ARGS is a list of arguments passed to `hmouse-function'.
Return t if help is displayed, nil otherwise."
  (let ((help-shown)
	(other-key-released (not (if assist-flag
				     action-key-depressed-flag
				   assist-key-depressed-flag))))
    (unwind-protect
	(setq help-shown
	      (cond ((and  action-key-help-flag other-key-released)
		     (setq action-key-help-flag nil)
		     (hmouse-function #'hkey-help assist-flag args)
		     t)
		    ((and  assist-key-help-flag other-key-released)
		     (setq assist-key-help-flag nil)
		     (hmouse-function #'hkey-assist-help assist-flag args)
		     t)))
      (when help-shown
	  ;; Then both Smart Keys have been released. 
	(setq action-key-cancelled nil
	      assist-key-cancelled nil)
	t))))

(defun hkey-operate (&optional arg)
  "Use the keyboard to emulate Smart Mouse Key drag actions.
Each invocation alternates between starting a drag and ending it.
Optional prefix ARG non-nil means emulate Assist Key rather than the
Action Key.

Only works when running under a window system, not from a dumb terminal."
  (interactive "P")
  (or (hyperb:window-system)
      (hypb:error "(hkey-operate): Drag actions require mouse support"))
  (if arg
      (if assist-key-depressed-flag
	  (progn (assist-mouse-key)
		 (message "Assist Key released."))
	(assist-key-depress)
	(message
	  "Assist Key depressed; go to release point and hit {%s %s}."
	  (substitute-command-keys "\\[universal-argument]")
	  (substitute-command-keys "\\[hkey-operate]")
	  ))
    (if action-key-depressed-flag
	(progn (action-mouse-key)
	       (message "Action Key released."))
      (action-key-depress)
      (message "Action Key depressed; go to release point and hit {%s}."
	       (substitute-command-keys "\\[hkey-operate]"))
      )))

(defun hkey-summarize (&optional current-window)
  "Display smart key operation summary in help buffer.
With optional arg CURRENT-WINDOW non-nil, force display of buffer within
the current window.  By default, it is displayed in another window."
  (interactive)
  (let* ((doc-file (hypb:hkey-help-file))
	 (buf-name (hypb:help-buf-name "Smart Keys"))
	 (wind (get-buffer-window buf-name))
	 owind)
    (when (file-readable-p doc-file)
      (if (br-in-browser)
	  (br-to-view-window))
      (if wind
	  (select-window wind)
	(hkey-help-show buf-name current-window)
	(select-window (get-buffer-window buf-name)))
      (setq buffer-read-only nil) (erase-buffer)
      (insert-file-contents doc-file)
      (goto-char (point-min))
      (set-buffer-modified-p nil))))


(defun hkey-toggle-debug (&optional arg)
  "Toggle whether conflicting local key bindings are overridden by Hyperbole.
With optional ARG, override them iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       hkey-debug))
      (progn (setq hkey-debug nil)
	     (message "Smart Key debugging is off."))
    (setq hkey-debug t)
    (message "Smart Key debugging is on; press a Smart Key to see its context.")))

(defun hmouse-depress-inactive-minibuffer-p (event)
  "Return the minibuffer window if the last Smart Mouse Key depress EVENT was in it and it was inactive, else nil."
  (let ((window (posn-window (event-start event))))
    (if (framep window) (setq window (frame-selected-window window)))
    (and (window-minibuffer-p window)
	 (not (minibuffer-window-active-p window))
	 window)))

;; Based on code from subr.el.
(defun hmouse-vertical-line-spacing (frame)
  "Return any extra vertical spacing in pixels between text lines or 0 if none."
  (let ((spacing (when (display-graphic-p frame)
                   (or (with-current-buffer (window-buffer (frame-selected-window frame))
                         line-spacing)
		       (frame-parameter frame 'line-spacing)))))
    (cond ((floatp spacing)
	   (setq spacing (truncate (* spacing (frame-char-height frame)))))
	  ((null spacing)
	   (setq spacing 0)))
    spacing))

(defun hmouse-window-at-absolute-pixel-position (&optional position release-flag)
  "Return the top-most Emacs window at optional POSITION ((x . y) in absolute pixels) or mouse position.
If POSITION is not in a window, return nil.  Considers all windows on
the same display as the selected frame.

If optional RELEASE-FLAG is non-nil, this is part of a Smart Key
release computation, so optimize window selection based on the depress
window already computed.

If the selected frame is a graphical macOS window and
`hmouse-verity-release-window-flag' is non-nil, then return the
top-most Emacs window only if it is the top-most application window at
the position (not below another application's window)."
  (interactive)
  (setq position (or position (mouse-absolute-pixel-position)))
  ;; Proper top-to-bottom listing of frames is available only in Emacs
  ;; 26 and above.  For prior versions, the ordering of the frames
  ;; returned is not guaranteed, so the frame whose window is returned
  ;; may not be the uppermost.
  (let* ((top-to-bottom-frames (if (fboundp 'frame-list-z-order)
				   (frame-list-z-order)
				 (frame-list)))
	 (pos-x (car position))
	 (pos-y (cdr position))
	 edges left top right bottom
	 frame
	 in-frame
	 window)
    ;; First find top-most frame containing position.
    (while (and (not in-frame) top-to-bottom-frames)
      (setq frame (car top-to-bottom-frames)
	    top-to-bottom-frames (cdr top-to-bottom-frames))
      ;; Check that in-frame is valid with frame-live-p since under macOS
      ;; when position is outside a frame, in-frame could be invalid and
      ;; frame-visible-p would trigger an error in that case.
      (when (and (frame-live-p frame) (frame-visible-p frame))
	(setq edges (frame-edges frame)
	      left   (nth 0 edges)
	      top    (nth 1 edges)
	      right  (nth 2 edges)
	      bottom (nth 3 edges))
	(when (and (>= pos-x left) (<= pos-x right)
		   (>= pos-y top)  (<= pos-y bottom))
	  (setq in-frame frame))))
    ;; If in-frame is found, find which of its windows contains
    ;; position and return that.  The window-at call below requires
    ;; character coordinates relative to in-frame, so compute them.
    (when in-frame
      (let ((depress-position (and release-flag (if assist-flag
						    assist-key-depress-position
						  action-key-depress-position)))
	    (depress-window  (and release-flag (if assist-flag
						   assist-key-depress-window
						 action-key-depress-window))))
	(if (and release-flag depress-window (equal position depress-position))
	    ;; This was a click, so we know that the frame of the click
	    ;; is topmost on screen or the mouse events would not have
	    ;; been routed to Emacs.  Reuse saved window of depress rather
	    ;; then running possibly expensive computation to find the
	    ;; topmost application window.
	    (setq window depress-window)
	  (let ((char-x (/ (- pos-x left) (frame-char-width in-frame)))
		(line-y (/ (- pos-y top) (+ (frame-char-height in-frame)
					    (hmouse-vertical-line-spacing in-frame)))))
	    (setq window (window-at char-x line-y in-frame)))
	  ;;
	  ;; Even if in-frame is found, under click-to-focus external window
	  ;; managers, Emacs may have received the drag release event when
	  ;; in-frame was covered by an external application's window.
	  ;; Emacs presently has no way to handle this.  However, for the
	  ;; macOS window system only, Hyperbole has a Python script, topwin, which
	  ;; computes the application of the topmost window at the point of release.
	  ;; If that is Emacs, then we have the right window and nothing need be
	  ;; done; otherwise, set window to nil and return.
	  ;;
	  (when (and hmouse-verify-release-window-flag
		     window (eq (window-system) 'ns))
	    (let ((topwin (executable-find "topwin"))
		  (case-fold-search t)
		  topmost-app)
	      (when (and topwin (file-executable-p topwin))
		(setq topmost-app (shell-command-to-string
				   (format "topwin %d %d" pos-x pos-y)))
		(cond ((string-match "emacs" topmost-app)) ; In an Emacs frame, do nothing.
		      ((or (equal topmost-app "")
			   ;; Any non-Emacs app window
			   (string-match "\\`\\[" topmost-app))
		       ;; Outside of any Emacs frame
		       (setq window nil))
		      (t		; topwin error message
		       ;; Setup of the topwin script is somewhat complicated,
		       ;; so don't trigger an error just because of it.  But
		       ;; display a message so the user knows something happened
		       ;; when topwin encounters an error.
		       (message "(Hyperbole): topwin Python script error: %s" topmost-app)))))))))

    (when (called-interactively-p 'interactive)
      (message "%s at absolute pixel position %s"
	       (or window "No Emacs window") position))
    window))

(defun hmouse-window-coordinates (&optional event)
  "Return a list (window (x-chars . y-chars)) for optional EVENT.
Always ignores EVENT coordinates and uses current mouse position.
The area of the EVENT is utilized. If EVENT is not given and the
free variable `assist-flag' is non-nil, EVENT is set to
`assist-key-release-args', otherwise, `action-key-release-args'.

The coordinates x-chars and y-chars are relative character
coordinates within the window.  If POSITION is not in a live
window, return nil.  Considers all windows on the selected frame's display."
  (interactive)
  (unless (eventp event)
    (setq event (if assist-flag assist-key-release-args action-key-release-args)))
  (let* ((position (mouse-absolute-pixel-position))
	 (pos-x (car position))
	 (pos-y (cdr position))
	 (window (hmouse-window-at-absolute-pixel-position position t))
	 (edges (when (window-live-p window) (window-edges window t t t)))
	 left top right bottom
	 frame)
    (when edges
      (setq left   (nth 0 edges)
	    top    (nth 1 edges)
	    right  (nth 2 edges)
	    bottom (nth 3 edges))
      (when (or (and event (eq (posn-area (event-start event)) 'mode-line))
		(and (>= pos-x left) (<= pos-x right)
		     (>= pos-y top)  (<= pos-y bottom)))
	;; If position is in a live window, compute position's character
	;; coordinates within the window and return the window with these
	;; coordinates.
	(setq frame (window-frame window)
	      pos-x (round (/ (- pos-x left) (frame-char-width frame)))
	      pos-y (round (/ (- pos-y top)  (+ (frame-char-height frame)
						(hmouse-vertical-line-spacing frame)))))))
    (when (called-interactively-p 'interactive)
      (message "%s at %s coordinates (%s . %s)"
	       (if edges window "No live Emacs window")
	       (if frame "character" "absolute pixel")
	       pos-x pos-y))
    (when edges (list window (cons pos-x pos-y)))))

(defun hmouse-key-release-window ()
  "Return the window of the current mouse position if any, else nil."
  (ignore-errors (hmouse-window-at-absolute-pixel-position nil t)))

(defun hmouse-key-release-args-emacs (event)
  "For GNU Emacs, return a possibly modified version of EVENT as a list.
For mouse drags and double and triple clicks, remove any depress location,
compute the actual release location and include that."
  (if (integerp event)
      (list event)
    (let ((ev-type-str (and (listp event) (symbol-name (car event)))))
      (if (or (and ev-type-str
		   (string-match "\\(double\\|triple\\)-mouse" ev-type-str))
	      (not (= (length event) 3)))
	  event
	(let ((pos (event-end event))
	      coords window-and-char-coords)
	  (when (and ev-type-str (string-match "drag-mouse" ev-type-str)
		     ;; end of drag event; If drag crossed frames, the location
		     ;; will contain the frame of the depress point and
		     ;; some relative coordinates; change these to the window of
		     ;; release and window's character coordinates if within a window
		     ;; and to nil if outside of Emacs (as best we can tell).
		     (framep (posn-window pos)))
	    (setq window-and-char-coords (hmouse-window-coordinates event)
		  window (car window-and-char-coords)
		  coords (cadr window-and-char-coords))
	    ;; Modify the values in the event-end structure even if no
	    ;; valid window was found.
	    (setcar pos window)
	    (setcar (nthcdr 2 pos) coords)))
	;; Remove depress coordinates and send only original release coordinates.
	(list (car event) (nth 2 event))))))

(defun hmouse-save-region (&optional frame)
  "Save any active region within the current buffer.
Under InfoDock and XEmacs, `zmacs-region' must be t; under GNU Emacs,
`transient-mark-mode' must be t or the function does nothing."
  (if (cond
       ;; Newer GNU Emacs
       ((fboundp 'use-region-p)
	(let ((use-empty-active-region))
	  (use-region-p)))
       ;; InfoDock and XEmacs
       ((fboundp 'region-exists-p)
	(and (fboundp 'region-active-p) (region-active-p) (region-exists-p)))
       ;; Older GNU Emacs
       ((boundp 'transient-mark-mode)
	(and transient-mark-mode mark-active)))
      (setq hkey-region (buffer-substring (region-beginning) (region-end)))
    (setq hkey-region nil)))

;; Save any active region to `hkey-region' when the mouse is moved between frames or buffers.
(if (featurep 'xemacs)
    (add-hook 'mouse-leave-frame-hook #'hmouse-save-region)
  ;; GNU Emacs
  (add-hook 'mouse-leave-buffer-hook #'hmouse-save-region))

;; BW - Last confirmed in 1999, for some reason, using this next
;; function in byte-compiled form caused the first character 
;; after a mouse key depress to be dropped from the input queue when running
;; Emacs under X.  The non-byte-compiled form always worked fine.  We
;; assume this is no longer a problem in 2016 but have this note here
;; in case it is.
(defun hmouse-set-point (args)
  "Set point to Smart Key press/release location given by ARGS.
Return argument list including x and y frame coordinates in characters and
lines or if ARGS is null and there is no graphical window system,
return current point as a marker."
  (and (car args) (listp (car args)) (setq args (car args)))
  (if (and args (hyperb:window-system))
      (progn (hmouse-set-point-at args)
	     (cond ((featurep 'xemacs)
		    (if (eventp current-mouse-event)
			(copy-event current-mouse-event)))
		   ((equal (hyperb:window-system) "next")
		    (let ((win (car args)))
		      (list win
			    (+ (nth 1 args) (nth 0 (window-edges win)))
			    (+ (nth 2 args) (nth 1 (window-edges win))))))
		   (t args)))
    (posn-at-point)))

(defun hmouse-set-point-at (set-point-arg-list)
  "Set point to cursor position using SET-POINT-ARG-LIST and returns t.
If 'hmouse-set-point-command' is not bound to a function, this does nothing
and returns nil."
  (if (fboundp hmouse-set-point-command)
      (or (if set-point-arg-list
	      (funcall hmouse-set-point-command set-point-arg-list)
	    (funcall hmouse-set-point-command))
	  t)))

;; "hsettings.el" contains documentation for this variable.
(or (boundp 'smart-scroll-proportional)
    (defvar smart-scroll-proportional t
      "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful."))

;; The smart keys scroll buffers when pressed at the end of lines.
;; These next two functions do the scrolling and keep point at the end
;; of line to simplify repeated scrolls when using keyboard smart keys.
;;
;; These functions may also be used to test whether the scroll action would
;; be successful, no action is taken if it would fail (because the beginning
;; or end of a buffer is already showing) and nil is returned.
;; t is returned whenever scrolling is performed.

(defun hmouse-function (func assist-flag set-point-arg-list)
  "Execute FUNC for Action Key (Assist Key with ASSIST-FLAG non-nil) and set point from SET-POINT-ARG-LIST.
FUNC may be nil in which case no function is called.
SET-POINT-ARG-LIST is passed to the call of the command bound to
`hmouse-set-point-command'.  Return nil if `hmouse-set-point-command' variable
is not bound to a valid function."
  (when (fboundp hmouse-set-point-command)
    (if assist-flag
	(setq assist-key-release-window (hmouse-key-release-window)
	      assist-key-release-prev-point (point-marker))
      (setq action-key-release-window (hmouse-key-release-window)
	    action-key-release-prev-point (point-marker)))
    (and (eq major-mode 'br-mode)
	 (setq action-mouse-key-prev-window 
	       (if (br-in-view-window-p)
		   (save-window-excursion
		     (br-next-listing-window)
		     (selected-window))
		 (selected-window))))
    (setq action-mouse-key-prefix-arg current-prefix-arg)
    (let ((release-args (hmouse-set-point set-point-arg-list)))
      (if assist-flag
	  (setq assist-key-release-args release-args)
	(setq action-key-release-args release-args)))
    (when func
      (funcall func)
      (setq action-mouse-key-prev-window nil
	    action-mouse-key-prefix-arg nil))
    t))

(defun smart-scroll-down ()
  "Scroll down according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the bottom window line,
scroll down (backward) a windowful.  Otherwise, try to bring current line
to the bottom of the window.  Leave point at end of line and return t if scrolled,
nil if not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already last in window, then scroll backward
	;; a windowful, otherwise make it last in window.
	(if (>= (point) (save-excursion
			  (goto-char (1- (window-end)))
			  (beginning-of-line) (point)))
	    (if (pos-visible-in-window-p (point-min))
		(setq rtn nil)
	      (scroll-down))
	  (recenter -1))
      (if (pos-visible-in-window-p (point-min))
	  (setq rtn nil)
	(scroll-down)))
    (end-of-line)
    (or rtn (progn (beep) (message "Beginning of buffer")))
    rtn))

(defun smart-scroll-up ()
  "Scroll up according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the top window line,
scroll up (forward) a windowful.  Otherwise, tyr to bring current line to
the top of the window.  Leave point at end of line and return t if scrolled, nil if
not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already first in window, then scroll forward a
	;; windowful, otherwise make it first in window.
	(if (<= (point) (save-excursion
			  (goto-char (window-start))
			  (end-of-line) (point)))
	    (if (pos-visible-in-window-p (point-max))
		(setq rtn nil)
	      (scroll-up))
	  (recenter 0))
      (if (pos-visible-in-window-p (point-max))
	  (setq rtn nil)
	(scroll-up)))
    (end-of-line)
    (or rtn (progn (beep) (message "End of buffer")))
    rtn))

(provide 'hmouse-drv)

;;; hmouse-drv.el ends here
