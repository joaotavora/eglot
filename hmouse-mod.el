;;; hmouse-mod.el ---  Action Key acts as CONTROL modifier and Assist Key as META modifier.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     8-Oct-92 at 19:08:31
;;
;; Copyright (C) 1992-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This is presently not used because Emacs binds Control and Meta
;;   mouse keys which interfere with this mode.  To make this work,
;;   Hyperbole will first have to unbind those mouse keys before
;;   invoking this mode.
;;
;;   ----
;;
;;   This defines a single minor mode, hmouse-mod-mode (Hmouse
;;   Modifier mode) which makes the Action Mouse Key operate as a
;;   Control- modifier key and the Assist Mouse Key operate as a Meta-
;;   modifier key.
;;
;;   If the Action Key is held down while alpha characters are typed,
;;   they are translated into Control keys instead.  The Assist
;;   Key translates them into Meta keys.  When both Smart Keys
;;   are depressed, Control-Meta keys are produced.  The commands bound
;;   to the characters produced are then run.
;;
;;   So the Smart Keys modify the keys typed, e.g. Action Key + {a}
;;   runs the function for {C-a}.
;;
;;   If no keys are typed while the Smart Keys are down, they operate as
;;   normally under Hyperbole.

;;   This module is for balancing keypress energy across both hands to
;;   reduce carpal tunnel stress.  It may also be used with a chord keyboard
;;   in one hand and a mouse in the other to point at things and
;;   operate upon them simultaneously.

;;   It requires that Hyperbole be loaded in order to work.  Hyperbole
;;   defines two Smart Keys, the Action Key and the Assist Key, on the
;;   shift-middle and shift-right buttons by default.  Use (hmouse-install
;;   t) to add an additional Action Key Key on the middle mouse button.
;;
;;   TO INVOKE:
;;
;;       {C-u M-x hmouse-mod-mode RET} or in Lisp: (hmouse-mod-mode 1)
;;
;;   TO QUIT:
;;
;;       {C-u 0 M-x hmouse-mod-mode RET} or in Lisp: (hmouse-mod-mode 0)
;;
;;   TO TOGGLE ON AND OFF:
;;
;;       {M-x hmouse-mod-mode RET} or in Lisp: (hmouse-mod-mode 'toggle)

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hyperbole)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hmouse-mod--global-map nil
  "Global key map installed by `hmouse-mod-enable' function.
Translates self-insert-command characters into control and meta characters if
the Action or Assist Keys are depressed at the time of key press.")

(defvar hmouse-mod--prior-global-map nil
  "The global keymap prior to enabling of `hmouse-mod-mode'.
Restore it by calling (hmouse-mod-mode 0).")

(defvar hmouse-mod--prefix nil
  "Prefix key part of current key sequence.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(define-minor-mode hmouse-mod-mode
  "Toggle use of the Smart Keys as Control- and Meta- modifiers (Hmouse Modifier mode).
With a prefix argument ARG, enable Hmouse Mod mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If the Action Key is held down while alpha characters are typed,
they are translated into Control keys instead.  The Assist Key
translates them into Meta keys.  When both Smart Keys are depressed,
Control-Meta keys are produced.  The commands bound to the
characters produced are then run.

Hmouse Modifier mode is a global minor mode.  It does not affect
unmodified keys.  Normal Smart Key operations work with this
mode, if no other key is pressed while a Smart Key is depressed."
  :global t :group 'hyperbole-keys :lighter " HyMod"
  (if hmouse-mod-mode
      (progn (hmouse-mod-enable)
	     (if (called-interactively-p 'interactive)
		 (message "Action Key acts as Control- modifier; Assist Key acts as Meta- modifier.")))
    (hmouse-mod-disable)
    (if (called-interactively-p 'interactive)
	(message "Smart Keys no longer act as Control- and Meta- modifiers."))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hmouse-mod-disable ()
  "Restores the global keymap to its state prior to enabling of `hmouse-mod-mode'.
This stops the Smart Keys from acting as modifier keys."
  (use-global-map (if (keymapp hmouse-mod--prior-global-map)
		      hmouse-mod--prior-global-map
		    global-map)))

(defun hmouse-mod-enable ()
  "Creates `hmouse-mod--global-map' and installs it as the current global map.
It accounts for modifier Smart Keys."
  (error "(hmouse-mod-mode): Don't use this; it conflicts with Emacs mouse bindings.")

  (setq hmouse-mod--global-map (copy-keymap global-map))
  (substitute-key-definition
    'self-insert-command 'hmouse-mod-insert-command hmouse-mod--global-map)
  (substitute-key-definition
    'keyboard-quit 'hmouse-mod-keyboard-quit hmouse-mod--global-map)
  (unless (where-is-internal 'hmouse-mod-insert-command)
    (setq hmouse-mod--prior-global-map (current-global-map)))
  (use-global-map hmouse-mod--global-map))

(defun hmouse-mod-execute-command (key count)
  "Executes the command associated with keyboard KEY or if KEY is a prefix, records it.
Second argument COUNT is used as a prefix argument to the command."
  (if (stringp key) (setq key (concat hmouse-mod--prefix key)))
  (let ((binding (key-binding key))
	(current-prefix-arg count))
    (cond ((and (not (or (vectorp binding) (stringp binding)))
		(commandp binding))
	   (if (> (length key) 1)
	       (or noninteractive (message (key-description key))))
	   (setq hmouse-mod--prefix nil)
	   (call-interactively
	    (if (eq binding 'hmouse-mod-insert-command)
		#'self-insert-command
	      binding)))
	  ((symbolp binding)
	   (setq hmouse-mod--prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} not bound to a command."
		  key))
	  ((integerp binding)
	   (setq hmouse-mod--prefix nil)
	   (error "(hmouse-mod-execute-command): {%s} invalid key sequence."
		  (key-description (vector key))))
	  ((stringp key)
	   (or noninteractive (message (key-description key)))
	   (setq hmouse-mod--prefix key))
	  (t ;; Unrecognized key type, log an error message
	   (beep)
	   (message "(HyDebug): hmouse-mod-execute-command - `%s' invalid key" key)))))

(defun hmouse-mod-insert-command (count)
  "Surrogate function for `self-insert-command'.  Accounts for modifier Smart Keys."
  (interactive "p")
  (if (and (boundp 'action-key-depressed-flag)
	   (boundp 'assist-key-depressed-flag))
      (let ((modifiers (event-modifiers last-command-event))
	    (c (hmouse-mod-last-char)))
	(cond ((and c action-key-depressed-flag assist-key-depressed-flag)
	       (setq action-key-cancelled t
		     assist-key-cancelled t)
	       ;; Control-Meta keys
	       (hmouse-mod-execute-command
		(vector (list 'control 'meta c)) count))
	      ((and c action-key-depressed-flag)
	       (setq action-key-cancelled t)
	       ;; Emulate Control keys
	       (hmouse-mod-execute-command
		(vector (list 'control c)) count))
	      ((and c assist-key-depressed-flag)
	       (setq assist-key-cancelled t)
	       ;; Emulate Meta keys
	       (hmouse-mod-execute-command
		(vector (list 'meta c)) count))
	      ((null c))
	      (t (self-insert-command count))))
    (self-insert-command count))
  (discard-input)
  (setq this-command 'self-insert-command))

(defun hmouse-mod-keyboard-quit ()
  "Surrogate function for `keyboard-quit'.  Cancels any `hmouse-mod--prefix'."
  (interactive)
  (setq hmouse-mod--prefix nil)
  (keyboard-quit))

(defun hmouse-mod-last-char ()
  (cond	((boundp 'last-command-char) ;; XEmacs
	 (and (>= 0 last-command-char) (< last-command-char 128) last-command-char))
	((characterp last-command-event) ;; GNU Emacs
	 last-command-event)))

(provide 'hmouse-mod)

;;; hmouse-mod.el ends here
