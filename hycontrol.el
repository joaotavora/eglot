;;; hycontrol.el --- Interactive sizing, moving, replicating and deleting of windows and frames.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-Jun-16 at 15:35:36
;;
;; Copyright (C) 2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This library provides full interactive control of window and
;;   frame sizes and locations utilizing quick single key commands.
;;   It has the ability to change from increasing a window height by
;;   5 lines, {.5 h}, to moving a frame 820 pixels, {.820 right-arrow},
;;   with just a few keystrokes.
;;
;;   It supplies two commands, both of which can toggle to the other
;;   by pressing {t}.  `hycontrol-frames' manages visible frame
;;   creation, deletion, sizing, position and face zooming (enlarging
;;   and shrinking); if called interactively, it stores the current
;;   frame configuration for restoration via a press of the `)' key.
;;   `hycontrol-windows' manages per frame window creation, deletion,
;;   sizing, reframing and face zooming; if called interactively, it
;;   stores the current window configuration for restoration via a
;;   press of the `)' key.
;;
;;   These commands are available under the Hyperbole Screen menu 
;;   and `hycontrol-windows' is typically bound by Hyperbole to
;;   {C-c \}.  Then press {t} if you want to switch to frame control.
;;
;;   HyControl allows placement of frames at screen edges and corners. 
;;   (A screen may span multiple physical monitors).  To prevent widgets
;;   and toolbars at the corners of the screen from being obscured,
;;   HyControl can offset each frame from each screen edge by a fixed
;;   number of pixels.  These offsets are specified by the variable,
;;   `hycontrol-screen-offset-alist' and can differ for each type of
;;   screen; see its documentation for details.  If you change its value,
;;   then call `hycontrol-set-screen-offsets' to set any new offset values.
;;   `hycontrol-get-screen-offsets' returns the list of offsets in clockwise
;;   order starting from the top edge.
;;
;;   When HyControl creates a new frame, it automatically sizes it to the
;;   same size as the previously selected frame and offsets it from that
;;   frame by the (X . Y) number of pixels given in the variable,
;;   `hycontrol-frame-offset'.
;;
;;   Please note that the frame zoom in/out commands on Z and z will
;;   not work unless you have the separately available "zoom-frm.el"
;;   library (which itself requires another library).  If not available,
;;   they command will just beep at you.  The window-based zoom commands
;;   utilize a built-in Emacs library, so they will always work under
;;   any window system.  These commands enlarge and shrink the default
;;   text face.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; Frame face enlarging/shrinking (zooming) requires this separately available library.
;; Everything else works fine without it, so don't make it a required dependency.
(require 'zoom-frm nil t)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hycontrol-maximum-units 1000
  "*Maximum units setting allowed for hycontrol commands.
The unit counter resets to the last digit entered whenever this value is exceeded."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (> value 0)
				(<= value (max 1000 (display-pixel-width))))))
  :group 'hyperbole-screen)

(defcustom hycontrol-frame-offset '(13 . 23)
  "*Increase in pixel offset for new hycontrol frames relative to the selected frame.
It's value is an (x-offset . y-offset) pair in pixels."
  :type '(cons integer integer)
  :group 'hyperbole-screen)

(defvar hycontrol-screen-offset-alist
  '(((1920 . 1080) . (10 0 68 0)) ; 24" iMac / HD display
    (t . (0 0 0 0)))
  "*Alist of (screen-predicate . (top-offset right-offset bottom-offset left-offset) pairs.
Offsets are integers given in pixels.  The offsets associated with the first
matching screen-predicate are used in HyControl screen edge frame placement
commands; this is set when HyControl is first loaded/used.

Screen-predicate must be one of: a boolean function of no arguments, an
integer dotted pair of (width . height) in pixels to match to, or an Emacs
Lisp boolean form to evaluate.

The final predicate should always be t, for default values, typically of zero.")

(defcustom hycontrol-screen-top-offset    0
  "*Pixel offset from top used when placing a frame at a top corner."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (>= value 0)
				(< value (display-pixel-height)))))
  :group 'hyperbole-screen)
(defcustom hycontrol-screen-right-offset  0
  "*Pixel offset from right used when placing a frame at a right corner."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (>= value 0)
				(< value (display-pixel-width)))))
  :group 'hyperbole-screen)
(defcustom hycontrol-screen-bottom-offset 0
  "*Pixel offset from bottom used when placing a frame at a bottom corner."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (>= value 0)
				(< value (display-pixel-height)))))
  :group 'hyperbole-screen)
(defcustom hycontrol-screen-left-offset   0
  "*Pixel offset from left used when placing a frame at a left corner."
  :type '(integer :match (lambda (_widget value)
			   (and (integerp value) (>= value 0)
				(< value (display-pixel-width)))))
  :group 'hyperbole-screen)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hycontrol--fconfig nil
  "Used to store a frame configuration while in hycontrol")

(defvar hycontrol--wconfig nil
  "Used to store a window configuration while in hycontrol")

(defvar hycontrol--screen-edge-position 0
  "Cycles between 0-7 representing corner and center edge positions in clockwise order from the upper left corner.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hycontrol-frames (&optional arg debug)
  "Interactively delete, jump to, move, replicate, and resize frames.
With optional numeric prefix ARG, move and resize by ARG (an integer) units.
If ARG = -1 or optional DEBUG is non-nil, debugging is enabled and
unhandled events are logged to the *Messages* buffer.  If ARG is < 1, it is
set to 1.  If it is > `hycontrol-maximum-units', it is set to
`hycontrol-maximum-units'."
  (interactive "p")
  (if (eq arg -1) (setq debug t))
  (and debug (not (integerp debug)) (setq debug message-log-max))
  (if (called-interactively-p 'interactive) (hycontrol-save-configurations))
  (let ((message-log-max nil)
	(resize-mini-windows t) ;; automatically shrink
	(use-dialog-box) ;; prevent y-or-n dialog boxes
	e w)
    (if (catch 'done
	  (cond ((or (not (integerp arg)) (< arg 1))
		 (setq arg 1))
		((> arg hycontrol-maximum-units)
		 (setq arg hycontrol-maximum-units)))
	  (while t
	    (message
	     (concat
	      "FRAME: (h=heighten, s=shorten, w=widen, n=narrow, %%/H/W=screen %%age, arrow=move frame) by %d unit%s, .=clear units\n"
	      ;; "c=cycle to edges, keypad=to edges, d/^/D=delete/iconify frame/others, o/O=other win/frame, [/]=create frame, (/)=save/restore fconfig\n"
	      "c=cycle to edges, keypad=to edges, d/D=delete frame/others, o/O=other win/frame, [/]=create frame, (/)=save/restore fconfig\n"
	      "i/j/k/m=expand to edges, -=min frame, +=max frame, u/b/~=un/bury/swap bufs, Z/z=zoom in/out, t=to win control, q=quit")
	     ;; No room to include this binding in the message: ==frames same size
	     arg
	     (if (= arg 1) "" "s"))
	    (condition-case ()
		(progn
		  (setq e (read-event))
		  (cond
		   ((memq e '(up down left right))
		    (hycontrol-move-frame e arg))
		   ((memq e '(kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9))
		    (hycontrol-numeric-keypad e arg))
		   ((eq e ?.) (setq arg 0) (hycontrol-frame-to-screen-edges 0)) ;; Clear arg
		   ((eq e ?b) (bury-buffer))
		   ((eq e ?c) (hycontrol-frame-to-screen-edges))
		   ((eq e ?d) (delete-frame))
		   ((eq e ?D) (hycontrol-delete-other-frames))
		   ((eq e ?\C-g) (keyboard-quit))
		   ((eq e ?%) (hycontrol-frame-percentage-of-screen arg))
		   ((eq e ?H) (hycontrol-frame-height-percentage-of-screen arg))
		   ((eq e ?W) (hycontrol-frame-width-percentage-of-screen arg))
		   ((eq e ?h) (set-frame-height nil (+ (frame-height) arg)))
		   ((eq e ?i) (hycontrol-frame-expand-to-top))
		   ((eq e ?j) (hycontrol-frame-expand-to-left))
		   ((eq e ?k) (hycontrol-frame-expand-to-right))
		   ((eq e ?m) (hycontrol-frame-expand-to-bottom))
		   ((eq e ?n) (set-frame-width nil (- (frame-width) arg)))
		   ((eq e ?o) (setq w (selected-window)) (other-window arg) (if (eq w (selected-window)) (other-window 1)))
		   ((eq e ?O) (setq w (selected-window)) (other-frame arg) (if (eq w (selected-window)) (other-frame 1)))
		   ;; ((memq e (list ?q (aref (kbd "<escape>") 0))) (throw 'done t))
		   ((eq e ?q) (throw 'done t))
		   ((eq e ?s) (set-frame-height nil (- (frame-height) arg)))
		   ((eq e ?t) (throw 'done nil))
		   ((eq e ?u) (unbury-buffer))
		   ((eq e ?w) (set-frame-width nil (+ (frame-width) arg)))
		   ((eq e ?Z) (if (> arg 9) (setq arg 1)) (hycontrol-frame-zoom 'zoom-frm-in arg debug))
		   ((eq e ?z) (if (> arg 9) (setq arg 1)) (hycontrol-frame-zoom 'zoom-frm-out arg debug))
		   ((memq e '(?\[ ?\])) (hycontrol-make-frame))
		   ((eq e ?\() (call-interactively 'hycontrol-save-frame-configuration))
		   ((eq e ?\)) (hycontrol-restore-frame-configuration))
		   ;; Something in this command's event handling slows down
		   ;; frame iconification under Mac OS X 100-fold, so
		   ;; don't enable it until this issue is resolved.
		   ;; ((eq e ?^)  (iconify-frame))
		   ((eq e ?~) (or (hycontrol-frame-swap-buffers) (hycontrol-window-swap-buffers)
				  (hycontrol-user-error debug "(HyControl): There must be only two windows on screen to swap buffers.")))
		   ((eq e ?-) (hycontrol-frame-minimize-lines))
		   ((eq e ?+) (toggle-frame-maximized))
		   ((eq e ?=)
		    (and (> (length (visible-frame-list)) 1)
			 (y-or-n-p "Resize all other frames to the size of the selected frame?")
			 (mapc (lambda (f) (set-frame-size f (frame-width) (frame-height))) (visible-frame-list))))
		   ((eq e ?\C-u)
		    (setq arg (* arg 4))
		    (if (> arg hycontrol-maximum-units) (setq arg 4)))
		   ((and (integerp e) (>= e ?0) (<= e ?9))
		    (setq arg (+ (* arg 10) (- e ?0)))
		    (if (> arg hycontrol-maximum-units) (setq arg (- e ?0))))
		   ((hycontrol-handle-event e arg))
		   (t (beep)
		      (if debug (hycontrol-message debug "(HyDebug): Frame/window unhandled event - %s"
						   (hycontrol-prettify-event e)))))
		  (discard-input))
	      (error (if debug (hycontrol-message debug "(HyDebug): Frame/window unhandled event - %s"
						  (hycontrol-prettify-event e)))
		     (discard-input)
		     (beep)))))
	(message "Finished controlling frames")
      (hycontrol-windows arg debug))))

;;;###autoload
(defun hycontrol-windows (&optional arg debug)
  "Interactively delete, jump to, rebalance, resize, and split windows.
With optional numeric prefix ARG, move and resize by ARG (an integer) units.
If ARG = -1 or optional DEBUG is non-nil, debugging is enabled and
unhandled events are logged to the *Messages* buffer.  If ARG is < 1, it is
set to 1.  If it is > `hycontrol-maximum-units', it is set to
`hycontrol-maximum-units'."
  (interactive "p")
  (if (eq arg -1) (setq debug t))
  (and debug (not (integerp debug)) (setq debug message-log-max))
  (if (called-interactively-p 'interactive) (hycontrol-save-configurations))
  (let ((message-log-max nil)
	(resize-mini-windows t) ;; automatically shrink
	(use-dialog-box) ;; prevent y-or-n dialog boxes
	e w)
    (if (catch 'done
	  (cond ((or (not (integerp arg)) (< arg 1))
		 (setq arg 1))
		((> arg hycontrol-maximum-units)
		 (setq arg hycontrol-maximum-units)))
	  (while t
	    (message
	     (concat
	      "WINDOW: (h=heighten, s=shorten, w=widen, n=narrow, arrow=move frame) by %d unit%s, .=clear units\n"
	      "d/D=delete win/others, o/O=other win/frame, [/]=split win atop/sideways, (/)=save/restore wconfig\n"
	      "f=win to own frame, -=min win,+=max win, u/b/~=un/bury/swap bufs, Z/z=zoom in/out, t=to frame control, q=quit")
	     ;; No room to include this binding in the message: ==wins same size
	     arg
	     (if (= arg 1) "" "s"))
	    (condition-case ()
		(progn
		  (setq e (read-event))
		  (cond
		   ((memq e '(up down left right))
		    (hycontrol-move-frame e arg))
		   ((eq e ?.) (setq arg 0)) ;; Clear arg
		   ((eq e ?b) (bury-buffer))
		   ;; ((memq e '(?d ?^)) (delete-window))
		   ((eq e ?d) (delete-window))
		   ((eq e ?D) (hycontrol-delete-other-windows))
		   ((eq e ?f) (hycontrol-window-to-frame))
		   ((eq e ?\C-g) (keyboard-quit))
		   ((eq e ?h) (enlarge-window arg))
		   ((eq e ?n) (shrink-window-horizontally arg))
		   ((eq e ?o) (setq w (selected-window)) (other-window arg) (if (eq w (selected-window)) (other-window 1)))
		   ((eq e ?O) (setq w (selected-window)) (other-frame arg) (if (eq w (selected-window)) (other-frame 1)))
		   ;; ((memq e (list ?q (aref (kbd "<escape>") 0))) (throw 'done t))
		   ((eq e ?q) (throw 'done t))
		   ((eq e ?s) (shrink-window arg))
		   ((eq e ?t) (throw 'done nil))
		   ((eq e ?u) (unbury-buffer))
		   ((eq e ?w) (enlarge-window-horizontally arg))
		   ((and (eq e ?Z) (fboundp 'text-scale-increase))
		    ;; Emacs autoloaded function
		    (text-scale-increase (if (< arg 10) arg (setq arg 1))))
		   ((and (eq e ?z) (fboundp 'text-scale-decrease))
		    ;; Emacs autoloaded function
		    (text-scale-decrease (if (< arg 10) arg (setq arg 1))))
		   ((eq e ?\[) (split-window-vertically))
		   ((eq e ?\]) (split-window-horizontally))
		   ((eq e ?\() (call-interactively 'hycontrol-save-window-configuration))
		   ((eq e ?\)) (hycontrol-restore-window-configuration))
		   ((eq e ?~) (or (hycontrol-window-swap-buffers) (hycontrol-frame-swap-buffers)
				  (hycontrol-user-error debug "(HyControl): There must be only two windows on screen to swap buffers.")))
		   ((eq e ?-) (hycontrol-window-minimize-lines))
		   ((eq e ?+) (hycontrol-window-maximize-lines))
		   ((eq e ?=)
		    (and (> (length (window-list)) 1)
			 (y-or-n-p "Resize windows evenly across this frame?")
			 (balance-windows)))
		   ((eq e ?\C-u)
		    (setq arg (* arg 4))
		    (if (> arg hycontrol-maximum-units) (setq arg 4)))
		   ((and (integerp e) (>= e ?0) (<= e ?9))
		    (setq arg (+ (* arg 10) (- e ?0)))
		    (if (> arg hycontrol-maximum-units) (setq arg (- e ?0))))
		   ((hycontrol-handle-event e arg))
		   (t (beep)
		      (if debug (hycontrol-message debug "(HyDebug): Frame/window unhandled event - %s"
						   (hycontrol-prettify-event e)))))
		  (discard-input))
	      (error (if debug (hycontrol-message debug "(HyDebug): Frame/window unhandled event - %s"
						   (hycontrol-prettify-event e)))
		     (beep)
		     (discard-input)))))
	(message "Finished controlling windows")
      (hycontrol-frames arg debug))))

;;; Frame Display Commands
(defun hycontrol-frame-swap-buffers ()
  "Swap the buffers displayed by each of two frames and return t.
The selected frame may have multiple windows; the selected window is
used.  The second frame must have a single window only; otherwise, do
nothing and return nil."
  (interactive)
  (let ((frames (frame-list))
	frame2
	windows2
	buf1 buf2)
    (when (= 2 (length frames))
      (setq frame2 (if (eq (car frames) (selected-frame))
		       (cadr frames)
		     (car frames))
	    windows2 (window-list frame2 'no-mini))
      (when (= 1 (length windows2))
	(setq buf1 (window-buffer (selected-window))
	      buf2 (window-buffer (car windows2)))
	(set-window-buffer (selected-window) buf2)
	(set-window-buffer (car windows2) buf1)
	t))))

;;; Frame Relocation Commands

(defun hycontrol-frame-to-screen-edges (&optional arg)
  "Cycle the selected frame's position clockwise through the middle of edges and corners of the screen; once per call.
With an optional arg of 0, just reset the cycle position to 0."
  (interactive)
  (if (and arg (zerop arg))
      (setq hycontrol--screen-edge-position 0)
    (funcall
     (nth hycontrol--screen-edge-position
	  '(hycontrol-frame-to-top-left hycontrol-frame-to-top-center
            hycontrol-frame-to-top-right hycontrol-frame-to-right-center
	    hycontrol-frame-to-bottom-right hycontrol-frame-to-bottom-center
	    hycontrol-frame-to-bottom-left hycontrol-frame-to-left-center)))
    (setq hycontrol--screen-edge-position (1+ hycontrol--screen-edge-position))
    (if (> hycontrol--screen-edge-position 7)
	(setq hycontrol--screen-edge-position 0))))

(defun hycontrol-frame-to-bottom ()
  "Move the selected frame to the bottom of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (car (frame-position))
		      (- (display-pixel-height) (hycontrol-frame-height)
			 hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-left ()
  "Move the selected frame to the left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil hycontrol-screen-left-offset (cdr (frame-position))))

(defun hycontrol-frame-to-right ()
  "Move the selected frame to the right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (- (display-pixel-width) (hycontrol-frame-width)
			 hycontrol-screen-right-offset)
		      (cdr (frame-position))))


(defun hycontrol-frame-to-top ()
  "Move the selected frame to the top of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil (car (frame-position)) hycontrol-screen-top-offset))

(defun hycontrol-frame-to-bottom-center ()
  "Move the selected frame to the center of the bottom of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2))
		      (- (display-pixel-height) (hycontrol-frame-height)
			 hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-center ()
  "Move the selected frame to the center of the screen."
  (interactive)
  (set-frame-position nil
		      (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2))
		      (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))


(defun hycontrol-frame-to-left-center ()
  "Move the selected frame to the center of the left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil hycontrol-screen-left-offset (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))

(defun hycontrol-frame-to-right-center ()
  "Move the selected frame to the center of the right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (- (display-pixel-width) (hycontrol-frame-width)
			 hycontrol-screen-right-offset)
		      (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))

(defun hycontrol-frame-to-top-center ()
  "Move the selected frame to the center of the top of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2)) hycontrol-screen-top-offset))

(defun hycontrol-frame-to-bottom-left ()
  "Move the selected frame to the bottom left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      hycontrol-screen-left-offset
		      (- (display-pixel-height) (hycontrol-frame-height)
			 hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-bottom-right ()
  "Move the selected frame to the bottom right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (- (display-pixel-width) (hycontrol-frame-width)
			 hycontrol-screen-right-offset)
		      (- (display-pixel-height) (hycontrol-frame-height)
			 hycontrol-screen-bottom-offset)))

;; Frame Resizing

(defun hycontrol-frame-to-top-left ()
  "Move the selected frame to the top left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil hycontrol-screen-left-offset hycontrol-screen-top-offset))

(defun hycontrol-frame-to-top-right ()
  "Move the selected frame to the top right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (set-frame-position nil
		      (- (display-pixel-width) (hycontrol-frame-width)
			 hycontrol-screen-right-offset)
		      hycontrol-screen-top-offset))

;;; Frame Resizing Commands
(defun hycontrol-frame-expand-to-bottom ()
  "Expand the selected frame to the bottom of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (let ((frame-resize-pixelwise t))
    (set-frame-height nil (- (display-pixel-height) (cdr (frame-position)) hycontrol-screen-bottom-offset)
		      nil t)))

(defun hycontrol-frame-expand-to-left ()
  "Expand the selected frame to the left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (let ((frame-resize-pixelwise t))
    (set-frame-width nil (round (- (+ (hycontrol-frame-width) (car (frame-position)))
				   (* 2.5 (frame-scroll-bar-width))
				   hycontrol-screen-left-offset))
		     nil t)
    (set-frame-position nil hycontrol-screen-left-offset (cdr (frame-position)))))

(defun hycontrol-frame-expand-to-right ()
  "Expand the selected frame to the right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (let ((frame-resize-pixelwise t))
    (set-frame-width nil (round (- (display-pixel-width) (car (frame-position))
				   (* 2.5 (frame-scroll-bar-width))
				   hycontrol-screen-right-offset))
		     nil t)))
  
(defun hycontrol-frame-expand-to-top ()
  "Expand the selected frame to the top of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (let ((frame-resize-pixelwise t))
    (set-frame-height nil (- (+ (cdr (frame-position)) (hycontrol-frame-height)) hycontrol-screen-top-offset)
		      nil t)
    (set-frame-position nil (car (frame-position)) hycontrol-screen-top-offset)))

(defun hycontrol-frame-minimize-lines ()
  "Shrink the frame to its approximate smallest number of lines to display all existing windows."
  (interactive)
  (let ((l 0))
    (save-window-excursion
      (mapc (lambda (w)
	      (select-window w t)
	      (setq l (+ 2 l (min (window-height) (count-lines (point-min) (point-max))))))
	    (window-list nil 'nomini)))
    (set-frame-height nil l)))

(defun hycontrol-frame-height-percentage-of-screen (percent)
  "Resize the selected frame's height to be approximately PERCENT of the screen."
  (interactive "nResize frame height to be this percent of the screen (1-100): ")
  (hycontrol-frame-percentage-of-screen percent 'height))

(defun hycontrol-frame-percentage-of-screen (percent &optional dimension)
  "Resize the selected frame to be approximately PERCENT of the screen.
PERCENT may be given as a decimal percentage or a number between 0 and 100.
Optional DIMENSION if given must be either of the symbols, height or
width to affect only that dimension." 
  (interactive "nResize frame to be this percent of the screen (1-100): ")
  (and (numberp percent) (>= percent 1)
       (setq percent (/ percent 100.0)))
  (if (and (numberp percent) (> percent 0) (<= percent 1))
      (let ((frame-resize-pixelwise t))
	(cond ((eq dimension 'height)
	       (set-frame-height nil (round (* (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset)
					       percent)) nil t))
	      ((eq dimension 'width)
	       (set-frame-width nil (round (* (- (display-pixel-width)
						 (* 2.5 (frame-scroll-bar-width))
						 hycontrol-screen-left-offset hycontrol-screen-right-offset)
					      percent)) nil t))
	      (t (set-frame-size nil (round (* (- (display-pixel-width)
						  (* 2.5 (frame-scroll-bar-width))
						  hycontrol-screen-left-offset hycontrol-screen-right-offset)
					       percent))
				 (round (* (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset)
					   percent))
				 t))))
    (error "(hycontrol-frame-fraction-of-screen): `%s', must be a percent value between 0 and 100." percent)))

(defun hycontrol-frame-width-percentage-of-screen (percent)
  "Resize the selected frame's width to be approximately PERCENT of the screen."
  (interactive "nResize frame width to be this percent of the screen (1-100): ")
  (hycontrol-frame-percentage-of-screen percent 'width))

;;; Window Commands
(defun hycontrol-window-maximize-lines ()
  "Grow window to its maximum possible number of lines without removing any windows."
  (interactive)
  (maximize-window))

(defun hycontrol-window-minimize-lines ()
  "Shrink window to its smallest possible number of lines to display entire buffer, if possible.
Otherwise, do nothing."
  (interactive)
  (let ((neg-shrink-amount (- (1+ (count-lines (point-min) (point-max)))))
	(window-min-height 1))
    ;; Don't use minimize-window here since it shrinks regardless of
    ;; buffer size.
    (if (window-resizable-p (selected-window) neg-shrink-amount)
	(shrink-window (+ (window-height) neg-shrink-amount)))))

(defun hycontrol-window-swap-buffers ()
  "Swap the buffers displayed by each of two windows within the selected frame and return t.
Do nothing and return nil if there are not precisely two windows."
  (interactive)
  (let ((windows (window-list nil 'no-mini))
	buf1 buf2)
    (when (= 2 (length windows))
      (setq buf1 (window-buffer (car windows))
	    buf2 (window-buffer (cadr windows)))
      (set-window-buffer (car windows) buf2)
      (set-window-buffer (cadr windows) buf1)
      t)))

;; Derived from Emacs mouse.el.
(defun hycontrol-window-to-frame ()
  "Delete the selected window, and create a new frame displaying its buffer."
  (interactive)
  (let ((w (selected-window))
	buf)
    (cond ((window-minibuffer-p w)
	   (beep)
	   (minibuffer-message "(Hyperbole): Select a non-minibuffer window"))
	  ((one-window-p t)
	   (beep)
	   (minibuffer-message "(Hyperbole): Selected window is already in its own frame"))
	  (t
	   ;; Give temporary modes such as isearch a chance to turn off.
	   (run-hooks 'mouse-leave-buffer-hook)
	   (setq buf (window-buffer w))
	   (delete-window w)
	   (display-buffer-pop-up-frame buf nil)
	   (set-frame-position nil (+ (car hycontrol-frame-offset) (car (frame-position)))
			       (+ (cdr hycontrol-frame-offset) (cdr (frame-position))))))))

;;; Screen Offsets - Set once when this file is loaded; `hycontrol-set-screen-offsets' resets them.
(defun hycontrol-display-screen-offsets ()
  "Display a user minibuffer message listing HyControl's screen edge offsets in pixels."
  (interactive)
  (message "Screen pixel offsets are: Top: %d; Right: %d; Bot: %d; Left: %d"
	   hycontrol-screen-top-offset
	   hycontrol-screen-right-offset
	   hycontrol-screen-bottom-offset
	   hycontrol-screen-left-offset))

(defun hycontrol-get-screen-offsets ()
  "Return the first matching list of screen edge offsets from `hycontrol-screen-offset-alist'.
See its documentation for more information."
  (interactive)
  (prog1 (catch 'result
	   (let (predicate offsets width height)
	     (mapc (lambda (pred-offsets)
		     (setq predicate (car pred-offsets)
			   offsets (cdr pred-offsets))
		     (cond ((functionp predicate)
			    (if (funcall predicate) (throw 'result offsets)))
			   ;; (width . height)
			   ((and (consp predicate)
				 (integerp (car predicate))
				 (setq width (car predicate))
				 (or (and (integerp (cdr predicate))
					  (setq height (cdr predicate)))
				     ;; In case, user forgets the . in the cons.
				     (and (listp (cdr predicate))
					  (integerp (cadr predicate))
					  (setq height (cadr predicate)))))
			    (and (= width (display-pixel-width))
				 (= height (display-pixel-height))
				 (throw 'result offsets)))
			   ;; Emacs Lisp form
			   ((eval predicate)
			    (throw 'result offsets))))
		   hycontrol-screen-offset-alist))
	   (error "(HyDebug): No matching predicate in `hycontrol-screen-offset-alist' - %s"
		  hycontrol-screen-offset-alist))
    (if (called-interactively-p 'interactive) (hycontrol-display-screen-offsets))))

(defun hycontrol-set-screen-offsets ()
  "Set screen edge offsets to the first matching list of offsets from `hycontrol-screen-offset-alist'.
See its documentation for more information."
  (interactive)
  (let ((offsets (hycontrol-get-screen-offsets)))
    (setq hycontrol-screen-top-offset    (nth 0 offsets)
	  hycontrol-screen-right-offset  (nth 1 offsets)
	  hycontrol-screen-bottom-offset (nth 2 offsets)
	  hycontrol-screen-left-offset   (nth 3 offsets))
    (if (called-interactively-p 'interactive) (hycontrol-display-screen-offsets))
    offsets))

(hycontrol-set-screen-offsets)

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hycontrol-delete-other-frames ()
  "Confirm and then delete all other frames."
  (if (y-or-n-p "Delete all frames on this screen other than the selected one?")
      (delete-other-frames)))

(defun hycontrol-delete-other-windows ()
  "Confirm and then delete all other windows in the selected frame."
  (if (y-or-n-p "Delete all windows in this frame other than the selected one?")
      (delete-other-windows)))

(defun hycontrol-prettify-event (e)
  "Return a formatted version of event E ready for printing."
  (cond ((integerp e)
	 (key-description (vector e)))
	((sequencep e) 
	 (key-description e))
	(t e)))

(defun hycontrol-frame-height (&optional frame)
  "Return the height of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
    (- (nth 3 (frame-edges frame 'outer-edges))
       (nth 1 (frame-edges frame 'outer-edges))))

(defun hycontrol-frame-width (&optional frame)
  "Return the height of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
    (- (nth 2 (frame-edges frame 'outer-edges))
       (nth 0 (frame-edges frame 'outer-edges))))

;; Frame Zoom Support
(defun hycontrol-frame-zoom (zoom-func arg max-msgs)
  "Zoom default frame face using ZOOM-FUNC and amount ARG (must be 1-9).
MAX-MSGS is a number used only if ZOOM-FUNC is undefined and an error message is logged."
  (if (fboundp zoom-func)
      (let ((frame-zoom-font-difference arg))
	(funcall zoom-func))
    (hycontrol-user-error max-msgs "(HyControl): Zooming requires separate \"zoom-frm.el\" Emacs Lisp library installation")))

(defun hycontrol-handle-event (e arg)
  "Process input event E with prefix ARG set and return it or return nil if cannot process it."
  (setq current-prefix-arg arg)
  (if (eq e 'escape) (setq e 27)) ; Convert escape symbol to character code.
  (cond ((listp e)
	 (cond
	  ((eq (car e) 'delete-frame)
	   (handle-delete-frame e))
	  ((eq (car e) 'focus-in)
	   (handle-focus-in e))
	  ((eq (car e) 'focus-out)
	   (handle-focus-out e))
	  ((eq (car e) 'switch-frame)
	   (handle-switch-frame e))
	  ((or (eq (car e) 'select-window)
	       (and (memq (car e) '(down-mouse-1 mouse-1))
		    (window-live-p (posn-window (event-end e)))))
	   (handle-select-window e)
	   (mouse-set-point e))
	  (t (setq e nil))))
	((symbolp e)
	 (if (commandp (key-binding (vector e)))
	     (call-interactively (key-binding (vector e)))
	   (setq e nil)))
	;; Ignore self-insert-chars since many characters are used as
	;; HyControl commands.
	((integerp e)
	 (cond ((keymapp (key-binding (vector e)))
		;; Read a whole key sequence to get the binding
		(let ((keys (vector)))
		  (while (and (if (eq e 'escape) (setq e 27) t) ; Convert escape symbol to character code.
			      (setq keys (vconcat keys (vector e)))
			      (keymapp (key-binding (vector e))))
		    (setq e (read-key)))
		  (cond ((and (commandp (key-binding keys))
			      (not (memq (key-binding keys) '(self-insert-command))))
			 (call-interactively (key-binding keys)))
			(t (setq e nil)))
		  (discard-input)))
	       ((and (commandp (key-binding (vector e)))
		     (not (memq (key-binding (vector e)) '(self-insert-command))))
		(call-interactively (key-binding (vector e))))
	       (t (setq e nil))))
	(t (setq e nil)))
  e)

(defun hycontrol-make-frame ()
   "Create and select a new frame with the same size and selected buffer as the selected frame.
It is offset from the selected frame by `hycontrol-frame-offset' (x . y) pixels."
   (select-frame (make-frame (list (cons 'width (frame-width)) (cons 'height (frame-height))
				   (cons 'left (+ (car hycontrol-frame-offset) (car (frame-position))))
				   (cons 'top  (+ (cdr hycontrol-frame-offset) (cdr (frame-position))))))))

(defun hycontrol-move-frame (arrow pixels)
  (let ((x (car (frame-position)))
	(y (cdr (frame-position))))
  (cond ((eq arrow 'up)
	 (set-frame-position nil x (- y pixels)))
	((eq arrow 'down)
	 (set-frame-position nil x (+ y pixels)))
	((eq arrow 'left)
	 (set-frame-position nil (- x pixels) y))
	((eq arrow 'right)
	 (set-frame-position nil (+ x pixels) y)))))

(defun hycontrol-numeric-keypad (e _arg)
  (let ((num (- (aref (symbol-name e) 3) ?0)))
    (funcall
     (nth num '(nil hycontrol-frame-to-bottom-left hycontrol-frame-to-bottom-center hycontrol-frame-to-bottom-right
		hycontrol-frame-to-left-center hycontrol-frame-to-center hycontrol-frame-to-right-center
		hycontrol-frame-to-top-left hycontrol-frame-to-top-center hycontrol-frame-to-top-right)))))

(defun hycontrol-restore-frame-configuration ()
  (when (and (y-or-n-p "Restore previously saved configuration of all frames?")
	     (frame-configuration-p hycontrol--fconfig))
    (set-frame-configuration hycontrol--fconfig)))

(defun hycontrol-save-frame-configuration ()
  (interactive)
  (setq hycontrol--fconfig (current-frame-configuration))
  (if (called-interactively-p 'interactive)
      (minibuffer-message "(Hyperbole): Saved configuration of all frames")))

(defun hycontrol-restore-window-configuration ()
  (when (and (y-or-n-p "Restore saved window configuration in this frame?")
	     (window-configuration-p hycontrol--wconfig))
    (set-window-configuration hycontrol--wconfig)))

(defun hycontrol-save-window-configuration ()
  (interactive)
  (setq hycontrol--wconfig (current-window-configuration))
  (if (called-interactively-p 'interactive)
      (minibuffer-message "(Hyperbole): Saved window configuration for this frame")))

(defun hycontrol-save-configurations ()
  (hycontrol-save-frame-configuration)
  (hycontrol-save-window-configuration))

(defun hycontrol-message (max-msgs &rest msg)
  "Log MAX-MSGS, adding MSG to the *Messages* buffer log."
  (let ((message-log-max max-msgs))
    (apply #'message msg)))

(defun hycontrol-user-error (max-msgs &rest err)
  "Log MAX-MSGS, adding ERR to the *Messages* buffer log; display ERR for 2 seconds."
  (let ((message-log-max max-msgs))
    (beep)
    (apply #'message err)
    (sit-for 2)))

(provide 'hycontrol)

;;; hycontrol.el ends here
