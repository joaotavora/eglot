;;; hycontrol.el --- Interactive sizing, moving, replicating and deleting of windows and frames
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     1-Jun-16 at 15:35:36
;;
;; Copyright (C) 2016-2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This library provides full interactive control of window and
;;   frame sizes and locations utilizing quick single key commands.
;;   It has the ability to change from increasing a window height by
;;   5 lines, {.5 h}, to moving a frame 82 pixels, {.82 right-arrow},
;;   with just a few keystrokes (the leading . just resets the numeric
;;   argument to 0 prior to typing the new number).
;;
;;   ----
;;
;;   HyControl is invoked via either of two global minor modes under
;;   the Hyperbole screen menu, both of which can toggle to the other
;;   by pressing {t}. `hycontrol-enable-frames-mode' bound to {C-h h s
;;   f} manages visible frame creation, deletion, sizing, position and
;;   face zooming (enlarging and shrinking); if called interactively,
;;   it stores the current frame configuration for restoration via a
;;   press of the `)' key.  `hycontrol-enable-windows-mode' manages
;;   per frame window creation, deletion, sizing, reframing and face
;;   zooming; if called interactively, it stores the current window
;;   configuration for restoration via a press of the `)' key.
;;   `hycontrol-enable-windows-mode' is typically bound by Hyperbole
;;   to {C-c \} or just use {C-h h s w}.  Then press {t} if you want
;;   to switch to frame control.
;;
;;   With a HyControl minor mode active, a multi-line help summary of
;;   most available key bindings is shown in the minibuffer.  Simply
;;   read this and try each command out to get a feel for it.  Below
;;   we highlight some of the most unique commands.
;;
;;   ----
;;
;;   In either HyControl mode, you can instantly create a grid of
;;   windows to display many buffers by choosing a number of rows as
;;   your first digit, then a number of columns of windows as the
;;   second digit and then pressing {@}, e.g. {.26 @} produces 2 rows,
;;   each with 6 columns of windows in the selected frame.  Grids can
;;   be from 1x1 to 9x9 windows.  This command also works outside of a
;;   HyControl mode when in Dired, Buffer Menu or IBuffer modes with
;;   a prefix argument (no preceding period).
;;
;;   The buffers displayed by the {@} command are chosen smartly.
;;   With a current buffer in Dired, Buffer Menu or IBuffer mode with
;;   marked items, the buffers associated with those items are
;;   displayed first.  Then the most recently used buffers are
;;   displayed in each window, first selecting from buffers which
;;   match any of the predicate expressions in
;;   `hycontrol-display-buffer-predicate-list'.  Then, if there are
;;   not enough buffers for all windows, the buffers that failed to
;;   match to any predicate are used.  The default predicate list
;;   chooses buffers with attached files.  In all cases, buffers whose
;;   names start with a space are filtered out.  If a prefix argument
;;   of 0 is given, a major mode symbol is prompted for and buffers
;;   with that major mode are preferred for display instead of those
;;   matching the predicate list.
;;
;;   ----
;;
;;   HyControl allows placement of frames at screen edges and corners
;;   using the keys of the numeric keypad, matching their physical
;;   layout, e.g. {3} moves to the lower right corner.  Press {p} for
;;   a prompt with a virtual numeric keypad if you lack a physical one.
;;   You can also cycle through all of these placement positions with
;;   the {c} key.
;;
;;   HyControl can rapidly resize frames to common percentages of
;;   screen sizes via a number of commands.  Each press of {a} or {A}
;;   cycles through resizing the selected frame's width and height
;;   respectively to a percentage of the screen given by the lists,
;;   `hycontrol-frame-widths' and `hycontrol-frame-heights', e.g. 25%,
;;   50%, etc.  The keys: {i} top, {j} left, {k} right, and {m}
;;   bottom, first maximize a frame to the respective screen edge and
;;   then with successive presses, shrink the frame dimension
;;   perpendicular to that edge by 50% while keeping the original edge
;;   fixed in place.  Try them and you will quickly see how they can
;;   help.
;;   
;;   ----
;;
;;   When HyControl creates a new frame, it automatically sizes it to the
;;   same size as the previously selected frame and offsets it from that
;;   frame by the (X . Y) number of pixels given in the variable,
;;   `hycontrol-frame-offset'.
;;
;;   A display screen may span multiple physical monitors.  To prevent
;;   widgets and toolbars at the corners of the screen from being
;;   obscured, HyControl can offset each frame from each screen edge
;;   by a fixed number of pixels.  These offsets are specified by the
;;   variable, `hycontrol-screen-offset-alist' and can differ for each
;;   type of screen; see its documentation for details.  If you change
;;   its value, then call `hycontrol-set-screen-offsets' to set any
;;   new offset values.  `hycontrol-get-screen-offsets' returns the
;;   list of offsets in clockwise order starting from the top edge.
;;
;;   ----
;;
;;   Please note that the frame zoom in/out commands on Z and z will
;;   not work unless you have the separately available "zoom-frm.el"
;;   library (which itself requires another library).  If not available,
;;   this command will just beep at you.  The window-based zoom commands
;;   utilize a built-in Emacs library, so they will always work under
;;   any window system.  These commands enlarge and shrink the default
;;   text face.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'set)
;; Frame face enlarging/shrinking (zooming) requires this separately available library.
;; Everything else works fine without it, so don't make it a required dependency.
(require 'zoom-frm nil t)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hycontrol-debug nil
  "When set non-nil by a user, some HyControl log debugging messages to the *Messages* buffer.")

(defvar hycontrol-display-buffer-predicate-list
  ;; Display only buffers attached to files.
  (list #'buffer-file-name)
  "List of single buffer/name predicates.
If any predicate returns non-nil for a buffer, include that buffer in
the list to display in the windows created by `hycontrol-windows-grid-rows-columns'.

A predicate may be either a function that takes a single buffer
argument or a boolean expression, in which case the expression is
evaluated with the buffer argument as the current buffer, e.g. (eq
major-mode 'c-mode).")

(defcustom hycontrol-help-flag t
  "*When t (the default), display key binding help in the minibuffer when in a HyControl mode."
  :type 'boolean
  :group 'hyperbole-screen)

(defcustom hycontrol-invert-mode-line-flag t
  "When t (default) and in a HyControl mode, invert mode-line to emphasize the special key bindings in effect."
  :type 'boolean
  :group 'hyperbole-screen)

(defcustom hycontrol-keep-window-flag nil
  "*When non-nil (default is nil), leave original window when tear off window to another frame."
  :type 'boolean
  :group 'hyperbole-screen)

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
  '(((1920 . 1080) . (0 10 0 68)) ; 24" iMac HD display
    ((2560 . 1440) . (0 15 0 93)) ; 27" iMac HD display
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

(defvar hycontrol-frame-widths
  '(1.0 0.75 0.666 0.5 0.333 0.25)
    "List of frame width percentages that HyControl cycles through when adjusting a frame's width.
0.75 and 75 are treated as the same percentage.")

(defvar hycontrol-frame-heights
  '(1.0 0.75 0.666 0.5 0.333 0.25)
  "List of frame height percentages that HyControl cycles through when adjusting a frame's height.
0.75 and 75 are treated as the same percentage.")

(defvar hycontrol-arg nil
  "HyControl copy of prefix-arg that it changes within key bindings.
`pre-command-hook' synchronizes this value to `prefix-arg'.")

;;; Frame Keys

(defvar hycontrol-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map) ;; Disable self-inserting keys.

    (define-key map [up]    (lambda () (interactive) (hycontrol-move-frame 'up hycontrol-arg)))
    (define-key map [down]  (lambda () (interactive) (hycontrol-move-frame 'down hycontrol-arg)))
    (define-key map [left]  (lambda () (interactive) (hycontrol-move-frame 'left hycontrol-arg)))
    (define-key map [right] (lambda () (interactive) (hycontrol-move-frame 'right hycontrol-arg)))

    (define-key map [kp-0]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-0 hycontrol-arg)))
    (define-key map [kp-1]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-1 hycontrol-arg)))
    (define-key map [kp-2]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-2 hycontrol-arg)))
    (define-key map [kp-3]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-3 hycontrol-arg)))
    (define-key map [kp-4]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-4 hycontrol-arg)))
    (define-key map [kp-5]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-5 hycontrol-arg)))
    (define-key map [kp-6]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-6 hycontrol-arg)))
    (define-key map [kp-7]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-7 hycontrol-arg)))
    (define-key map [kp-8]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-8 hycontrol-arg)))
    (define-key map [kp-9]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-9 hycontrol-arg)))

    ;; Clear hycontrol-arg
    (define-key map "."     (lambda () (interactive) (setq hycontrol-arg 0) (hycontrol-frame-to-screen-edges 0)))
    (define-key map "@"     'hycontrol-windows-grid)
    (define-key map "?"     'hycontrol-toggle-help)
    (define-key map "a"     'hycontrol-frame-adjust-widths)
    (define-key map "A"     'hycontrol-frame-adjust-heights)
    (define-key map "b"     'bury-buffer)
    (define-key map "c"     'hycontrol-frame-to-screen-edges)
    (define-key map "d"     'delete-frame)
    (define-key map "D"     'hycontrol-delete-other-frames)
    (define-key map "f"     'hycontrol-clone-window-to-new-frame)
    (define-key map "F"     'hycontrol-window-to-new-frame)
    (define-key map "\C-g"  'hycontrol-abort-mode)
    (define-key map "%"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-percentage-of-screen hycontrol-arg))))
    (define-key map "H"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-height-percentage-of-screen hycontrol-arg))))
    (define-key map "W"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-width-percentage-of-screen hycontrol-arg))))
    (define-key map "h"     (lambda () (interactive) (hycontrol-set-frame-height nil (+ (frame-height) hycontrol-arg))))
    (define-key map "i"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-top hycontrol-arg))))
    (define-key map "j"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-left hycontrol-arg))))
    (define-key map "k"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-right hycontrol-arg))))
    (define-key map "l"     'lower-frame)
    (define-key map "m"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-bottom hycontrol-arg))))
    (define-key map "n"     (lambda () (interactive) (hycontrol-set-frame-width nil (- (frame-width) hycontrol-arg))))
    (define-key map "o"     (lambda () (interactive) (let ((w (selected-window))) (other-window hycontrol-arg) (if (eq w (selected-window)) (other-window 1)))))
    (define-key map "O"     (lambda () (interactive) (let ((w (selected-window))) (other-frame hycontrol-arg) (if (eq w (selected-window)) (other-frame 1)))))
    ;; Numeric keypad emulation for keyboards that lack one.
    (define-key map "p"     (lambda () (interactive) (hycontrol-virtual-numeric-keypad hycontrol-arg)))
    (define-key map "q"     'hycontrol-quit-frames-mode)
    (define-key map "r"     'raise-frame)
    (define-key map "s"     (lambda () (interactive) (hycontrol-set-frame-height nil (- (frame-height) hycontrol-arg))))
    (define-key map "t"     'hycontrol-enable-windows-mode)
    (define-key map "u"     'unbury-buffer)
    (define-key map "w"     (lambda () (interactive) (hycontrol-set-frame-width nil (+ (frame-width) hycontrol-arg))))
    (define-key map "Z"     (lambda () (interactive) (if (> hycontrol-arg 9) (setq hycontrol-arg 1)) (hycontrol-frame-zoom 'zoom-frm-in hycontrol-arg hycontrol-debug)))
    (define-key map "z"     (lambda () (interactive) (if (> hycontrol-arg 9) (setq hycontrol-arg 1)) (hycontrol-frame-zoom 'zoom-frm-out hycontrol-arg hycontrol-debug)))
    (define-key map "\["    'hycontrol-make-frame)
    (define-key map "\]"    'hycontrol-make-frame)
    (define-key map "\("    'hycontrol-save-frame-configuration)
    (define-key map "\)"    'hycontrol-restore-frame-configuration)
    ;; Something in this command's event handling when used within HyControl's event loop slows down
    ;; frame iconification under macOS 100-fold, so don't enable it until this issue is resolved.
    ;; (define-key map "^"    'iconify-frame)
    (define-key map "~"     (lambda () (interactive)
			      (or (hycontrol-frame-swap-buffers) (hycontrol-window-swap-buffers)
				  (hycontrol-user-error hycontrol-debug "(HyControl): There must be only two windows on screen to swap buffers."))))
    (define-key map "-"     'hycontrol-frame-minimize-lines)
    (define-key map "+"     'toggle-frame-maximized)
    (define-key map "="     (lambda () (interactive)
			      (and (> (length (visible-frame-list)) 1)
				   (y-or-n-p "Resize all other frames to the size of the selected frame?")
				   (mapc (lambda (f)
					   (hycontrol-set-frame-size
					    f (frame-pixel-width) (frame-pixel-height) t))
					 (visible-frame-list)))))

    (define-key map "\C-u"  (lambda () (interactive) (setq hycontrol-arg (* hycontrol-arg 4))
			      (if (> hycontrol-arg hycontrol-maximum-units) (setq hycontrol-arg 4))))
    (define-key map "0"     (lambda () (interactive) (hycontrol-universal-arg-digit 0)))
    (define-key map "1"     (lambda () (interactive) (hycontrol-universal-arg-digit 1)))
    (define-key map "2"     (lambda () (interactive) (hycontrol-universal-arg-digit 2)))
    (define-key map "3"     (lambda () (interactive) (hycontrol-universal-arg-digit 3)))
    (define-key map "4"     (lambda () (interactive) (hycontrol-universal-arg-digit 4)))
    (define-key map "5"     (lambda () (interactive) (hycontrol-universal-arg-digit 5)))
    (define-key map "6"     (lambda () (interactive) (hycontrol-universal-arg-digit 6)))
    (define-key map "7"     (lambda () (interactive) (hycontrol-universal-arg-digit 7)))
    (define-key map "8"     (lambda () (interactive) (hycontrol-universal-arg-digit 8)))
    (define-key map "9"     (lambda () (interactive) (hycontrol-universal-arg-digit 9)))

    map)
  "Keymap to use when in Hyperbole HyControl frames mode.")

;;; Window Keys

;;;###autoload
(eval-after-load "buff-menu" '(define-key Buffer-menu-mode-map "@" 'hycontrol-windows-grid))
;;;###autoload
(eval-after-load "ibuffer"   '(define-key ibuffer-mode-map     "@" 'hycontrol-windows-grid))
;;;###autoload
(eval-after-load "dired"     '(define-key dired-mode-map       "@" 'hycontrol-windows-grid))

(defvar hycontrol-windows-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map) ;; Disable self-inserting keys.

    (define-key map [up]    (lambda () (interactive) (hycontrol-move-frame 'up hycontrol-arg)))
    (define-key map [down]  (lambda () (interactive) (hycontrol-move-frame 'down hycontrol-arg)))
    (define-key map [left]  (lambda () (interactive) (hycontrol-move-frame 'left hycontrol-arg)))
    (define-key map [right] (lambda () (interactive) (hycontrol-move-frame 'right hycontrol-arg)))

    (define-key map [kp-0]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-0 hycontrol-arg)))
    (define-key map [kp-1]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-1 hycontrol-arg)))
    (define-key map [kp-2]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-2 hycontrol-arg)))
    (define-key map [kp-3]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-3 hycontrol-arg)))
    (define-key map [kp-4]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-4 hycontrol-arg)))
    (define-key map [kp-5]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-5 hycontrol-arg)))
    (define-key map [kp-6]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-6 hycontrol-arg)))
    (define-key map [kp-7]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-7 hycontrol-arg)))
    (define-key map [kp-8]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-8 hycontrol-arg)))
    (define-key map [kp-9]  (lambda () (interactive) (hycontrol-numeric-keypad 'kp-9 hycontrol-arg)))

    ;; Clear hycontrol-arg
    (define-key map "."     (lambda () (interactive) (setq hycontrol-arg 0) (hycontrol-frame-to-screen-edges 0)))
    (define-key map "@"     'hycontrol-windows-grid)
    (define-key map "?"     'hycontrol-toggle-help)
    (define-key map "a"     'hycontrol-frame-adjust-widths)
    (define-key map "A"     'hycontrol-frame-adjust-heights)
    (define-key map "b"     'bury-buffer)
    (define-key map "c"     'hycontrol-frame-to-screen-edges)
    (define-key map "d"     'delete-window)
    (define-key map "D"     'hycontrol-delete-other-windows)
    (define-key map "f"     'hycontrol-clone-window-to-new-frame)
    (define-key map "F"     'hycontrol-window-to-new-frame)
    (define-key map "\C-g"  'hycontrol-abort-mode)
    (define-key map "h"     (lambda () (interactive) (enlarge-window hycontrol-arg)))

    ;; Allow frame resizing even when in window control mode because
    ;; it may be used often.
    (define-key map "i"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-top hycontrol-arg))))
    (define-key map "j"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-left hycontrol-arg))))
    (define-key map "k"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-right hycontrol-arg))))
    (define-key map "m"     (lambda () (interactive) (setq hycontrol-arg (hycontrol-frame-resize-to-bottom hycontrol-arg))))
    (define-key map "n"     (lambda () (interactive) (shrink-window-horizontally hycontrol-arg)))

    (define-key map "o"     (lambda () (interactive) (let ((w (selected-window))) (other-window hycontrol-arg) (if (eq w (selected-window)) (other-window 1)))))
    (define-key map "O"     (lambda () (interactive) (let ((w (selected-window))) (other-frame hycontrol-arg) (if (eq w (selected-window)) (other-frame 1)))))
    ;; Numeric keypad emulation for keyboards that lack one.
    (define-key map "p"     (lambda () (interactive) (hycontrol-virtual-numeric-keypad hycontrol-arg)))
    (define-key map "q"     'hycontrol-quit-windows-mode)
    (define-key map "s"     (lambda () (interactive) (shrink-window hycontrol-arg)))
    (define-key map "t"     'hycontrol-enable-frames-mode)
    (define-key map "u"     'unbury-buffer)
    (define-key map "w"     (lambda () (interactive) (enlarge-window-horizontally hycontrol-arg)))
    (define-key map "Z"     (lambda () (interactive) (if (fboundp 'text-scale-increase)
							 ;; Emacs autoloaded function
							 (text-scale-increase (if (< hycontrol-arg 10) hycontrol-arg (setq hycontrol-arg 1))))))
    (define-key map "z"     (lambda () (interactive) (if (fboundp 'text-scale-decrease)
							 ;; Emacs autoloaded function
							 (text-scale-decrease (if (< hycontrol-arg 10) hycontrol-arg (setq hycontrol-arg 1))))))

    ;; Don't call these interactively because a prefix arg of 1 tries
    ;; to make one window 1 line tall.
    (define-key map "\["    (lambda () (interactive) (split-window-vertically)))
    (define-key map "\]"    (lambda () (interactive) (split-window-horizontally)))

    (define-key map "\("    'hycontrol-save-frame-configuration)
    (define-key map "\)"    'hycontrol-restore-frame-configuration)

    (define-key map "~"     (lambda () (interactive)
			      (or (hycontrol-window-swap-buffers) (hycontrol-frame-swap-buffers)
				  (hycontrol-user-error hycontrol-debug "(HyControl): There must be only two windows on screen to swap buffers."))))
    (define-key map "-"     'hycontrol-window-minimize-lines)
    (define-key map "+"     'hycontrol-window-maximize-lines)
    (define-key map "="     (lambda () (interactive) (and (> (length (window-list)) 1)
							  (y-or-n-p "Resize windows evenly across this frame?")
							  (balance-windows))))

    (define-key map "\C-u"  (lambda () (interactive) (setq hycontrol-arg (* hycontrol-arg 4))
			      (if (> hycontrol-arg hycontrol-maximum-units) (setq hycontrol-arg 4))))
    (define-key map "0"     (lambda () (interactive) (hycontrol-universal-arg-digit 0)))
    (define-key map "1"     (lambda () (interactive) (hycontrol-universal-arg-digit 1)))
    (define-key map "2"     (lambda () (interactive) (hycontrol-universal-arg-digit 2)))
    (define-key map "3"     (lambda () (interactive) (hycontrol-universal-arg-digit 3)))
    (define-key map "4"     (lambda () (interactive) (hycontrol-universal-arg-digit 4)))
    (define-key map "5"     (lambda () (interactive) (hycontrol-universal-arg-digit 5)))
    (define-key map "6"     (lambda () (interactive) (hycontrol-universal-arg-digit 6)))
    (define-key map "7"     (lambda () (interactive) (hycontrol-universal-arg-digit 7)))
    (define-key map "8"     (lambda () (interactive) (hycontrol-universal-arg-digit 8)))
    (define-key map "9"     (lambda () (interactive) (hycontrol-universal-arg-digit 9)))

    map)
  "Keymap to use when in Hyperbole HyControl window mode.")


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hycontrol--frames-prompt-format
 (concat "FRAME: (h=heighten, s=shorten, w=widen, n=narrow, %%/H/W=screen %%age, arrow=move frame) by %d unit%s, .=clear units\n"
	 ;; d/^/D=delete/iconify frame/others - iconify left out due to some bug on macOS (see comment near ^ below)
	 "a/A=cycle adjust width/height, d/D=delete frame/others, o/O=other win/frame, [/]=create frame, (/)=save/restore fconfig\n"
	 "@=row-col matrix of wins, f/F=clone/move win to new frame, -/+=minimize/maximize frame, ==frames same size, u/b/~=un/bury/swap bufs\n"
	 "Frame to edges: c=cycle, i/j/k/m=expand/contract, p/num-keypad=move; z/Z=zoom out/in, t=to WINDOW:, q=quit")
 "HyControl frames-mode minibuffer prompt string to pass to format.
Pass it with 2 arguments: prefix-arg and a plural string indicating if
prefix-arg is not equal to 1.")

(defvar hycontrol--windows-prompt-format
  (concat
   "WINDOW: (h=heighten, s=shorten, w=widen, n=narrow, arrow=move frame) by %d unit%s, .=clear units\n"
   "a/A=cycle adjust frame width/height, d/D=delete win/others, o/O=other win/frame, [/]=split win atop/sideways, (/)=save/restore wconfig\n"
   "@=row-col matrix of wins, f/F=clone/move win to new frame, -/+=minimize/maximize win, ==wins same size, u/b/~=un/bury/swap bufs\n"
   "Frame to edges: c=cycle, i/j/k/m=expand/contract, p/num-keypad=move; z/Z=zoom out/in, t=to FRAME:, q=quit")
  "HyControl windows-mode minibuffer prompt string to pass to format.
Pass it with 2 arguments: prefix-arg and a plural string indicating if
prefix-arg is not equal to 1.")

(defvar hycontrol--prompt-format nil
  "The current HyControl mode help format string or nil if not active.")

(defvar hycontrol--exit-status nil
  "Internal HyControl status indicator of how it was exited.
After exit, it should be one of the following symbols triggered by the
associated key: quit {q}, abort {C-g}, or toggle {t}.")


(defvar hycontrol--fconfig nil
  "Used to store a frame configuration while in hycontrol")

(defvar hycontrol--wconfig nil
  "Used to store a window configuration while in hycontrol")


(defvar hycontrol--invert-display-buffer-predicates nil)


(defvar hycontrol--quit-function nil
  "Stores function auto-generated by a call to `set-transient-map' to remove the transient-map later.")


(defvar hycontrol--screen-edge-position 0
  "Cycles between 0-7 representing corner and center edge positions in clockwise order from the upper left corner.")

(defvar hycontrol--frame-widths-pointer nil)
(defvar hycontrol--frame-heights-pointer nil)
(defvar hycontrol--initial-which-key-inhibit nil
  "Stores value of `which-key-inhibit' flag from \"which-key\" package upon entry to HyControl, if any.")

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

;;; HyControl private per keyboard key functions

(defun hycontrol-pre-command-hook ()
  "Added to `pre-command-hook' while in any HyControl mode."
  (when (null hycontrol-arg) (setq hycontrol-arg 1))
  (setq prefix-arg hycontrol-arg))

(defun hycontrol-post-command-hook ()
  "Added to `post-command-hook' while in HyControl frames mode."
  (when (null hycontrol-arg) (setq hycontrol-arg 1))
  (if (zerop (minibuffer-depth))
      (if hycontrol-help-flag
	  (let ((message-log-max nil) ; prevent logging of HyControl help messages
		(resize-mini-windows t)	 ; automatically shrink
		(use-dialog-box))	 ; prevent y-or-n dialog boxes
	    (message hycontrol--prompt-format hycontrol-arg (if (= hycontrol-arg 1) "" "s"))))
    ;; Quit from HyControl at any minibuffer prompt so that
    ;; self-insert-keys work for typing at the prompt.
    (hycontrol-disable-modes)))

(defun hycontrol-end-mode ()
  "Prepare to abort or quit from HyControl."
  (interactive)
  (remove-hook 'pre-command-hook  'hycontrol-pre-command-hook)
  (remove-hook 'post-command-hook 'hycontrol-post-command-hook)
  (if (boundp 'which-key-inhibit)
      (setq which-key-inhibit hycontrol--initial-which-key-inhibit))
  ;; May be called when turning on HyControl before this next function
  ;; is defined.
  (if (functionp hycontrol--quit-function)
      (funcall hycontrol--quit-function))
  (setq inhibit-quit nil
 	hycontrol--exit-status t
	hycontrol-arg 1
	prefix-arg nil
	hycontrol-debug nil
	hycontrol-frames-mode nil
	hycontrol-windows-mode nil
	hycontrol--prompt-format nil))

(defun hycontrol-stay-in-mode ()
  "Return non-nil if HyControl mode should remain active."
  (null hycontrol--exit-status))

(defun hycontrol-universal-arg-digit (digit)
  "Return the new prefix argument based on existing `hycontrol-arg' and new DIGIT."
  (setq hycontrol-arg (+ (* hycontrol-arg 10) digit)) (if (> hycontrol-arg hycontrol-maximum-units) (setq hycontrol-arg digit))
  hycontrol-arg)


;;; HyControl private initialization functions

(defun hycontrol-setup (arg setup-function)
  "HyControl initialization; passes through ARG and SETUP-FUNCTION.
SETUP-FUNCTION is HyControl mode-specific."
  ;; Save status value of which-key help package and quit any
  ;; in-progress which-key help without any user alert.
  (when (boundp 'which-key-inhibit)
    (setq hycontrol--initial-which-key-inhibit which-key-inhibit
	  which-key-inhibit t)
    (which-key--hide-popup-ignore-command))

  (setq arg (prefix-numeric-value arg)
	inhibit-quit t
	hycontrol--exit-status nil)
  (and hycontrol-debug (not (integerp hycontrol-debug)) (setq hycontrol-debug message-log-max))
  (if (called-interactively-p 'interactive) (hycontrol-save-configurations))
  (cond ((or (not (integerp arg)) (< arg 1))
	 (setq arg 1))
	((> arg hycontrol-maximum-units)
	 (setq arg hycontrol-maximum-units)))
  (setq hycontrol-arg arg
	prefix-arg arg)
  (hycontrol-invert-mode-line)
  (add-hook 'pre-command-hook  'hycontrol-pre-command-hook)
  (add-hook 'post-command-hook 'hycontrol-post-command-hook)
  (funcall setup-function))

(defun hycontrol-frames-setup ()
  "HyControl frames-specific initializations."
  (setq hycontrol--prompt-format hycontrol--frames-prompt-format)
  (hycontrol-post-command-hook)
  ;; Use normal event loop with transient-map until {C-g} or {q} is
  ;; pressed, then exit.
  (setq hycontrol--quit-function
	(set-transient-map hycontrol-frames-mode-map #'hycontrol-stay-in-mode)))

(defun hycontrol-frames (&optional arg)
  "Interactively delete, jump to, move, replicate, and resize frames.
With optional numeric prefix ARG, move and resize by ARG (an
integer) units.  If ARG is < 1, it is set to 1.  If it is >
`hycontrol-maximum-units', it is set to `hycontrol-maximum-units'."
  (interactive "p")
  (hycontrol-setup arg #'hycontrol-frames-setup)
  (unless hycontrol-help-flag
    (message "(HyControl) Frames global minor mode enabled; use {%s} for help"
	     (hycontrol-help-key-description))))

(defun hycontrol-windows-setup ()
  "HyControl windows-specific initializations."
  (setq hycontrol--prompt-format hycontrol--windows-prompt-format)
  (hycontrol-post-command-hook)
  ;; Use normal event loop with transient-map until {C-g} or {q} is
  ;; pressed, then exit.
  (setq hycontrol--quit-function
	(set-transient-map hycontrol-windows-mode-map #'hycontrol-stay-in-mode)))

(defun hycontrol-windows (&optional arg)
  "Interactively delete, jump to, rebalance, resize, and split windows.
With optional numeric prefix ARG, move and resize by ARG (an
integer) units.  If ARG is < 1, it is set to 1.  If it is >
`hycontrol-maximum-units', it is set to `hycontrol-maximum-units'."
  (interactive "p")
  (hycontrol-setup arg #'hycontrol-windows-setup)
  (unless hycontrol-help-flag
    (message "(HyControl) Windows global minor mode enabled; use {%s} for help"
	     (hycontrol-help-key-description))))


;;; HyControl general private functions

(defsubst hycontrol-frame-edges (&optional frame)
  "Return the outermost edge coordinates of optional or selected FRAME.
FRAME must be a live frame and defaults to the selected one.  The
list returned has the form (Left Top Right Bottom) where all
values are in pixels relative to the origin - the position (0, 0)
- of FRAME’s display.  For terminal frames all values are
relative to Left and Top which are both zero."
  (frame-edges frame 'outer-edges))

(defsubst hycontrol-frame-x-origin (&optional frame)
  "Return the X origin coordinate (upper left point) of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
  (nth 0 (hycontrol-frame-edges frame)))

(defsubst hycontrol-frame-y-origin (&optional frame)
  "Return the Y origin coordinate (upper left point) of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
  (nth 1 (hycontrol-frame-edges frame)))

(defun hycontrol-frame-height (&optional frame)
  "Return the height of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
  (frame-pixel-height frame))

(defun hycontrol-frame-width (&optional frame)
  "Return the width of optional FRAME or the selected frame.  This includes all graphical window manager decorations.
Under a graphical window system, this is in pixels; otherwise, it is in characters."
  (frame-pixel-width frame))

;; Frame Resizing Support
(defconst hycontrol-screen-offset-sensitivity 12
  "Number of pixels a frame dimension can be off from its screen-offset and still be considered at the screen edge.")

(defun hycontrol-frame-at-left-p ()
  "Return non-nil if selected frame's left edge is at the left edge of the screen sans `hycontrol-screen-left-offset'."
  (<= (- (nth 0 (hycontrol-frame-edges)) hycontrol-screen-left-offset)
      hycontrol-screen-offset-sensitivity))

(defun hycontrol-frame-at-top-p ()
  "Return non-nil if selected frame's bottom is at the top of the screen sans `hycontrol-screen-top-offset'."
  (<= (- (nth 1 (hycontrol-frame-edges)) hycontrol-screen-top-offset
	 ;; Under macOS, frames are automatically offset vertically by
	 ;; the height of the global menubar, so account for that.
	 (if (eq system-type 'darwin) 23 0))
      hycontrol-screen-offset-sensitivity))

(defun hycontrol-frame-at-right-p ()
  "Return non-nil if selected frame's right edge is at the right edge of the screen sans `hycontrol-screen-right-offset'."
  (<= (- (display-pixel-width) (nth 2 (hycontrol-frame-edges)) hycontrol-screen-right-offset)
      hycontrol-screen-offset-sensitivity))

(defun hycontrol-frame-at-bottom-p ()
  "Return non-nil if selected frame's bottom is at the bottom of the screen sans `hycontrol-screen-bottom-offset'."
  (<= (- (display-pixel-height) (nth 3 (hycontrol-frame-edges)) hycontrol-screen-bottom-offset
	 ;; Under macOS, frames are automatically offset vertically by
	 ;; the height of the global menubar, so account for that.
	 (if (eq system-type 'darwin) -23 0))
      hycontrol-screen-offset-sensitivity))

;; Frame Zoom Support
(defun hycontrol-frame-zoom (zoom-func arg max-msgs)
  "Zoom default frame face using ZOOM-FUNC and amount ARG (must be 1-9).
MAX-MSGS is a number used only if ZOOM-FUNC is undefined and an error message is logged."
  (if (fboundp zoom-func)
      (let ((frame-zoom-font-difference arg))
	(funcall zoom-func))
    (hycontrol-user-error max-msgs "(HyControl): Zooming requires separate \"zoom-frm.el\" Emacs Lisp library installation")))


(defun hycontrol-make-frame ()
  "Create and select a new frame with the same size and selected buffer as the selected frame.
It is offset from the selected frame by `hycontrol-frame-offset' (x . y) pixels."
  (interactive)
  (select-frame (make-frame (list (cons 'width (frame-width)) (cons 'height (frame-height))
				  (cons 'left (+ (car hycontrol-frame-offset) (car (frame-position))))
				  (cons 'top  (+ (cdr hycontrol-frame-offset) (cdr (frame-position))))))))

(defun hycontrol-move-frame (arrow pixels)
  (let ((x (car (frame-position)))
	(y (cdr (frame-position))))
    (pcase arrow
      ('up    (set-frame-position nil x (- y pixels)))
      ('down  (set-frame-position nil x (+ y pixels)))
      ('left  (set-frame-position nil (- x pixels) y))
      ('right (set-frame-position nil (+ x pixels) y)))))

(defun hycontrol-numeric-keypad (e _arg)
  "Move the selected frame to a screen location based on the location of the last pressed numeric keypad key."
  (let ((num (if (integerp e)
		 e
	       ;; kp-<num> symbol
	       (- (aref (symbol-name e) 3) ?0))))
    (funcall
     (nth num '(nil hycontrol-frame-to-bottom-left hycontrol-frame-to-bottom-center hycontrol-frame-to-bottom-right
		hycontrol-frame-to-left-center hycontrol-frame-to-center hycontrol-frame-to-right-center
		hycontrol-frame-to-top-left hycontrol-frame-to-top-center hycontrol-frame-to-top-right)))))

(defun hycontrol-set-frame-height (frame height &optional pretend pixelwise)
  "Set text height of frame FRAME to HEIGHT lines and fit it to the screen.
Optional third arg PRETEND non-nil means that redisplay should use
HEIGHT lines but that the idea of the actual height of the frame should
not be changed.

Optional fourth argument PIXELWISE non-nil means that FRAME should be
HEIGHT pixels high.  Note: When ‘frame-resize-pixelwise’ is nil, some
window managers may refuse to honor a HEIGHT that is not an integer
multiple of the default frame font height."
  (let ((frame-resize-pixelwise t))
    (set-frame-height frame height pretend pixelwise)
    (hycontrol-frame-fit-to-screen frame)))

(defun hycontrol-set-frame-position (frame x y)
  "Set position of FRAME to (X, Y) and ensure it fits on screen.
FRAME must be a live frame and defaults to the selected one.  X and Y,
if positive, specify the coordinate of the left and top edge of FRAME’s
outer frame in pixels relative to an origin (0, 0) of FRAME’s display.
If any of X or Y is negative, it specifies the coordinates of the right
or bottom edge of the outer frame of FRAME relative to the right or
bottom edge of FRAME’s display."
  (let ((frame-resize-pixelwise t))
    (hycontrol-frame-fit-to-screen frame)
    (set-frame-position frame x y)))

(defun hycontrol-set-frame-size (frame width height &optional pixelwise)
  "Set text size of FRAME to WIDTH by HEIGHT, measured in characters.
Ensure frame fits within the screen size.

Optional argument PIXELWISE non-nil means to measure in pixels.  Note:
When ‘frame-resize-pixelwise’ is nil, some window managers may refuse to
honor a WIDTH that is not an integer multiple of the default frame font
width or a HEIGHT that is not an integer multiple of the default frame
font height."
  (let ((x-origin (hycontrol-frame-x-origin))
	(y-origin (hycontrol-frame-y-origin))
	(frame-resize-pixelwise t))
    (set-frame-size frame width height pixelwise)
    (hycontrol-frame-fit-to-screen frame x-origin y-origin)))

(defun hycontrol-set-frame-width (frame width &optional pretend pixelwise)
  "Set text width of frame FRAME to WIDTH columns and fit it to the screen.
Optional third arg PRETEND non-nil means that redisplay should use WIDTH
columns but that the idea of the actual width of the frame should not
be changed.

Optional fourth argument PIXELWISE non-nil means that FRAME should be
WIDTH pixels wide.  Note: When ‘frame-resize-pixelwise’ is nil, some
window managers may refuse to honor a WIDTH that is not an integer
multiple of the default frame font width."
  (let ((x-origin (hycontrol-frame-x-origin))
	(y-origin (hycontrol-frame-y-origin))
	(frame-resize-pixelwise t))
    (set-frame-width frame width pretend pixelwise)
    (hycontrol-frame-fit-to-screen frame x-origin y-origin)))


(defun hycontrol-display-buffer-predicate-results (buffer)
  (condition-case err
      (mapcar (lambda (expr)
		(if (functionp expr)
		    (funcall expr buffer)
		  (with-current-buffer buffer
		    (eval expr))))
	      hycontrol-display-buffer-predicate-list)
    (error "(HyDebug): Invalid expression in `hycontrol-display-buffer-predicate-list' - %s" err)))

(defun hycontrol-window-display-buffer (window)
  "Given a WINDOW, choose the next appropriate buffer to display therein using `hycontrol-display-buffer-predicate-list'.
Also uses the value of free variable `buffer-list' as the list of
buffers to distribute among the windows."
  (let ((buf (car hycontrol--buffer-list-pointer)))
    (setq hycontrol--buffer-list-pointer (cdr hycontrol--buffer-list-pointer))
    (unless buf
      ;; Now on each new pass through buffer-list, the buffer predicate tests will
      ;; be inverted to expand the list of allowed buffers because the
      ;; 1st pass did not produce a buffer for this window.
      (setq hycontrol--buffer-list-pointer buffer-list
	    buf (car hycontrol--buffer-list-pointer)
	    hycontrol--buffer-list-pointer (cdr hycontrol--buffer-list-pointer))
      (unless (eq hycontrol--invert-display-buffer-predicates 'ignore)
	(setq hycontrol--invert-display-buffer-predicates (not hycontrol--invert-display-buffer-predicates))))
    (while (and buf (or (= (aref (buffer-name buf) 0) ?\ )
			(and (not hycontrol--invert-display-buffer-predicates)
			     (not (eval (cons 'or (hycontrol-display-buffer-predicate-results buf)))))
			(and hycontrol--invert-display-buffer-predicates
			     (not (eq hycontrol--invert-display-buffer-predicates 'ignore))
			     (eval (cons 'or (hycontrol-display-buffer-predicate-results buf))))))
      ;; Buffer is not one to display, get the next one and test again.
      (setq buf (car hycontrol--buffer-list-pointer)
	    hycontrol--buffer-list-pointer (cdr hycontrol--buffer-list-pointer)))
    (cond (buf
	   (set-window-buffer window buf))
	  (t
	   ;; Start 2nd or greater pass through buffer list with predicates inverted.
	   (hycontrol-window-display-buffer window)))))


(defun hycontrol-message (max-msgs &rest msg-args)
  "Log MAX-MSGS, adding MSG to the *Messages* buffer log."
  (let ((message-log-max max-msgs))
    (apply #'message msg-args)))

(defun hycontrol-user-error (max-msgs &rest err)
  "Log MAX-MSGS, adding ERR to the *Messages* buffer log; display ERR for 2 seconds."
  (let ((message-log-max max-msgs))
    (beep)
    (apply #'message err)
    (sit-for 2)))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; HyControl Global Minor Modes

;;;###autoload
(defun hycontrol-enable-frames-mode (&optional arg)
  "Globally enable HyControl Frames mode for rapid Emacs frame control.

  Interactively delete, jump to, move, replicate, and resize frames.
With optional numeric prefix ARG, move and resize by ARG (an
integer) units.  If ARG is < 1, it is set to 1.  If it is >
`hycontrol-maximum-units', it is set to `hycontrol-maximum-units'."
  (interactive "p")
  (hycontrol-disable-modes)
  (hycontrol-frames-mode (if (and (integerp arg) (>= arg 1)) arg 1)))

;;;###autoload
(defun hycontrol-enable-windows-mode (&optional arg)
  "Globally enable HyControl Windows mode for rapid Emacs window control.

Interactively delete, jump to, rebalance, resize, and split windows.
Optional non-negative numeric prefix ARG is used as the number of
units for commands issued while the mode is active.  If ARG is < 1, it
is set to 1.  If it is > `hycontrol-maximum-units', it is set to
`hycontrol-maximum-units'."
  (interactive "p")
  (hycontrol-disable-modes)
  (hycontrol-windows-mode (if (and (integerp arg) (>= arg 1)) arg 1)))

(defun hycontrol-disable-modes ()
  "Disable HyControl Frames and Windows modes when active."
  (interactive)
  (if (or hycontrol-frames-mode hycontrol-windows-mode)
      (hycontrol-invert-mode-line))
  (hycontrol-frames-mode -1)
  (hycontrol-windows-mode -1))

(defun hycontrol-abort-mode ()
  "Abort HyControl, typically on a press of {C-g}."
  (interactive)
  (hycontrol-disable-modes)
  (keyboard-quit))

(defun hycontrol-quit-frames-mode ()
  "Globally quit HyControl Frames mode, typically on a press of {q}."
  (interactive)
  (hycontrol-disable-modes)
  (message "Finished controlling frames"))

(defun hycontrol-quit-windows-mode ()
  "Globally quit HyControl Windows mode, typically on a press of {q}."
  (interactive)
  (hycontrol-disable-modes)
  (message "Finished controlling windows"))

;;;###autoload
(define-global-minor-mode hycontrol-frames-mode hycontrol-local-frames-mode
  (lambda () (hycontrol-local-frames-mode 1)))

;; These hooks run by the generated `hycontrol-frames-mode' function
;; do the global work of turning on and off the mode.
(add-hook 'hycontrol-frames-mode-on-hook
	  (lambda () (hycontrol-frames current-prefix-arg)))

(add-hook 'hycontrol-frames-mode-off-hook 'hycontrol-end-mode)

;; This just sets the keymap locally and shows the minor mode
;; indicator in the buffer's mode-line; the separate global minor mode
;; turns things on and off.
(define-minor-mode hycontrol-local-frames-mode
  "Toggle Hyperbole Frames control minor mode in the current buffer."
  nil " HyFrm" nil
  :group 'hyperbole-screen
  :global t)

;;;###autoload
(define-global-minor-mode hycontrol-windows-mode hycontrol-local-windows-mode
  (lambda () (hycontrol-local-windows-mode 1)))

;; These hooks run by the generated `hycontrol-windows-mode' function
;; do the global work of turning on and off the mode.
(add-hook 'hycontrol-windows-mode-on-hook
	  (lambda () (hycontrol-windows current-prefix-arg)))

(add-hook 'hycontrol-windows-mode-off-hook 'hycontrol-end-mode)

;; This just sets the keymap locally and shows the minor mode
;; indicator in the buffer's mode-line; the separate global minor mode
;; turns things on and off.
(define-minor-mode hycontrol-local-windows-mode
  "Toggle Hyperbole Windows control minor mode in the current buffer."
  nil " HyWin" nil
  :group 'hyperbole-screen
  :global t)


;;; Frame Display Commands
(defun hycontrol-delete-other-frames ()
  "Confirm and then delete all other frames."
  (interactive)
  (if (y-or-n-p "Delete all frames on this screen other than the selected one?")
      (delete-other-frames)))

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

(defconst hycontrol--vnk-string
  "(Virtual   7 8 9   Enter a digit to
 Numeric   4 5 6   move the frame
 Keypad)   1 2 3   to that quadrant"
  "HyControl prompt string for virtual numeric keypad (emulate keypad when not available)")

(defun hycontrol-virtual-numeric-keypad (arg)
  (catch 'quit
    (while (and (setq e (read-char hycontrol--vnk-string))
		(not (when (memq e '(?q ?\C-g)) (throw 'quit nil)))
		(or (not (numberp e)) (< e ?0) (> e ?9)))
      (beep))
    (hycontrol-numeric-keypad (- e ?0) arg)))

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
  (hycontrol-set-frame-position
   nil (car (frame-position))
   (- (display-pixel-height) (hycontrol-frame-height)
      hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-left ()
  "Move the selected frame to the left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position nil hycontrol-screen-left-offset (cdr (frame-position))))

(defun hycontrol-frame-to-right ()
  "Move the selected frame to the right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil (- (display-pixel-width) (hycontrol-frame-width)
	  hycontrol-screen-right-offset)
   (cdr (frame-position))))

(defun hycontrol-frame-fit-to-screen (&optional frame x-origin y-origin)
  "Ensure the selected frame fits within the screen, allowing for hycontrol-screen-*-offsets.
Accepts optional arguments FRAME, X-ORIGIN, and Y-ORIGIN (in pixels) to use when resizing FRAME (defaults to selected frame)."
  (let ((max-width (- (display-pixel-width) hycontrol-screen-left-offset hycontrol-screen-right-offset 2))
	(max-height (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset 2))
	(frame-resize-pixelwise t))
    (setq x-origin (or x-origin (hycontrol-frame-x-origin frame))
	  y-origin (or y-origin (hycontrol-frame-y-origin frame)))
    (when (> (hycontrol-frame-width frame) max-width)
      ;; Adjust frame size to fit within screen
      (set-frame-width frame (min (hycontrol-frame-width frame) max-width) nil t)
      (if hycontrol-debug (hycontrol-message hycontrol-debug "(HyDebug): "Screen (X,Y): %d, %d; Frame Edges (L,T,R,B): %s"
					      (display-pixel-width) (display-pixel-height) (hycontrol-frame-edges frame))))
    (when (> (hycontrol-frame-height frame) max-height)
      ;; Adjust frame size to fit within screen
      (set-frame-height frame (min (hycontrol-frame-height frame) max-height) nil t)
      (if hycontrol-debug (hycontrol-message hycontrol-debug "(HyDebug): "Screen (X,Y): %d, %d; Frame Edges (L,T,R,B): %s"
				   (display-pixel-width) (display-pixel-height) (hycontrol-frame-edges frame))))
    ;; Ensure entire frame is positioned onscreen, keeping the
    ;; original frame origin coordinates if possible.
    (set-frame-position frame
			(min (max 0 x-origin) 
			     (- (display-pixel-width) (hycontrol-frame-width frame) hycontrol-screen-right-offset))
			(min (max 0 y-origin)
			     (- (display-pixel-height) (hycontrol-frame-height frame) hycontrol-screen-bottom-offset)))
    (if hycontrol-debug (hycontrol-message hycontrol-debug "(HyDebug): "Screen (X,Y): %d, %d; Frame Edges (L,T,R,B): %s"
					    (display-pixel-width) (display-pixel-height) (hycontrol-frame-edges frame)))))


(defun hycontrol-frame-to-top ()
  "Move the selected frame to the top of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position nil (car (frame-position)) hycontrol-screen-top-offset))

(defun hycontrol-frame-to-bottom-center ()
  "Move the selected frame to the center of the bottom of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2))
   (- (display-pixel-height) (hycontrol-frame-height)
      hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-center ()
  "Move the selected frame to the center of the screen."
  (interactive)
  (hycontrol-set-frame-position
   nil
   (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2))
   (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))


(defun hycontrol-frame-to-left-center ()
  "Move the selected frame to the center of the left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil hycontrol-screen-left-offset (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))

(defun hycontrol-frame-to-right-center ()
  "Move the selected frame to the center of the right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil (- (display-pixel-width) (hycontrol-frame-width)
	  hycontrol-screen-right-offset)
   (round (/ (- (display-pixel-height) (hycontrol-frame-height)) 2))))

(defun hycontrol-frame-to-top-center ()
  "Move the selected frame to the center of the top of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position nil (round (/ (- (display-pixel-width) (hycontrol-frame-width)) 2)) hycontrol-screen-top-offset))

(defun hycontrol-frame-to-bottom-left ()
  "Move the selected frame to the bottom left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position nil
				hycontrol-screen-left-offset
				(- (display-pixel-height) (hycontrol-frame-height)
				   hycontrol-screen-bottom-offset)))

(defun hycontrol-frame-to-bottom-right ()
  "Move the selected frame to the bottom right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil
   (- (display-pixel-width) (hycontrol-frame-width) hycontrol-screen-right-offset)
   (- (display-pixel-height) (hycontrol-frame-height) hycontrol-screen-bottom-offset)))

;; Frame Resizing

(defun hycontrol-frame-to-top-left ()
  "Move the selected frame to the top left of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position nil hycontrol-screen-left-offset hycontrol-screen-top-offset))

(defun hycontrol-frame-to-top-right ()
  "Move the selected frame to the top right of the screen, allowing for hycontrol-screen-*-offsets."
  (interactive)
  (hycontrol-set-frame-position
   nil (- (display-pixel-width) (hycontrol-frame-width)
	  hycontrol-screen-right-offset)
   hycontrol-screen-top-offset))

;;; Frame Resizing Commands
(defun hycontrol-frame-resize-percentage (arg)
  "ARG should be between 0 and 100.  0 means don't resize (return 1).
1 is the default value which means cut the frame along the given dimension
in half (return 0.5).  2-100 is converted to a percentage to multiply by.
Over 100 is set to 100.  Under 0 is set to 0.  Floats between 0 and 1
are taken as percentages and used.  Other floats are rounded.
non-integer arguments are ignored and the default value is used."
  (cond ((numberp arg)
	 (cond 
	  ((= arg 0) 1)
	  ((= arg 1) 0.5)
	  ((and (> arg 1) (<= arg 100)) (/ arg 100.0))
	  ((< arg 0) 0)
	  ((> arg 100) 1)))
	(t (hycontrol-frame-resize-percentage 1))))

(defun hycontrol-frame-resize-arg (arg)
  "Inverse result of `hycontrol-frame-resize-percentage' to provide feedback on any argument value adjustment."
  (pcase arg
    (0 0)
    (1 1)
    (50 1)
    ;; Arg must be a percentage, scale it so not fractional.
    ((pred numberp) (round (* arg 100)))
    (_ 1)))

(defun hycontrol-frame-resize-to-bottom (&optional arg)
  "Expand the selected frame to the bottom of the screen, allowing for hycontrol-screen-*-offsets.
If already at the bottom, adjust its height to ARG percent of the screen (50% by default
if ARG is 1 or nil) but keep it at the bottom of the screen."
  (interactive "p")
  (setq arg (hycontrol-frame-resize-percentage arg))
  (let ((frame-resize-pixelwise t))
    (if (hycontrol-frame-at-bottom-p)
	;; Reduce frame height to ARG percent, keeping bottom side fixed.
	(set-frame-height nil (min (floor (* (frame-pixel-height) arg))
				   (hycontrol-frame-height))
			  nil t)
      ;; Expand frame height all the way to the bottom, keeping top side fixed.
      (set-frame-height nil (- (display-pixel-height) (cdr (frame-position)) hycontrol-screen-bottom-offset)
			nil t))
    (hycontrol-frame-to-bottom))
  (hycontrol-frame-resize-arg arg))

(defun hycontrol-frame-resize-to-left (&optional arg)
  "Expand the selected frame to the left of the screen, allowing for hycontrol-screen-*-offsets.
If already at the left, adjust its width to ARG percent of the screen (50% by default
if ARG is 1 or nil) but keep it at the left of the screen."
  (interactive "p")
  (setq arg (hycontrol-frame-resize-percentage arg))
  (let ((frame-resize-pixelwise t))
    (if (hycontrol-frame-at-left-p)
	;; Reduce frame width to ARG percent, keeping left side fixed.
	(set-frame-width nil (floor (* (frame-pixel-width) arg)) nil t)
      ;; Expand frame width all the way to the left, keeping right side fixed.
      (set-frame-width nil (floor (- (+ (hycontrol-frame-width) (car (frame-position)))
				     (* 2.5 (frame-scroll-bar-width))
				     hycontrol-screen-left-offset))
		       nil t))
    (hycontrol-frame-to-left))
  (hycontrol-frame-resize-arg arg))

(defun hycontrol-frame-resize-to-right (&optional arg)
  "Expand the selected frame to the right of the screen, allowing for hycontrol-screen-*-offsets.
If already at the right, adjust its width to ARG percent of the screen (50% by default
if ARG is 1 or nil) but keep it at the right of the screen."
  (interactive "p")
  (setq arg (hycontrol-frame-resize-percentage arg))
  (let ((frame-resize-pixelwise t))
    (if (hycontrol-frame-at-right-p)
	;; Reduce frame width to ARG percent, keeping right side fixed.
	(set-frame-width nil (floor (* (frame-pixel-width) arg)) nil t)
      ;; Expand frame width all the way to the right, keeping left side fixed.
      (set-frame-width nil (floor (- (display-pixel-width) (car (frame-position))
				     (* 2.5 (frame-scroll-bar-width))
				     hycontrol-screen-right-offset))
		       nil t))
    (hycontrol-frame-to-right))
  (hycontrol-frame-resize-arg arg))

(defun hycontrol-frame-resize-to-top (&optional arg)
  "Expand the selected frame to the top of the screen, allowing for hycontrol-screen-*-offsets.
If already at the top, adjust its height to ARG percent of the screen (50% by default
if ARG is 1 or nil) but keep it at the top of the screen."
  (interactive "p")
  (setq arg (hycontrol-frame-resize-percentage arg))
  (let ((frame-resize-pixelwise t)
	(top (nth 1 (hycontrol-frame-edges))))
    (if (hycontrol-frame-at-top-p)
	;; Reduce frame height to ARG percent, keeping top side fixed.
	(set-frame-height nil (floor (* (frame-pixel-height) arg)) nil t)
      ;; Expand frame height all the way to the top, keeping bottom side fixed.
      (set-frame-height nil (- (+ (cdr (frame-position)) (hycontrol-frame-height)) hycontrol-screen-top-offset)
			nil t))
    (hycontrol-frame-to-top))
  (hycontrol-frame-resize-arg arg))


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

(defun hycontrol-frame-percentage-of-screen (percent &optional dimension)
  "Resize the selected frame to be approximately PERCENT of the screen.
PERCENT may be given as a decimal percentage or a number between 0 and 100.
Optional DIMENSION if given must be either of the symbols, height or
width to affect only that dimension." 
  (interactive "nResize frame to be this percent of the screen (1-100): ")
  (if (and (numberp percent)
	   (progn
	     ;; Normalize to a fractional percentage
	     (when (> percent 1)
	       (setq percent (/ percent 100.0)))
	     (setq percent (max (min (float percent) 0.998) 0.0))
	     (> percent 0.0)))
      (let ((frame-resize-pixelwise t)
	    max-height
	    max-width)
	(cond ((eq dimension 'height)
	       (set-frame-height
		nil (min (floor (* (setq max-height (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset))
				   percent))
			 max-height)
		nil t))
	      ((eq dimension 'width)
	       (set-frame-width
		nil (min (floor (* (setq max-width (- (display-pixel-width)
						      (* 2.5 (frame-scroll-bar-width))
						      hycontrol-screen-left-offset hycontrol-screen-right-offset))
				   percent))
			 max-width)
		nil t))
	      (t (set-frame-size
		  nil (min (floor (* (setq max-width (- (display-pixel-width)
							(* 2.5 (frame-scroll-bar-width))
							hycontrol-screen-left-offset hycontrol-screen-right-offset))
				     percent))
			   max-width)
		  (min (floor (* (setq max-height (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset))
				 percent))
		       max-height)
		  t)))
	;; If resize has caused right or bottom edge to move
	;; offscreen, align these edges to the edge of the screen
	;; (moving the frame).
	(when (> (+ (hycontrol-frame-x-origin) (hycontrol-frame-width))
		 (- (display-pixel-width) hycontrol-screen-right-offset))
	  (hycontrol-frame-to-right))
	(when (> (+ (hycontrol-frame-y-origin) (hycontrol-frame-height))
		 (- (display-pixel-height) hycontrol-screen-bottom-offset))
	  (hycontrol-frame-to-bottom))
	;; Return the scaled percentage for setting as numeric argument.
	(floor (* percent 100)))
    (error "(hycontrol-frame-fraction-of-screen): `%s', must be a percent value above 0 and less than or equal to 100." percent)))

(defun hycontrol-frame-height-percentage-of-screen (percent)
  "Resize the selected frame's height to be approximately PERCENT of the screen."
  (interactive "nResize frame height to be this percent of the screen (1-100): ")
  (hycontrol-frame-percentage-of-screen percent 'height))

(defun hycontrol-frame-width-percentage-of-screen (percent)
  "Resize the selected frame's width to be approximately PERCENT of the screen."
  (interactive "nResize frame width to be this percent of the screen (1-100): ")
  (hycontrol-frame-percentage-of-screen percent 'width))

;;; Frame Cycle Common Sizes

(defun hycontrol-set-width-percentage-full-height (width-percentage)
  (hycontrol-frame-width-percentage-of-screen width-percentage)
  (hycontrol-frame-height-percentage-of-screen 1))

(defun hycontrol-set-height-percentage-full-width (height-percentage)
  (hycontrol-frame-width-percentage-of-screen 1)
  (hycontrol-frame-height-percentage-of-screen height-percentage))

;;;###autoload
(defun hycontrol-frame-adjust-widths ()
  "Cycle through different common width adjustments of a frame.
Widths are given in screen percentages by the list
`hycontrol-frame-widths' and typically go from widest to narrowest."
  (interactive)
  (when (null hycontrol--frame-widths-pointer)
    (setq hycontrol--frame-widths-pointer hycontrol-frame-widths))
  (hycontrol-frame-width-percentage-of-screen
   (car hycontrol--frame-widths-pointer))
  (message "Screen Percentage: Width %.1f%%; Fixed Height %.1f%%"
	   (* 100.0 (car hycontrol--frame-widths-pointer))
	   (* 100.0 (/ (float (hycontrol-frame-height))
		       (- (display-pixel-height) hycontrol-screen-top-offset hycontrol-screen-bottom-offset))))
  (setq hycontrol--frame-widths-pointer
	(cdr hycontrol--frame-widths-pointer)))

;;;###autoload
(defun hycontrol-frame-adjust-widths-full-height ()
  "Cycle through different common widths adjustments of a frame after fixing its height full-screen.
Widths are given in screen percentages by the list
`hycontrol-frame-widths' and typically go from widest to narrowest."
  (interactive)
  (when (null hycontrol--frame-widths-pointer)
    (setq hycontrol--frame-widths-pointer hycontrol-frame-widths))
  (hycontrol-set-width-percentage-full-height
   (car hycontrol--frame-widths-pointer))
  (message "Screen Percentage: Width %.1f%%; Fixed Height %d%%"
	   (* (car hycontrol--frame-widths-pointer) 100.0) 100)
  (setq hycontrol--frame-widths-pointer
	(cdr hycontrol--frame-widths-pointer)))

;;;###autoload
(defun hycontrol-frame-adjust-heights ()
  "Cycle through different common height adjustments of a frame.
Heights are given in screen percentages by the list
`hycontrol-frame-heights' and typically go from tallest to shortest."
  (interactive)
  (when (null hycontrol--frame-heights-pointer)
    (setq hycontrol--frame-heights-pointer hycontrol-frame-heights))
  (hycontrol-frame-height-percentage-of-screen
   (car hycontrol--frame-heights-pointer))
  (message "Screen Percentage: Fixed Width %.1f%%; Height %.1f%%"
	   (* 100.0 (/ (float (hycontrol-frame-width))
		       (- (display-pixel-width)
			  hycontrol-screen-left-offset hycontrol-screen-right-offset)))
	   (* 100.0 (car hycontrol--frame-heights-pointer)))
  (setq hycontrol--frame-heights-pointer
	(cdr hycontrol--frame-heights-pointer)))

;;;###autoload
(defun hycontrol-frame-adjust-heights-full-width ()
  "Cycle through different common height adjustments of a frame after fixing its width full-screen.
Heights are given in screen percentages by the list
`hycontrol-frame-heights' and typically go from tallest to shortest."
  (interactive)
  (when (null hycontrol--frame-heights-pointer)
    (setq hycontrol--frame-heights-pointer hycontrol-frame-heights))
  (hycontrol-set-height-percentage-full-width
   (car hycontrol--frame-heights-pointer))
  (message "Screen Percentage: Fixed Width %d%%; Height %.1f%%"
	   100 (* (car hycontrol--frame-heights-pointer) 100.0))
  (setq hycontrol--frame-heights-pointer
	(cdr hycontrol--frame-heights-pointer)))

;;; Frame Configuratons

(defun hycontrol-restore-frame-configuration ()
  (interactive)
  (when (and (y-or-n-p "Restore previously saved configuration of all frames?")
	     (frame-configuration-p hycontrol--fconfig))
    (set-frame-configuration hycontrol--fconfig)))

(defun hycontrol-save-frame-configuration ()
  (interactive)
  (setq hycontrol--fconfig (current-frame-configuration))
  (if (called-interactively-p 'interactive)
      (minibuffer-message "(Hyperbole): Saved configuration of all frames")))

(defun hycontrol-save-configurations ()
  (interactive)
  (hycontrol-save-frame-configuration)
  (hycontrol-save-window-configuration))


;;; Window Commands

(defun hycontrol-invert-mode-line ()
  "If `hycontrol-invert-mode-line-flag' is non-nil, invert the background and foreground faces of the selected window mode-line."
  (when hycontrol-invert-mode-line-flag
    (let* ((bg (face-background 'mode-line))
	   (fg (face-foreground 'mode-line)))
      (set-face-foreground 'mode-line bg)
      (set-face-background 'mode-line fg))
    (redraw-modeline t)))

(defun hycontrol-windows-grid-buffer-list ()
  "Return the existing frame's buffer list with any marked items prepended.
Marked items are included when the current buffer is in Dired, Buffer
Menu or IBuffer mode."
  ;; If selecting buffers by major-mode, then ignore any marked items.
  (if (and (boundp 'mode) (symbolp mode))
      (buffer-list (selected-frame))
    ;; Get the list of marked items if in an item list buffer and
    ;; convert items to buffers.
    (let ((items (cond ((and (eq major-mode 'dired-mode)
			     (mapcar #'find-file-noselect (dired-get-marked-files))))
		       ((and (eq major-mode 'Buffer-menu-mode)
			     (Buffer-menu-marked-buffers)))
		       ((and (eq major-mode 'ibuffer-mode)
			     (ibuffer-get-marked-buffers))))))
      ;; Ignore buffer list predicate filters when items exist so the
      ;; items are not filtered out.
      (setq hycontrol--invert-display-buffer-predicates
	    (when items 'ignore))
      ;; Prepend items to buffer list.
      (apply 'set:create (nconc items (buffer-list (selected-frame)))))))

;;;###autoload
(defun hycontrol-windows-grid (arg)
  "Create a grid of windows in the selected frame according to prefix ARG.

If ARG is 0, prompt for a major mode whose buffers should be
displayed first in the windows of the selected frame and then for
the number of rows and columns of windows to display in the grid.
Otherwise, split the selected frame into left digit of ARG rows
and right digit of ARG columns of windows.

With a current buffer in Dired, Buffer Menu or IBuffer mode that
contains marked items, the buffers associated with those items
are displayed first in the grid.  Then the most recently used
buffers are displayed in each window, first selecting only those
buffers which match any of the predicate expressions in
`hycontrol-display-buffer-predicate-list'.  (The default
predicate list chooses buffers with attached files).  Then, if
there are not enough buffers for all windows, the buffers that
failed to match to any predicate are used.  In all cases, buffers
whose names start with a space are ignored.

When done, resets the persistent prefix argument to 1 to prevent
following commands from using the often large grid size argument."
  (interactive "p")
  (setq arg (abs (prefix-numeric-value (or arg current-prefix-arg))))
  (if (/= arg 0)
      (hycontrol-windows-grid-rows-columns arg)
    (setq current-prefix-arg 0)
    (call-interactively #'hycontrol-windows-grid-by-major-mode)))

;;; Split selected frame into a matrix of windows given by row and
;;; column count, displaying different buffers in each window.
;;;###autoload
(defun hycontrol-windows-grid-by-major-mode (arg mode)
  "Split selected frame into left digit of ARG rows and right digit of ARG columns of windows, preferring buffers with major MODE.
Then, if not enough buffers for all windows, use the buffers that
failed to match in the first pass, aside from those whose names
begin with a space."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
	 (let* ((set:equal-op 'eq)
		(mode-strings (mapcar 'symbol-name (apply #'set:create (mapcar (lambda (buf) (buffer-local-value 'major-mode buf))
									       (hycontrol-windows-grid-buffer-list))))))
	   (intern-soft (completing-read "(HyControl Grid Windows): Major mode of buffers to display: "
					 mode-strings nil t (symbol-name major-mode))))))
  (let ((hycontrol-display-buffer-predicate-list `((eq major-mode ',mode))))
    (hycontrol-windows-grid-rows-columns arg)))

;;;###autoload
(defun hycontrol-windows-grid-repeatedly (&optional arg)
  "Repeatedly displays different window grid layouts according to prefix ARG prompted for each time.

See documentation of `hycontrol-windows-grid' for details."
  (interactive "p")
  (catch 'done
    (let (hycontrol-help-flag)
      (while t
	(while (not (or (eq arg 0) (and (integerp arg) (>= arg 11) (<= arg 99))))
	  (setq arg (read-string "Split frame into a matrix of ROW digit by COLUMN digit windows, e.g. 23 for 2R by 3C (RET to quit): "))
	  (setq arg (if (string-equal arg "")
			(throw 'done t)
		      (string-to-number arg)))
	  (unless (or (eq arg 0) (and (integerp arg) (>= arg 11) (<= arg 99)))
	    (beep)))
	(hycontrol-windows-grid arg)
	(setq arg nil)))))

(defun hycontrol-windows-grid-rows-columns (arg)
  "Split selected frame into left digit of ARG rows and right digit of ARG columns of windows.

See documentation of `hycontrol-windows-grid' for details."
  (interactive "p")

  ;; Check ARG, must be 2 digits of [1-9], else read a new ARG or
  ;; signal an error when in a HyControl mode and help is displayed.
  (if (and (and hycontrol-help-flag (or hycontrol-frames-mode hycontrol-windows-mode))
	   (not (and (integerp arg) (>= arg 11) (<= arg 99))))
      (let ((hyc-mode (if hycontrol-frames-mode #'hycontrol-frames-mode #'hycontrol-windows-mode)))
	(hycontrol-disable-modes)
	(setq arg 0)
	(while (not (and (integerp arg) (and (>= arg 11) (<= arg 99))))
	  (unless (eq arg 0) (beep))
	  (setq arg (read-number "Split frame into a matrix of ROW digit by COLUMN digit windows, e.g. 23 for 2R by 3C: ")))
	(funcall hyc-mode arg))
    (while (not (and (integerp arg) (and (>= arg 11) (<= arg 99))))
      (unless (eq arg 0) (beep))
      (setq arg (read-number "Split frame into a matrix of ROW digit by COLUMN digit windows, e.g. 23 for 2R by 3C: "))))

  (let ((wconfig (current-window-configuration)))
    ;; If an error occurs during a window split because the window is
    ;; too small, then restore prior window configuration.
    (condition-case err
	;; Make 1 window in selected frame
	(progn (delete-other-windows)

	       (let* ((rows (floor (/ arg 10)))
		      (columns (- arg (* rows 10)))
		      (row-index (1- rows))
		      (row-window-list (list (selected-window)))
		      col-index)

		 ;; Create ARG left-digit rows via split-windows,
		 ;; balancing each time.
		 (while (> row-index 0)
		   (setq row-window-list (cons (split-window-vertically) row-window-list))
		   (balance-windows)
		   (setq row-index (1- row-index)))

		 ;; Create ARG right-digit columns in each row via
		 ;; split-windows, balancing each time.
		 (setq row-index rows)
		 (while (> row-index 0)
		   (with-selected-window (car row-window-list)
		     (setq col-index (1- columns))
		     (while (> col-index 0)
		       (split-window-horizontally)
		       (balance-windows)
		       (setq col-index (1- col-index)))
		     (setq row-index (1- row-index)
			   row-window-list (cdr row-window-list)))))

	       ;; Walk windows in this frame and display different
	       ;; buffers.  In the first pass, select only buffers
	       ;; that pass at least one predicate test in
	       ;; `hycontrol-display-buffer-predicate-list'.  If run
	       ;; out of buffers before windows, then start a 2nd
	       ;; pass at the start of the buffer list and use the
	       ;; inverse, choosing only those buffers that fail all
	       ;; the predicate tests.  Always ignore buffers that
	       ;; start with a space.  With each succeeding pass, the
	       ;; predicate list is inverted again.
	       (let ((buffer-list (hycontrol-windows-grid-buffer-list)))
		 (setq hycontrol--buffer-list-pointer buffer-list)
  		 (walk-windows #'hycontrol-window-display-buffer 'no-minibuf))

	       ;; Prevent user from mistakenly using the typically
	       ;; large argument that invoked this function; reset it
	       ;; to 1 if there was no error.
	       (setq hycontrol-arg 1))
      (error (set-window-configuration wconfig)
	     (if (and hycontrol-help-flag (or hycontrol-frames-mode hycontrol-windows-mode))
		 (pop-to-buffer "*Messages*"))
	     (error "(HyDebug): %s" err)))))


(defun hycontrol-delete-other-windows ()
  "Confirm and then delete all other windows in the selected frame."
  (interactive)
  (if (y-or-n-p "Delete all windows in this frame other than the selected one?")
      (delete-other-windows)))

(defun hycontrol-window-maximize-lines ()
  "Grow window to its maximum possible number of lines without removing any windows."
  (interactive)
  (maximize-window))

(defun hycontrol-window-minimize-lines ()
  "Shrink window to its smallest possible number of lines to display entire buffer, if possible.
Otherwise or if the window is already displaying all of its lines, shrink it to about one line,
if possible."
  (interactive)
  (let ((neg-shrink-amount (- (1+ (count-lines (point-min) (point-max)))))
	(window-min-height 1))
    ;; Don't use minimize-window here since it shrinks regardless of
    ;; buffer size.
    (if (window-resizable-p (selected-window) neg-shrink-amount)
	(progn (goto-char (point-min))
	       (shrink-window (+ (window-height) neg-shrink-amount)))
      (shrink-window (1- (window-height))))))

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
;;;###autoload
(defun hycontrol-window-to-new-frame ()
  "Create a new frame sized to match the selected window and with its buffer.
If there is only one window in the source frame or if `hycontrol-keep-window-flag'
is non-nil, leave the original window and just clone it into the new frame."
  (interactive)
  (let ((w (selected-window))
	(frame-resize-pixelwise t)
	(only-one-window (one-window-p))
	buf)
    (cond ((window-minibuffer-p w)
	   (beep)
	   (minibuffer-message "(Hyperbole): Select a non-minibuffer window"))
	  (t
	   ;; Give temporary modes such as isearch a chance to turn off.
	   (run-hooks 'mouse-leave-buffer-hook)
	   (setq buf (window-buffer w))
	   (select-frame (make-frame (frame-parameters)))
	   (unless only-one-window
	     (hycontrol-set-frame-size nil (window-size w t t) (window-size w nil t) t))
	   (set-frame-position nil (+ (car hycontrol-frame-offset)
				      (car (frame-position (window-frame w))))
			       (+ (cdr hycontrol-frame-offset)
				  (cdr (frame-position (window-frame w)))))
	   (with-selected-frame (window-frame w)
	     (unless (or hycontrol-keep-window-flag (one-window-p t))
	       (delete-window w)))))))

;;;###autoload
(defun hycontrol-clone-window-to-new-frame ()
  "Create a new frame sized to match the selected window and with its buffer."
  (interactive)
  (let ((hycontrol-keep-window-flag t))
    (hycontrol-window-to-new-frame)))

(defun hycontrol-restore-window-configuration ()
  (interactive)
  (when (and (y-or-n-p "Restore saved window configuration in this frame?")
	     (window-configuration-p hycontrol--wconfig))
    (set-window-configuration hycontrol--wconfig)))

(defun hycontrol-save-window-configuration ()
  (interactive)
  (setq hycontrol--wconfig (current-window-configuration))
  (if (called-interactively-p 'interactive)
      (minibuffer-message "(Hyperbole): Saved window configuration for this frame")))

;;; Screen Offsets - Set once when this file is loaded; `hycontrol-set-screen-offsets' resets them.
(defun hycontrol-display-screen-offsets ()
  "Display a user minibuffer message listing HyControl's screen edge offsets in pixels."
  (interactive)
  (message "Screen pixel offsets are: Left: %d; Top: %d; Right: %d; Bot: %d"
	   hycontrol-screen-left-offset
	   hycontrol-screen-top-offset
	   hycontrol-screen-right-offset
	   hycontrol-screen-bottom-offset))

(defun hycontrol-get-screen-offsets ()
  "Return the first matching list of screen edge .50%%%offsets from `hycontrol-screen-offset-alist'.
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
    (setq hycontrol-screen-left-offset   (nth 0 offsets)
          hycontrol-screen-top-offset    (nth 1 offsets)
	  hycontrol-screen-right-offset  (nth 2 offsets)
	  hycontrol-screen-bottom-offset (nth 3 offsets))
    (if (called-interactively-p 'interactive) (hycontrol-display-screen-offsets))
    offsets))

(hycontrol-set-screen-offsets)

(defun hycontrol-help-key-description ()
  "Return the key description for the HyControl help key."
  (key-description (where-is-internal 'hycontrol-toggle-help hycontrol-frames-mode-map t)))

(defun hycontrol-toggle-help ()
  "Toggle whether HyControl displays key binding help in the minibuffer."
  (interactive)
  (setq hycontrol-help-flag (not hycontrol-help-flag))
  (unless (and hycontrol-help-flag (called-interactively-p))
    (message "(HyControl): Minibuffer help is off; use {%s} to turn it on"
	     (hycontrol-help-key-description))))

(provide 'hycontrol)

;;; hycontrol.el ends here
