;;; chess-display.el --- Code shared by all chess displays

;; Copyright (C) 2002, 2004, 2005, 2008, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains common code for all chessboard displays.

;;; Code:

(require 'chess-fen)
(require 'chess-input)
(require 'chess-message)
(require 'chess-module)
(require 'chess-random)
(require 'chess-var)

(defgroup chess-display nil
  "Options common to all chessboard displays."
  :group 'chess
  :link '(custom-manual "(chess)Chessboard displays"))

(defcustom chess-display-popup t
  "If non-nil (the default), popup displays whenever a significant event
occurs."
  :type 'boolean
  :group 'chess-display)

(make-variable-buffer-local 'chess-display-popup)

(defcustom chess-display-highlight-legal t
  "If non-nil, highlight legal target squares when a piece is selected."
  :type 'boolean
  :group 'chess-display)

(defcustom chess-display-highlight-last-move nil
  "If non-nil, highlight the last move made on the game."
  :type 'boolean
  :group 'chess-display)

(chess-message-catalog 'english
  '((mode-white     . "White")
    (mode-black     . "Black")
    (mode-start     . "START")
    (mode-checkmate . "CHECKMATE")
    (mode-aborted   . "ABORTED")
    (mode-resigned  . "RESIGNED")
    (mode-stalemate . "STALEMATE")
    (mode-flag-fell . "FLAG FELL")
    (mode-drawn     . "DRAWN")
    (mode-edit      . "EDIT")))

(defcustom chess-display-mode-line-format
  '("  " chess-display-side-to-move "  "
    chess-display-move-text "   "
    (:eval (chess-display-clock-string))
    "(" (:eval (chess-game-tag chess-module-game "White")) "-"
    (:eval (chess-game-tag chess-module-game "Black")) ", "
    (:eval (chess-game-tag chess-module-game "Site"))
    (:eval (let ((date (chess-game-tag chess-module-game "Date")))
	     (and (string-match "\\`\\([0-9]\\{4\\}\\)" date)
		  (concat " " (match-string 1 date))))) ")")
  "The format of a chess display's modeline.
See `mode-line-format' for syntax details."
  :type 'sexp
  :group 'chess-display)

(defface chess-display-black-face
  '((t (:background "Black" :foreground "White")))
  "*The face used for the word Black in the mode-line."
  :group 'chess-display)

(defface chess-display-white-face
  '((t (:background "White" :foreground "Black")))
  "*The face used for the word White in the mode-line."
  :group 'chess-display)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(defvar chess-display-index)
(defvar chess-display-move-text)
(defvar chess-display-side-to-move)
(defvar chess-display-perspective)
(defvar chess-display-event-handler nil)
(defvar chess-display-edit-mode nil)
(defvar chess-display-index-positions nil)

(make-variable-buffer-local 'chess-display-index)
(make-variable-buffer-local 'chess-display-move-text)
(make-variable-buffer-local 'chess-display-side-to-move)
(put 'chess-display-side-to-move 'risky-local-variable t)
(make-variable-buffer-local 'chess-display-perspective)
(make-variable-buffer-local 'chess-display-event-handler)
(make-variable-buffer-local 'chess-display-edit-mode)
(make-variable-buffer-local 'chess-display-index-positions)

(defvar chess-display-handling-event nil
  "If non-nil, chess-display is already handling the event.  This variable
is used to avoid reentrancy.")

(defvar chess-display-style)

(chess-message-catalog 'english
  '((no-such-style . "There is no such chessboard display style '%s'")
    (cannot-yet-add . "Cannot insert moves into a game (yet)")))

(defun chess-display-create (game style perspective)
  "Create a chess display, for displaying chess objects.
Where GAME is the chess game object to use, STYLE should be the display
type to use (a symbol) and PERSPECTIVE determines the viewpoint
of the board, if non-nil, the board is viewed from White's perspective."
  (interactive (list (if current-prefix-arg
			 (chess-game-create (chess-fen-to-pos
					     (read-string "FEN: ")))
		       (chess-game-create))
                     (intern-soft
                      (concat "chess-" (completing-read "Display style: "
							'(("ics1")
							  ("images")
							  ("plain")))))
                     (y-or-n-p "View from White's perspective? ")))
  (if (require style nil t)
      (let* ((chess-display-style style)
	     (display (chess-module-create 'chess-display game "*Chessboard*"
			     perspective)))
	(if (called-interactively-p 'any)
	    (progn
	      (chess-display-update display)
	      (chess-display-popup display))
	  display))))

(defalias 'chess-display-destroy 'chess-module-destroy)

(defun chess-display-clone (display style perspective)
  (let ((new-display (chess-display-create (chess-display-game display)
					   style perspective)))
    ;; the display will have already been updated by the `set-' calls,
    ;; it's just not visible yet
    (chess-display-popup new-display)
    new-display))

(defsubst chess-display-perspective (display)
  "Return the current perspective of DISPLAY."
  (chess-with-current-buffer display
    chess-display-perspective))

(defun chess-display-set-perspective* (display perspective)
  (chess-with-current-buffer display
    (setq chess-display-perspective perspective
	  chess-display-index-positions nil)
    (erase-buffer)))			; force a complete redraw

(defun chess-display-set-perspective (display perspective)
  "Set PERSPECTIVE of DISPLAY."
  (chess-with-current-buffer display
    (chess-display-set-perspective* nil perspective)
    (chess-display-update nil)))

(defun chess-display-set-position (display &optional position my-color)
  "Set the game associated with DISPLAY to use POSITION and MY-COLOR."
  (chess-with-current-buffer display
    (if position
	(progn
	  (chess-game-set-start-position chess-module-game position)
	  (chess-game-set-data chess-module-game 'my-color my-color))
      (chess-game-set-start-position chess-module-game
				     chess-starting-position)
      (chess-game-set-data chess-module-game 'my-color t))
    (chess-display-set-index nil 0)))

(defvar chess-display-edit-position nil)
(make-variable-buffer-local 'chess-display-edit-position)

(defun chess-display-position (display)
  "Return the position currently viewed on DISPLAY."
  (chess-with-current-buffer display
    (if chess-display-edit-mode
	chess-display-edit-position
      (chess-game-pos chess-module-game chess-display-index))))

(defun chess-display-set-ply (display ply)
  (chess-with-current-buffer display
    (let ((chess-game-inhibit-events t))
      (chess-display-set-index nil 1))
    (chess-game-set-plies chess-module-game
			  (list ply (chess-ply-create*
				     (chess-ply-next-pos ply))))))

(defun chess-display-ply (display)
  (chess-with-current-buffer display
    (chess-game-ply chess-module-game chess-display-index)))

(defun chess-display-set-variation (display variation &optional index)
  "Set DISPLAY VARIATION.
If INDEX is not specified, this will cause the first ply in the variation
to be displayed, with the user able to scroll back and forth through the
moves in the variation.  Any moves made on the board will extend/change the
variation that was passed in."
  (chess-with-current-buffer display
    (let ((chess-game-inhibit-events t))
      (chess-display-set-index nil (or index (chess-var-index variation))))
    (chess-game-set-plies chess-module-game variation)))

(defun chess-display-variation (display)
  (chess-with-current-buffer display
    (chess-game-main-var chess-module-game)))

(defun chess-display-set-game* (display game &optional index)
  "Set the game associated with the given DISPLAY."
  (chess-with-current-buffer display
    (chess-module-set-game* display game)
    (chess-display-set-index nil (or index (chess-game-index game)))))

(defun chess-display-set-game (display game &optional index)
  "Set the given DISPLAY to display the GAME object, optionally at INDEX.
This is the function to call to cause a display to view a game.  It
will also update all of the listening engines and other displays to
also view the same game."
  (chess-with-current-buffer display
    (chess-game-copy-game chess-module-game game)
    (chess-display-set-index nil (or index (chess-game-index game)))))

(defalias 'chess-display-game 'chess-module-game)

(defun chess-display-clock-string ()
  (let ((white (chess-game-data chess-module-game 'white-remaining))
	(black (chess-game-data chess-module-game 'black-remaining)))
    (unless (and white black)
      (let ((last-ply (chess-game-ply chess-module-game
				      (1- chess-display-index))))
	(setq white (chess-ply-keyword last-ply :white)
	      black (chess-ply-keyword last-ply :black))))
    (if (and white black)
	(format "W %s%02d:%02d B %s%02d:%02d   "
		(if (and (< white 0) (= 0 (floor white))) "-" "")
		(/ (floor white) 60) (% (abs (floor white)) 60)
		(if (and (< black 0) (= 0 (floor black))) "-" "")
		(/ (floor black) 60) (% (abs (floor black)) 60)))))

(defun chess-display-set-index (display index)
  (chess-with-current-buffer display
    (if (not (or (not (integerp index))
		 (< index 0)
		 (> index (chess-game-index chess-module-game))))
	(chess-game-run-hooks chess-module-game 'set-index index)
      (when (and (> index (chess-game-index chess-module-game))
		 (not (chess-ply-final-p (chess-game-ply chess-module-game))))
	(chess-game-run-hooks chess-module-game 'forward)))))

(defun chess-display-set-index* (display index)
  (chess-with-current-buffer display
    (setq chess-display-index index
	  chess-display-move-text
	  (if (= index 0)
	      (chess-string 'mode-start)
	    (concat (int-to-string (if (> index 1)
				       (if (= (mod index 2) 0)
					   (/ index 2)
					 (1+ (/ index 2)))
				     1))
		    "." (and (= 0 (mod index 2)) "..")
		    (chess-ply-to-algebraic
		     (chess-game-ply chess-module-game (1- index)))))
	  chess-display-side-to-move
	  (let ((status (chess-game-status chess-module-game index)))
	    (cond
	     ((eq status :aborted)   (chess-string 'mode-aborted))
	     ((eq status :resign)    (chess-string 'mode-resigned))
	     ((eq status :drawn)     (chess-string 'mode-drawn))
	     ((eq status :checkmate) (chess-string 'mode-checkmate))
	     ((eq status :stalemate) (chess-string 'mode-stalemate))
	     ((eq status :flag-fell) (chess-string 'mode-flag-fell))
	     (t
	      (let* ((color (or chess-pos-always-white
				(chess-game-side-to-move chess-module-game
							 index)))
		     (str (format " %s " (if color
					     (chess-string 'mode-white)
					   (chess-string 'mode-black)))))
		(add-text-properties 0 (length str)
				     (list 'face (if color
						     'chess-display-white-face
						   'chess-display-black-face))
				     str)
		str)))))
    (force-mode-line-update)))

(defsubst chess-display-index (display)
  (chess-with-current-buffer display
    chess-display-index))

(defun chess-display-update (display &optional popup)
  "Update the chessboard DISPLAY.  POPUP too, if that arg is non-nil."
  (chess-with-current-buffer display
    (funcall chess-display-event-handler 'draw
	     (chess-display-position nil) chess-display-perspective)
    (if (and popup chess-display-popup
	     (chess-module-leader-p nil))
	(chess-display-popup nil))))

(defun chess-display-redraw (&optional display)
  "Just redraw the current display."
  (interactive)
  (chess-with-current-buffer display
    (let ((here (point)))
      (erase-buffer)
      (chess-display-update nil)
      (goto-char here))))

(defun chess-display-index-pos (display index)
  (chess-with-current-buffer display
    (unless chess-display-index-positions
      (setq chess-display-index-positions (make-vector 64 nil))
      (let ((pos (next-single-property-change (point-min) 'chess-coord))
	    pos-index)
	(while pos
	  (if (setq pos-index (get-text-property pos 'chess-coord))
	      (aset chess-display-index-positions pos-index pos))
	  (setq pos (next-single-property-change pos 'chess-coord)))
	(unless (aref chess-display-index-positions 0)
	  (aset chess-display-index-positions 0
		(if chess-display-perspective
		    (point-min)
		  (1- (point-max)))))
	(unless (aref chess-display-index-positions 63)
	  (aset chess-display-index-positions 63
		(if chess-display-perspective
		    (1- (point-max))
		  (point-min))))))
    (aref chess-display-index-positions index)))

(defun chess-display-draw-square (display index &optional piece pos)
  "(Re)draw the square of DISPLAY indicated by INDEX.
Optional argument PIECE indicates the piece (or blank) to draw.
If it is not provided, `chess-display-position' is consulted.
Optional argument POS indicates the buffer position to draw the square at.
If that is not provided, `chess-display-index-pos' is called.

This function is especially useful to clear a previously set highlight."
  (cl-check-type display (or null buffer))
  (cl-check-type index (integer 0 63))
  (cl-check-type piece (member nil ?  ?P ?N ?B ?R ?Q ?K ?p ?n ?b ?r ?q ?k))
  (chess-with-current-buffer display
    (cl-check-type pos (or null (number ((point-min)) ((point-max)))))
    (funcall chess-display-event-handler 'draw-square
	     (or pos (chess-display-index-pos nil index))
	     (or piece (chess-pos-piece (chess-display-position nil) index))
	     index)))

(defun chess-display-paint-move (display ply)
  (cl-check-type display (or null buffer))
  (chess-with-current-buffer display
    (if chess-display-highlight-last-move
	(chess-display-redraw))
    (let ((position (chess-ply-pos ply))
	  (ch (chess-ply-changes ply)))
      (while ch
	(if (symbolp (car ch))
	    (setq ch nil)
	  (let ((from (car ch))
		(to (cadr ch)))
	    (chess-display-draw-square nil from ? )
	    (chess-display-draw-square
	     nil to (or (let ((new-piece (chess-ply-keyword ply :promote)))
			  (when new-piece
			    (if (chess-pos-side-to-move position)
				new-piece (downcase new-piece))))
			(chess-pos-piece position from)))
	    (when (chess-ply-keyword ply :en-passant)
	      (chess-display-draw-square nil (chess-pos-en-passant position) ? )))
	  (setq ch (cddr ch)))))
    (if chess-display-highlight-last-move
	(chess-display-highlight-move display ply))))

(chess-message-catalog 'english
  '((not-your-move . "It is not your turn to move")
    (game-is-over  . "This game is over")))

(defsubst chess-display-active-p ()
  "Return non-nil if the displayed chessboard reflects an active game.
Basically, it means we are playing, not editing or reviewing."
  (and (chess-game-data chess-module-game 'active)
       (= chess-display-index (chess-game-index chess-module-game))
       (not (chess-game-over-p chess-module-game))
       (not chess-display-edit-mode)))

(defun chess-display-move (display ply)
  "Move a piece on DISPLAY, by applying the given PLY.
The position of PLY must match the currently displayed position.

This adds PLY to the game associated with DISPLAY."
  (chess-with-current-buffer display
    (cond ((and (chess-display-active-p)
		;; `active' means we're playing against an engine
		(chess-game-data chess-module-game 'active)
		(not (eq (chess-game-data chess-module-game 'my-color)
			 (chess-game-side-to-move chess-module-game))))
	   (chess-error 'not-your-move))

	  ((and (= chess-display-index (chess-game-index chess-module-game))
		(chess-game-over-p chess-module-game))
	   (chess-error 'game-is-over))

	  ((= chess-display-index (chess-game-index chess-module-game))
	   (let ((chess-display-handling-event t))
	     (chess-game-move chess-module-game ply)
	     (chess-display-paint-move nil ply)
	     (chess-display-set-index* nil (chess-game-index chess-module-game))
	     (redisplay)                   ; FIXME: This is clearly necessary, but why?
	     (chess-game-run-hooks chess-module-game 'post-move)))

	  (t ;; jww (2002-03-28): This should beget a variation within the
	     ;; game, or alter the game, just as SCID allows
	     (chess-error 'cannot-yet-add)))))

(defun chess-display-highlight (display &rest args)
  "Highlight the square at INDEX on the current position.
The given highlighting MODE is used, or the default if the style you
are displaying with doesn't support that mode.  `selected' is a mode
that is supported by most displays, and is the default mode."
  (chess-with-current-buffer display
    (let ((mode :selected))
      (dolist (arg args)
	(if (or (symbolp arg) (stringp arg))
	    (setq mode arg)
	  (funcall chess-display-event-handler 'highlight arg mode))))))

(defun chess-display-highlight-legal (display index)
  "Highlight all legal move targets from INDEX."
  (chess-with-current-buffer display
    (dolist (ply (chess-legal-plies (chess-display-position nil) :index index))
      (chess-display-highlight nil "pale green"
			       (chess-ply-target ply)))))

(defun chess-display-highlight-move (display ply)
  "Highlight the last move made in the current game."
  (chess-display-highlight display "medium sea green"
			   (chess-ply-source ply)
			   (chess-ply-target ply)))

(defun chess-display-highlight-passed-pawns (&optional display)
  (interactive)
  (mapc
   (lambda (index) (chess-display-highlight display index :selected))
   (append
    (chess-pos-passed-pawns (chess-display-position display) t)
    (chess-pos-passed-pawns (chess-display-position display) nil))))

(defun chess-display-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (unless (eq (get-buffer-window (current-buffer))
		(selected-window))
      (funcall chess-display-event-handler 'popup))))

(defun chess-display-enable-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (setq chess-display-popup nil)))

(defun chess-display-disable-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (setq chess-display-popup t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default window and frame popup functions
;;

(defun chess-display-popup-in-window ()
  "Popup the given DISPLAY, so that it's visible to the user."
  (unless (get-buffer-window (current-buffer))
    (if (> (length (window-list)) 1)
	(fit-window-to-buffer (display-buffer (current-buffer)))
      (display-buffer (current-buffer)))))

(defun chess-display-popup-in-frame (height width font
				     &optional display no-minibuffer)
  "Popup the given DISPLAY, so that it's visible to the user."
  (let ((window (get-buffer-window (current-buffer) t)))
    (if window
	(let ((frame (window-frame window)))
	  (unless (eq frame (selected-frame))
	    (raise-frame frame)))
      (let ((params (list (cons 'name "*Chessboard*")
			  (cons 'height height)
			  (cons 'width width))))
	(if display
	    (push (cons 'display display) params))
	(if font
	    (push (cons 'font font) params))
	(if no-minibuffer
	    (push (cons 'minibuffer nil) params))
	(select-frame (make-frame params))
	(set-window-dedicated-p (selected-window) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Event handler
;;

(defcustom chess-display-interesting-events
  '(set-index)
  "Events which will cause a display refresh."
  :type '(repeat symbol)
  :group 'chess-display)

(defcustom chess-display-momentous-events
  '(orient post-undo setup-game pass move resign abort)
  "Events that will refresh, and cause 'main' displays to popup.
These are displays for which `chess-display-set-main' has been
called."
  :type '(repeat symbol)
  :group 'chess-display)

(defun chess-display-handler (game event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (unless chess-display-handling-event
    (if (eq event 'initialize)
	(progn
	  (chess-display-mode)
	  (setq chess-display-index (chess-game-index game)
		chess-display-side-to-move
		(if (chess-pos-side-to-move (chess-game-pos game))
		    (chess-string 'mode-white)
		  (chess-string 'mode-black))
		chess-display-move-text (chess-string 'mode-start)
		chess-display-perspective (car args)
		chess-display-event-handler
		(intern-soft (concat (symbol-name chess-display-style)
				     "-handler")))
	  (and chess-display-event-handler
	       (funcall chess-display-event-handler 'initialize)))
      (cond
       ((eq event 'pass)
	(let ((my-color (chess-game-data game 'my-color)))
	  (chess-game-set-data game 'my-color (not my-color))
	  (chess-display-set-perspective* nil (not my-color))))

       ((eq event 'set-index)
	(chess-display-set-index* nil (car args)))

       ((eq event 'orient)
	(let ((my-color (chess-game-data game 'my-color)))
	  ;; Set the display's perspective to whichever color I'm
	  ;; playing
	  (chess-display-set-perspective* nil my-color))))

      (if (memq event chess-display-momentous-events)
	  (progn
	    (chess-display-set-index* nil (chess-game-index game))
	    (if (eq event 'move)
		(progn
		  (chess-display-paint-move nil (car args))
		  (if chess-display-popup
		      (chess-display-popup nil)))
	      (chess-display-update nil chess-display-popup)))
	(if (memq event chess-display-interesting-events)
	    (chess-display-update nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chess-display-mode
;;

(defvar chess-display-safe-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map nil)

    (define-key map [(control ?i)] 'chess-display-invert)
    (define-key map [tab] 'chess-display-invert)

    (define-key map [??] 'describe-mode)
    (define-key map [?L] 'chess-display-list-buffers)
    ;;(define-key map [?C] 'chess-display-duplicate)
    (define-key map [?I] 'chess-display-invert)

    (define-key map [?<] 'chess-display-move-first)
    (define-key map [?,] 'chess-display-move-backward)
    (define-key map [(meta ?<)] 'chess-display-move-first)
    (define-key map [?>] 'chess-display-move-last)
    (define-key map [?.] 'chess-display-move-forward)
    (define-key map [(meta ?>)] 'chess-display-move-last)

    (define-key map [(meta ?w)] 'chess-display-kill-board)

    (define-key map [(control ?l)] 'chess-display-redraw)

    map)
  "The mode map used in read-only display buffers.")

(defvar chess-display-mode-map
  (let ((map (copy-keymap chess-display-safe-map)))
    (define-key map [space] 'chess-display-pass)
    (define-key map [? ] 'chess-display-pass)
    (define-key map [??] 'describe-mode)
    (define-key map [?@] 'chess-display-remote)
    (define-key map [?A] 'chess-display-manual-move)
    (define-key map [(control ?c) (control ?a)] 'chess-display-abort)
    (define-key map [?C] 'chess-display-duplicate)
    (define-key map [?D] 'chess-display-decline)
    (define-key map [(control ?c) (control ?c)] 'chess-display-force)
    (define-key map [(control ?c) (control ?d)] 'chess-display-draw)
    (define-key map [?E] 'chess-display-edit-board)
    (define-key map [?F] 'chess-display-set-from-fen)
    (define-key map [(control ?c) (control ?f)] 'chess-display-call-flag)
    (define-key map [?M] 'chess-display-match)
    (define-key map [(control ?c) (control ?r)] 'chess-display-resign)
    (define-key map [?R] 'chess-display-retract)
    (define-key map [?S] 'chess-display-shuffle)
    (define-key map [(control ?c) (control ?t)] 'chess-display-undo)
    (define-key map [?X] 'chess-display-quit)
    (define-key map [?Y] 'chess-display-accept)

    (define-key map [?\{] 'chess-display-annotate)
    (define-key map [?\"] 'chess-display-chat)
    (define-key map [?\'] 'chess-display-chat)
    (define-key map [?\~] 'chess-display-check-autosave)

    (define-key map [(control ?r)] 'chess-display-search-backward)
    (define-key map [(control ?s)] 'chess-display-search-forward)
    (define-key map [(control ?y)] 'chess-display-yank-board)

    (dolist (key '(?a ?b ?c ?d ?e ?f ?g ?h
		   ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8
		   ?r ?n ?b ?q ?k
		   ?R ?N ?B ?Q ?K
		   ?o ?O ?x ?=))
      (define-key map (vector key) 'chess-input-shortcut))
    (define-key map [backspace] 'chess-input-shortcut-delete)
    (define-key map "\d" 'chess-input-shortcut-delete)

    (define-key map [(control ?m)] 'chess-display-select-piece)
    (define-key map [return] 'chess-display-select-piece)
    (cond
     ((featurep 'xemacs)
      (define-key map [(button1)] 'chess-display-mouse-select-piece)
      (define-key map [(button2)] 'chess-display-mouse-select-piece)
      (define-key map [(button3)] 'ignore))
     (t
      (define-key map [down-mouse-1] 'chess-display-mouse-select-piece)
      (define-key map [drag-mouse-1] 'chess-display-mouse-select-piece)

      (define-key map [down-mouse-2] 'chess-display-mouse-select-piece)
      (define-key map [drag-mouse-2] 'chess-display-mouse-select-piece)

      (define-key map [mouse-3] 'ignore)))

    (define-key map [menu-bar files] 'undefined)
    (define-key map [menu-bar edit] 'undefined)
    (define-key map [menu-bar options] 'undefined)
    (define-key map [menu-bar buffer] 'undefined)
    (define-key map [menu-bar tools] 'undefined)
    (define-key map [menu-bar help-menu] 'undefined)

    map)
  "The mode map used in a chessboard display buffer.")

(defvar chess-display-move-menu nil)
(unless chess-display-move-menu
  (easy-menu-define
    chess-display-move-menu chess-display-mode-map ""
    '("History"
      ["First" chess-display-move-first t]
      ["Previous" chess-display-move-backward t]
      ["Next" chess-display-move-forward t]
      ["Last" chess-display-move-last t])))

(defun chess-display-mode ()
  "A mode for displaying and interacting with a chessboard.
The key bindings available in this mode are:
\\{chess-display-mode-map}"
  (interactive)
  (setq major-mode 'chess-display-mode
	mode-name "Chessboard")
  (use-local-map chess-display-mode-map)
  (buffer-disable-undo)
  (setq buffer-auto-save-file-name nil
	mode-line-format chess-display-mode-line-format)
  (setq chess-input-position-function
	(function
	 (lambda ()
	   (chess-display-position nil))))
  (setq chess-input-move-function 'chess-display-move))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands used by the keyboard bindings above
;;

(defun chess-display-invert ()
  "Invert the perspective of the current chess board."
  (interactive)
  (chess-display-set-perspective nil (not chess-display-perspective)))

(defun chess-display-set-from-fen (fen)
  "Send the current board configuration to the user."
  (interactive "sSet from FEN string: ")
  (chess-display-set-position nil (chess-fen-to-pos fen)))

(declare-function chess-game-to-pgn "chess-pgn" (game &optional indented to-string))

(defun chess-display-kill-board (&optional arg)
  "Send the current board configuration to the user."
  (interactive "P")
  (let ((x-select-enable-clipboard t)
	(game chess-module-game))
    (if arg
	(kill-new (with-temp-buffer
		    (chess-game-to-pgn game)
		    (buffer-string)))
      (kill-new (chess-pos-to-fen (chess-display-position nil) t)))))

(declare-function chess-pgn-to-game "chess-pgn" (&optional string))

(defun chess-display-yank-board ()
  "Send the current board configuration to the user."
  (interactive)
  (let ((x-select-enable-clipboard t)
	(display (current-buffer))
	(text (current-kill 0)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (while (and (bolp) (not (bobp)))
	(delete-char -1))
      (goto-char (point-min))
      (cond
       ((search-forward "[Event " nil t)
	(goto-char (match-beginning 0))
	(chess-game-copy-game chess-module-game (chess-pgn-to-game)))
       ((looking-at (concat chess-algebraic-regexp "$"))
	(let ((move (buffer-string)))
	  (with-current-buffer display
	    (chess-display-manual-move move))))
       (t
	(with-current-buffer display
	  (chess-display-set-from-fen (buffer-string))))))))

(defvar chess-display-search-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (dolist (key '(?a ?b ?c ?d ?e ?f ?g ?h
		      ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8
		      ?r ?n ?b ?q ?k
		      ?R ?N ?B ?Q ?K
		      ?o ?O ?x))
      (define-key map (vector key) 'chess-display-search-key))
    (define-key map [backspace] 'chess-display-search-delete)
    (define-key map [delete] 'chess-display-search-delete)
    (define-key map [(control ?h)] 'chess-display-search-delete)
    (define-key map [(control ?r)] 'chess-display-search-again)
    (define-key map [(control ?s)] 'chess-display-search-again)
    map))

(defvar chess-display-search-direction nil)
(defvar chess-current-display nil)
(defvar chess-display-previous-index nil)

(make-variable-buffer-local 'chess-display-previous-index)

(chess-message-catalog 'english
  '((san-not-found . "Could not find a matching move")))

(defun chess-display-search (&optional reset again)
  (interactive)
  (let ((str (concat "\\`" (minibuffer-contents)))
	limit index)
    (with-current-buffer chess-current-display
      (setq index (if reset
		      chess-display-previous-index
		    chess-display-index))
      (if again
	  (setq index (if chess-display-search-direction
			  (1+ index)
			(- index 2))))
      (catch 'found
	(while (if chess-display-search-direction
		   (< index (or limit
				(setq limit
				      (chess-game-index chess-module-game))))
		 (>= index 0))
	  (let* ((ply (chess-game-ply chess-module-game index))
		 (san (chess-ply-keyword ply :san))
		 (case-fold-search t))
	    (when (and san (string-match str san))
	      (chess-display-set-index nil (1+ index))
	      (throw 'found t)))
	  (setq index (funcall (if chess-display-search-direction '1+ '1-)
			       index)))
	(chess-error 'san-not-found)))))

(defun chess-display-search-again ()
  (interactive)
  (chess-display-search nil t))

(defun chess-display-search-key ()
  (interactive)
  (call-interactively 'self-insert-command)
  (chess-display-search))

(defun chess-display-search-delete ()
  (interactive)
  (call-interactively 'delete-backward-char)
  (chess-display-search t))

(defun chess-display-search-backward (&optional direction)
  (interactive)
  (setq chess-display-previous-index chess-display-index)
  (condition-case nil
      (let ((chess-display-search-direction direction)
	    (chess-current-display (current-buffer)))
	(read-from-minibuffer "Find algebraic move: " nil
			      chess-display-search-map))
    (quit
     (chess-display-set-index nil chess-display-previous-index))))

(defun chess-display-search-forward ()
  (interactive)
  (chess-display-search-backward t))

(chess-message-catalog 'english
  '((illegal-notation . "Illegal move notation: %s")
    (want-to-quit     . "Do you really want to quit? ")))

(defun chess-display-quit ()
  "Quit the game associated with the current display."
  (interactive)
  (if (or (not (chess-module-leader-p nil))
	  (yes-or-no-p (chess-string 'want-to-quit)))
      (chess-module-destroy nil)))

(defun chess-display-annotate ()
  (interactive)
  (chess-game-run-hooks chess-module-game 'switch-to-annotations))

(defun chess-display-chat ()
  (interactive)
  (chess-game-run-hooks chess-module-game 'switch-to-chat))

(defun chess-display-manual-move (move)
  "Move a piece manually, using chess notation."
  (interactive
   (list (read-string
	  (format "%s(%d): "
		  (if (chess-pos-side-to-move (chess-display-position nil))
		      "White" "Black")
		  (1+ (/ (or chess-display-index 0) 2))))))
  (let ((ply (chess-algebraic-to-ply (chess-display-position nil) move)))
    (unless ply
      (chess-error 'illegal-notation move))
    (chess-display-move nil ply)))

(defvar chess-images-separate-frame)

(defun chess-display-remote (display)
  (interactive "sDisplay this game on X server: ")
  (require 'chess-images)
  (let ((chess-images-separate-frame display))
    (chess-display-clone (current-buffer) 'chess-images
			 chess-display-perspective)))

(defun chess-display-duplicate (style)
  (interactive
   (list (concat "chess-"
		 (read-from-minibuffer "Create new display using style: "))))
  (chess-display-clone (current-buffer) (intern-soft style)
		       chess-display-perspective))

(defun chess-display-pass ()
  "Pass the move to your opponent.  Only valid on the first move."
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'pass)
    (ding)))

(defun chess-display-shuffle ()
  "Generate a shuffled opening position."
  (interactive)
  (require 'chess-random)
  (if (and (chess-display-active-p)
	   (= 0 chess-display-index))
      (chess-game-set-start-position chess-module-game
				     (chess-fischer-random-position))
    (ding)))

(defun chess-display-match ()
  "Request a match with any listening engine."
  (interactive)
  (chess-game-run-hooks chess-module-game 'match))

(defun chess-display-accept ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'accept)
    (ding)))

(defun chess-display-decline ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'decline)
    (ding)))

(defun chess-display-retract ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'retract)
    (ding)))

(defun chess-display-call-flag ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'call-flag)
    (ding)))

(defun chess-display-force ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'force)
    (ding)))

(defun chess-display-check-autosave ()
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'check-autosave)
    (ding)))

(defun chess-display-resign ()
  "Resign the current game."
  (interactive)
  (if (chess-display-active-p)
      (chess-game-end chess-module-game :resign)
    (ding)))

(defun chess-display-abort ()
  "Abort the current game."
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-module-game 'abort)
    (ding)))

(chess-message-catalog 'english
  '((draw-offer . "You offer a draw")))

(defun chess-display-draw ()
  "Offer to draw the current game."
  (interactive)
  (if (chess-display-active-p)
      (progn
	(chess-message 'draw-offer)
	(chess-game-run-hooks chess-module-game 'draw))
    (ding)))

(defun chess-display-undo (count)
  "Abort the current game."
  (interactive "P")
  (if (chess-display-active-p)
      (progn
	;; we can't call `chess-game-undo' directly, because not all
	;; engines will accept it right away!  So we just signal the
	;; desire to undo
	(setq count
	      (if count
		  (prefix-numeric-value count)
		(if (eq (chess-pos-side-to-move (chess-display-position nil))
			(chess-game-data chess-module-game 'my-color))
		    2 1)))
	(chess-game-run-hooks chess-module-game 'undo count))
    (ding)))

(defun chess-display-list-buffers ()
  "List all buffders related to this display's current game."
  (interactive)
  (let ((chess-game chess-module-game)
        (lb-command (lookup-key ctl-x-map [(control ?b)])))
    ;; FIXME: Running "whatever code is bound to `C-x b'" (which could really
    ;; be anything, if the user is using a completely different key layout, as
    ;; in Evil, ErgoEmacs, or whatnot) while rebinding buffer-list is
    ;; pretty risky!
    (cl-letf (((symbol-function 'buffer-list)
               (lambda (&optional _frame)
                 (delq nil
                       (mapcar (function
                                (lambda (cell)
                                  (and (bufferp (cdr cell))
                                       (buffer-live-p (cdr cell))
                                       (cdr cell))))
                               (chess-game-hooks chess-game))))))
      (call-interactively lb-command))))

(chess-message-catalog 'english
  '((return-to-current . "Use '>' to return to the current position")))

(defun chess-display-set-current (dir)
  "Change the currently displayed board.
Direction may be - or +, to move forward or back, or t or nil to jump
to the end or beginning."
  (let ((index (cond ((eq dir ?-) (1- chess-display-index))
		     ((eq dir ?+) (1+ chess-display-index))
		     ((eq dir t) nil)
		     ((eq dir nil) 0))))
    (chess-display-set-index
     nil (or index (chess-game-index chess-module-game)))
    (unless (chess-display-active-p)
      (chess-message 'return-to-current))))

(defun chess-display-move-backward ()
  (interactive)
  (chess-display-set-current ?-))

(defun chess-display-move-forward ()
  (interactive)
  (chess-display-set-current ?+))

(defun chess-display-move-first ()
  (interactive)
  (chess-display-set-current nil))

(defun chess-display-move-last ()
  (interactive)
  (chess-display-set-current t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chess-display-edit-mode (for editing the position directly)
;;

(defvar chess-display-edit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)

    (define-key map [(control ?l)] 'chess-display-redraw)
    (define-key map [(control ?i)] 'chess-display-invert)
    (define-key map "\t" 'chess-display-invert)

    (define-key map [??] 'describe-mode)
    (define-key map [?L] 'chess-display-list-buffers)
    ;;(define-key map [?C] 'chess-display-duplicate)
    (define-key map [?I] 'chess-display-invert)

    (define-key map [?C] 'chess-display-clear-board)
    (define-key map [?G] 'chess-display-restore-board)
    (define-key map [?S] 'chess-display-send-board)
    (define-key map [?X] 'chess-display-quit)

    (let ((keys '(?  ?p ?r ?n ?b ?q ?k ?P ?R ?N ?B ?Q ?K)))
      (while keys
	(define-key map (vector (car keys)) 'chess-display-set-piece)
	(setq keys (cdr keys))))

    (cond
     ((featurep 'xemacs)
      (define-key map [(button1)] 'chess-display-mouse-select-piece)
      (define-key map [(button2)] 'chess-display-mouse-set-piece)
      (define-key map [(button3)] 'chess-display-mouse-set-piece))
     (t
      (define-key map [down-mouse-1] 'chess-display-mouse-select-piece)
      (define-key map [drag-mouse-1] 'chess-display-mouse-select-piece)

      (define-key map [mouse-2] 'chess-display-mouse-set-piece)
      (define-key map [down-mouse-2] 'chess-display-mouse-set-piece)
      (define-key map [mouse-3] 'chess-display-mouse-set-piece)
      (define-key map [down-mouse-3] 'chess-display-mouse-set-piece)))

    map)
  "The mode map used for editing a chessboard position.")

(chess-message-catalog 'english
  '((editing-directly
     . "Now editing position directly, use S when complete...")
    (clear-chessboard-q . "Really clear the chessboard? ")))

(defun chess-display-edit-board ()
  "Setup the current board for editing."
  (interactive)
  (setq chess-display-edit-position
	(chess-pos-copy (chess-display-position nil))
	chess-display-edit-mode t
	chess-display-side-to-move (chess-string 'mode-edit))
  (force-mode-line-update)
  (use-local-map chess-display-edit-mode-map)
  (funcall chess-display-event-handler 'start-edit)
  (chess-message 'editing-directly))

(defun chess-display-end-edit-mode ()
  (setq chess-display-edit-mode nil)
  (funcall chess-display-event-handler 'end-edit)
  (use-local-map chess-display-mode-map))

(defun chess-display-send-board ()
  "Send the current board configuration to the user."
  (interactive)
  (chess-display-end-edit-mode)
  (chess-game-set-start-position chess-module-game
				 chess-display-edit-position))

(defun chess-display-restore-board ()
  "Cancel editing."
  (interactive)
  (chess-display-end-edit-mode)
  ;; reset the modeline
  (chess-display-set-index* nil chess-display-index)
  (chess-display-update nil))

(defun chess-display-clear-board ()
  "Setup the current board for editing."
  (interactive)
  (when (y-or-n-p (chess-string 'clear-chessboard-q))
    (let ((position (chess-display-position nil)))
      (dotimes (rank 8)
	(dotimes (file 8)
	  (chess-pos-set-piece position (cons rank file) ? ))))
    (chess-display-update nil)))

(defun chess-display-set-piece (&optional piece)
  "Set the piece under point to command character, or space for clear."
  (interactive)
  (when (or (null piece) (characterp piece))
    (let ((index (get-text-property (point) 'chess-coord)))
      (chess-pos-set-piece chess-display-edit-position index
			   (or piece last-command-event))
      (chess-display-draw-square nil index
				 (or piece last-command-event) (point)))))

(unless (fboundp 'event-window)
  (defalias 'event-point 'ignore))

(defun chess-display-mouse-set-piece (event)
  "Select the piece the user clicked on."
  (interactive "e")
  (if (fboundp 'event-window)		; XEmacs
      (progn
	(set-buffer (window-buffer (event-window event)))
	(and (event-point event) (goto-char (event-point event))))
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event))))
  (let ((pieces (if (memq (car event) '(down-mouse-3 mouse-3))
		    '("Set black piece"
		      ("Pieces"
		       ("Pawn"   . ?p)
		       ("Knight" . ?n)
		       ("Bishop" . ?b)
		       ("Queen"  . ?q)
		       ("King"   . ?k)))
		  '("Set white piece"
		    ("Pieces"
		     ("Pawn"   . ?P)
		     ("Knight" . ?N)
		     ("Bishop" . ?B)
		     ("Queen"  . ?Q)
		     ("King"   . ?K))))))
    (chess-display-set-piece (x-popup-menu t pieces))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mousing around on the chess-display
;;

(defvar chess-display-last-selected nil)

(make-variable-buffer-local 'chess-display-last-selected)

(chess-message-catalog 'english
  '((cannot-mount   . "You cannot move pieces on top of each other")
    (move-not-legal . "That is not a legal move")
    (not-your-move  . "It is not your turn to move")
    (wrong-color    . "You cannot move your opponent's pieces")
    (selected-empty . "You cannot select an empty square")
    (piece-immobile . "That piece cannot move now")))

(defun chess-display-select-piece ()
  "Select the piece under the cursor.
Clicking once on a piece selects it; then click on the target location."
  (interactive)
  (let ((coord (get-text-property (point) 'chess-coord))
	(position (chess-display-position nil))
	message)
    (when coord
      (setq message
	    (catch 'message
	      (if chess-display-last-selected
		  (let ((last-sel chess-display-last-selected))
		    ;; if they select the same square again, just deselect
		    ;; it by redrawing the square to remove highlights.
		    (if (= (point) (car last-sel))
			(funcall chess-display-event-handler 'draw-square
				 (car last-sel)
				 (chess-pos-piece position (cdr last-sel))
				 (cdr last-sel))
		      (let ((s-piece (chess-pos-piece position (cdr last-sel)))
			    (t-piece (chess-pos-piece position coord)) ply)
			(if chess-display-edit-mode
			    (progn
			      (chess-pos-set-piece position (cdr last-sel) ? )
			      (chess-pos-set-piece position coord s-piece)
			      (chess-display-update nil))
			  (if (and (/= t-piece ? )
				   (or (and (< t-piece ?a)
					    (< s-piece ?a))
				       (and (> t-piece ?a)
					    (> s-piece ?a))))
			      (throw 'message (chess-string 'cannot-mount)))
			  (unless (setq ply (chess-ply-create position nil
							      (cdr last-sel)
							      coord))
			    (throw 'message (chess-string 'move-not-legal)))
			  (condition-case err
			      (chess-display-move nil ply)
			    (error
			     (throw 'message (error-message-string err)))))))
		    ;; Redraw legal targets to clear highlight.
		    (when chess-display-highlight-legal
		      (dolist (index (mapcar #'chess-ply-target
					     (chess-legal-plies
					      position
					      :index (cdr last-sel))))
			(unless (= index coord)
			  (chess-display-draw-square nil index))))
		    (setq chess-display-last-selected nil))
		(let ((piece (chess-pos-piece position coord)))
		  (cond
		   ((= piece ? )
		    (throw 'message (chess-string 'selected-empty)))
		   ((not (or chess-display-edit-mode
			     (not (chess-display-active-p))
			     (eq (chess-pos-side-to-move position)
				 (chess-game-data chess-module-game
						  'my-color))))
		    (throw 'message (chess-string 'not-your-move)))
		   ((and (not chess-display-edit-mode)
			 (if (chess-pos-side-to-move position)
			     (> piece ?a)
			   (< piece ?a)))
		    (throw 'message (chess-string 'wrong-color)))
		   ((and (not chess-display-edit-mode)
			 chess-display-highlight-legal
			 (null (chess-legal-plies position :any :index coord)))
		    (throw 'message (chess-string 'piece-immobile))))
		  (setq chess-display-last-selected (cons (point) coord))
		  (chess-display-highlight nil coord)
		  (if (and (not chess-display-edit-mode)
			   chess-display-highlight-legal)
		      (chess-display-highlight-legal nil coord))))))
      (when message
	(when chess-display-last-selected
	  (funcall chess-display-event-handler 'draw-square
		   (car chess-display-last-selected)
		   (chess-pos-piece position
				    (cdr chess-display-last-selected))
		   (cdr chess-display-last-selected))
	  (setq chess-display-last-selected nil))
	(message message)))))

(defun chess-display-mouse-select-piece (event)
  "Select the piece the user clicked on."
  (interactive "e")
  (if (featurep 'xemacs)
      (progn
	(set-buffer (window-buffer (event-window event)))
	(and (event-point event) (goto-char (event-point event))))
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))
  (chess-display-select-piece))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maintain a face cache for given color strings
;;

(defvar chess-display-face-cache '((t . t)))

(defun chess-display-get-face (color)
  (or (cdr (assoc color chess-display-face-cache))
      (let ((face (make-face 'chess-display-highlight)))
	(set-face-attribute face nil :background color)
	(add-to-list 'chess-display-face-cache (cons color face))
	face)))

(provide 'chess-display)

;;; chess-display.el ends here
