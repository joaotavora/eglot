;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code shared by all chess displays
;;
;; $Revision$

(require 'chess-game)
(require 'chess-var)
(require 'chess-algebraic)
(require 'chess-fen)

(defgroup chess-display nil
  "Common code used by chess displays."
  :group 'chess)

(defcustom chess-display-separate-frame (display-multi-frame-p)
  "If non-nil, chessboard display use their own frame."
  :type 'boolean
  :group 'chess-images)

(defcustom chess-display-popup t
  "If non-nil, popup displays whenever a significant event occurs."
  :type 'boolean
  :group 'chess-ics1)

(defcustom chess-display-highlight-legal nil
  "If non-nil, highlight legal target squares when a piece is selected."
  :type 'boolean
  :group 'chess-ics1)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(defvar chess-display-style)
(defvar chess-display-game)
(defvar chess-display-index)
(defvar chess-display-perspective)
(defvar chess-display-main-p nil)
(defvar chess-display-event-handler nil)
(defvar chess-display-no-popup nil)
(defvar chess-display-edit-mode nil)
(defvar chess-display-mode-line "")

(make-variable-buffer-local 'chess-display-style)
(make-variable-buffer-local 'chess-display-game)
(make-variable-buffer-local 'chess-display-index)
(make-variable-buffer-local 'chess-display-perspective)
(make-variable-buffer-local 'chess-display-main-p)
(make-variable-buffer-local 'chess-display-event-handler)
(make-variable-buffer-local 'chess-display-no-popup)
(make-variable-buffer-local 'chess-display-edit-mode)
(make-variable-buffer-local 'chess-display-mode-line)

(defmacro chess-with-current-buffer (buffer &rest body)
  `(let ((buf ,buffer))
     (if buf
	 (with-current-buffer buf
	   ,@body)
       ,@body)))

(chess-message-catalog 'english
  '((no-such-style . "There is no such chessboard display style '%s'")))

(defun chess-display-create (game style perspective &optional main read-only)
  "Create a chess display, for displaying chess objects.
The display is drawn using the given STYLE, from the PERSPECTIVE
color's point of view.  If MAIN is non-nil, then this is a main
display, which means it will popup on significant events, and will
cause the underlying game object to be shutdown when it is destroyed.
If READ-ONLY is non-nil, then the display will not allow the user to
makes moves, or any other changes to the underlying game."
  (let* ((name (symbol-name style))
	 (handler (intern-soft (concat name "-handler")))
	 buffer)
    (unless handler
      (chess-error 'no-such-style name))
    (with-current-buffer (generate-new-buffer "*Chessboard*")
      (setq buffer (current-buffer))
      (chess-display-mode read-only)
      (when (funcall handler 'initialize)
	(add-hook 'kill-buffer-hook 'chess-display-quit nil t)
	(setq chess-display-style style
	      chess-display-perspective perspective
	      chess-display-event-handler handler)
	(if main
	    (chess-display-set-main nil))
	(chess-display-set-game* nil game)
	buffer))))

(defun chess-display-clone (display style perspective)
  (let ((new-display (chess-display-create chess-display-game
					   style perspective)))
    ;; the display will have already been updated by the `set-' calls,
    ;; it's just not visible yet
    (chess-display-popup new-display)
    new-display))

(defsubst chess-display-style (display)
  (chess-with-current-buffer display
    chess-display-style))

(defsubst chess-display-perspective (display)
  (chess-with-current-buffer display
    chess-display-perspective))

(defun chess-display-set-perspective* (display perspective)
  (chess-with-current-buffer display
    (setq chess-display-perspective perspective)
    (erase-buffer)))			; force a complete redraw

(defun chess-display-set-perspective (display perspective)
  (chess-with-current-buffer display
    (chess-display-set-perspective* nil perspective)
    (chess-display-update nil)))

(defsubst chess-display-main-p (display)
  (chess-with-current-buffer display
    chess-display-main-p))

(defun chess-display-set-main (display)
  (chess-with-current-buffer display
    (setq chess-display-main-p t)))

(defun chess-display-clear-main (display)
  (chess-with-current-buffer display
    (setq chess-display-main-p nil)))

(defun chess-display-set-position (display &optional position my-color)
  (chess-with-current-buffer display
    (if position
	(progn
	  (chess-game-set-start-position chess-display-game position)
	  (chess-game-set-data chess-display-game 'my-color my-color))
      (chess-game-set-start-position chess-display-game
				     chess-starting-position)
      (chess-game-set-data chess-display-game 'my-color t))))

(defun chess-display-position (display)
  "Return the position currently viewed."
  (chess-with-current-buffer display
    (chess-game-pos chess-display-game chess-display-index)))

(defun chess-display-set-ply (display ply)
  (chess-with-current-buffer display
    (setq chess-game-index 1)
    (chess-game-set-plies chess-display-game
			  (list ply (chess-ply-create
				     (chess-ply-next-pos ply))))))

(defun chess-display-ply (display)
  (chess-with-current-buffer display
    (chess-game-ply chess-display-game chess-display-index)))

(defun chess-display-set-variation (display variation &optional index)
  "Set the display variation.
This will cause the first ply in the variation to be displayed, with
the user able to scroll back and forth through the moves in the
variation.  Any moves made on the board will extend/change the
variation that was passed in."
  (chess-with-current-buffer display
    (setq chess-game-index (or index (chess-var-index variation)))
    (chess-game-set-plies chess-display-game variation)))

(defun chess-display-variation (display)
  (chess-with-current-buffer display
    (chess-game-main-var chess-display-game)))

(defun chess-display-set-game* (display game &optional index)
  "Set the game associated with the given DISPLAY.
If that display is already associated with a game object, detach it
from the display and associate the new GAME with it.  This is very
different from `chess-display-set-game', which only copies the details
of the game, so that in effect it is the same, while preserving all of
the event handlers registered on the display's previous game object."
  (chess-with-current-buffer display
    (assert game)
    (if chess-display-game
	(chess-display-detach-game nil))
    (setq chess-display-game game
	  chess-display-index (or index (chess-game-index game)))
    (chess-game-add-hook game 'chess-display-event-handler
			 (or display (current-buffer)))
    (chess-display-update nil t)))

(defun chess-display-set-game (display game &optional index)
  "Set the given DISPLAY to display the GAME object, optionally at INDEX.
This is the function to call to cause a display to view a game.  It
will also update all of the listening engines and other displays to
also view the same game."
  (chess-with-current-buffer display
    (setq chess-display-index (or index (chess-game-index game)))
    (chess-game-set-tags chess-display-game (chess-game-tags game))
    ;; this call triggers `setup-game' for us
    (chess-game-set-plies chess-display-game (chess-game-plies game))))

(defun chess-display-detach-game (display)
  "Set the display game.
This will cause the first ply in the game's main variation to be
displayed.  Also, information about the game is shown in the
modeline."
  (chess-with-current-buffer display
    (chess-game-remove-hook chess-display-game
			    'chess-display-event-handler
			    (or display (current-buffer)))))

(defsubst chess-display-game (display)
  (chess-with-current-buffer display
    chess-display-game))

(defun chess-display-set-index* (display index)
  (chess-with-current-buffer display
    (unless (or (not (integerp index))
		(< index 0)
		(> index (chess-game-index chess-display-game)))
      (setq chess-display-index index))))

(defun chess-display-set-index (display index)
  (chess-with-current-buffer display
    (chess-display-set-index* nil index)
    (chess-display-update nil)))

(defsubst chess-display-index (display)
  (chess-with-current-buffer display
    chess-display-index))

(defun chess-display-update (display &optional popup)
  "Update the chessboard DISPLAY.  POPUP too, if that arg is non-nil."
  (chess-with-current-buffer display
    (funcall chess-display-event-handler 'draw
	     (chess-display-position nil)
	     (chess-display-perspective nil))
    (chess-display-set-modeline)
    (if (and popup (not chess-display-no-popup)
	     (chess-display-main-p nil))
	(chess-display-popup nil))))

(defun chess-display-move (display ply)
  "Move a piece on DISPLAY, by applying the given PLY.
The position of PLY must match the currently displayed position.
If only START is given, it must be in algebraic move notation."
  (chess-with-current-buffer display
    ;; jww (2002-03-28): This should beget a variation within the
    ;; game, or alter the game, just as SCID allows
    (if (= chess-display-index (chess-game-index chess-display-game))
	(chess-game-move chess-display-game ply)
      (error "What to do here??  NYI"))
    (chess-display-update nil)))

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

(defun chess-display-highlight-legal (display pos)
  "Highlight all legal move targets from POS."
  (chess-with-current-buffer display
    (dolist (ply (chess-legal-plies (chess-display-position nil)
				    :index pos))
      (chess-display-highlight nil "pale green"
			       (chess-ply-target ply)))))

(defun chess-display-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (funcall chess-display-event-handler 'popup)))

(defun chess-display-enable-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (setq chess-display-no-popup nil)))

(defun chess-display-disable-popup (display)
  "Popup the given DISPLAY, so that it's visible to the user."
  (chess-with-current-buffer display
    (setq chess-display-no-popup t)))

(defun chess-display-destroy (display)
  "Destroy a chess display object, killing all of its buffers."
  (let ((buf (or display (current-buffer))))
    (when (buffer-live-p buf)
      (chess-display-event-handler chess-display-game buf 'destroy)
      (with-current-buffer buf
	(remove-hook 'kill-buffer-hook 'chess-display-quit t))
      (kill-buffer buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Event handler
;;

(defcustom chess-display-interesting-events nil
  "Events which will cause a display refresh."
  :type '(repeat symbol)
  :group 'chess-display)

(defcustom chess-display-momentous-events
  '(orient update setup-game pass move resign)
  "Events that will refresh, and cause 'main' displays to popup.
These are displays for which `chess-display-set-main' has been
called."
  :type '(repeat symbol)
  :group 'chess-display)

(defun chess-display-event-handler (game display event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (with-current-buffer display
    (apply chess-display-event-handler event args)

    (cond
     ((eq event 'shutdown)
      (chess-display-destroy nil))

     ((eq event 'destroy)
      (chess-display-detach-game nil))

     ((eq event 'pass)
      (let ((my-color (chess-game-data game 'my-color)))
	(chess-game-set-data game 'my-color (not my-color))
	(chess-display-set-perspective* nil (not my-color))))

     ((eq event 'orient)
      ;; Set the display's perspective to whichever color I'm playing
      (chess-display-set-perspective* nil (chess-game-data game 'my-color))))

    (let ((momentous (memq event chess-display-momentous-events)))
      (if momentous
	  (chess-display-set-index* nil (chess-game-index game)))
      (if (or momentous (memq event chess-display-interesting-events))
	  (chess-display-update nil momentous)))))

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
    (define-key map [? ] 'chess-display-pass)
    (define-key map [??] 'describe-mode)
    (define-key map [?@] 'chess-display-remote)
    (define-key map [(control ?c) (control ?a)] 'chess-display-abort)
    (define-key map [?C] 'chess-display-duplicate)
    (define-key map [?D] 'chess-display-duplicate)
    (define-key map [(control ?c) (control ?d)] 'chess-display-draw)
    (define-key map [?E] 'chess-display-edit-board)
    (define-key map [?F] 'chess-display-set-from-fen)
    ;;(define-key map [?M] 'chess-display-manual-move)
    (define-key map [?M] 'chess-display-match)
    (define-key map [(control ?c) (control ?r)] 'chess-display-resign)
    (define-key map [?S] 'chess-display-shuffle)
    (define-key map [?U] 'chess-display-undo)
    (define-key map [?X] 'chess-display-quit)

    (define-key map [(control ?y)] 'chess-display-yank-board)

    (dolist (key '(?a ?b ?c ?d ?e ?f ?g ?h
		      ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8
		      ?r ?n ?b ?q ?k ?o
		      ?R ?N ?B ?Q ?K ?O))
      (define-key map (vector key) 'chess-keyboard-shortcut))
    (define-key map [backspace] 'chess-keyboard-shortcut-delete)
    (define-key map [?x] 'ignore)

    (define-key map [(control ?m)] 'chess-display-select-piece)
    (define-key map [return] 'chess-display-select-piece)
    (cond
     ((featurep 'xemacs)
      (define-key map [(button1)] 'chess-display-mouse-select-piece)
      (define-key map [(button2)] 'chess-display-mouse-select-piece))
     (t
      (define-key map [down-mouse-1] 'chess-display-mouse-select-piece)
      (define-key map [drag-mouse-1] 'chess-display-mouse-select-piece)

      (define-key map [down-mouse-2] 'chess-display-mouse-select-piece)
      (define-key map [drag-mouse-2] 'chess-display-mouse-select-piece)))

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

(defun chess-display-mode (&optional read-only)
  "A mode for displaying and interacting with a chessboard.
If READ-ONLY is non-nil, then no modifications are allowed.
The key bindings available in this mode are:
\\{chess-display-mode-map}"
  (interactive)
  (setq major-mode 'chess-display-mode mode-name "Chessboard")
  (if read-only
      (use-local-map chess-display-safe-map)
    (use-local-map chess-display-mode-map))
  (buffer-disable-undo)
  (setq buffer-auto-save-file-name nil
	mode-line-format 'chess-display-mode-line))

(chess-message-catalog 'english
  '((mode-white     . "White")
    (mode-black     . "Black")
    (mode-start     . "START")
    (mode-checkmate . "CHECKMATE")
    (mode-resigned  . "RESIGNED")
    (mode-stalemate . "STALEMATE")
    (mode-drawn     . "DRAWMN")))

(defun chess-display-set-modeline ()
  "Set the modeline to reflect the current game position."
  (let ((color (chess-pos-side-to-move (chess-display-position nil)))
	(index chess-display-index))
    (if (= index 0)
	(setq chess-display-mode-line
	      (format "   %s   %s" (if color (chess-string 'mode-white)
				     (chess-string 'mode-black))
		      (chess-string 'mode-start)))
      (let ((ply (chess-game-ply chess-display-game (1- index))))
	(setq chess-display-mode-line
	      (concat
	       "   "
	       (let ((final (chess-ply-final-p ply)))
		 (cond
		  ((eq final :checkmate) (chess-string 'mode-checkmate))
		  ((eq final :resign)    (chess-string 'mode-resigned))
		  ((eq final :stalemate) (chess-string 'mode-stalemate))
		  ((eq final :draw)      (chess-string 'mode-drawn))
		  (t
		   (if color (chess-string 'mode-white)
		     (chess-string 'mode-black)))))
	       (if index
		   (concat "   " (int-to-string
				  (if (> index 1)
				      (/ index 2) (1+ (/ index 2))))))
	       (if ply
		   (concat ". " (if color "... ")
			   (or (chess-ply-to-algebraic ply)
			       "???")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commands used by the keyboard bindings above
;;

(defun chess-display-redraw ()
  "Just redraw the current display."
  (interactive)
  (erase-buffer)
  (chess-display-update nil))

(defsubst chess-display-active-p ()
  "Return non-nil if the displayed chessboard reflects an active game.
Basically, it means we are playing, not editing or reviewing."
  (and (= chess-display-index
	  (chess-game-index chess-display-game))
       (not (chess-game-over-p chess-display-game))
       (not chess-display-edit-mode)))

(defun chess-display-invert ()
  "Invert the perspective of the current chess board."
  (interactive)
  (chess-display-set-perspective nil (not (chess-display-perspective nil))))

(defun chess-display-set-from-fen (fen)
  "Send the current board configuration to the user."
  (interactive "sSet from FEN string: ")
  (chess-display-set-position nil (chess-fen-to-pos fen)))

(defun chess-display-kill-board (&optional arg)
  "Send the current board configuration to the user."
  (interactive "P")
  (let ((x-select-enable-clipboard t))
    (if arg
	(kill-new (with-temp-buffer
		    (chess-game-to-pgn chess-display-game)
		    (buffer-string)))
      (kill-new (chess-pos-to-fen (chess-display-position nil))))))

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
	(delete-backward-char 1))
      (goto-char (point-min))
      (cond
       ((search-forward "[Event" nil t)
	(goto-char (match-beginning 0))
	(chess-display-copy-game display (chess-pgn-to-game)))
       ((looking-at (concat chess-algebraic-regexp "$"))
	(let ((move (buffer-string)))
	  (with-current-buffer display
	    (chess-display-manual-move move))))
       (t
	(with-current-buffer display
	  (chess-display-set-from-fen (buffer-string))))))))

(defun chess-display-set-piece ()
  "Set the piece under point to command character, or space for clear."
  (interactive)
  (unless (chess-display-active-p)
    (chess-pos-set-piece (chess-display-position nil)
			 (get-text-property (point) 'chess-coord)
			 last-command-char)
    (chess-display-update nil)))

(defun chess-display-quit ()
  "Quit the current game."
  (interactive)
  (if chess-display-main-p
      (chess-game-run-hooks chess-display-game 'shutdown)
    (chess-display-destroy nil)))

(chess-message-catalog 'english
  '((illegal-notation . "Illegal move notation: %s")))

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

(defun chess-display-remote (display)
  (interactive "sDisplay this game on X server: ")
  (require 'chess-images)
  (let ((chess-images-separate-frame display))
    (chess-display-clone (current-buffer) 'chess-images
			 (chess-display-perspective nil))))

(defun chess-display-duplicate (style)
  (interactive
   (list (concat "chess-"
		 (read-from-minibuffer
		  "Create new display using style: "
		  (substring (symbol-name (chess-display-style nil))
			     0 (length "chess-"))))))
  (chess-display-clone (current-buffer) (intern-soft style)
		       (chess-display-perspective nil)))

(defun chess-display-pass ()
  "Pass the move to your opponent.  Only valid on the first move."
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-display-game 'pass)
    (ding)))

(defun chess-display-shuffle ()
  "Generate a shuffled opening position."
  (interactive)
  (require 'chess-random)
  (if (and (chess-display-active-p)
	   (= 0 chess-display-index))
      (chess-game-set-start-position chess-display-game
				     (chess-fischer-random-position))
    (ding)))

(defun chess-display-match ()
  "Resign the current game."
  (chess-game-run-hooks chess-display-game 'match))

(defun chess-display-resign ()
  "Resign the current game."
  (interactive)
  (if (chess-display-active-p)
      (progn
	(chess-game-end chess-display-game :resign)
	(chess-game-run-hooks chess-display-game 'resign))
    (ding)))

(defun chess-display-abort ()
  "Abort the current game."
  (interactive)
  (if (chess-display-active-p)
      (chess-game-run-hooks chess-display-game 'abort)
    (ding)))

(chess-message-catalog 'english
  '((draw-offer . "You offer a draw")))

(defun chess-display-draw ()
  "Offer to draw the current game."
  (interactive)
  (if (chess-display-active-p)
      (progn
	(chess-message 'draw-offer)
	(chess-game-run-hooks chess-display-game 'draw))
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
			(chess-game-data chess-display-game 'my-color))
		    2 1)))
	(chess-game-run-hooks chess-display-game 'undo count))
    (ding)))

(defun chess-display-list-buffers ()
  "List all buffers related to this display's current game."
  (interactive)
  (let ((buffer-list-func (symbol-function 'buffer-list)))
    (unwind-protect
	(let ((chess-game chess-display-game)
	      (lb-command (lookup-key ctl-x-map [(control ?b)]))
	      (ibuffer-maybe-show-regexps nil))
	  (fset 'buffer-list
		(function
		 (lambda ()
		   (delq nil
			 (mapcar (function
				  (lambda (cell)
				    (and (bufferp (cdr cell))
					 (buffer-live-p (cdr cell))
					 (cdr cell))))
				 (chess-game-hooks chess-game))))))
	  (call-interactively lb-command))
      (fset 'buffer-list buffer-list-func))))

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
     nil (or index (chess-game-index chess-display-game)))
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
    (set-keymap-parent map chess-display-mode-map)

    (define-key map [?C] 'chess-display-clear-board)
    (define-key map [?G] 'chess-display-restore-board)
    (define-key map [?S] 'chess-display-send-board)

    (let ((keys '(?  ?p ?r ?n ?b ?q ?k ?P ?R ?N ?B ?Q ?K)))
      (while keys
	(define-key map (vector (car keys)) 'chess-display-set-piece)
	(setq keys (cdr keys))))
    map)
  "The mode map used for editing a chessboard position.")

(chess-message-catalog 'english
  '((editing-directly
     . "Now editing position directly, use S when complete...")))

(defun chess-display-edit-board ()
  "Setup the current board for editing."
  (interactive)
  (setq chess-display-edit-mode t)
  ;; Take us out of any game/ply/variation we might be looking at,
  ;; since we are not moving pieces now, but rather placing them --
  ;; for which purpose the movement keys can still be used.
  (chess-display-set-position nil (chess-display-position nil))
  ;; jww (2002-03-28): setup edit-mode keymap here
  (chess-message 'editing-directly))

(defun chess-display-send-board ()
  "Send the current board configuration to the user."
  (interactive)
  (chess-game-set-start-position chess-display-game
				 (chess-display-position nil))
  (setq chess-display-edit-mode nil))

(defun chess-display-restore-board ()
  "Setup the current board for editing."
  (interactive)
  ;; jww (2002-03-28): NYI
  (setq chess-display-edit-mode nil)
  (chess-display-update nil))

(defun chess-display-clear-board ()
  "Setup the current board for editing."
  (interactive)
  (when (y-or-n-p "Really clear the chessboard? ")
    (let ((position (chess-display-position nil)))
      (dotimes (rank 8)
	(dotimes (file 8)
	  (chess-pos-set-piece position (cons rank file) ? ))))
    (chess-display-update nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Default window and frame popup functions
;;

(defun chess-display-popup-in-window ()
  "Popup the given DISPLAY, so that it's visible to the user."
  (unless (get-buffer-window (current-buffer))
    (fit-window-to-buffer (display-buffer (current-buffer)))))

(defun chess-display-popup-in-frame (height width &optional display)
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
	(select-frame (make-frame params))
	(set-window-dedicated-p (selected-window) t)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyboard entry of algebraic notation, using shortcutting
;;
;; This scheme was adapted from the way SCID
;; (http://scid.sourceforge.net), by Shane Hudson, behaves.  It's the
;; only way to move your pieces around!
;;

(defvar chess-move-string "")
(defvar chess-legal-moves-pos nil)
(defvar chess-legal-moves nil)

(make-variable-buffer-local 'chess-move-string)
(make-variable-buffer-local 'chess-legal-moves-pos)
(make-variable-buffer-local 'chess-legal-moves)

(chess-message-catalog 'english
  '((not-your-move . "It is not your turn to move")
    (game-is-over  . "This game is over")))

(defun chess-display-assert-can-move ()
  (if (and (chess-display-active-p)
	   ;; `active' means we're playing against an engine
	   (chess-game-data chess-display-game 'active)
	   (not (eq (chess-game-data chess-display-game 'my-color)
		    (chess-pos-side-to-move position))))
      (chess-error 'not-your-move)
    (if (and (= chess-display-index
		(chess-game-index chess-display-game))
	     (chess-game-over-p chess-display-game))
	(chess-error 'game-is-over))))

(defun chess-keyboard-test-move (move-ply)
  "Return the given MOVE if it matches the user's current input."
  (let* ((move (cdr move-ply))
	 (i 0) (x 0) (l (length move))
	 (xl (length chess-move-string))
	 (match t))
    (unless (or (and (equal (downcase chess-move-string) "ok")
		     (string-match "\\`O-O[+#]\\'" move))
		(and (equal (downcase chess-move-string) "oq")
		     (string-match "\\`O-O-O[+#]\\'" move)))
      (while (and (< i l) (< x xl))
	(let ((move-char (aref move i))
	      (entry-char (aref chess-move-string x)))
	  (if (= move-char ?x)
	      (setq i (1+ i))
	    (if (/= entry-char (if (< entry-char ?a)
				   move-char
				 (downcase move-char)))
		(setq match nil i l)
	      (setq i (1+ i) x (1+ x)))))))
    (if match
	move-ply)))

(defsubst chess-keyboard-display-moves (&optional move-list)
  (if (> (length chess-move-string) 0)
      (message "[%s] %s" chess-move-string
	       (mapconcat 'cdr
			  (or move-list
			      (delq nil (mapcar 'chess-keyboard-test-move
						(cdr chess-legal-moves))))
			  " "))))

(defun chess-keyboard-shortcut-delete ()
  (interactive)
  (when (and chess-move-string
	     (stringp chess-move-string)
	     (> (length chess-move-string) 0))
    (setq chess-move-string
	  (substring chess-move-string 0 (1- (length chess-move-string))))
    (chess-keyboard-display-moves)))

(defun chess-keyboard-shortcut (&optional display-only)
  (interactive)
  (let* ((position (chess-display-position nil))
	 (color (chess-pos-side-to-move position))
	 char)
    (chess-display-assert-can-move)
    (unless (memq last-command '(chess-keyboard-shortcut
				 chess-keyboard-shortcut-delete))
      (setq chess-move-string nil))
    (unless display-only
      (setq chess-move-string
	    (concat chess-move-string (char-to-string last-command-char))))
    (unless (and chess-legal-moves
		 (eq position chess-legal-moves-pos)
		 (or (> (length chess-move-string) 1)
		     (eq (car chess-legal-moves) last-command-char)))
      (setq char (if (eq (downcase last-command-char) ?o) ?k
		   last-command-char)
	    chess-legal-moves-pos position
	    chess-legal-moves
	    (cons char
		  (sort
		   (mapcar
		    (function
		     (lambda (ply)
		       (cons ply (chess-ply-to-algebraic ply))))
		    (if (eq char ?b)
			(append (chess-legal-plies
				 position :piece (if color ?P ?p) :file 1)
				(chess-legal-plies
				 position :piece (if color ?B ?b)))
		      (if (and (>= char ?a)
			       (<= char ?h))
			  (chess-legal-plies position
					     :piece (if color ?P ?p)
					     :file (- char ?a))
			(chess-legal-plies position
					   :piece (if color
						      (upcase char)
						    (downcase char))))))
		   (function
		    (lambda (left right)
		      (string-lessp (cdr left) (cdr right)))))))))
  (let ((moves (delq nil (mapcar 'chess-keyboard-test-move
				 (cdr chess-legal-moves)))))
    (cond
     ((or (= (length moves) 1)
	  ;; if there is an exact match except for case, it must be an
	  ;; abiguity between a bishop and a b-pawn move.  In this
	  ;; case, always take the b-pawn move; to select the bishop
	  ;; move, use B to begin the keyboard shortcut
	  (and (= (length moves) 2)
	       (string= (downcase (cdr (car moves)))
			(downcase (cdr (cadr moves))))
	       (setq moves (cdr moves))))
      (chess-display-move nil (caar moves))
      (setq chess-move-string nil
	    chess-legal-moves nil
	    chess-legal-moves-pos nil))
     ((null moves)
      (chess-keyboard-shortcut-delete))
     (t
      (chess-keyboard-display-moves moves)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mousing around on the chess-display
;;

(defvar chess-display-last-selected nil)

(make-variable-buffer-local 'chess-display-last-selected)

(chess-message-catalog 'english
  '((cannot-mount   . "You cannot move pieces on top of each other")
    (move-not-legal . "That is not a legal move")
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
		    ;; it by redrawing the display and removing all
		    ;; highlights
		    (if (= (point) (car last-sel))
			(chess-display-update nil)
		      (let ((s-piece (chess-pos-piece position
						      (cadr last-sel)))
			    (t-piece (chess-pos-piece position coord)) ply)
			(if (and (/= t-piece ? )
				 (or (and (< t-piece ?a)
					  (< s-piece ?a))
				     (and (> t-piece ?a)
					  (> s-piece ?a))))
			    (throw 'message (chess-string 'cannot-mount)))
			(unless (setq ply (chess-ply-create position
							    (cadr last-sel) coord))
			  (throw 'message (chess-string 'move-not-legal)))
			(chess-display-move nil ply)))
		    (setq chess-display-last-selected nil))
		(chess-display-assert-can-move)
		(let ((piece (chess-pos-piece position coord)))
		  (cond
		   ((eq piece ? )
		    (throw 'message (chess-string 'selected-empty)))
		   ((if (chess-pos-side-to-move position)
			(> piece ?a)
		      (< piece ?a))
		    (throw 'message (chess-string 'wrong-color)))
		   ((null (chess-legal-plies position :index coord))
		    (throw 'message (chess-string 'piece-immobile))))
		  (setq chess-display-last-selected (list (point) coord))
		  (chess-display-highlight nil coord)
		  (if chess-display-highlight-legal
		      (chess-display-highlight-legal nil coord))))))
      (when message
	(setq chess-display-last-selected nil)
	(chess-display-update nil)
	(error message)))))

(defun chess-display-mouse-select-piece (event)
  "Select the piece the user clicked on."
  (interactive "e")
  (if (fboundp 'event-window)		; XEmacs
      (progn
	(set-buffer (window-buffer (event-window event)))
	(and (event-point event) (goto-char (event-point event))))
    (if (equal (event-start event) (event-end event))
	(progn
	  (set-buffer (window-buffer (posn-window (event-start event))))
	  (goto-char (posn-point (event-start event)))
	  (chess-display-select-piece))
      (goto-char (posn-point (event-end event)))
      (chess-display-select-piece))))

(provide 'chess-display)

;;; chess-display.el ends here
