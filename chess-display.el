;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code shared by all chess displays
;;
;; $Revision$

;;; Code:

(require 'chess-game)
(require 'chess-algebraic)
(require 'chess-fen)

(defgroup chess-display nil
  "Common code used by chess displays."
  :group 'chess)

(defcustom chess-display-use-faces t
  "If non-nil, provide colored faces for ASCII displays."
  :type 'boolean
  :group 'chess-display)

(defface chess-display-black-face
  '((((class color) (background light)) (:foreground "Green"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:bold t)))
  "*The face used for black pieces on the ASCII display."
  :group 'chess-display)

(defface chess-display-white-face
  '((((class color) (background light)) (:foreground "Yellow"))
    (((class color) (background dark)) (:foreground "Yellow"))
    (t (:bold t)))
  "*The face used for white pieces on the ASCII display."
  :group 'chess-display)

(defface chess-display-highlight-face
  '((((class color) (background light)) (:background "#add8e6"))
    (((class color) (background dark)) (:background "#add8e6")))
  "Face to use for highlighting pieces that have been selected."
  :group 'chess-display)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(defvar chess-display-game)
(defvar chess-display-search-function)
(defvar chess-display-variation)
(defvar chess-display-index)
(defvar chess-display-ply)
(defvar chess-display-position)
(defvar chess-display-perspective)
(defvar chess-display-draw-function nil)
(defvar chess-display-highlight-function nil)
(defvar chess-display-edit-mode nil)
(defvar chess-display-mode-line "")

(make-variable-buffer-local 'chess-display-game)
(make-variable-buffer-local 'chess-display-search-function)
(make-variable-buffer-local 'chess-display-variation)
(make-variable-buffer-local 'chess-display-index)
(make-variable-buffer-local 'chess-display-ply)
(make-variable-buffer-local 'chess-display-position)
(make-variable-buffer-local 'chess-display-perspective)
(make-variable-buffer-local 'chess-display-draw-function)
(make-variable-buffer-local 'chess-display-highlight-function)
(make-variable-buffer-local 'chess-display-edit-mode)
(make-variable-buffer-local 'chess-display-mode-line)

(defmacro chess-with-current-buffer (buffer &rest body)
  `(let ((buf ,buffer))
     (if buf
	 (with-current-buffer buf
	   ,@body)
       ,@body)))

(defun chess-display-create (style perspective &optional search-func)
  "Create a chess display, for displaying chess objects."
  (let* ((name (symbol-name style))
	 (draw (intern-soft (concat name "-draw")))
	 (highlight (intern-soft (concat name "-highlight")))
	 (initialize (intern-soft (concat name "-initialize"))))
    (unless draw
      (error "There is no known chessboard display style '%s'" name))
    (with-current-buffer (generate-new-buffer "*Chessboard*")
      (setq cursor-type nil
	    chess-display-draw-function draw
	    chess-display-highlight-function highlight
	    chess-display-perspective perspective
	    chess-display-search-function search-func)
      (chess-display-mode)
      (if initialize
	  (funcall initialize))
      (current-buffer))))

(defsubst chess-display-destroy (display)
  "Destroy a chess display object, killing all of its buffers."
  (let ((buf (or display (current-buffer))))
    (if (buffer-live-p buf)
	(kill-buffer buf))))

(defsubst chess-display-perspective (display)
  (chess-with-current-buffer display
    chess-display-perspective))

(defun chess-display-set-perspective (display perspective)
  (chess-with-current-buffer display
    (setq chess-display-perspective perspective)
    (chess-display-update nil)))

(defun chess-display-set-search-function (display search-func)
  (chess-with-current-buffer display
    (if chess-display-game
	(error "Cannot alter a display's search function when viewing a game")
      (setq chess-display-search-function search-func))))

(defun chess-display-search-function (display)
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-game-search-function chess-display-game)
      (or chess-display-search-function
	  'chess-standard-search-position))))

(defsubst chess-display-search-position (display position target piece)
  (chess-with-current-buffer display
    (funcall (chess-display-search-function nil)
	     position target piece)))

(defun chess-display-set-position (display position &optional search-func)
  "Set the display position.
Note that when a single position is being displayed, out of context of
a game, the user's move will cause a new variation to be created,
without a game object.
If the position is merely edited, it will change the POSITION object
that was passed in."
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-display-detach-game nil))
    (setq chess-display-game nil
	  chess-display-search-function search-func
	  chess-display-variation nil
	  chess-display-index nil
	  chess-display-ply nil
	  chess-display-position position)
    (chess-display-update nil)))

(defun chess-display-position (display)
  "Return the position currently viewed."
  (chess-with-current-buffer display
    (or (and chess-display-game
	     (chess-game-pos chess-display-game chess-display-index))
	(and chess-display-variation
	     (chess-ply-next-pos
	      (nth chess-display-index chess-display-variation)))
	(and chess-display-ply
	     (chess-ply-next-pos chess-display-ply))
	chess-display-position)))

(defun chess-display-set-ply (display ply &optional search-func)
  "Set the display ply.
This differs from a position display, only in that the algebraic form
of the move made to the reach the displayed position will be shown in
the modeline."
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-display-detach-game nil))
    (setq chess-display-game nil
	  chess-display-search-function search-func
	  chess-display-variation nil
	  chess-display-index nil
	  chess-display-ply ply
	  chess-display-position nil)
    (chess-display-update display)))

(defun chess-display-ply (display)
  (chess-with-current-buffer display
    (or (and chess-display-game
	     (chess-game-ply chess-display-game chess-display-index))
	(and chess-display-variation
	     (nth chess-display-index chess-display-variation))
	chess-display-ply)))

(defun chess-display-set-variation (display plies &optional index search-func)
  "Set the display variation.
This will cause the first ply in the variation to be displayed, with
the user able to scroll back and forth through the moves in the
variation.  Any moves made on the board will extend/change the
variation that was passed in."
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-display-detach-game nil))
    (setq chess-display-game nil
	  chess-display-search-function search-func
	  chess-display-variation plies
	  chess-display-index (or index 0)
	  chess-display-ply nil
	  chess-display-position nil)
    (chess-display-update nil)))

(defun chess-display-variation (display)
  (chess-with-current-buffer display
    (or (and chess-display-game
	     (chess-game-plies chess-display-game))
	chess-display-variation)))

(defun chess-display-set-game (display game &optional index)
  "Set the display game.
This will cause the first ply in the game's main variation to be
displayed.  Also, information about the game is shown in the
modeline."
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-display-detach-game nil))
    (setq chess-display-game game
	  chess-display-search-function nil
	  chess-display-variation nil
	  chess-display-index (or index 0)
	  chess-display-ply nil
	  chess-display-position nil)
    (chess-game-add-hook game 'chess-display-event-handler display)
    (chess-display-update nil)))

(defun chess-display-detach-game (display)
  "Set the display game.
This will cause the first ply in the game's main variation to be
displayed.  Also, information about the game is shown in the
modeline."
  (chess-with-current-buffer display
    (if chess-display-game
	(chess-game-remove-hook chess-display-game
				'chess-display-event-handler))))

(defsubst chess-display-game (display)
  (chess-with-current-buffer display
    chess-display-game))

(defun chess-display-set-index (display index)
  (chess-with-current-buffer display
    (unless chess-display-index
      (error "There is no game or variation currently being displayed."))
    (unless (or (not (integerp index))
		(< index 0)
		(> index (if chess-display-game
			     (chess-game-index chess-display-game)
			   (1+ (length chess-display-variation)))))
      (setq chess-display-index index)
      (chess-display-update nil))))

(defsubst chess-display-index (display)
  (chess-with-current-buffer display
    chess-display-index))

(defun chess-display-update (display)
  "This should be called if any object passed in has been changed.
That is, if you call `chess-display-set-position', and then later
change that position object, the display can be updated by calling
`chess-display-update'."
  (chess-with-current-buffer display
    (if chess-display-draw-function
	(funcall chess-display-draw-function))
    (chess-display-set-modeline)))

(defun chess-display-move (display ply)
  "Move a piece on DISPLAY, by applying the given PLY.
The position of PLY must match the currently displayed position.
If only START is given, it must be in algebraic move notation."
  (chess-with-current-buffer display
    (cond
     (chess-display-game
      ;; jww (2002-03-28): This should beget a variation within the
      ;; game, or alter the game, just as SCID allows
      (if (= (chess-display-index nil)
	     (chess-game-index chess-display-game))
	  (chess-game-move chess-display-game ply)
	(error "What to do here??  NYI")))
     (chess-display-variation
      ;; jww (2002-04-02): what if we're in the middle?
      (nconc chess-display-variation (list ply))
      (setq chess-display-index (1- (length chess-display-variation))))
     (chess-display-ply
      (setq chess-display-ply ply))
     (chess-display-position		; an ordinary position
      (setq chess-display-position (chess-ply-next-pos ply))))
    (chess-display-update nil)))

(defun chess-display-highlight (display index &optional mode)
  "Highlight the square at INDEX on the current position.
The given highlighting MODE is used, or the default if the style you
are displaying with doesn't support that mode.  `selected' is a mode
that is supported by most displays, and is the default mode."
  (chess-with-current-buffer display
    (if chess-display-highlight-function
	(funcall chess-display-highlight-function index
		 (or mode 'selected)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Event handler
;;

(defun chess-display-event-handler (game display event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (cond
   ((eq event 'shutdown)
    (chess-display-destroy display))

   ((eq event 'pass)
    (chess-display-set-perspective
     display (not (chess-display-perspective display))))

   ((eq event 'move)
    (chess-display-set-index
     display (chess-game-index (chess-display-game display)))))

  (unless (eq event 'shutdown)
    (chess-display-update display)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chess-display-mode
;;

(defvar chess-display-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map nil)

    (define-key map [(control ?i)] 'chess-display-invert)
    (define-key map [tab] 'chess-display-invert)

    (define-key map [??] 'describe-mode)
    (define-key map [?C] 'chess-display-clear-board)
    (define-key map [?E] 'chess-display-edit-board)
    (define-key map [?F] 'chess-display-set-from-fen)
    (define-key map [?I] 'chess-display-invert)
    (define-key map [?X] 'chess-display-quit)
    (define-key map [?M] 'chess-display-manual-move)

    (define-key map [?<] 'chess-display-move-first)
    (define-key map [?,] 'chess-display-move-backward)
    (define-key map [(meta ?<)] 'chess-display-move-first)
    (define-key map [?>] 'chess-display-move-last)
    (define-key map [?.] 'chess-display-move-forward)
    (define-key map [(meta ?>)] 'chess-display-move-last)

    (define-key map [(meta ?w)] 'chess-display-copy-board)
    (define-key map [(control ?y)] 'chess-display-paste-board)

    (define-key map [(control ?l)] 'chess-display-redraw)

    (dolist (key '(?a ?b ?c ?d ?e ?f ?g ?h
		      ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8
		      ?r ?n ?b ?q ?k ?o))
      (define-key map (vector key) 'chess-keyboard-shortcut))
    (define-key map [backspace] 'chess-keyboard-shortcut-delete)

    (define-key map [(control ?m)] 'chess-display-select-piece)
    (define-key map [return] 'chess-display-select-piece)
    (cond
     ((featurep 'xemacs)
      (define-key map [(button1)] 'chess-display-mouse-select-piece)
      (define-key map [(button2)] 'chess-display-mouse-select-piece))
     (t
      (define-key map [mouse-1] 'chess-display-mouse-select-piece)
      (define-key map [mouse-2] 'chess-display-mouse-select-piece)))
    map)
  "The mode map used in a chessboard display buffer.")

(defun chess-display-redraw ()
  "Just redraw the current display."
  (interactive)
  (chess-display-update nil))

(defun chess-display-mode ()
  "A mode for displaying and interacting with a chessboard.
The key bindings available in this mode are:
\\{chess-display-mode-map}"
  (interactive)
  (setq major-mode 'chess-display-mode mode-name "Chessboard")
  (use-local-map chess-display-mode-map)
  (buffer-disable-undo)
  (setq buffer-auto-save-file-name nil
	mode-line-format 'chess-display-mode-line))

(defun chess-display-set-modeline ()
  "Set the modeline to reflect the current game position."
  (let ((color (chess-pos-side-to-move (chess-display-position nil)))
	(index (chess-display-index nil))
	ply)
    (if (and index (= index 0))
	(setq chess-display-mode-line
	      (format "   %s   START" (if color "White" "BLACK")))
      (cond
       (chess-display-ply
	(setq ply chess-display-ply))
       (chess-display-game
	(setq ply (chess-game-ply chess-display-game (1- index))))
       (chess-display-variation
	(setq ply (nth chess-display-variation (1- index)))))
      (if ply
	  (setq chess-display-mode-line
		(concat
		 "  " (if color "White" "BLACK")
		 (if index
		     (concat "   " (int-to-string
				    (if (> index 1)
					(/ index 2) (1+ (/ index 2))))))
		 (if ply
		     (concat ". " (if color "... ")
			     (or (chess-ply-to-algebraic
				  ply nil
				  (chess-display-search-function nil))
				 "???")))))))))

(defsubst chess-display-active-p ()
  "Return non-nil if the displayed chessboard reflects an active game.
Basically, it means we are playing, not editing or reviewing."
  (and chess-display-game
       (= (chess-display-index nil)
	  (chess-game-index chess-display-game))
       (not chess-display-edit-mode)))

(defun chess-display-invert ()
  "Invert the perspective of the current chess board."
  (interactive)
  (chess-display-set-perspective nil (not (chess-display-perspective nil))))

(defun chess-display-set-from-fen (fen)
  "Send the current board configuration to the user."
  (interactive "sSet from FEN string: ")
  (chess-display-set-position nil (chess-fen-to-pos fen)))

(defun chess-display-copy-board ()
  "Send the current board configuration to the user."
  (interactive)
  (let* ((x-select-enable-clipboard t)
	 (fen (chess-fen-from-pos (chess-display-position nil))))
    (kill-new fen)
    (message "Copied board: %s" fen)))

(defun chess-display-paste-board ()
  "Send the current board configuration to the user."
  (interactive)
  (let* ((x-select-enable-clipboard t)
	 (fen (current-kill 0)))
    (chess-display-set-from-fen fen)
    (message "Pasted board: %s" fen)))

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
  (if chess-display-game
      (chess-game-run-hooks chess-display-game 'shutdown)
    (chess-display-destroy nil)))

(defun chess-display-manual-move (move)
  "Move a piece manually, using chess notation."
  (interactive
   (list (read-string
	  (format "%s(%d): "
		  (if (chess-pos-side-to-move (chess-display-position nil))
		      "White" "Black")
		  (1+ (/ (or (chess-display-index nil) 0) 2))))))
  (let ((ply (chess-algebraic-to-ply
	      (chess-display-position nil) move
	      (chess-display-search-function nil))))
    (unless ply
      (error "Illegal move notation: %s" move))
    (chess-display-move nil ply)))

(defun chess-display-set-current (dir)
  "Change the currently displayed board.
Direction may be - or +, to move forward or back, or t or nil to jump
to the end or beginning."
  (let ((index (cond ((eq dir ?-) (1- chess-display-index))
		     ((eq dir ?+) (1+ chess-display-index))
		     ((eq dir t) nil)
		     ((eq dir nil) 0))))
    (chess-display-set-index
     nil (or index
	     (if chess-display-game
		 (chess-game-index chess-display-game)
	       (1- (length chess-display-variation)))))
    (unless (chess-display-active-p)
      (message "Use '>' to return to the current position"))))

(defun chess-display-move-backward ()
  (interactive)
  (if chess-display-index
      (chess-display-set-current ?-)))

(defun chess-display-move-forward ()
  (interactive)
  (if chess-display-index
      (chess-display-set-current ?+)))

(defun chess-display-move-first ()
  (interactive)
  (if chess-display-index
      (chess-display-set-current nil)))

(defun chess-display-move-last ()
  (interactive)
  (if chess-display-index
      (chess-display-set-current t)))

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

(defun chess-display-edit-board ()
  "Setup the current board for editing."
  (interactive)
  (setq cursor-type t
	chess-display-edit-mode t)
  ;; Take us out of any game/ply/variation we might be looking at,
  ;; since we are not moving pieces now, but rather placing them --
  ;; for which purpose the movement keys can still be used.
  (chess-display-set-position nil (chess-display-position nil))
  ;; jww (2002-03-28): setup edit-mode keymap here
  (message "Now editing position directly, use S when complete..."))

(defun chess-display-send-board ()
  "Send the current board configuration to the user."
  (interactive)
  (if chess-display-game
      (chess-game-set-plies
       chess-display-game
       (list (chess-ply-create (chess-display-position nil)))))
  (setq cursor-type nil
	chess-display-edit-mode nil))

(defun chess-display-restore-board ()
  "Setup the current board for editing."
  (interactive)
  (setq cursor-type nil
	chess-display-edit-mode nil)
  ;; jww (2002-03-28): NYI
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
;; Allow for quick entry of algebraic moves via keyboard
;;

(defvar chess-move-string nil)
(defvar chess-legal-moves-pos nil)
(defvar chess-legal-moves nil)

(make-variable-buffer-local 'chess-move-string)
(make-variable-buffer-local 'chess-legal-moves-pos)
(make-variable-buffer-local 'chess-legal-moves)

(defun chess-keyboard-test-move (move)
  "Return the given MOVE if it matching the user's current input."
  (let ((i 0) (x 0)
	(l (length move))
	(xl (length chess-move-string))
	(match t))
    (unless (or (and (equal chess-move-string "ok")
		     (equal move "O-O"))
		(and (equal chess-move-string "oq")
		     (equal move "O-O-O")))
      (while (and (< i l) (< x xl))
	(if (= (aref move i) ?x)
	    (setq i (1+ i)))
	(if (/= (downcase (aref move i))
		(aref chess-move-string x))
	    (setq match nil i l)
	  (setq i (1+ i) x (1+ x)))))
    (if match move)))

(defsubst chess-keyboard-display-moves (&optional move-list)
  (if (> (length chess-move-string) 0)
      (message "[%s] %s" chess-move-string
	       (mapconcat 'identity
			  (or move-list
			      (delq nil (mapcar 'chess-keyboard-test-move
						chess-legal-moves))) " "))))

(defun chess-keyboard-shortcut-delete ()
  (interactive)
  (setq chess-move-string
	(substring chess-move-string 0
		   (1- (length chess-move-string))))
  (chess-keyboard-display-moves))

(defun chess-keyboard-shortcut (&optional display-only)
  (interactive)
  (unless (memq last-command '(chess-keyboard-shortcut
			       chess-keyboard-shortcut-delete))
    (setq chess-move-string nil))
  (unless display-only
    (setq chess-move-string
	  (concat chess-move-string
		  (char-to-string (downcase last-command-char)))))
  (let ((position (chess-display-position nil))
	(search-func (chess-display-search-function nil)))
    (unless (and chess-legal-moves
		 (eq position chess-legal-moves-pos))
      (setq chess-legal-moves-pos position
	    chess-legal-moves
	    (sort (mapcar (function
			   (lambda (ply)
			     (chess-ply-to-algebraic ply nil search-func)))
			  (chess-legal-plies position search-func))
		  'string-lessp)))
    (let ((moves (delq nil (mapcar 'chess-keyboard-test-move
				   chess-legal-moves))))
      (cond
       ((= (length moves) 1)
	(let ((ply (chess-algebraic-to-ply
		    (chess-display-position nil) (car moves)
		    (chess-display-search-function nil))))
	  (unless ply
	    (error "Illegal move notation: %s" (car moves)))
	  (chess-display-move nil ply))
	(setq chess-move-string nil
	      chess-legal-moves nil
	      chess-legal-moves-pos nil))
       ((null moves)
	(chess-keyboard-shortcut-delete))
       (t
	(chess-keyboard-display-moves moves))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manage a face cache for textual displays
;;

(defvar chess-display-face-cache '((t . t)))

(defsubst chess-display-get-face (color)
  (or (cdr (assoc color chess-display-face-cache))
      (let ((face (make-face 'chess-display-highlight)))
	(set-face-attribute face nil :background color)
	(add-to-list 'chess-display-face-cache (cons color face))
	face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mousing around on the chess-display
;;

(defvar chess-display-last-selected nil)
(make-variable-buffer-local 'chess-display-last-selected)

(defun chess-display-select-piece ()
  "Select the piece under the cursor.
Clicking once on a piece selects it; then click on the target location."
  (interactive)
  (let ((coord (get-text-property (point) 'chess-coord)))
    (when coord
      (if chess-display-last-selected
	  (let ((last-sel chess-display-last-selected))
	    ;; if they select the same square again, just deselect it
	    (if (/= (point) (car last-sel))
		(chess-display-move
		 nil (chess-ply-create (chess-display-position nil)
				       (cadr last-sel) coord))
	      ;; put the board back to rights
	      (chess-display-update nil))
	    (setq chess-display-last-selected nil))
	(setq chess-display-last-selected (list (point) coord))
	;; just as in a normal chess game, if you touch the piece,
	;; your opponent will see this right away
	(chess-display-highlight nil coord 'selected)))))

(defun chess-display-mouse-select-piece (event)
  "Select the piece the user clicked on."
  (interactive "e")
  (cond ((fboundp 'event-window)	; XEmacs
	 (set-buffer (window-buffer (event-window event)))
	 (and (event-point event) (goto-char (event-point event))))
	((fboundp 'posn-window)		; Emacs
	 (set-buffer (window-buffer (posn-window (event-start event))))
	 (goto-char (posn-point (event-start event)))))
  (chess-display-select-piece))

(provide 'chess-display)

;;; chess-display.el ends here
