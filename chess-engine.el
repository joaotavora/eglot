;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Obtain movements and other information from an engine
;;
;; $Revision$

;;; Commentary:

(require 'chess-game)
(require 'chess-algebraic)
(require 'chess-fen)

(defgroup chess-engine nil
  "Code for reading movements and other commands from an engine."
  :group 'chess)

(defvar chess-engine-regexp-alist nil)
(defvar chess-engine-event-handler nil)
(defvar chess-engine-response-handler nil)
(defvar chess-engine-current-marker nil)
(defvar chess-engine-position nil)
(defvar chess-engine-game nil)

(make-variable-buffer-local 'chess-engine-regexp-alist)
(make-variable-buffer-local 'chess-engine-event-handler)
(make-variable-buffer-local 'chess-engine-response-handler)
(make-variable-buffer-local 'chess-engine-current-marker)
(make-variable-buffer-local 'chess-engine-position)
(make-variable-buffer-local 'chess-engine-game)

(defvar chess-engine-process nil)
(defvar chess-engine-last-pos nil)
(defvar chess-engine-working nil)
(defvar chess-engine-handling-event nil)

(make-variable-buffer-local 'chess-engine-process)
(make-variable-buffer-local 'chess-engine-last-pos)
(make-variable-buffer-local 'chess-engine-working)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(defmacro chess-with-current-buffer (buffer &rest body)
  `(let ((buf ,buffer))
     (if buf
	 (with-current-buffer buf
	   ,@body)
       ,@body)))

(defun chess-engine-do-move (ply)
  (cond
   (chess-engine-game
    (chess-game-move chess-engine-game ply))
   (chess-engine-position
    (setq chess-engine-position (chess-ply-next-pos ply)))))

(defun chess-engine-default-handler (event &rest args)
  (let ((chess-engine-handling-event t)
	(game (chess-engine-game nil))
	(position (chess-engine-position nil)))
    (cond
     ((eq event 'move)
      (if (null game)
	  (if position
	      (let ((ply
		     (if (stringp (car args))
			 (or (chess-algebraic-to-ply position (car args))
			     (message "Received invalid move from engine: %s"
				      (car args)))
		       (car args))))
		(if ply
		    (setq chess-engine-position (chess-ply-next-pos ply)))
		t))
	(if (chess-game-data game 'active)
	    (let ((ply
		   (if (stringp (car args))
		       (or (chess-algebraic-to-ply position (car args))
			   (message "Received invalid move from engine: %s"
				    (car args)))
		     (car args))))
	      (when ply
		;; if the game index is still 0, then our opponent
		;; is white, and we need to pass over the move
		(when (and game (chess-game-data game 'my-color)
			   (= (chess-game-index game) 0))
		  (message "Your opponent played the first move, you are now black")
		  (chess-game-run-hooks game 'pass)
		  ;; if no one else flipped my-color, we'll do it
		  (if (chess-game-data game 'my-color)
		      (chess-game-set-data game 'my-color nil)))
		(chess-engine-do-move ply))
	      t))))

     ((eq event 'pass)
      (when (and game (chess-game-data game 'active))
	(message "Your opponent has passed the move to you")
	t))

     ((eq event 'connect)
      (if (and game (chess-game-data game 'active))
	  (chess-engine-command nil 'busy)
	(if (y-or-n-p
	     (if (and (car args) (> (length (car args)) 0))
		 (format "Do you wish to play a chess game against %s? "
			 (car args))
	       (format "Do you wish to play a chess game against an anonymous opponent? ")))
	    (chess-engine-command nil 'accept)
	  (chess-engine-send nil 'decline)))
      t)

     ((eq event 'accept)
      (unless (and game (chess-game-data game 'active))
	(if (and (car args) (> (length (car args)) 0))
	    (message "Your opponent, %s, is now ready to play" (car args))
	  (message "Your opponent is now ready to play"))

	;; NOTE: There will be no display for this game object!  This
	;; is really only useful if you are using a computer on the
	;; accepting side
	(unless game
	  (setq game (chess-engine-set-game nil (chess-game-create))))
	(chess-engine-set-start-position engine)
	t))

     ((eq event 'setup-pos)
      (let ((position (if (stringp (car args))
			  (chess-fen-to-pos (car args))
			(car args))))
	(when position
	  (chess-engine-set-start-position nil position t)
	  t)))

     ((eq event 'setup-game)
      (let ((new-game (if (stringp (car args))
			  (chess-pgn-to-game (car args))
			(car args))))
	(when new-game
	  (if (null game)
	      (chess-engine-set-game nil new-game)
	    (let ((chess-game-inhibit-events t))
	      (chess-engine-copy-game nil new-game)
	      (chess-game-set-data game 'active t)
	      (if (string= chess-full-name (chess-game-tag game "White"))
		  (chess-game-set-data game 'my-color t)
		(chess-game-set-data game 'my-color nil)))
	    (chess-game-run-hooks game 'orient))
	  t)))

     ((eq event 'quit)
      (message "Your opponent has quit playing")
      (if game
	  (chess-game-set-data game 'active nil))
      t)

     ((eq event 'resign)
      (when game
	(chess-game-resign game)
	(chess-game-set-data game 'active nil)
	t)))))

(defun chess-engine-set-start-position (engine &optional position my-color)
  (chess-with-current-buffer engine
    (let ((game (chess-engine-game nil)))
      (if (null game)
	  (chess-engine-set-position nil (or position
					     chess-starting-position))
	(let ((chess-game-inhibit-events t))
	  (if position
	      (progn
		(chess-game-set-start-position game position)
		(chess-game-set-data game 'my-color my-color))
	    (chess-game-set-start-position game chess-starting-position)
	    (chess-game-set-data game 'my-color t))
	  (chess-game-set-data game 'active t))
	(chess-game-run-hooks game 'orient)))))

(defun chess-engine-create (module &optional response-handler &rest args)
  (let ((regexp-alist (intern-soft (concat (symbol-name module)
					   "-regexp-alist")))
	(handler (intern-soft (concat (symbol-name module) "-handler"))))
    (with-current-buffer (generate-new-buffer " *chess-engine*")
      (let ((proc (apply handler 'initialize args)))
	(setq chess-engine-regexp-alist (symbol-value regexp-alist)
	      chess-engine-event-handler handler
	      chess-engine-response-handler
	      (or response-handler 'chess-engine-default-handler))
	(when (processp proc)
	  (unless (memq (process-status proc) '(run open))
	    (error "Failed to start chess engine process"))
	  (setq chess-engine-process proc)
	  (set-process-buffer proc (current-buffer))
	  (set-process-filter proc 'chess-engine-filter))
	(setq chess-engine-current-marker (point-marker)))
      (add-hook 'kill-buffer-hook 'chess-engine-on-kill nil t)
      (current-buffer))))

(defun chess-engine-on-kill ()
  "Function called when the buffer is killed."
  (chess-engine-command (current-buffer) 'shutdown))

(defun chess-engine-destroy (engine)
  (let ((buf (or engine (current-buffer))))
    (when (buffer-live-p buf)
      (chess-engine-command engine 'destroy)
      (remove-hook 'kill-buffer-hook 'chess-engine-on-kill t)
      (kill-buffer buf))))

(defun chess-engine-command (engine event &rest args)
  (chess-with-current-buffer engine
    (apply 'chess-engine-event-handler
	   (chess-engine-game nil) engine event args)))

;; 'ponder
;; 'search-depth
;; 'wall-clock

(defun chess-engine-set-option (engine option value)
  (chess-with-current-buffer engine
    ))

(defun chess-engine-option (engine option) 'ponder 'search-depth 'wall-clock
  (chess-with-current-buffer engine
    ))

(defun chess-engine-set-position (engine position)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-engine-detach-game nil))
    (setq chess-engine-game nil
	  chess-engine-position position)
    (chess-engine-command nil 'setup-pos position)))

(defun chess-engine-position (engine)
  (chess-with-current-buffer engine
    (or (and chess-engine-game
	     (chess-game-pos chess-engine-game))
	chess-engine-position)))

(defun chess-engine-set-game (engine game &optional no-setup)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-engine-detach-game nil))
    (setq chess-engine-game game
	  chess-engine-position nil)
    (chess-game-add-hook game 'chess-engine-event-handler engine)
    (unless no-setup
      (chess-engine-command nil 'setup-game game))))

(defsubst chess-engine-set-game* (engine game)
  "This function is a special variant of `chess-engine-set-game'.
It should be used only if:
  ENGINE is an engine which is newly created, and has not been used.
  GAME is a new game at the starting position, which has not been used.

This function exists because all engines start out assuming the
starting position, which in effect means that `setup-game's work has
already been done, and therefore does not need to be duplicated.

There is no harm in calling `chess-engine-set-game' instead of this
function in all cases; this is merely a bandwidth-saver."
  (chess-engine-set-game engine game t))

(defun chess-engine-copy-game (engine game)
  (chess-with-current-buffer engine
    (if (null chess-engine-game)
	(chess-engine-set-game nil game)
      (chess-game-set-tags chess-engine-game game)
      ;; this call triggers `setup-game' for us
      (chess-game-set-plies chess-engine-game game))))

(defun chess-engine-detach-game (engine)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-remove-hook chess-engine-game
				'chess-engine-event-handler
				(or engine (current-buffer))))))

(defun chess-engine-game (engine)
  (chess-with-current-buffer engine
    chess-engine-game))

(defun chess-engine-index (engine)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-index chess-engine-game))))

(defun chess-engine-move (engine ply)
  (chess-with-current-buffer engine
    (chess-engine-do-move ply)
    (chess-engine-command engine 'move ply)))

(defun chess-engine-pass (engine)
  (chess-with-current-buffer engine
    (chess-engine-command engine 'pass)))

(defun chess-engine-send (engine string)
  "Send the given STRING to ENGINE."
  (chess-with-current-buffer engine
    (let ((proc chess-engine-process))
      (if proc
	  (if (memq (process-status proc) '(run open))
	      (process-send-string proc string)
	    (message "The engine you were using is no longer running")
	    (chess-engine-command nil 'destroy))
	(chess-engine-command nil 'send string)))))

(defun chess-engine-submit (engine string)
  "Submit the given STRING, so ENGINE sees it in its input stream."
  (chess-with-current-buffer engine
    (let ((proc chess-engine-process))
      (when (and (processp proc)
		 (not (memq (process-status proc) '(run open))))
	(message "The engine you were using is no longer running")
	(chess-engine-command nil 'destroy))
      (chess-engine-filter nil string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primary event handler
;;

(defun chess-engine-event-handler (game engine event &rest args)
  "Handle any commands being sent to this instance of this module."
  (unless chess-engine-handling-event
    (if (buffer-live-p engine)
	(with-current-buffer engine
	  (assert (eq game (chess-engine-game nil)))
	  (apply chess-engine-event-handler event args)))
    (cond
     ((eq event 'shutdown)
      (chess-engine-destroy engine))

     ((eq event 'destroy)
      (chess-engine-detach-game engine)))))

(defun chess-engine-filter (proc string)
  "Filter for receiving text for an engine from an outside source."
  (let ((buf (if (processp proc)
		 (process-buffer proc)
	       (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(let ((moving (= (point) chess-engine-current-marker)))
	  (save-excursion
	    ;; Insert the text, advancing the marker.
	    (goto-char chess-engine-current-marker)
	    (insert string)
	    (set-marker chess-engine-current-marker (point)))
	  (if moving (goto-char chess-engine-current-marker)))
	(unless chess-engine-working
	  (setq chess-engine-working t)
	  (unwind-protect
	      (save-excursion
		(if chess-engine-last-pos
		    (goto-char chess-engine-last-pos)
		  (goto-char (point-min)))
		(beginning-of-line)
		(while (not (eobp))
		  (let ((triggers chess-engine-regexp-alist))
		    (while triggers
		      ;; this could be accelerated by joining
		      ;; together the regexps
		      (if (and (looking-at (caar triggers))
			       (funcall (cdar triggers)))
			  (setq triggers nil)
			(setq triggers (cdr triggers)))))
		  (forward-line)))
	    (setq chess-engine-last-pos (point)
		  chess-engine-working nil)))))))

(provide 'chess-engine)

;;; chess-engine.el ends here
