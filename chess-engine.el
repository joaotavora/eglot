;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Obtain movements and other information from an engine
;;

;;; Commentary:

(require 'chess-module)

(defgroup chess-engine nil
  "Code for reading movements and other commands from an engine."
  :group 'chess)

(defvar chess-engine-regexp-alist nil)
(defvar chess-engine-response-handler nil)
(defvar chess-engine-current-marker nil)
(defvar chess-engine-pending-offer nil)
(defvar chess-engine-pending-arg nil)
(defvar chess-engine-opponent-name nil)

(make-variable-buffer-local 'chess-engine-regexp-alist)
(make-variable-buffer-local 'chess-engine-response-handler)
(make-variable-buffer-local 'chess-engine-current-marker)
(make-variable-buffer-local 'chess-engine-pending-offer)
(make-variable-buffer-local 'chess-engine-pending-arg)
(make-variable-buffer-local 'chess-engine-opponent-name)

(defvar chess-engine-process nil)
(defvar chess-engine-last-pos nil)
(defvar chess-engine-working nil)
(defvar chess-engine-handling-event nil)

(make-variable-buffer-local 'chess-engine-process)
(make-variable-buffer-local 'chess-engine-last-pos)
(make-variable-buffer-local 'chess-engine-working)

(defvar chess-engine-inhibit-auto-pass nil)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(chess-message-catalog 'english
  '((invalid-fen    . "Received invalid FEN string: %s")
    (invalid-pgn    . "Received invalid PGN text")
    (now-black	    . "Your opponent played the first move, you are now black")
    (move-passed    . "Your opponent has passed the move to you")
    (want-to-play   . "Do you wish to play a chess game against %s? ")
    (want-to-play-a . "Do you wish to play a chess game against an anonymous opponent? ")
    (opp-quit	    . "Your opponent has quit playing")
    (opp-resigned   . "Your opponent has resigned")
    (opp-draw	    . "Your opponent offers a draw, accept? ")
    (opp-abort	    . "Your opponent wants to abort this game, accept? ")
    (opp-undo	    . "Your opponent wants to take back %d moves, accept? ")
    (opp-ready	    . "Your opponent, %s, is now ready to play")
    (opp-ready-a    . "Your opponent is ready to play; pass or make your move")
    (opp-draw-acc   . "Your draw offer was accepted")
    (opp-abort-acc  . "Your offer to abort was accepted")
    (opp-undo-acc   . "Request to undo %d moves was accepted")
    (opp-draw-dec   . "Your draw offer was declined")
    (opp-abort-dec  . "Your offer to abort was declined")
    (opp-undo-dec   . "Your request to undo %d moves was decline")
    (opp-draw-ret   . "Your opponent has retracted their draw offer")
    (opp-abort-ret  . "Your opponent has retracted their offer to abort")
    (opp-undo-ret   . "Your opponent has retracted their request to undo %d moves")
    (opp-illegal    . "Your opponent states your last command was illegal")
    (opp-call-flag  . "Your flag fell, and your opponent has called time")
    (opp-flag-fell  . "Your opponent has forfeited the game on time")
    (failed-start   . "Failed to start chess engine process")))

(defsubst chess-engine-convert-algebraic (move &optional trust-check)
  "Convert algebraic move to a ply in reference to the engine position.
If conversion fails, this function fired an 'illegal event."
  (or (chess-algebraic-to-ply (chess-engine-position nil) move trust-check)
      (chess-engine-command nil 'illegal)))

(defsubst chess-engine-convert-fen (fen)
  (or (chess-fen-to-pos fen)
      (ignore (chess-message 'invalid-fen fen))))

(defsubst chess-engine-convert-pgn (pgn)
  (or (chess-pgn-to-game pgn)
      (ignore (chess-message 'invalid-pgn))))

(defun chess-engine-default-handler (event &rest args)
  "Default engine response handler."
  (let ((game (chess-engine-game nil)))
    (cond
     ((eq event 'move)
      (let ((chess-engine-handling-event t))
	(when (and (car args)
		   (chess-game-data game 'active))
	  ;; if the game index is still 0, then our opponent
	  ;; is white, and we need to pass over the move
	  (when (and (not chess-engine-inhibit-auto-pass)
		     (chess-game-data game 'my-color)
		     (zerop (chess-game-index game)))
	    (chess-game-set-tag game "White" chess-engine-opponent-name)
	    (chess-game-set-tag game "Black" chess-full-name)
	    (chess-message 'now-black)
	    (chess-game-run-hooks game 'pass)
	    ;; if no one else flipped my-color, we'll do it
	    (if (chess-game-data game 'my-color)
		(chess-game-set-data game 'my-color nil)))

	  (chess-game-move game (car args))

	  (if (chess-game-over-p game)
	      (chess-game-set-data game 'active nil))
	  t)))

     ((eq event 'pass)
      (when (chess-game-data game 'active)
	(chess-message 'move-passed)
	t))

     ((eq event 'match)
      (if (chess-game-data game 'active)
	  (chess-engine-command nil 'busy)
	(let ((name (and (> (length (car args)) 0) (car args))))
	  (if (y-or-n-p (if name
			    (chess-string 'want-to-play (car args))
			  (chess-string 'want-to-play-a)))
	      (progn
		(setq chess-engine-opponent-name (or name "Anonymous"))
		(let ((chess-engine-handling-event t))
		  (chess-engine-set-position nil))
		(chess-engine-command nil 'accept name))
	    (chess-engine-command nil 'decline))))
      t)

     ((eq event 'setup-pos)
      (when (car args)
	;; we don't want the `setup-game' event coming back to us
	(let ((chess-engine-handling-event t))
	  (chess-engine-set-position nil (car args) t))
	t))

     ((eq event 'setup-game)
      (when (car args)
	;; we don't want the `setup-game' event coming back to us
	(let ((chess-engine-handling-event t)
	      (chess-game-inhibit-events t))
	  (chess-engine-set-game nil (car args))
	  (chess-game-set-data game 'active t)
	  (if (string= chess-full-name
		       (chess-game-tag game "White"))
	      (chess-game-set-data game 'my-color t)
	    (chess-game-set-data game 'my-color nil)))
	t))

     ((eq event 'quit)
      (chess-message 'opp-quit)
      (let ((chess-engine-handling-event t))
	(chess-game-set-data game 'active nil))
      t)

     ((eq event 'resign)
      (let ((chess-engine-handling-event t))
	(chess-message 'opp-resigned)
	(chess-game-end game :resign)
	t))

     ((eq event 'draw)
      (if (y-or-n-p (chess-string 'opp-draw))
	  (progn
	    (let ((chess-engine-handling-event t))
	      (chess-game-end game :drawn)
	      (chess-game-set-data game 'active nil))
	    (chess-engine-command nil 'accept))
	(chess-engine-command nil 'decline))
      t)

     ((eq event 'abort)
      (if (y-or-n-p (chess-string 'opp-abort))
	  (progn
	    (let ((chess-engine-handling-event t))
	      (chess-game-end game :aborted)
	      (chess-game-set-data game 'active nil))
	    (chess-engine-command nil 'accept))
	(chess-engine-command nil 'decline))
      t)

     ((eq event 'undo)
      (if (y-or-n-p (chess-string 'opp-undo (car args)))
	  (progn
	    (let ((chess-engine-handling-event t))
	      (chess-game-undo game (car args)))
	    (chess-engine-command nil 'accept))
	(chess-engine-command nil 'decline))
      t)

     ((eq event 'accept)
      (when chess-engine-pending-offer
	(if (eq chess-engine-pending-offer 'match)
	    (unless (chess-game-data game 'active)
	      (let ((name (and (> (length (car args)) 0)
			       (car args))))
		(if name
		    (chess-message 'opp-ready (car args))
		  (chess-message 'opp-ready-a))
		(setq chess-engine-opponent-name (or name "Anonymous"))
		(let ((chess-engine-handling-event t))
		  (chess-engine-set-position nil))))
	  (let ((chess-engine-handling-event t))
	    (cond
	     ((eq chess-engine-pending-offer 'draw)
	      (chess-message 'opp-draw-acc)
	      (chess-game-end game :drawn)
	      (chess-game-set-data game 'active nil))

	     ((eq chess-engine-pending-offer 'abort)
	      (chess-message 'opp-abort-acc)
	      (chess-game-end game :aborted)
	      (chess-game-set-data game 'active nil))

	     ((eq chess-engine-pending-offer 'undo)
	      (chess-message 'opp-undo-acc chess-engine-pending-arg)
	      (chess-game-undo game chess-engine-pending-arg))
	     ((eq chess-engine-pending-offer 'my-undo)
	      (chess-game-undo game (car args))))))
	(setq chess-engine-pending-offer nil
	      chess-engine-pending-arg nil)
	t))

     ((eq event 'decline)
      (when chess-engine-pending-offer
	(cond
	 ((eq chess-engine-pending-offer 'draw)
	  (chess-message 'opp-draw-dec))

	 ((eq chess-engine-pending-offer 'abort)
	  (chess-message 'opp-abort-dec))

	 ((eq chess-engine-pending-offer 'undo)
	  (chess-message 'opp-undo-dec chess-engine-pending-arg)))

	(setq chess-engine-pending-offer nil
	      chess-engine-pending-arg nil)
	t))

     ((eq event 'retract)
      (when chess-engine-pending-offer
	(cond
	 ((eq chess-engine-pending-offer 'draw)
	  (chess-message 'opp-draw-ret))

	 ((eq chess-engine-pending-offer 'abort)
	  (chess-message 'opp-abort-ret))

	 ((eq chess-engine-pending-offer 'undo)
	  (chess-message 'opp-undo-ret chess-engine-pending-arg)))

	(setq chess-engine-pending-offer nil
	      chess-engine-pending-arg nil)
	t))

     ((eq event 'illegal)
      (chess-message 'opp-illegal)
      (let ((chess-engine-handling-event t))
	(chess-game-undo game 1)))

     ((eq event 'call-flag)
      (let ((remaining
	     (if (car args)
		 -1
	       (chess-game-data game (if (chess-game-data game 'my-color)
					 'white-remaining
				       'black-remaining)))))
	(when (< remaining 0)
	  (chess-message 'opp-call-flag)
	  (chess-game-run-hooks game 'flag-fell))))

     ((eq event 'flag-fell)
      (chess-message 'opp-flag-fell)
      (chess-game-end game :flag-fell)
      (chess-game-set-data game 'active nil))

     ((eq event 'kibitz)
      (let ((chess-engine-handling-event t))
	(chess-game-run-hooks game 'kibitz (car args))))

     ((eq event 'chat)
      (let ((chess-engine-handling-event t))
	(chess-game-run-hooks game 'chat (car args)))))))

(defun chess-engine-create (module game &optional response-handler
				 &rest handler-ctor-args)
  "Create a new chess engine MODULE (a symbol) associated with GAME.
Optionally supply a new RESPONSE-HANDLER."
  (let* ((engine (apply 'chess-module-create module game nil
			handler-ctor-args)))
    (when engine
      (with-current-buffer engine
	(setq chess-engine-regexp-alist
	      (copy-alist
	       (symbol-value
		(let ((sym (intern-soft (concat (symbol-name module) "-regexp-alist"))))
		  (when (boundp sym) sym))))
	      chess-engine-response-handler
	      (or response-handler 'chess-engine-default-handler))
	(let ((proc chess-engine-process))
	  (when (and proc (processp proc))
	    (unless (memq (process-status proc) '(run open listen))
	      (chess-error 'failed-start))
	    (unless (process-filter proc)
	      (set-process-filter proc 'chess-engine-filter)))
	  (setq chess-engine-current-marker (point-marker))
	  (chess-game-set-data game 'engine (current-buffer)))))))

(defalias 'chess-engine-destroy 'chess-module-destroy)

(defun chess-engine-command (engine event &rest args)
  "Call the handler of ENGINE with EVENT (a symbol) and ARGS."
  (chess-with-current-buffer engine
    (apply chess-module-event-handler chess-module-game event args)))

;; 'ponder
;; 'search-depth
;; 'wall-clock

(defun chess-engine-set-option (engine option value)
  "Set ENGINE OPTION to VALUE by invoking its handler with the 'set-option
event."
  (chess-with-current-buffer engine
    (chess-engine-command engine 'set-option option value)))

(defun chess-engine-set-response-handler (engine &optional response-handler)
  "Set a new RESPONSE-HANDLER for ENGINE."
  (chess-with-current-buffer engine
    (setq chess-engine-response-handler
	  (or response-handler 'chess-engine-default-handler))))

(defun chess-engine-response-handler (engine)
  "Return the function currently defined as the response-handler for ENGINE."
  (chess-with-current-buffer engine
    chess-engine-response-handler))

(defun chess-engine-set-position (engine &optional position my-color)
  (chess-with-current-buffer engine
    (let ((chess-game-inhibit-events t))
      (if position
	  (progn
	    (chess-game-set-start-position chess-module-game position)
	    (chess-game-set-data chess-module-game 'my-color my-color))
	(chess-game-set-start-position chess-module-game
				       chess-starting-position)
	(chess-game-set-data chess-module-game 'my-color t))
      (chess-game-set-data chess-module-game 'active t))
    (chess-game-run-hooks chess-module-game 'orient)))

(defun chess-engine-position (engine)
  "Return the current position of the game associated with ENGINE."
  (chess-with-current-buffer engine
    (chess-game-pos chess-module-game)))

(defalias 'chess-engine-game 'chess-module-game)
(defalias 'chess-engine-set-game 'chess-module-set-game)
(defalias 'chess-engine-set-game* 'chess-module-set-game*)
(defalias 'chess-engine-index 'chess-module-game-index)

(defun chess-engine-move (engine ply)
  (chess-with-current-buffer engine
    (chess-game-move chess-module-game ply)
    (chess-engine-command engine 'move ply)))

(chess-message-catalog 'english
  '((engine-not-running . "The engine you were using is no longer running")))

(defun chess-engine-send (engine string)
  "Send the given STRING to ENGINE.
If `chess-engine-process' is a valid process object, use `process-send-string'
to submit the data.  Otherwise, the 'send event is triggered and the engine
event handler can take care of the data."
  (chess-with-current-buffer engine
    (let ((proc chess-engine-process))
      (if proc
	  (if (memq (process-status proc) '(run open))
	      (process-send-string proc string)
	    (chess-message 'engine-not-running)
	    (chess-engine-command nil 'destroy))
	(chess-engine-command nil 'send string)))))

(defun chess-engine-submit (engine string)
  "Submit the given STRING, so ENGINE sees it in its input stream."
  (chess-with-current-buffer engine
    (let ((proc chess-engine-process))
      (when (and proc (processp proc)
		 (not (memq (process-status proc) '(run open))))
	(chess-message 'engine-not-running)
	(chess-engine-command nil 'destroy))
      (chess-engine-filter nil string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primary event handler
;;

(defun chess-engine-sentinal (proc event)
  (when (buffer-live-p (process-buffer proc))
    (set-buffer (process-buffer proc))
    (chess-engine-destroy nil)))

(defun chess-engine-filter (proc &optional string)
  "Filter for receiving text for an engine from an outside source."
  (let ((buf (if (and proc (processp proc))
		 (process-buffer proc)
	       (current-buffer)))
	(inhibit-redisplay t)
	last-point last-line-no-newline)
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(if (stringp proc)
	    (setq string proc)
	  (let ((moving (= (point) chess-engine-current-marker)))
	    (save-excursion
	      ;; Insert the text, advancing the marker.
	      (goto-char chess-engine-current-marker)
	      (insert string)
	      (set-marker chess-engine-current-marker (point)))
	    (if moving (goto-char chess-engine-current-marker))))
	(unless chess-engine-working
	  (setq chess-engine-working t)
	  (save-excursion
	    (if chess-engine-last-pos
		(goto-char chess-engine-last-pos)
	      (goto-char (point-min)))
	    (unwind-protect
		(while (and (not (eobp)) (not last-line-no-newline))
		  (let ((case-fold-search nil)
			(triggers chess-engine-regexp-alist)
			last-trigger result)
		    (while triggers
		      ;; this could be accelerated by joining
		      ;; together the regexps
		      (if (and (re-search-forward (caar triggers)
						  (line-end-position) t)
			       (setq result (funcall (cdar triggers))))
			  (progn
			    (when (eq result 'once)
			      (if last-trigger
				  (setcdr last-trigger (cdr triggers))
				(setq chess-engine-regexp-alist
				      (cdr triggers))))
			    (setq triggers nil))
			(setq last-trigger triggers
			      triggers (cdr triggers)))))
		  (if (= (line-end-position) (point-max))
		      (setq last-line-no-newline t)
		    (forward-line)))
	      (setq chess-engine-last-pos (point)
		    chess-engine-working nil))))))))

(provide 'chess-engine)

;;; chess-engine.el ends here
