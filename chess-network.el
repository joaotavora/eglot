;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against an opponent over the network
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defvar chess-network-now-moving nil)

(defvar chess-network-regexp-alist
  (list (cons (concat "<move color=\"\\(black\\|white\\)\">"
		      chess-algebraic-regexp "</move>")
	      'chess-network-perform-move)
	(cons "<pass/>"
	      (function
	       (lambda ()
		 (message "Your opponent has passed the move to you"))))
	(cons "<connect name=\"\\(.*\\)\"/>"
	      (function
	       (lambda ()
		 ;; jww (2002-04-07): Set the appropriate Black or
		 ;; White tag at this point
		 (message "Your opponent, %s, has connected"
			  (match-string 1)))))
	(cons "<setup fen=\"\\(.*\\)\"/>"
	      (function
	       (lambda ()
		 (let* ((position (chess-fen-to-pos (match-string 1)))
			(ply (chess-ply-create position)))
		   (chess-game-set-plies (chess-engine-game nil)
					 (list ply))))))
	(cons "<quit/>"
	      (function
	       (lambda ()
		 (message "Your opponent has quit playing"))))))

(defun chess-network-perform-move ()
  (let ((position (chess-engine-position nil))
	(move (match-string 2)) ply)
    (when (string= (if (chess-pos-side-to-move position)
		       "white" "black")
		   (match-string 1))
      (setq ply (chess-algebraic-to-ply position move))
      (unless ply
	(error "Could not convert engine move: %s" move))
      (let ((chess-network-now-moving t))
	(funcall chess-engine-response-handler 'move ply)))))

(defun chess-network-handler (event &rest args)
  "Initialize the network chess engine."
  (cond
   ((eq event 'initialize)
    (let ((which (read-char "Are you the c)lient or s)erver? "))
	  proc)
      (message "Starting network client/server...")
      (setq proc (if (eq which ?s)
		     (start-process "*chess-network*"
				    (current-buffer) "/usr/bin/nc"
				    "-l" "-p" (read-string "Port: "))
		   (open-network-stream "*chess-network*" (current-buffer)
					(read-string "Host: ")
					(read-string "Port: "))))
      (if (eq which ?s)
	  (message "Now waiting for your opponent to connect...")
	(process-send-string proc (format "CONNECT %s\n" (user-full-name)))
	(message "You have connected; pass now or make your move."))
      proc))

   ((eq event 'shutdown)
    (ignore-errors
     (chess-engine-send nil "<quit/>\n")))

   ((eq event 'setup)
    (chess-engine-send nil (format "<setup fen=\"%s\">\n"
				   (chess-pos-to-fen (car args)))))

   ((eq event 'pass)
    (chess-engine-send nil "<pass/>\n"))

   ((eq event 'move)
    (unless chess-network-now-moving
      (chess-engine-send
       nil (concat "<move color=\""
		   (if (chess-pos-side-to-move (chess-ply-pos (car args)))
		       "white"
		     "black")
		   "\">"
		   (chess-ply-to-algebraic (car args))
		   "</move>\n"))))))

(provide 'chess-network)

;;; chess-network.el ends here
