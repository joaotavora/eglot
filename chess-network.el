;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against an opponent over the network
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defvar chess-network-regexp-alist
  (list
   (cons (concat chess-algebraic-regexp "$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 0))))))
   (cons "chess match\\(\\s-+\\(.+\\)\\)?$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'match
		     (match-string 2)))))
   (cons "fen\\s-+\\(.+\\)"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'setup-pos
		     (chess-engine-convert-fen (match-string 1))))))
   (cons "pgn\\s-+\\(.+\\)"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'setup-game
		     (chess-engine-convert-pgn (match-string 1))))))
   (cons "pass$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'pass))))
   (cons "quit$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'quit))))
   (cons "resign$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'resign))))
   (cons "draw$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'draw))))
   (cons "abort$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'abort))))
   (cons "takeback\\s-+\\([0-9]+\\)$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'undo
		     (string-to-int (match-string 1))))))
   (cons "accept\\(\\s-+\\(.+\\)\\)?$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'accept
		     (match-string 2)))))
   (cons "decline$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'decline))))
   (cons "retract$"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'retract))))))

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
	(chess-network-handler 'match)
	(message "You have connected; pass now or make your move."))
      proc))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n"))

   ((eq event 'setup-pos)
    (chess-engine-send nil (format "fen %s\n"
				   (chess-pos-to-string (car args)))))

   ((eq event 'setup-game)
    (chess-engine-send nil (format "pgn %s\n"
				   (chess-game-to-string (car args)))))

   ((eq event 'pass)
    (chess-engine-send nil "pass\n"))

   ((eq event 'busy)
    (chess-engine-send nil "playing\n"))

   ((eq event 'match)
    (setq chess-engine-pending-offer 'match)
    (chess-engine-send nil (format "chess match %s\n" chess-full-name)))

   ((eq event 'resign)
    (chess-engine-send nil "resign\n"))

   ((eq event 'draw)
    (if chess-engine-pending-offer
	(chess-engine-command nil 'retract))
    (setq chess-engine-pending-offer 'draw)
    (chess-engine-send nil "draw\n"))

   ((eq event 'abort)
    (if chess-engine-pending-offer
	(chess-engine-command nil 'retract))
    (setq chess-engine-pending-offer 'abort)
    (chess-engine-send nil "abort\n"))

   ((eq event 'undo)
    (if chess-engine-pending-offer
	(chess-engine-command nil 'retract))
    (setq chess-engine-pending-offer 'undo
	  chess-engine-pending-arg (car args))
    (chess-engine-send nil (format "takeback %d\n" (car args))))

   ((eq event 'accept)
    (chess-engine-send nil "accept\n"))

   ((eq event 'decline)
    (chess-engine-send nil "decline\n"))

   ((eq event 'retract)
    (chess-engine-send nil "retract\n"))

   ((eq event 'move)
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))))

(provide 'chess-network)

;;; chess-network.el ends here
