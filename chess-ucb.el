;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Engine interface to the Novag Universal Chess Board
;;
;; jww (2002-04-25): This code has not been tested yet, since I don't
;; have access to a UCB.  If anybody wants to donate one, or the money
;; for one ($300), I would be happy to correct this module.  :)
;;

(require 'chess-common)

(defgroup chess-ucb nil
  "Interface to the Novag Universal Chess Board."
  :group 'chess-engine)

(defcustom chess-ucb-device "/dev/ttyS0"
  "The serial device used to talk to the Novag UCB."
  :type 'file
  :group 'chess-ucb)

(defvar chess-ucb-handling-event nil)

(defvar chess-ucb-regexp-alist
  (list
   (cons "^M\\(..\\)\\(..\\)\\(/\\([QRNB]\\)\\)?\r\n"
	 (function
	  (lambda ()
	    (let ((move (concat (match-string 1)
				"-"
				(match-string 2)))
		  (promote (match-string 4)))
	      (if promote
		  (setq move (concat move "=" promote)))
	      (setq move (chess-engine-convert-algebraic move))
	      ;; I don't use the usual engine logic for this, since
	      ;; technically the UCB is just an input interface, not a
	      ;; true engine.
	      (let ((chess-ucb-handling-event t))
		(chess-game-move (chess-engine-game nil) move))))))))

(defun chess-ucb-handler (game event &rest args)
  (unless chess-ucb-handling-event
    (cond
     ((eq event 'initialize)
      (when (file-exists-p chess-ucb-device)
	;; jww (2002-04-25): cat is not bidirectional, so I need
	;; something like "nc" that can talk with characters devices
	;; at 9600 8N1.
	(setq chess-engine-process
	      (start-process "*chess-ucb*" (current-buffer)
			     (executable-find "cat") chess-ucb-device))
	t))

     ((memq event 'orient)
      (chess-engine-send nil "N\r\n")
      (chess-engine-set-position nil)

      ;; jww (2002-04-25): What happens if we're orienting to a
      ;; non-standard starting position?  How do we inform the UCB of
      ;; the new position?  If it doesn't test move legality, I
      ;; suppose we could just move all the pieces around one by
      ;; one...
      (unless (eq chess-starting-position (chess-engine-position nil))
	nil))

     ((eq event 'undo)
      (dotimes (i (car args))
	(chess-engine-send nil "T\r\n"))
      ;; prevent us from handling the `undo' event which this triggers
      (let ((chess-engine-handling-event t))
	(chess-game-undo game (car args))))

     ((eq event 'move)
      (let ((move (chess-ply-to-algebraic (car args) t)))
	(cond
	 ((chess-ply-keyword (car args) :en-passant)
	  (setq move (concat move "ep")))
	 ((chess-ply-keyword (car args) :castle)
	  (if (chess-pos-side-to-move (chess-ply-pos (car args)))
	      (setq move "e1-g1")
	    (setq move "e8-g8")))
	 ((chess-ply-keyword (car args) :long-castle)
	  (if (chess-pos-side-to-move (chess-ply-pos (car args)))
	      (setq move "e1-c1")
	    (setq move "e8-c8"))))
	(chess-engine-send nil (format "M%s\r\n" move)))))))

(provide 'chess-ucb)

;;; chess-ucb.el ends here
