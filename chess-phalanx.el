;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against phalanx!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defgroup chess-phalanx nil
  "The publically available chess engine 'phalanx'."
  :group 'chess-engine)

(defcustom chess-phalanx-path (executable-find "phalanx")
  "The path to the phalanx executable."
  :type 'file
  :group 'chess-phalanx)

(defvar chess-phalanx-regexp-alist
  (list
   (cons (concat "my move is P?\\(" chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 1))))))
   (cons "Illegal move:\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (signal 'chess-illegal (match-string 1)))))))

(defun chess-phalanx-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let (proc)
      (message "Starting chess program 'phalanx'...")
      (unless chess-phalanx-path
	(error "Cannot find phalanx executable; check `chess-phalanx-path'"))
      (setq proc (start-process "chess-process" (current-buffer)
				chess-phalanx-path))
      (message "Starting chess program 'phalanx'...done")
      (process-send-string proc "nopost\n")
      proc))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n"))

   ((eq event 'ready)
    (and (chess-engine-game nil)
	 (chess-game-set-data (chess-engine-game nil) 'active t)))

   ((eq event 'pass)
    (chess-engine-send nil "go\n"))

   ((memq event '(abort resign))
    (chess-engine-send nil "new\n")
    (and (chess-engine-game nil)
	 (chess-engine-set-start-position nil)))

   ((eq event 'undo)
    (when (chess-engine-game nil)
      (dotimes (i (car args))
	(chess-engine-send nil "undo\n"))
      (chess-game-undo (chess-engine-game nil) (car args))))

   ((eq event 'move)
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))))

(provide 'chess-phalanx)

;;; chess-phalanx.el ends here
