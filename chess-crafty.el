;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against crafty!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defvar chess-crafty-regexp-alist
  (list (cons
	 (concat "\\s-*\\(White\\|Black\\)\\s-*([0-9]+):\\s-+\\("
		 chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (let ((position (chess-engine-position nil))
		  (move (match-string 2))
		  ply)
	      (when (string= (if (chess-pos-side-to-move position)
				 "White" "Black")
			     (match-string 1))
		(setq ply (chess-algebraic-to-ply position move))
		(unless ply
		  (error "Could not convert engine move: %s" move))
		(funcall chess-engine-response-handler 'move ply))))))
	(cons "Illegal move:\\s-*\\(.*\\)"
	      (function
	       (lambda ()
		 (signal 'chess-illegal (match-string 1)))))))

(defun chess-crafty-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let (proc)
      (message "Starting chess program 'crafty'...")
      (setq proc (start-process "chess-process" (current-buffer)
				(or (executable-find "crafty")
				    (executable-find "wcrafty"))))
      (message "Starting chess program 'crafty'...done")
      (process-send-string proc (concat "display nogeneral\n"
					"display nochanges\n"
					"display noextstats\n"
					"display nohashstats\n"
					"display nomoves\n"
					"display nonodes\n"
					"display noply1\n"
					"display nostats\n"
					"display notime\n"
					"display novariation\n"
					"alarm off\n"
					"ansi off\n"))
      proc))
   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n"))
   ((eq event 'setup)
    (chess-engine-send nil (format "setboard %s\n"
				   (chess-pos-to-fen (car args)))))
   ((eq event 'pass)
    (chess-engine-send nil "go\n"))
   ((eq event 'move)
    (chess-engine-send
     nil (concat (chess-ply-to-algebraic
		  (car args) nil
		  (chess-engine-search-function nil)) "\n")))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
