;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against gnuchess!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defvar chess-gnuchess-now-moving nil)

(defvar chess-gnuchess-regexp-alist
  (list (cons (concat "My move is : \\(" chess-algebraic-regexp "\\)")
	      (function
	       (lambda ()
		 (let* ((move (match-string 1))
			(ply (chess-algebraic-to-ply
			      (chess-engine-position nil) move)))
		   (unless ply
		     (error "Could not convert engine move: %s" move))
		   (let ((chess-gnuchess-now-moving t))
		     (funcall chess-engine-response-handler 'move ply))))))
	(cons "Illegal move:"
	      (function
	       (lambda ()
		 (signal 'chess-illegal "Illegal move"))))))

(defun chess-gnuchess-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let (proc)
      (message "Starting chess program 'gnuchess'...")
      (setq proc (start-process "chess-process" (current-buffer)
				(executable-find "gnuchess")))
      (message "Starting chess program 'gnuchess'...done")
      proc))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n"))

   ((eq event 'setup)
    (chess-engine-send nil (format "setboard %s\n"
				   (chess-pos-to-fen (car args)))))

   ((eq event 'pass)
    (chess-engine-send nil "go\n"))

   ((eq event 'move)
    (unless chess-gnuchess-now-moving
      (chess-engine-send nil (concat (chess-ply-to-algebraic
				      (car args) nil
				      (chess-engine-search-function nil))
				     "\n"))))))

(provide 'chess-gnuchess)

;;; chess-gnuchess.el ends here
