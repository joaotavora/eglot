;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against gnuchess!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defgroup chess-gnuchess nil
  "The publically available chess engine 'gnuchess'."
  :group 'chess-engine)

(defcustom chess-gnuchess-path (executable-find "gnuchess")
  "The path to the gnuchess executable."
  :type 'file
  :group 'chess-gnuchess)

(defvar chess-gnuchess-temp-files nil)
(defvar chess-gnuchess-bad-board nil)
(make-variable-buffer-local 'chess-gnuchess-temp-files)
(make-variable-buffer-local 'chess-gnuchess-bad-board)

(defvar chess-gnuchess-regexp-alist
  (list (cons (concat "My move is : \\(" chess-algebraic-regexp "\\)")
	      (function
	       (lambda ()
		 (funcall chess-engine-response-handler 'move
			  (match-string 1)))))
	(cons "Illegal move:"
	      (function
	       (lambda ()
		 (signal 'chess-illegal "Illegal move"))))
	(cons "Board is wrong!"
	      (function
	       (lambda ()
		 ;; gnuchess didn't like the given position, which
		 ;; means it won't play against it unless we send a
		 ;; "go" after the user's move
		 (setq chess-gnuchess-bad-board t))))))

(defun chess-gnuchess-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let (proc)
      (message "Starting chess program 'gnuchess'...")
      (unless chess-gnuchess-path
	(error "Cannot find gnuchess executable; check `chess-gnuchess-path'"))
      (setq proc (start-process "chess-process" (current-buffer)
				chess-gnuchess-path))
      (message "Starting chess program 'gnuchess'...done")
      (process-send-string proc "nopost\n")
      proc))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n")
    (dolist (file chess-gnuchess-temp-files)
      (if (file-exists-p file)
	  (delete-file file))))

   ((eq event 'ready)
    (let ((game (chess-engine-game nil)))
      (if game
	  (chess-game-set-data game 'active t))))

   ((eq event 'setup-pos)
    (if (equal (car args) chess-starting-position)
	(chess-engine-send nil "new\n")
      (let ((file (make-temp-file "gch")))
	(with-temp-file file
	  (insert (chess-pos-to-string (car args)) ?\n))
	(chess-engine-send nil (format "epdload %s\n" file))
	(push file chess-gnuchess-temp-files))))

   ((eq event 'setup-game)
    (let ((file (make-temp-file "gch")))
      (with-temp-file file
	(insert (chess-game-to-string (car args)) ?\n))
      (chess-engine-send nil (format "pgnload %s\n" file))
      (push file chess-gnuchess-temp-files)))

   ((eq event 'pass)
    (chess-engine-send nil (concat (if (chess-pos-side-to-move
					(chess-engine-position nil))
				       "white" "black")
				   "\n"))
    (chess-engine-send nil "go\n")
    (setq chess-gnuchess-bad-board nil))

   ((eq event 'move)
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n"))
    (when chess-gnuchess-bad-board
      (chess-engine-send nil "go\n")
      (setq chess-gnuchess-bad-board nil)))))

(provide 'chess-gnuchess)

;;; chess-gnuchess.el ends here
