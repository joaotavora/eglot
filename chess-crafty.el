;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against crafty!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-fen)
(require 'chess-algebraic)

(defgroup chess-crafty nil
  "The publically available chess engine 'crafty'."
  :group 'chess-engine)

(defcustom chess-crafty-path (or (executable-find "crafty")
				 (executable-find "wcrafty"))
  "The path to the crafty executable."
  :type 'file
  :group 'chess-crafty)

(defvar chess-crafty-temp-files nil)
(make-variable-buffer-local 'chess-crafty-temp-files)

(defvar chess-crafty-regexp-alist
  (list
   (cons (concat "\\(White\\|Black\\)\\s-*([0-9]+):\\s-+\\("
		 chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 2))))))
   (cons "\\(Illegal move\\|unrecognized/illegal command\\):\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (signal 'chess-illegal (match-string 1)))))))

(defun chess-crafty-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let (proc)
      (message "Starting chess program 'crafty'...")
      (unless chess-crafty-path
	(error "Cannot find crafty executable; check `chess-crafty-path'"))
      (setq proc (start-process "chess-process" (current-buffer)
				chess-crafty-path))
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
    (chess-engine-send nil "quit\n")
    (dolist (file chess-crafty-temp-files)
      (if (file-exists-p file)
	  (delete-file file))))

   ((eq event 'ready)
    (and (chess-engine-game nil)
	 (chess-game-set-data (chess-engine-game nil) 'active t)))

   ((eq event 'setup-pos)
    (chess-engine-send nil (format "setboard %s\n"
				   (chess-pos-to-fen (car args)))))

   ((eq event 'setup-game)
    (let ((file (make-temp-file "cra")))
      (with-temp-file file
	(insert (chess-game-to-string (car args)) ?\n))
      (chess-engine-send nil (format "read %s\n" file))
      (push file chess-crafty-temp-files)))

   ((eq event 'pass)
    (chess-engine-send nil "go\n"))

   ((memq event '(abort resign))
    (chess-engine-send nil "new\n")
    (and (chess-engine-game nil)
	 (chess-engine-set-start-position nil)))

   ((eq event 'draw)
    (chess-engine-default-handler 'decline-draw))

   ((eq event 'undo)
    (when (chess-engine-game nil)
      (dotimes (i (car args))
	(chess-engine-send nil "undo\n"))
      (chess-game-undo (chess-engine-game nil) (car args))))

   ((eq event 'move)
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
