;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against crafty!
;;
;; $Revision$

(require 'chess-engine)
(require 'chess-common)

(defgroup chess-crafty nil
  "The publically available chess engine 'crafty'."
  :group 'chess-engine)

(defcustom chess-crafty-path (or (executable-find "crafty")
				 (executable-find "wcrafty"))
  "The path to the crafty executable."
  :type 'file
  :group 'chess-crafty)

(defvar chess-crafty-evaluation nil)

(make-variable-buffer-local 'chess-crafty-evaluation)

(defvar chess-crafty-regexp-alist
  (list
   (cons (concat "\\(White\\|Black\\)\\s-*([0-9]+):\\s-+\\("
		 chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 2) t)))))
   (cons "total evaluation\\.+\\s-+\\([-+0-9.]+\\)"
	 (function
	  (lambda ()
	    (setq chess-crafty-evaluation
		  (string-to-number (match-string 1))))))
   (cons "\\(Illegal move\\|unrecognized/illegal command\\):\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (error (match-string 1)))))
   (cons "command not legal now"
	 (function
	  (lambda ()
	    (error (match-string 0)))))))

(defun chess-crafty-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-common-handler game 'initialize "crafty")))
	(when (and (processp proc)
		   (eq (process-status proc) 'run))
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
	  t)))

     ((eq event 'setup-pos)
      (chess-engine-send nil (format "setboard %s\n"
				     (chess-pos-to-string (car args)))))

     ((eq event 'evaluate)
      (setq chess-crafty-evaluation nil)
      (chess-engine-send nil "display general\nscore\ndisplay nogeneral\n")
      (let ((limit 50))
	(while (and (null chess-crafty-evaluation)
		    (> (setq limit (1- limit)) 0))
	  (sit-for 0 100 t))
	chess-crafty-evaluation))

     ((eq event 'setup-game)
      (let ((file (chess-with-temp-file
		      (insert (chess-game-to-string (car args)) ?\n))))
	(chess-engine-send nil (format "read %s\n" file))))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after crafty moves"))

      (apply 'chess-common-handler game event args)))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
