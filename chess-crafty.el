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
    (let ((proc (chess-common-handler 'initialize "crafty")))
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

   ((eq event 'setup-pos)
    (chess-engine-send nil (format "setboard %s\n"
				   (chess-pos-to-string (car args)))))

   ((eq event 'setup-game)
    (let ((file (chess-with-temp-file
		    (insert (chess-game-to-string (car args)) ?\n))))
      (chess-engine-send nil (format "read %s\n" file))))

   (t
    (apply 'chess-common-handler event args))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
