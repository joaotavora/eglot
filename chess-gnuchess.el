;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against the gnuchess engine
;;
;; $Revision$

(require 'chess-process)

(defgroup chess-gnuchess nil
  "Interface code for playing against gnuchess.  Uses `chess-process'."
  :group 'chess)

(defcustom chess-gnuchess-command (and (require 'executable)
				       (executable-find "gnuchess"))
  "The name of the gnuchess program."
  :type 'string
  :group 'chess-gnuchess)

;;;###autoload
(defun chess-gnuchess (session buffer event &rest args)
  (if (not (eq event 'initialize))
      (apply 'chess-process session buffer event args)
    (chess-process session buffer event
		   (list (list
			  (concat "My move is : \\("
				  chess-algebraic-regexp "\\)")
			  (function
			   (lambda (move)
			     (chess-session-event
			      chess-current-session 'move
			      (chess-algebraic-to-ply
			       (chess-game-pos chess-process-game) move)))) 1)
			 '("Illegal move:" (error "Illegal move")))
		   chess-gnuchess-command)))

(provide 'chess-gnuchess)

;;; chess-gnuchess.el ends here
