;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against the gnuchess engine
;;
;; $Revision$

(require 'chess-process)

(defgroup chess-gnuchess nil
  "Interface code for playing against gnuchess.  Uses `chess-process'."
  :group 'chess)

(defcustom chess-gnuchess-command "gnuchess"
  "The name of the gnuchess program."
  :type 'string
  :group 'chess-gnuchess)

;;;###autoload
(defun chess-gnuchess (session process event &rest args)
  (chess-process
   session process event
   (list (list (concat "My move is : \\(" chess-algebraic-regexp "\\)")
	       (function
		(lambda (move)
		  (chess-game-move chess-process-game move nil))) 1)
	 '("Illegal move:" (error "Illegal move")))
   (if (file-name-absolute-p chess-gnuchess-command)
       chess-gnuchess-command
     (executable-find chess-gnuchess-command))))

(provide 'chess-gnuchess)

;;; chess-gnuchess.el ends here
