;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against phalanx!
;;

(require 'chess-common)

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
		     (chess-engine-convert-algebraic (match-string 1) t)))))
   (cons "Illegal move:\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (error (match-string 1)))))))

(defun chess-phalanx-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-common-handler game 'initialize "phalanx")))
	(when (and proc (processp proc)
		   (eq (process-status proc) 'run))
	  (process-send-string proc "nopost\n")
	  (setq chess-engine-process proc
		chess-engine-opponent-name "Phalanx")
	  t)))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-phalanx)

;;; chess-phalanx.el ends here
