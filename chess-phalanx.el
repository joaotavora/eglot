;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against phalanx!
;;
;; $Revision$

(require 'chess-engine)
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
		     (chess-engine-convert-algebraic (match-string 1))))))
   (cons "Illegal move:\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (signal 'chess-illegal (match-string 1)))))))

(defun chess-phalanx-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let ((proc (chess-common-handler 'initialize "phalanx")))
      (process-send-string proc "nopost\n")
      proc))

   (t
    (apply 'chess-common-handler event args))))

(provide 'chess-phalanx)

;;; chess-phalanx.el ends here
