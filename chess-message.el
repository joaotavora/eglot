;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code shared by all chess displays
;;

(defgroup chess-message nil
  "Support for message catalogs in chess.el."
  :group 'chess)

(defcustom chess-message-language 'english
  "The language to use when reporting messages."
  :type 'symbol
  :group 'chess-message)

;;; Code:

(defvar chess-message-catalog nil)

(defun chess-message-catalog (catalog definitions)
  (let ((entry (assq catalog chess-message-catalog)))
    (if entry
	(dolist (def definitions)
	  (let ((str (assq (car def) (cdr entry))))
	    (if str
		(setcdr str (cdr def))
	      (setcdr entry (cons def (cdr entry))))))
      (push (cons catalog definitions) chess-message-catalog))))

(defun chess-string (key &rest arguments)
  (let* ((entry (assq chess-message-language chess-message-catalog))
	 (msg (and entry (cdr (assq key (cdr entry))))))
    (if msg
	(apply 'format msg arguments)
      "message not found")))

(defsubst chess-message (key &rest arguments)
  (message (apply 'chess-string key arguments)))

(defsubst chess-error (key &rest arguments)
  (error (apply 'chess-string key arguments)))

(put 'chess-message-catalog 'lisp-indent-function 1)

(provide 'chess-message)

;;; chess-message.el ends here
