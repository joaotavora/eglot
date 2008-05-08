;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Log chess events, as an aid to debugging
;;

;;; Commentary:

(require 'chess-module)

(defgroup chess-log nil
  "Code for logging chess events."
  :group 'chess)

(defun chess-log (&rest args)
  (with-current-buffer (get-buffer-create "*Chess Log*")
    (insert (apply 'format args) ?\n)))

(provide 'chess-log)

;;; chess-log.el ends here
