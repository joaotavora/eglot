;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against the crafty engine
;;
;; $Revision$

(require 'chess-process)

(defgroup chess-crafty nil
  "Interface code for playing against crafty.  Uses `chess-process'."
  :group 'chess)

(defcustom chess-crafty-command "crafty"
  "The name of the crafty program."
  :type 'string
  :group 'chess-crafty)

;;;###autoload
(defun chess-crafty (game)
  (chess-process game 'chess-crafty-handler nil
		 (if (file-name-absolute-p chess-crafty-command)
		     chess-crafty-command
		   (executable-find chess-crafty-command))))

;;; Code:

(defun chess-crafty-handler (game buffer command &rest args)
  (unless (apply 'chess-process-handler game buffer command args)
    (ignore
     (if (eq command 'initialize)
	 (process-send-string (get-buffer-process buffer)
			      "alarm off\nansi off\n")))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
