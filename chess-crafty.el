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

;;; Code:

;;;###autoload
(defun chess-crafty (session buffer event &rest args)
  (if (not (eq event 'initialize))
      (apply 'chess-process session buffer event args)
    (with-current-buffer
	(chess-process session buffer event
		       chess-process-triggers
		       (if (file-name-absolute-p chess-crafty-command)
			   chess-crafty-command
			 (executable-find chess-crafty-command)))
      (process-send-string
       (get-buffer-process (current-buffer))
       (concat "display nogeneral\n"
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
      (current-buffer))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
