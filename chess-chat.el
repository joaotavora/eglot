;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements chess chat, which is very much like kibitzing, but not
;; saved.  RET is used to send each chat line.
;;

(defvar chess-chat-input-last nil)

(make-variable-buffer-local 'chess-chat-input-last)

(define-derived-mode chess-chat-mode text-mode "Chat"
  "A mode for editing chess annotations."
  (set-buffer-modified-p nil)
  (setq chess-chat-input-last (copy-marker (point-max) t))
  (let ((map (current-local-map)))
    (define-key map [return] 'chess-chat-send)
    (define-key map [(control ?m)] 'chess-chat-send)))

(defun chess-chat-send ()
  (interactive)
  (chess-game-run-hooks chess-module-game 'chat
			(buffer-substring-no-properties
			 chess-chat-input-last (point-max)))
  (set-marker chess-chat-input-last (point-max))
  (set-buffer-modified-p nil))

(defun chess-chat-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (set-buffer (generate-new-buffer "*Chat*"))
    (chess-chat-mode)
    t)

   ((eq event 'switch-to-chat)
    (switch-to-buffer-other-window (current-buffer)))

   ((eq event 'chat)
    (chess-chat-handler 'switch-to-chat)
    (save-excursion
      (goto-char chess-chat-input-last)
      (insert (car args))))))

(provide 'chess-chat)

;;; chess-chat.el ends here
