;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements chess kibitzing, stored as annotations to the game being
;; viewed or played.  C-c C-c is used to save a kibitzing comment.
;;

(defvar chess-kibitz-input-last nil)
(defvar chess-kibitz-index nil)

(make-variable-buffer-local 'chess-kibitz-input-last)
(make-variable-buffer-local 'chess-kibitz-index)

(define-derived-mode chess-kibitz-mode text-mode "Kibitz"
  "A mode for editing chess annotations."
  (set-buffer-modified-p nil)
  (setq chess-kibitz-input-last (copy-marker (point-max) t))
  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?c)] 'chess-kibitz-save)))

(defun chess-kibitz-save ()
  (interactive)
  (let ((ann (buffer-substring-no-properties chess-kibitz-input-last
					     (point-max))))
    (chess-game-run-hooks chess-module-game 'kibitz ann)
    (chess-pos-add-annotation (chess-game-pos chess-kibitz-index) ann))
  (set-marker chess-kibitz-input-last (point-max))
  (set-buffer-modified-p nil))

(defun chess-kibitz-show-annotations (index)
  (setq chess-kibitz-index index)
  (erase-buffer)
  (let ((position (chess-game-pos chess-module-game index))
	popup)
    (dolist (ann (chess-pos-annotations position))
      (when (stringp ann)
	(insert ann ?\n)
	(setq popup t)))
    (if popup
	(display-buffer (current-buffer)))))

(defun chess-kibitz-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (set-buffer (generate-new-buffer "*Annotations*"))
    (chess-kibitz-mode)
    t)

   ((eq event 'switch-to-annotations)
    (switch-to-buffer-other-window (current-buffer)))

   ((eq event 'kibitz)
    (chess-kibitz-handler 'switch-to-annotations)
    (save-excursion
      (goto-char chess-kibitz-input-last)
      (insert (car args))))

   ((eq event 'set-index)
    (chess-kibitz-show-annotations (car args)))

   ((memq event '(post-undo move))
    (chess-kibitz-show-annotations (chess-game-index game)))))

(provide 'chess-kibitz)

;;; chess-kibitz.el ends here
