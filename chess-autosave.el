;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A special kind of display that merely autosaves the game
;;

(require 'chess-game)

(defgroup chess-autosave nil
  "A special display that autosaves after each move."
  :group 'chess-display)

(defcustom chess-autosave-file "~/.chess-save"
  "Filename in which to autosave chess games."
  :type '(choice file (const :tag "Do not auto-save" nil))
  :group 'chess-autosave)

(chess-message-catalog 'english
  '((chess-read-autosave   . "There is a chess autosave file, read it? ")
    (chess-delete-autosave . "Delete the autosave file? ")
    (chess-disable-autosave . "Disable autosaving for this game? ")))

(defun chess-autosave-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (let ((result t))
      (if (file-readable-p chess-autosave-file)
	  (if (y-or-n-p (chess-string 'chess-read-autosave))
	      (progn
		(chess-game-copy-game game (chess-autosave-read
					    chess-autosave-file))
		(delete-file chess-autosave-file))
	    (if (y-or-n-p (chess-string 'chess-delete-autosave))
		(delete-file chess-autosave-file)
	      (if (y-or-n-p (chess-string 'chess-disable-autosave))
		  (setq result nil)))))
      (kill-buffer (current-buffer))
      (set-buffer (find-file-noselect chess-autosave-file t))
      result))

   ((eq event 'post-move)
    (chess-autosave-write game chess-autosave-file))

   ((eq event 'disable-autosave)
    (chess-autosave-handler game 'destroy)
    (chess-module-destroy (current-buffer)))

   ((eq event 'destroy)
    (if (file-readable-p chess-autosave-file)
	(delete-file chess-autosave-file)))))

(defun chess-autosave-write (game file)
  "Write a chess GAME to FILE as raw Lisp."
  (with-current-buffer (find-file-noselect file t)
    (erase-buffer)
    (insert "(nil ")
    (prin1 (chess-game-tags game) (current-buffer))
    (insert " nil (")
    (dolist (ply (chess-game-plies game))
      (insert "([")
      (let ((pos (chess-ply-pos ply)))
	(dotimes (i 74)
	  (prin1 (aref pos i) (current-buffer))
	  (unless (= i 73)
	    (insert ? ))))
      (insert ?\])
      (let ((changes (chess-ply-changes ply)))
	(if changes
	    (insert ? ))
	(while changes
	  (if (eq (car changes) :next-pos)
	      (setq changes (cddr changes))
	    (prin1 (car changes) (current-buffer))
	    (if (cdr changes)
		(insert ? ))
	    (setq changes (cdr changes)))))
      (insert ") "))
    (insert "))")
    (basic-save-buffer)
    (message nil)))

(defun chess-autosave-read (file)
  "Read a chess game as raw Lisp from FILE."
  (with-current-buffer (find-file-noselect file t)
    (goto-char (point-min))
    (read (current-buffer))))

(provide 'chess-autosave)

;;; chess-autosave.el ends here
