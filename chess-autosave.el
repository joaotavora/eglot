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
    (chess-delete-autosave . "Delete the autosave file? ")))

(defun chess-autosave-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (if (file-readable-p chess-autosave-file)
	(if (y-or-n-p (chess-string 'chess-read-autosave))
	    (prog1
		(chess-game-copy-game
		 game (chess-autosave-read chess-autosave-file))
	      (delete-file chess-autosave-file))
	  (ignore
	   (if (y-or-n-p (chess-string 'chess-delete-autosave))
	       (delete-file chess-autosave-file)))))
    (kill-buffer (current-buffer))
    (set-buffer (find-file-noselect chess-autosave-file t))
    t)

   ((eq event 'post-move)
    (chess-autosave-write game chess-autosave-file))

   ((eq event 'destroy)
    (if (file-readable-p chess-autosave-file)
	(delete-file chess-autosave-file)))))

(defun chess-autosave-write (game file)
  "Write a chess GAME to FILE as raw Lisp."
  (let ((game-copy (copy-alist game)))
    (chess-game-set-hooks game-copy nil)
    (chess-game-set-data-alist game-copy nil)
    (with-current-buffer (find-file-noselect file t)
      (erase-buffer)
      (prin1 game-copy (current-buffer))
      (basic-save-buffer)
      (message nil))))

(defun chess-autosave-read (file)
  "Read a chess game as raw Lisp from FILE."
  (with-current-buffer (find-file-noselect file t)
    (goto-char (point-min))
    (read (current-buffer))))

(provide 'chess-autosave)

;;; chess-autosave.el ends here
