;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A special kind of display that merely autosaves the game
;;

(require 'chess-game)
(require 'chess-database)
(require 'chess-message)

(defgroup chess-autosave nil
  "A special display that autosaves after each move."
  :group 'chess-display)

(defcustom chess-autosave-file "~/.chess-save"
  "Filename in which to autosave chess games."
  :type '(choice file (const :tag "Do not auto-save" nil))
  :group 'chess-autosave)

(defcustom chess-autosave-database nil
  "If non-nil, a chess database file in which completed games are appended.
If a function, it will receive a game object and is expected to do the
work of saving the game object to whichever database(s) it chooses.
Whether it closes those databases or caches them for later use is up
to the user."
  :type '(choice (const :tag "Do not save completed games" nil)
		 file function)
  :group 'chess-autosave)

(chess-message-catalog 'english
  '((chess-read-autosave    . "There is a chess autosave file, read it? ")
    (chess-delete-autosave  . "Delete the autosave file? ")
    (chess-disable-autosave . "Disable autosaving for this game? ")
    (autosave-available     . "There is an autosave file; type ~ after connecting to read it")))

(defun chess-autosave-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (set-buffer (find-file-noselect chess-autosave-file t))
    (buffer-disable-undo)
    (setq buffer-auto-save-file-name nil)
    t)

   ((eq event 'check-autosave)
    (if (file-readable-p chess-autosave-file)
	(if (y-or-n-p (chess-string 'chess-read-autosave))
	    (progn
	      (chess-autosave-read game chess-autosave-file)
	      (erase-buffer))
	  (if (y-or-n-p (chess-string 'chess-delete-autosave))
	      (erase-buffer)
	    (if (y-or-n-p (chess-string 'chess-disable-autosave))
		(chess-autosave-handler game 'disable-autosave))))))

   ((eq event 'announce-autosave)
    (if (file-readable-p chess-autosave-file)
	(chess-message 'autosave-available)))

   ((eq event 'disable-autosave)
    (chess-autosave-handler game 'destroy)
    (chess-module-destroy (current-buffer)))

   ((eq event 'post-move)
    (if (not (chess-game-over-p game))
	(chess-autosave-write game chess-autosave-file)
      (erase-buffer)
      (if chess-autosave-database
	  (if (functionp chess-autosave-database)
	      (funcall chess-autosave-database game)
	    (let ((database (chess-database-open chess-autosave-database)))
	      (when database
		(chess-database-write database game)
		(chess-database-close database)))))))

   ((eq event 'destroy)
    (set-buffer-modified-p nil)
    (if (file-exists-p chess-autosave-file)
	(delete-file chess-autosave-file)))))

(defun chess-prin1-ply (ply)
  (insert "([")
  (let ((pos (chess-ply-pos ply)))
    (dotimes (i 74)
      (prin1 (aref pos i) (current-buffer))
      (insert ? )))
  (insert "nil]")
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
  (insert ")"))

(defun chess-autosave-write (game file)
  "Write a chess GAME to FILE as raw Lisp."
  (let ((index (chess-game-index game)))
    (if (or (= 1 index) (and (bobp) (eobp)))
	(progn
	  (erase-buffer)
	  (prin1 (chess-game-tags game)
		 (current-buffer))
	  (insert "\n(\n;;## ply 0\n"))
      (goto-char (point-max))
      (re-search-backward "^;;## ply")
      (forward-line)
      (delete-region (point) (point-max)))
    (chess-prin1-ply (chess-game-ply game (1- index)))
    (insert (format "\n;;## ply %d\n" index))
    (chess-prin1-ply (chess-game-ply game))
    (insert ")\n")
    (basic-save-buffer)
    (message nil)))

(defun chess-autosave-read (game file)
  "Read a chess game as raw Lisp from FILE."
  (goto-char (point-min))
  (chess-game-set-tags game (read (current-buffer)))
  (let* ((plies (read (current-buffer)))
	 (game-plies plies)
	 prev-ply)
    (while plies
      (if prev-ply
	  (chess-pos-set-preceding-ply (chess-ply-pos (car plies))
				       prev-ply))
      (if (cdr plies)
	  (chess-ply-set-keyword (car plies) :next-pos
				 (chess-ply-pos (cadr plies))))
      (setq prev-ply (car plies)
	    plies (cdr plies)))

    (chess-game-set-plies game game-plies)))

(provide 'chess-autosave)

;;; chess-autosave.el ends here
