;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A game database that stores PGN format games in a single file.
;;
;; This is basically what you expect from a file ending in .pgn.
;;

(defvar chess-file-locations)

(make-variable-buffer-local 'chess-file-locations)

(defun chess-file-handler (event &rest args)
  (cond
   ((eq event 'open)
    (with-current-buffer (find-file-noselect (car args))
      (when (or (string-match "\\.pgn\\'" (car args))
		(save-excursion
		  (re-search-forward "^\\[Event" nil t)))
	(chess-file-handler 'rescan)
	(current-buffer))))

   ((eq event 'rescan)
    (save-excursion
      (goto-char (point-min))
      (setq chess-file-locations nil)
      (while (re-search-forward "^\\[Event " nil t)
	(goto-char (match-beginning 0))
	(push (point) chess-file-locations)
	(forward-char 1))
      (setq chess-file-locations (nreverse chess-file-locations))))

   ((eq event 'read-only-p)
    buffer-read-only)

   ((eq event 'filename)
    buffer-file-name)

   ((eq event 'save)
    (save-buffer))

   ((eq event 'count)
    (length chess-file-locations))

   ((eq event 'read)
    (let ((index (car args)) game)
      (when (and (>= index 0)
		 (< index (chess-file-handler 'count)))
	(goto-char (nth index chess-file-locations))
	(when (setq game (chess-pgn-to-game))
	  (chess-game-set-data game 'database (current-buffer))
	  (chess-game-set-data game 'database-index index)
	  (chess-game-set-data game 'database-count
			       (chess-file-handler 'count))
	  game))))

   ((eq event 'write)
    (goto-char (point-max))
    (while (memq (char-before) '(?  ?\t ?\n ?\r))
      (delete-backward-char 1))
    (insert ?\n ?\n)
    (push (point) chess-file-locations)
    (chess-game-to-pgn (car args))
    (1- (chess-file-handler 'count)))

   ((eq event 'replace)
    (let ((index (or (cadr args)
		     (chess-game-data (car args) 'database-index)))
	  (count (chess-file-handler 'count)))
      (when (and (>= index 0)
		 (< index count))
	(goto-char (nth index chess-file-locations))
	(delete-region (point) (if (= (1+ index) count)
				   (point-max)
				 (nth (1+ index) chess-file-locations)))
	(chess-game-to-pgn (car args))
	(insert ?\n))))))

(provide 'chess-file)

;;; chess-file.el ends here
