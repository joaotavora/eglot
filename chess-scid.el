;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A game database that uses SCID for storage/retrieval
;;
;; The advantage is that it's much faster than PGN, and far, far more
;; compact.
;;

(defvar chess-scid-process)

(make-variable-buffer-local 'chess-scid-process)

(defun chess-scid-get-result (command)
  (let ((here (point-max)))
    (process-send-string chess-scid-process command)
    (accept-process-output chess-scid-process)
    (goto-char (point-max))
    (while (memq (char-before) '(?  ?\t ?\n ?\r ?\%))
      (backward-char 1))
    (buffer-substring here (point))))

(defun chess-scid-handler (event &rest args)
  (cond
   ((eq event 'open)
    (if (file-readable-p (concat (car args) ".sg3"))
	(let* ((buffer (generate-new-buffer " *chess-scid*"))
	       (proc (start-process "*chess-scid*" buffer
				    (executable-find "tcscid"))))
	  (if (and proc (eq (process-status proc) 'run))
	      (with-current-buffer buffer
		(accept-process-output proc)
		(setq chess-scid-process proc)
		(if (= 1 (string-to-int
			  (chess-scid-get-result
			   (format "sc_base open %s\n"
				   (expand-file-name (car args))))))
		    buffer
		  (kill-process proc)
		  (kill-buffer buffer)
		  nil))
	    (kill-buffer buffer)
	    nil))))

   ((eq event 'close)
    (process-send-string chess-scid-process "sc_base close\nexit\n")
    (while (eq (process-status chess-scid-process) 'run)
      (sit-for 0 250)))

   ((eq event 'count)
    (string-to-int (chess-scid-get-result "sc_base numGames\n")))

   ((eq event 'read)
    (let ((here (point-max)) game)
      (process-send-string chess-scid-process
			   (format "sc_game load %d\nsc_game pgn\n"
				   (car args)))
      (accept-process-output chess-scid-process)
      (goto-char here)
      (when (setq game (chess-pgn-to-game))
	(chess-game-set-data game 'database (current-buffer))
	(chess-game-set-data game 'database-index (car args))
	(chess-game-set-data game 'database-count
			     (chess-scid-handler 'count))
	game)))

   ((eq event 'write)
    (chess-scid-handler 'replace (car args) 0))

   ((eq event 'replace)
    (let ((index (or (cadr args)
		     (chess-game-data (car args) 'database-index))))
      (process-send-string
       chess-scid-process
       (format "sc_game import \"%s\"\n"
	       (with-temp-buffer
		 (chess-pgn-insert-plies (car args) 1
					 (chess-game-plies (car args)))
		 (insert (or (chess-game-tag (car args) "Result") "*"))
		 (buffer-string))))
      (dolist (tag (chess-game-tags (car args)))
	;; jww (2002-05-01): how do I set extra tags?
	(unless (string= (car tag) "TimeControl")
	  (process-send-string
	   chess-scid-process
	   (concat "sc_game tags set "
		   (cond
		    ((string= (car tag) "Event")     "-event")
		    ((string= (car tag) "Site")      "-site")
		    ((string= (car tag) "Date")      "-date")
		    ((string= (car tag) "Round")     "-round")
		    ((string= (car tag) "White")     "-white")
		    ((string= (car tag) "WhiteElo")  "-whiteElo")
		    ((string= (car tag) "Black")     "-black")
		    ((string= (car tag) "BlackElo")  "-blackElo")
		    ((string= (car tag) "Result")    "-result")
		    ((string= (car tag) "ECO")       "-eco")
		    ((string= (car tag) "EventDate") "-eventdate")
		    ((string= (car tag) "Extra")     "-extra"))
		   " \"" (cdr tag) "\"\n"))))
      (process-send-string chess-scid-process
			   (format "sc_game save %d\n" index))))))

(provide 'chess-scid)

;;; chess-scid.el ends here
