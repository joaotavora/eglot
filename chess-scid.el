;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A game database that uses SCID for storage/retrieval
;;
;; The advantage is that it's much faster than PGN, and far, far more
;; compact.
;;

(defvar chess-scid-process)

(make-variable-buffer-local 'chess-scid-process)

(defsubst chess-scid-send (string)
  (process-send-string chess-scid-process (concat string "\n")))

(defun chess-scid-get-result (command)
  (let ((here (point-max)))
    (chess-scid-send command)
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
			   (format "sc_base open %s"
				   (expand-file-name (car args))))))
		    buffer
		  (kill-process proc)
		  (kill-buffer buffer)
		  nil))
	    (kill-buffer buffer)
	    nil))))

   ((eq event 'close)
    (chess-scid-send "sc_base close\nexit")
    (while (eq (process-status chess-scid-process) 'run)
      (sit-for 0 250)))

   ((eq event 'read-only-p)
    (if (zerop (string-to-int (chess-scid-get-result "sc_base isReadOnly")))
	nil
      t))

   ((eq event 'filename)
    (chess-scid-get-result "sc_base filename"))

   ((eq event 'count)
    (string-to-int (chess-scid-get-result "sc_base numGames")))

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
	(chess-game-set-data game 'database-count (chess-scid-handler 'count))
	game)))

   ((and (eq event 'query)
	 (eq (car args) 'tree-search))
    (if (string=
	 (chess-scid-get-result
	  (format "sc_game import \"%s\""
		  (with-temp-buffer
		    (chess-pgn-insert-plies
		     (cadr args) 1 (chess-game-plies (cadr args)))
		    (insert " " (or (chess-game-tag (cadr args) "Result") "*"))
		    (buffer-string))))
	 "PGN text imported with no errors or warnings.")
	(let ((here (point-max)))
	  (chess-scid-send "sc_tree search")
	  (accept-process-output chess-scid-process)
	  (goto-char here)
	  (let ((lines (list t)))
	    (while
		(re-search-forward
		 (concat "\\s-*\\([1-9][0-9]*\\):\\s-+\\([^ ]+\\)\\s-+"
			 "\\([0-9]+\\):\\s-*\\([0-9.]+\\)%\\s-+"
			 "\\([0-9.]+\\)%\\s-+\\([0-9]+\\|    \\)\\s-+"
			 "\\([0-9]+\\|    \\)\\s-+\\([0-9]+\\|    \\)\\s-+"
			 "\\([0-9.]+\\)%")
		 nil t)
	      (let ((move (match-string 2))
		    (freq (string-to-int (match-string 3)))
		    (score (string-to-number (match-string 5)))
		    (avgelo (string-to-int (match-string 6)))
		    (perf (string-to-int (match-string 7)))
		    (avgyear (string-to-int (match-string 8)))
		    (draws (string-to-number (match-string 9))))
		(nconc lines
		       (list
			(append
			 (list move freq score draws)
			 (cond
			  ((and (zerop avgyear) (zerop avgelo) (zerop perf))
			   nil)
			  ((and (zerop avgelo) (zerop perf))
			   (list avgyear))
			  (t
			   (list
			    (unless (zerop avgyear) avgyear)
			    (unless (zerop avgelo) avgelo)
			    (unless (zerop perf) perf)))))))))
	    (when (re-search-forward
		   (concat "TOTAL:\\s-+\\([0-9]+\\):100.0%\\s-+"
			   "\\([0-9.]+\\)%\\s-+\\([0-9]+\\|    \\)\\s-+"
			   "\\([0-9]+\\|    \\)\\s-+\\([0-9]+\\|    \\)\\s-+"
			   "\\([0-9.]+\\)%") nil t)
	      (goto-char (point-max))
	      (append
	       (list (string-to-int (match-string 1))
		     (string-to-number (match-string 2))
		     (string-to-number (match-string 6)))
	       (cdr lines)))))
      (error "Unable to import game")))

   ((eq event 'write)
    (chess-scid-handler 'replace (car args) 0))

   ((eq event 'replace)
    (unless (chess-scid-handler 'read-only-p)
      (let ((index (or (cadr args)
		       (chess-game-data (car args) 'database-index))))
	(chess-scid-send
	 (format "sc_game import \"%s\""
		 (with-temp-buffer
		   (chess-pgn-insert-plies
		    (car args) 1 (chess-game-plies (car args)))
		   (insert (or (chess-game-tag (car args) "Result") "*"))
		   (buffer-string))))
	(dolist (tag (chess-game-tags (car args)))
	  ;; jww (2002-05-01): how do I set extra tags?
	  (unless (string= (car tag) "TimeControl")
	    (chess-scid-send
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
		     " \"" (cdr tag) "\""))))
	(chess-scid-send (format "sc_game save %d" index)))))))

(provide 'chess-scid)

;;; chess-scid.el ends here
