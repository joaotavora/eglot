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
    (process-send-string chess-scid-process "sc_base close\nexit\n"))

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
    (chess-scid-handler 'replace 0 (car args)))

   ((eq event 'replace)
    (let ((index (or (cadr args)
		     (chess-game-data (car args) 'database-index))))
      (process-send-string chess-scid-process
			   (format "sc_game import \"%s\"\n"
				   (chess-game-to-string (cadr args))))
      (process-send-string chess-scid-process
			   (format "sc_game save %d\n" index))))))

(provide 'chess-scid)

;;; chess-scid.el ends here
