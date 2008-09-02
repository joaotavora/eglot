;; Soon: Put Emacs Chess through an enormous battery of tests.

(eval-when-compile
  (require 'cl))

(require 'chess-database)
(require 'chess-game)

(defun chess-test (&optional file start count)
  (unless file
    (setq file (nth 0 command-line-args-left)))
  (unless start
    (setq start (ignore-errors
		  (string-to-number (nth 1 command-line-args-left)))))
  (unless count
    (setq count (ignore-errors
		  (string-to-number (nth 2 command-line-args-left)))))

  (message "Opening chess database '%s'" file)

  (let ((database (chess-database-open file))
	error-occurred)
    (if database
	(progn
	  (message "Running validation suite...")
	  (let* ((db-count (chess-database-count database))
		 (ply-count 0)
		 (index (if start
			    (1- start)
			  0))
		 (last-index (if (and count (> count 0))
				 (min db-count (+ index count))
			       db-count))
		 (begin (current-time))
		 (read-count 0))
	    (message "Testing legality of games in range [%d, %d]:"
		     index (1- last-index))
	    (while (< index last-index)
	      ;; Reading in the game will cause it to be converted from PGN
	      ;; (this is true currently) to a chess-game, during which time
	      ;; every move will be tested for legality.
	      ;;
	      ;; jww (2008-08-31): We should add some extra checks here, if we
	      ;; want to verify the final position and such.
	      (condition-case err
		  (let ((game (chess-database-read database index)))
		    (when game
		      (setq read-count (1+ read-count)
			    ply-count
			    (+ ply-count (length (chess-game-plies game))))
		      (if (and (> read-count 0) (= 0 (mod read-count 1000)))
			  (message "Read %d games (now at game %d): %d total plies (%.2f ply/sec)"
				   read-count index ply-count
				   (/ (float ply-count)
				      (float
				       (time-to-seconds
					(subtract-time (current-time)
						       begin))))))))
		(error
		 (setq error-occurred t)
		 (message "Error reading game %d: %s"
			  index (error-message-string err))))
	      (setq index (1+ index)))
	    (message "Read %d games (up to game %d): %d plies (%.2f ply/sec, %.2f seconds)"
		     read-count (1- index) ply-count
		     (/ (float ply-count)
			(float
			 (time-to-seconds
			  (subtract-time (current-time)
					 begin))))
		     (time-to-seconds (subtract-time (current-time)
						     begin)))
	    (message "Running validation suite...done")
	    (chess-database-close database)))
      (error "Failed to open chess database '%s'" file))
    (if error-occurred
	(error "Some of the tests failed"))))

;;; chess-test.el ends here
