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

  (let ((database (chess-database-open file)))
    (if database
	(progn
	  (message "Running validation suite...")
	  (condition-case err
	      (let* ((db-count (chess-database-count database))
		     (ply-count 0)
		     (index (if start
				(max start 1)
			      1))
		     (last-index (if count
				     (min db-count (+ index count))
				   db-count))
		     (begin (current-time))
		     (read-count 0))
		(message "Testing legality of games in range [%d, %d):"
			 index last-index)
		(while (< index last-index)
		  ;; Reading in the game will cause it to be converted from PGN
		  ;; (this is true currently) to a chess-game, during which time
		  ;; every move will be tested for legality.
		  ;;
		  ;; jww (2008-08-31): We should add some extra checks here, if we
		  ;; want to verify the final position and such.
		  (let ((game (chess-database-read database index)))
		    (when game
		      (setq read-count (1+ read-count)
			    ply-count
			    (+ ply-count (length (chess-game-plies game))))
		      (if (= 0 (mod index 1000))
			  (message "Read %d games (now at game %d): %d total plies (%.2f ply/sec)"
				   read-count index ply-count
				   (/ (float ply-count)
				      (float
				       (time-to-seconds
					(subtract-time (current-time)
						       begin))))))))
		  (setq index (1+ index)))
		(message "Read %d games (up to game %d): %d plies (%.2f ply/sec, %.2f seconds)"
			 read-count index ply-count
			 (/ (float ply-count)
			    (float
			     (time-to-seconds
			      (subtract-time (current-time)
					     begin))))
			 (time-to-seconds (subtract-time (current-time)
							 begin)))
		(message "Running validation suite...done"))
	    (t
	     (error "Failed to open chess database '%s'" file)))
	  (chess-database-close database)))))

;;; chess-test.el ends here
