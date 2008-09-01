;; Soon: Put Emacs Chess through an enormous battery of tests.

(eval-when-compile
  (require 'cl))

(require 'chess-database)
(require 'chess-game)

(defun chess-test (&optional file start finish)
  (unless file
    (setq file (nth 0 command-line-args-left)))
  (unless start
    (setq start (string-to-number (nth 1 command-line-args-left))))
  (unless finish
    (setq finish (string-to-number (nth 2 command-line-args-left))))

  (message "Opening chess database '%s'" file)

  (let ((database (chess-database-open file)))
    (if database
	(progn
	  (message "Running chess unit tests...")
	  (condition-case err
	      (let* ((count (chess-database-count database))
		     (ply-count 0)
		     (index (if start
				(max start 1)
			      1))
		     (last-index (if finish
				     (min count finish)
				   count))
		     (begin (current-time)))
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
		      (setq ply-count
			    (+ ply-count (length (chess-game-plies game))))
		      (if (= 0 (mod index 1000))
			  (message "Read %d games: %d total plies (%.2f ply/sec)"
				   index ply-count
				   (/ (float ply-count)
				      (float
				       (time-to-seconds
					(subtract-time (current-time)
						       begin))))))))
		  (setq index (1+ index)))
		(message "Running chess unit tests...done"))
	    (t
	     (error "Failed to open chess database '%s'" file)))
	  (chess-database-close database)))))

;;; chess-test.el ends here
