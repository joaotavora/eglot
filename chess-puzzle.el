;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A special kind of display that merely autosaves the game
;;

(require 'chess-game)
(require 'chess-random)
(require 'chess-database)

(defgroup chess-puzzle nil
  "A mode for playing games from a database of puzzles."
  :group 'chess)

(defcustom chess-puzzle-auto-next nil
  "If non-nil, move to the next puzzle once the position is won."
  :type 'boolean
  :group 'chess-puzzle)

(defvar chess-puzzle-indices nil)
(defvar chess-puzzle-position nil)

(make-variable-buffer-local 'chess-puzzle-indices)
(make-variable-buffer-local 'chess-puzzle-position)

(chess-message-catalog 'english
  '((bad-game-read . "Error reading game at position %d")
    (end-of-puzzles . "There are no more puzzles in this collection")))

;;;###autoload
(defun chess-puzzle (file &optional index)
  "Pick a random puzzle from FILE, and solve it against the default engine.
The spacebar in the display buffer is bound to `chess-puzzle-next',
making it easy to go on to the next puzzle once you've solved one."
  (interactive "fRead chess puzzles from: ")
  (let* ((database (chess-database-open file))
	 (objects (and database (chess-session)))
	 (engine (car objects))
	 (display (cadr objects)))
    (when database
      (if engine
	  (chess-engine-set-option engine 'resign nil))
      (with-current-buffer display
	(chess-game-set-data (chess-display-game nil) 'database database)
	(if chess-puzzle-auto-next
	    (chess-game-add-hook (chess-display-game nil)
				 'chess-puzzle-handler display))
	(define-key (current-local-map) [? ] 'chess-puzzle-next)
	(let ((count (chess-database-count database)))
	  (setq chess-puzzle-indices (make-vector count nil))
	  (dotimes (i count)
	    (aset chess-puzzle-indices i i))
	  (random t)
	  (chess-shuffle-vector chess-puzzle-indices)
	  (setq chess-puzzle-position 0))
	(chess-puzzle-next)))))

(defun chess-puzzle-next ()
  "Play the next puzzle in the collection, selected randomly."
  (interactive)
  (let* ((game (chess-display-game nil))
	 (database (chess-game-data game 'database))
	 (index chess-puzzle-position)
	 next-game)
    (if (= index (length chess-puzzle-indices))
	(chess-message 'end-of-puzzles)
      ;; setup and load the next puzzle position
      (setq chess-puzzle-position (1+ chess-puzzle-position))
      (if (null (setq next-game
		      (chess-database-read database
					   (aref chess-puzzle-indices index))))
	  (chess-error 'bag-game-read (aref chess-puzzle-indices index))
	(chess-display-set-game nil next-game 0)
	(chess-game-set-data game 'my-color
			     (chess-pos-side-to-move (chess-game-pos game)))
	(dolist (key '(database database-index database-count))
	  (chess-game-set-data game key (chess-game-data next-game key)))
	(let ((chess-display-handling-event nil))
	  (chess-game-run-hooks game 'orient))))))

(defun chess-puzzle-handler (game display event &rest args)
  (if (and (eq event 'move)
	   (chess-game-over-p game))
      (with-current-buffer display
	(chess-puzzle-next))))

(provide 'chess-puzzle)

;;; chess-puzzle.el ends here
