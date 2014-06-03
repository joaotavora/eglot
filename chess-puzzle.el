;;; chess-puzzle.el --- Support for viewing and solving chess puzzles

;; Copyright (C) 2002, 2004, 2008, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WARNING: While this module does handle *some* chess puzzle data,
;; it is not complete yet.  In particular, PGN based puzzles inclusing
;; variation annotations are not properly handled yet.

;;; Code:

(require 'chess)
(require 'chess-algebraic)
(require 'chess-database)
(require 'chess-display)
(require 'chess-engine)
(require 'chess-game)
(require 'chess-random)

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
(defun chess-puzzle (file &optional index) ;FIXME: index not used!
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
	(define-key (current-local-map) [??] 'chess-puzzle-show-solution)
	(let ((count (chess-database-count database)))
	  (setq chess-puzzle-indices (make-vector count nil))
	  (dotimes (i count)
	    (aset chess-puzzle-indices i i))
	  (random t)
	  (chess-shuffle-vector chess-puzzle-indices)
	  (setq chess-puzzle-position 0))
	(chess-game-run-hooks (chess-display-game display) 'disable-autosave)
	(chess-puzzle-next)))))

(defvar chess-display-handling-event)

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
	  (chess-error 'bad-game-read (aref chess-puzzle-indices index))
	(chess-display-set-game nil next-game 0)
	(chess-game-set-data game 'my-color (chess-game-side-to-move game 0))
	(dolist (key '(database database-index database-count))
	  (chess-game-set-data game key (chess-game-data next-game key)))
	(let ((chess-display-handling-event nil))
	  (chess-game-run-hooks game 'orient))))))

(defun chess-puzzle-show-solution ()
  (interactive)
  (let ((game (chess-display-game nil)))
    (when game
      (let ((bm (chess-pos-epd (chess-game-pos game 0) 'bm))
	    (pv (chess-pos-epd (chess-game-pos game 0) 'pv)))
	(when (or bm pv)
	  (message "Best move %s %s%s"
		   (if (zerop (chess-game-index game)) "is" "would have been")
		   (chess-ply-to-algebraic (car bm))
		   (if pv
		       (concat ", predicted variation "
			       (chess-var-to-algebraic pv))
		     "")))))))


(defun chess-puzzle-handler (game display event &rest _args)
  (if (and (eq event 'move)
	   (chess-game-over-p game))
      (with-current-buffer display
	(chess-puzzle-next))))

(provide 'chess-puzzle)

;;; chess-puzzle.el ends here
