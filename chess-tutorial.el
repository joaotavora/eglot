;;; chess-tutorial.el --- A simple chess training display

;; Copyright (C) 2002, 2004, 2014  Free Software Foundation, Inc.

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

;; `M-x chess-tutorial' implements a simple knight movement exercise.
;; The objective is to take all pawns on the chessboard without moving to a
;; square which is attacked by a queen.

;;; Code:

(require 'chess)
(require 'chess-display)
(require 'chess-game)
(require 'chess-message)

(chess-message-catalog 'english
  '((queen-would-take . "The queen would take your knight!")
    (congratulations  . "Congratulations!")
    (knight-1-done    . "Goal: take all the pawns, without letting the queen take your knight")
    (cannot-take-queen . "You cannot take the queen")))

(defun chess-tutorial-knight-1 (game _ignore event &rest _args)
  (if (eq event 'move)
      (let ((position (chess-game-pos game)))
	(if (null (chess-pos-search position ?p))
	    (chess-message 'congratulations)
	  (cond
	   ((chess-search-position position
				   (car (chess-pos-search position ?N)) ?q)
	    (let ((chess-display-handling-event nil))
	      (chess-game-undo game 1))
	    (chess-error 'queen-would-take))
	   ((not (chess-pos-search position ?q))
	    (let ((chess-display-handling-event nil))
	      (chess-game-undo game 1))
	    (chess-error 'cannot-take-queen)))))))

;;;###autoload
(defun chess-tutorial ()
  "A simple chess training display."
  (interactive)
  (with-current-buffer (chess-create-display t)
    (chess-module-set-leader nil)
    (chess-display-set-from-fen "8/3p1p/2p3p/4q/2p3p/3p1p/8/N w - -")
    (chess-game-add-hook (chess-display-game nil) 'chess-tutorial-knight-1)
    (setq chess-pos-always-white t)
    (chess-display-popup nil)
    (chess-message 'knight-1-done)))

(provide 'chess-tutorial)

;;; chess-tutorial.el ends here
