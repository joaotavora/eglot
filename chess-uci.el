;;; chess-uci.el --- Universal chess interface protocol for emacs-chess

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'chess-common)

(defvar chess-uci-move-regexp "[a-h][1-8][a-h][1-8][nbrq]?"
  "A regular expression matching a UCI move.")

(defun chess-uci-position (game)
  (concat "position fen " (chess-pos-to-fen (chess-game-pos game 0) t)
	  " moves " (mapconcat (lambda (ply)
				 (let ((source (chess-ply-source ply))
				       (target (chess-ply-target ply)))
				   (if (and source target)
				       (concat (chess-index-to-coord source)
					       (chess-index-to-coord target)
					       (if (chess-ply-keyword ply :promote)
						   (string (downcase (chess-ply-keyword ply :promote)))
						 ""))
				     "")))
			       (chess-game-plies game) " ")
	  "\n"))

(defun chess-uci-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'move)
      (when (= 1 (chess-game-index game))
	(chess-game-set-tag game "White" chess-full-name)
	(chess-game-set-tag game "Black" chess-engine-opponent-name))

      (chess-engine-send nil (concat (chess-uci-position game) "go\n"))
      (if (chess-game-over-p game)
	  (chess-game-set-data game 'active nil)))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-uci)

;;; chess-uci.el ends here
