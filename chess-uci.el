;;; chess-uci.el --- Common functions for the Universal Chess Interface protocol

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games, processes

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

;; Common functions for engines based on the Universal Chess Interface.
;; See <http://en.wikipedia.org/wiki/Universal_Chess_Interface>.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'chess-common)
(require 'chess-polyglot)

(defgroup chess-uci nil
  "Customisations for Chess engines based on the UCI protocol"
  :group 'chess)

(defvar chess-uci-long-algebraic-regexp "\\([a-h][1-8]\\)\\([a-h][1-8]\\)\\([nbrq]\\)?"
  "A regular expression matching a UCI log algebraic move.")

(defun chess-uci-long-algebraic-to-ply (position move)
  "Convert the long algebraic notation MOVE for POSITION to a ply."
  (cl-check-type position chess-pos)
  (cl-check-type move string)
  (let ((case-fold-search nil))
    (when (string-match chess-uci-long-algebraic-regexp move)
      (let ((color (chess-pos-side-to-move position))
	    (from (chess-coord-to-index (match-string 1 move)))
	    (to (chess-coord-to-index (match-string 2 move)))
	    (promotion (match-string 3 move)))
	(apply #'chess-ply-create position nil
	       (if (and (= from (chess-pos-king-index position color))
			(= (chess-index-rank from) (chess-index-rank to))
			(> (abs (- (chess-index-file from)
				   (chess-index-file to))) 1))
		   (chess-ply-castling-changes
		    position
		    (< (- (chess-index-file to) (chess-index-file from)) 0))
		 (cons from (cons to
				  (when promotion
				    (list :promote
					  (upcase (aref promotion 0))))))))))))

(defsubst chess-uci-convert-long-algebraic (move)
  "Convert long algebraic MOVE to a ply in reference to the engine position.
If conversion fails, this function fired an 'illegal event."
  (or (chess-uci-long-algebraic-to-ply (chess-engine-position nil) move)
      (chess-engine-command nil 'illegal)))

(defvar chess-uci-regexp-alist
  (list
   (cons "^id\\s-+name\\s-+\\(.+\\)$"
	 (function
	  (lambda ()
	    (setq-local chess-engine-opponent-name (match-string 1))
 	    'once)))
   (cons (concat "^bestmove\\s-+\\(" chess-uci-long-algebraic-regexp "\\)")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-uci-convert-long-algebraic (match-string 1)))))))
  "Patterns matching responses of a standard UCI chess engine.")

(defun chess-uci-position (game)
  "Convert the current GAME position to a UCI position command string."
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
  "Default handler for UCI based engines."
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (when (and chess-polyglot-book-file
		 (file-exists-p chess-polyglot-book-file))
	(unless chess-polyglot-book
	  (setq chess-polyglot-book (chess-polyglot-book-open
				chess-polyglot-book-file))))
      (apply #'chess-common-handler game event args))

     ((eq event 'new)
      (chess-engine-send nil "ucinewgame\n")
      (chess-engine-set-position nil))

     ((eq event 'resign)
      (chess-game-set-data game 'active nil))

     ((eq event 'move)
      (when (= 1 (chess-game-index game))
	(chess-game-set-tag game "White" chess-full-name)
	(chess-game-set-tag game "Black" chess-engine-opponent-name))

      (if (chess-game-over-p game)
	  (chess-game-set-data game 'active nil)))

     ((eq event 'post-move)
      (let ((book-ply (and chess-polyglot-book (bufferp chess-polyglot-book)
			   (buffer-live-p chess-polyglot-book)
			   (chess-polyglot-book-ply
			    chess-polyglot-book
			    (chess-game-pos game)))))
	(if book-ply
	    (let ((chess-display-handling-event nil))
	      (funcall chess-engine-response-handler 'move book-ply))
	  (chess-engine-send nil (concat (chess-uci-position game) "go\n")))))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-uci)

;;; chess-uci.el ends here
