;;; chess-ai.el --- A Chess playing module

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>

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
(require 'chess-pos)
(require 'chess-ply)
(require 'cl)

(defgroup chess-ai ()
  "A simple chess engine written in elisp."
  :group 'chess)

(defcustom chess-ai-depth 2
  "*The depth used to prune the search tree."
  :group 'chess-ai
  :type 'integer)

(defvar chess-pawn-value 100)
(defvar chess-knight-value 300)
(defvar chess-bishop-value 300)
(defvar chess-rook-value 500)
(defvar chess-queen-value 900)
(defvar chess-king-value 10000)

(defun chess-eval-static (position)
  "Find the static score for POSITION."
  (assert (vectorp position))
  (let ((v 0)
	(status (chess-pos-status position)))
    (if (eq status :checkmate)
	-64000
      (if (eq status :stalemate)
	  v
	(dotimes (i 64 (if (chess-pos-side-to-move position) v (- v)))
	  (let ((piece (aref position i)))
	    (cond
	     ((= piece ?P) (incf v chess-pawn-value))
	     ((= piece ?p) (decf v chess-pawn-value))
	     ((= piece ?K) (incf v chess-king-value))
	     ((= piece ?k) (decf v chess-king-value))
	     ((= piece ?Q) (incf v chess-queen-value))
	     ((= piece ?q) (decf v chess-queen-value))
	     ((= piece ?R) (incf v chess-rook-value))
	     ((= piece ?r) (decf v chess-rook-value))
	     ((= piece ?B) (incf v chess-bishop-value))
	     ((= piece ?b) (decf v chess-bishop-value))
	     ((= piece ?N) (incf v chess-knight-value))
	     ((= piece ?n) (decf v chess-knight-value)))))))))

(defun chess-ai-eval (position depth alpha beta &optional line)
  "Evaluate POSITION using a simple AlphaBeta search algorithm using at most
DEPTH plies."
  ;; TBD: We do far too much consing
  (if (= depth 0)
      (cons (chess-eval-static position) line)
    (let ((plies (chess-legal-plies
		  position :color (chess-pos-side-to-move position)))
	  (ret (cons alpha line)))
      (if (= (length plies) 0)
	  (cons (chess-eval-static position) line)
	(while plies
	  (let* ((tmp1 (chess-ai-eval (chess-ply-next-pos (car plies))
				       (1- depth) (- beta) (- alpha)
				       (cons (car plies) line)))
		 (tmp (- (car tmp1))))
	    (if (> tmp alpha) (setq alpha tmp
				    ret (cons tmp (cdr tmp1))))
	    (if (>= alpha beta)
		(setq plies nil)
	      (setq plies (cdr plies)))))
	ret))))

(defun chess-ai-best-move (position depth &optional func)
  "Find the best move for POSITION using `chess-ai-eval' with DEPTH.
Returns (VALUE . LIST-OF-PLIES) where
 VALUE is the evaluated score of the move and
 LIST-OF-PLIES is the list of plies which were actually found."
  (let ((res (chess-ai-eval position  depth -100000 100000)))
    (cons (car res)
	  (if (functionp func)
	      (mapcar func (nreverse (cdr res)))
	    (nreverse (cdr res))))))

(defun chess-ai-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (setq chess-engine-opponent-name "Emacs AI")
      t)

     ((eq event 'new)
      (chess-engine-set-position nil))

     ((eq event 'move)
      (when (= 1 (chess-game-index game))
	(chess-game-set-tag game "White" chess-full-name)
	(chess-game-set-tag game "Black" chess-engine-opponent-name))
      (when (chess-game-over-p game)
	(chess-game-set-data game 'active nil)))

     ((eq event 'post-move)
      (unless (chess-game-over-p game)
	(let (chess-display-handling-event)
	  (message "Thinking...")
	  (funcall chess-engine-response-handler
		   'move (cadr (chess-ai-best-move (chess-engine-position nil)
						   chess-ai-depth)))
	  (message "Thinking... done"))))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-ai)
;;; chess-ai.el ends here
