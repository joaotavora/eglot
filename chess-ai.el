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
  "A simple chess engine written in elisp.

This module does not allow for configuring search time used to calculate
reply moves.  You can only specify the search depth (see `chess-ai-depth')."
  :group 'chess)

(defcustom chess-ai-depth 2
  "*The default depth used to prune the search tree."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-pawn-value 100
  "*Value of a Pawn."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-knight-value 300
  "*Value of a Knight."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-bishop-value 300
  "*Value of a Bishop."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-rook-value 500
  "*Value of a Rook."
  :group 'chess-ai
  :type 'intger)

(defcustom chess-ai-queen-value 900
  "*Value of a Queen."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-passed-pawn 50
  "*Extra Score for a passed Pawn."
  :group 'chess-ai
  :type 'integer)

(defun chess-eval-static (position)
  "Calculate the static score for POSITION."
  (assert (vectorp position))
  (let ((v 0)
	(status (chess-pos-status position)))
    (if (eq status :checkmate)
	-32767
      (if (eq status :stalemate)
	  0
	(let (white-queens black-queens white-rooks black-rooks
	      white-bishops black-bishops white-knights black-knights
	      white-pawns black-pawns)
	  (dotimes (i 64)
	    (let ((piece (aref position i)))
	      (unless (= piece ? )
		(cond
		 ((= piece ?P) (push i white-pawns) (incf v chess-ai-pawn-value))
		 ((= piece ?p) (push i black-pawns) (decf v chess-ai-pawn-value))
		 ((= piece ?Q) (push i white-queens) (incf v chess-ai-queen-value))
		 ((= piece ?q) (push i black-queens) (decf v chess-ai-queen-value))
		 ((= piece ?R) (push i white-rooks) (incf v chess-ai-rook-value))
		 ((= piece ?r) (push i black-rooks) (decf v chess-ai-rook-value))
		 ((= piece ?B) (push i white-bishops) (incf v chess-ai-bishop-value))
		 ((= piece ?b) (push i black-bishops) (decf v chess-ai-bishop-value))
		 ((= piece ?N) (push i white-knights) (incf v chess-ai-knight-value))
		 ((= piece ?n) (push i black-knights) (decf v chess-ai-knight-value))))))
	  ;; Reward passed Pawns
	  (when white-pawns
	    (setq v (+ (* (length
			   (chess-pos-passed-pawns position t white-pawns))
			  chess-ai-passed-pawn))))
	  (when black-pawns
	    (setq v (- v
		       (* (length
			   (chess-pos-passed-pawns position nil black-pawns))
			  chess-ai-passed-pawn))))
	  ;; Mobility
	  (setq
	   v
	   (+
	    v
	    (-
	     (length
	      (append (when white-queens
			(chess-legal-plies position :piece ?Q :candidates white-queens))
		      (when white-rooks
			(chess-legal-plies position :piece ?R :candidates white-rooks))
		      (when white-bishops
			(chess-legal-plies position :piece ?B :candidates white-bishops))
		      (when white-knights
			(chess-legal-plies position :piece ?N :candidates white-knights))))
	     (length
	      (append (when black-queens
			(chess-legal-plies position :piece ?q :candidates black-queens))
		      (when black-rooks
			(chess-legal-plies position :piece ?r :candidates black-rooks))
		      (when black-bishops
			(chess-legal-plies position :piece ?b :candidates black-bishops))
		      (when black-knights
			(chess-legal-plies position :piece ?n :candidates black-knights)))))))
	  (if (chess-pos-side-to-move position)
	      v
	    (- v)))))))

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
