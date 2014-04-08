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

(require 'chess)
(require 'chess-common)
(require 'chess-pos)
(require 'chess-ply)
(eval-when-compile
  (require 'cl-lib))

(defgroup chess-ai ()
  "A simple chess engine written in Emacs Lisp.

This module does not allow for configuring search time used to calculate
reply moves.  You can only specify the search depth (see `chess-ai-depth')."
  :group 'chess)

(defcustom chess-ai-depth 2
  "The default search depth used to prune the search tree."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-pawn-value 100
  "Value of a Pawn."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-knight-value 300
  "Value of a Knight."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-bishop-value 300
  "Value of a Bishop."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-rook-value 500
  "Value of a Rook."
  :group 'chess-ai
  :type 'intger)

(defcustom chess-ai-queen-value 900
  "Value of a Queen."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-passed-pawn 50
  "Extra Score for a passed Pawn."
  :group 'chess-ai
  :type 'integer)

(defun chess-ai-eval-static (position)
  "Calculate the static score for POSITION."
  (cl-assert (vectorp position))
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
		 ((= piece ?P) (push i white-pawns) (cl-incf v chess-ai-pawn-value))
		 ((= piece ?p) (push i black-pawns) (cl-decf v chess-ai-pawn-value))
		 ((= piece ?Q) (push i white-queens) (cl-incf v chess-ai-queen-value))
		 ((= piece ?q) (push i black-queens) (cl-decf v chess-ai-queen-value))
		 ((= piece ?R) (push i white-rooks) (cl-incf v chess-ai-rook-value))
		 ((= piece ?r) (push i black-rooks) (cl-decf v chess-ai-rook-value))
		 ((= piece ?B) (push i white-bishops) (cl-incf v chess-ai-bishop-value))
		 ((= piece ?b) (push i black-bishops) (cl-decf v chess-ai-bishop-value))
		 ((= piece ?N) (push i white-knights) (cl-incf v chess-ai-knight-value))
		 ((= piece ?n) (push i black-knights) (cl-decf v chess-ai-knight-value))))))
	  ;; Reward passed Pawns
	  (when white-pawns
	    (setq v (+ v (* (length
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
	     (+
	      (if white-queens
		  (length (chess-legal-plies position :piece ?Q
					     :candidates white-queens))
		0)
	      (if white-rooks
		  (length (chess-legal-plies position :piece ?R
					     :candidates white-rooks))
		0)
	      (if white-bishops
		  (length (chess-legal-plies position :piece ?B
					     :candidates white-bishops))
		0)
	      (if white-knights
		  (length (chess-legal-plies position :piece ?N
					     :candidates white-knights))
		0))
	     (+
	      (if black-queens
		  (length (chess-legal-plies position :piece ?q
					     :candidates black-queens))
		0)
	      (if black-rooks
		  (length (chess-legal-plies position :piece ?r
					     :candidates black-rooks))
		0)
	      (if black-bishops
		  (length (chess-legal-plies position :piece ?b
					     :candidates black-bishops))
		0)
	      (if black-knights
		  (length (chess-legal-plies position :piece ?n
					     :candidates black-knights))
		0)))))

	  (if (chess-pos-side-to-move position)
	      v
	    (- v)))))))

(defun chess-ai-eval-1 (position depth achievable cutoff eval-fn)
  (if (zerop depth)
      (funcall eval-fn position)
    (let ((plies (chess-legal-plies position
				    :color (chess-pos-side-to-move position))))
      (if (null plies)
	  (funcall eval-fn position)
	(let ((best-ply (car plies)))
	  (cl-loop for ply in plies
		   do (let ((value (- (chess-ai-eval-1 (chess-ply-next-pos ply)
						       (1- depth)
						       (- cutoff) (- achievable)
						       eval-fn))))
			(when (> value achievable)
			  (setq achievable value
				best-ply ply)))
		   until (>= achievable cutoff))
	  achievable)))))

(defun chess-ai-eval (position depth achievable cutoff eval-fn)
  "Evaluate POSITION using a simple AlphaBeta search algorithm using at most
DEPTH plies."
  (if (zerop depth)
      (cons (funcall eval-fn position) position)
    (let ((plies (chess-legal-plies
		  position :color (chess-pos-side-to-move position))))
      (if (null plies)
	  (cons (funcall eval-fn position) position)
	(let ((progress (make-progress-reporter "Thinking..." 0 (1- (length plies))))
	      (best-ply (car plies)))
	  (cl-loop for i from 0
		   for ply in plies
		   do (let ((value (- (chess-ai-eval-1 (chess-ply-next-pos ply)
						       (1- depth)
						       (- cutoff) (- achievable)
						       eval-fn))))
			(progress-reporter-update progress i)
			(when (> value achievable)
			  (setq achievable value
				best-ply ply)))
		   until (>= achievable cutoff))
	  (progress-reporter-done progress)
	  (cons achievable best-ply))))))

(defun chess-ai-best-move (position depth)
  "Find the best move for POSITION using `chess-ai-eval' with DEPTH."
  (cdr
   (chess-ai-eval position depth (1+ most-negative-fixnum) most-positive-fixnum
		  #'chess-ai-eval-static)))

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
	  (funcall chess-engine-response-handler
		   'move (chess-ai-best-move (chess-engine-position nil)
						   chess-ai-depth)))))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-ai)
;;; chess-ai.el ends here
