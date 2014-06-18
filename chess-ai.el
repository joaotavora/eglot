;;; chess-ai.el --- A native Emacs Lisp Chess playing module

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>

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

;; This file implements simple alpha-beta and quiescence search for chess
;; positions.  Contrary to other engine modules, it does not delegate position
;; evaluation to an external program, it directly tries to calculate move
;; evaluations in Emacs Lisp.

;; See `chess-ai-best-move' for programmatic access.

;; Use "C-u M-x chess RET ai RET" to play against this engine.

;;; Code:

(require 'chess-algebraic)
(require 'chess-common)
(require 'chess-polyglot)
(require 'chess-ply)
(require 'chess-pos)
(require 'cl-lib)

(defgroup chess-ai ()
  "A simple chess engine written in Emacs Lisp.

This module does not allow to configure search time used to calculate
reply moves.  You can only specify the search depth (see `chess-ai-depth')."
  :group 'chess
  :link '(custom-manual "(chess)AI"))

(defcustom chess-ai-depth 2
  "The default search depth used to prune the search tree.

If `chess-ai-quiescence' is non-nil, quiescence search will be performed after
this ply depth limit has been reached."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-quiescence-depth 2
  "Search depth for quiescence search."
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
  "Extra score for a passed Pawn."
  :group 'chess-ai
  :type 'integer)

(defcustom chess-ai-mobility t
  "Non-nil if piece mobility should be considered during static evaluation."
  :group 'chess-ai
  :type 'boolean)

(defcustom chess-ai-quiescence t
  "Non-nil if quiescence search should be performed."
  :group 'chess-ai
  :type 'boolean)

;;;; Static evaluation

(defun chess-ai-eval-static (position)
  "Calculate the static score for POSITION."
  (cl-check-type position chess-pos)
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
	  (when (and white-pawns (< (length black-pawns) 7))
	    (setq v (+ v (* (length
			     (chess-pos-passed-pawns position t white-pawns))
			    chess-ai-passed-pawn))))
	  (when (and black-pawns (< (length white-pawns) 7))
	    (setq v (- v
		       (* (length
			   (chess-pos-passed-pawns position nil black-pawns))
			  chess-ai-passed-pawn))))
	  ;; Mobility
	  (when chess-ai-mobility
	    (let ((chess-ply-checking-mate t))
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
		    0)))))))

	  (if (chess-pos-side-to-move position)
	      v
	    (- v)))))))

;;;; Move ordering

(defun chess-ai-plies (position &optional capture-only)
  "Return an ordered list of all legal plies for POSITION.
Move ordering is rather naive at the moment.  Capturing
moves come first, the rest is sorted according to the square
index."
  (let* ((side-to-move (chess-pos-side-to-move position))
	 (plies (chess-legal-plies position :color side-to-move)))
    (if (not capture-only)
	(sort plies
	      (lambda (a b)
		(let ((l '(?Q ?R ?B ?N ?P ? ))
		      (p1 (cadr (chess-ply-changes a)))
		      (p2 (cadr (chess-ply-changes b))))
		  (or (< (length (memq (chess-pos-piece position p1) l))
			 (length (memq (chess-pos-piece position p2) l)))
		      (funcall (if side-to-move #'< #'>) p1 p2)))))
      (cl-delete-if (lambda (ply)
		      (= (chess-pos-piece position
					  (cadr (chess-ply-changes ply)))
			 ? ))
		    plies))))

;;;; Searching

(defun chess-ai-quiescence (position depth lower-bound upper-bound eval-fn)
  "Try to find a quiet position by evaluating only capturing moves."
  (let ((stand-pat (funcall eval-fn position)))
    (if (>= stand-pat upper-bound)
	upper-bound
      (when (> stand-pat lower-bound) (setq lower-bound stand-pat))
      (if (zerop depth)
	  lower-bound
	(cl-loop for ply in (chess-ai-plies position t)
		 for value = (- (chess-ai-quiescence
				 (chess-ply-next-pos ply)
				 (1- depth) (- upper-bound) (- lower-bound) eval-fn))
		 when (> value lower-bound) do (setq lower-bound value)
		 until (>= lower-bound upper-bound))
	lower-bound))))

(defun chess-ai-search (position depth lower-bound upper-bound eval-fn)
  (if (zerop depth)
      (if chess-ai-quiescence
	  (chess-ai-quiescence position chess-ai-quiescence-depth
			       lower-bound upper-bound eval-fn)
	(funcall eval-fn position))
    (let ((plies (chess-ai-plies position)))
      (if (null plies)
	  (funcall eval-fn position)
	(cl-loop for ply in plies
		 for value = (- (chess-ai-search (chess-ply-next-pos ply)
						 (1- depth)
						 (- upper-bound) (- lower-bound)
						 eval-fn))
		 when (> value lower-bound) do (setq lower-bound value)
		 until (>= lower-bound upper-bound))
	lower-bound))))

(defun chess-ai-eval (position depth lower-bound upper-bound eval-fn)
  "Evaluate POSITION using a simple alpha-beta search algorithm using at least
DEPTH plies.  If `chess-ai-quiescence' is non-nil additionally only capturing
moves are examined until a quiet position is reached.  EVAL-FN is called
for all leave nodes of the resulting tree.
A `cons' cell is returned where `cdr' is the supposedly best move from POSITION
and `car' is the score of that move.  If there is no legal move from POSITION
\(or DEPTH is 0), `cdr' will be nil."
  (if (zerop depth)
      (cons (funcall eval-fn position) nil)
    (let ((plies (let ((chess-ai-mobility nil)
		       (chess-ai-quiescence nil))
		   (sort
		    (mapcar
		     (lambda (ply)
		       (chess-ply-set-keyword
			ply :score
			(- (chess-ai-search (chess-ply-next-pos ply)
					    1
					    (1+ most-negative-fixnum)
					    most-positive-fixnum
					    #'chess-ai-eval-static)))
		       ply)
		     (chess-legal-plies
		      position :color (chess-pos-side-to-move position)))
		    (lambda (lhs rhs)
		      (> (chess-ply-keyword lhs :score)
			 (chess-ply-keyword rhs :score)))))))
      (if (null plies)
	  (cons (funcall eval-fn position) nil)
	(let* ((best-ply (car plies))
	       (progress (make-progress-reporter
			  (format "Thinking... (%s) "
				  (chess-ply-to-algebraic best-ply))
			  0 (length plies))))
	  (cl-loop for i from 1
		   for ply in plies
		   do (let ((value (- (chess-ai-search
				       (chess-ply-next-pos ply)
				       (1- depth) (- upper-bound) (- lower-bound)
				       eval-fn))))
			(progress-reporter-update progress i)
			(accept-process-output nil 0.05)
			(when (> value lower-bound)
			  (setq lower-bound value
				best-ply ply)
			  (progress-reporter-force-update
			   progress
			   i
			   (format "Thinking... (%s {cp=%d}) "
				   (chess-ply-to-algebraic best-ply)
				   lower-bound))))
		   until (>= lower-bound upper-bound))
	  (progress-reporter-done progress)
	  (cons lower-bound best-ply))))))

(defun chess-ai-best-move (position &optional depth eval-fn)
  "Find the best move for POSITION.
DEPTH defaults to the value of `chess-ai-depth'."
  (cdr (chess-ai-eval position (or depth chess-ai-depth)
		      (1+ most-negative-fixnum) most-positive-fixnum
		      (or eval-fn #'chess-ai-eval-static))))

(defvar chess-full-name)

(defun chess-ai-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (setq chess-engine-opponent-name "Emacs AI")
      t)

     ((eq event 'new) (chess-engine-set-position nil))

     ((eq event 'move)
      (when (= 1 (chess-game-index game))
	(chess-game-set-tag game "White" chess-full-name)
	(chess-game-set-tag game "Black" chess-engine-opponent-name))
      (when (chess-game-over-p game)
	(chess-game-set-data game 'active nil)))

     ((eq event 'post-move)
      (unless (chess-game-over-p game)
	(let ((chess-display-handling-event nil)
	      (position (chess-engine-position nil)))
	  (funcall chess-engine-response-handler 'move
		   (or (and (bufferp chess-polyglot-book)
			    (buffer-live-p chess-polyglot-book)
			    (chess-polyglot-book-ply chess-polyglot-book
						     position))
		       (chess-ai-best-move position))))))

     (t (apply 'chess-common-handler game event args)))))

(provide 'chess-ai)
;;; chess-ai.el ends here
