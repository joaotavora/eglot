;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Routines for manipulating chess plies
;;
;; $Revision$

;;; Commentary:

;; A ply is the differential between two positions.  Or, it is the
;; coordinate transformations applied to one position in order to
;; arrive at the following position.  It is also informally called "a
;; move".
;;
;; A ply is represented in Lisp using a cons cell of the form:
;;
;;   (BASE-POSITION .
;;    (FROM-COORD1 TO-COORD1 [FROM-COORD2 TO-COORD2] [KEYWORDS]))
;;
;; The KEYWORDS indicate special actions that are not really chess
;; moves:
;;
;;   :promote PIECE     ; promote pawn to PIECE on arrival
;;   :resign            ; a resignation causes the game to end
;;   :stalemate
;;   :repetition
;;   :perpetual
;;   :check             ; check is announced
;;   :checkmate
;;   :draw              ; a draw was offered and accepted
;;   :draw-offered      ; a draw was offered but not accepted
;;
;; A ply may be represented in ASCII by printing the FEN string of the
;; base position, and then printing the positional transformation in
;; algebraic notation.  Since the starting position is usually known,
;; the FEN string is optional.  A ply may be represented graphically
;; by moving the chess piece(s) involved.  It may be rendered verbally
;; by voicing which piece is to move, where it will move to, and what
;; will happen a result of the move (piece capture, check, etc).
;;
;; Plies may be sent over network connections, postal mail, e-mail,
;; etc., so long as the current position is maintained at both sides.
;; Transmitting the base position's FEN string along with the ply
;; offers a form of confirmation during the course of a game.

;;; Code:

(require 'chess-pos)

(defgroup chess-ply nil
  "Routines for manipulating chess plies."
  :group 'chess)

(defsubst chess-ply-pos (ply)
  (car ply))

(defsubst chess-ply-set-pos (ply position)
  (setcar ply position))

(defsubst chess-ply-changes (ply)
  (cdr ply))

(defsubst chess-ply-set-changes (ply changes)
  (setcdr ply changes))

(defun chess-ply-next-pos (ply)
  (apply 'chess-pos-move (chess-pos-copy (chess-ply-pos ply))
	 (chess-ply-changes ply)))

(defsubst chess-ply-create (position &rest changes)
  (cons position changes))

(defun chess-legal-plies (position color)
  "Return a list of all legal plies in POSITION for COLOR."
  (let (plies)
    (dotimes (rank 8)
      (dotimes (file 8)
	(let* ((to (chess-rf-to-index rank file))
	       (piece (chess-pos-piece position to)))
	  (when (or (eq piece ? )
		    (if color
			(> piece ?a)
		      (< piece ?a)))
	    (dolist (candidate (funcall (car chess-modules)
					nil nil 'search position to t))
	      (push (chess-ply-create position candidate to)
		    plies))))))
    plies))

(provide 'chess-ply)

;;; chess-ply.el ends here
