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

(defun chess-ply-has-keyword (ply &rest keywords)
  (catch 'found
    (dolist (keyword keywords)
      (if (memq keyword (chess-ply-changes ply))
	  (throw 'found keyword)))))

(defsubst chess-ply-source (ply)
  (car (chess-ply-changes ply)))

(defsubst chess-ply-target (ply)
  (cadr (chess-ply-changes ply)))

(defsubst chess-ply-next-pos (ply)
  (apply 'chess-pos-move (chess-pos-copy (chess-ply-pos ply))
	 (chess-ply-changes ply)))

(defconst chess-piece-name-table
  '(("queen"  . ?q)
    ("rook"   . ?r)
    ("knight" . ?n)
    ("bishop" . ?b)))

(defun chess-ply-create-castle (position &optional long)
  "Create a castling ply; this function supports Fischer Random castling."
  (let* ((color (chess-pos-side-to-move position))
	 (king (car (chess-pos-search position (if color ?K ?k))))
	 (king-target (chess-rf-to-index (if color 7 0) (if long 2 6)))
	 (king-file (chess-index-file king))
	 (file (if long 0 7))
	 rook)
    (while (funcall (if long '< '>) file king-file)
      (let ((index (chess-rf-to-index (if color 7 0) file)))
	(if (chess-pos-piece-p position index (if color ?R ?r))
	    (setq rook index file king-file)
	  (setq file (funcall (if long '1+ '1-) file)))))
    (if (and rook
	     (chess-search-position position king-target (if color ?K ?k)))
	(cons (chess-pos-copy position)
	      (list king king-target rook
		    (chess-rf-to-index (if color 7 0) (if long 3 5))
		    (if long :long-castle :castle))))))

(defun chess-ply-create (position &rest changes)
  "Create a ply from the given POSITION by applying the suppiled CHANGES.
This function will guarantee the resulting ply is legal, and will also
annotate the ply with :check or other modifiers as necessary.  It will
also extend castling, and will prompt for a promotion piece.

Note: Do not pass in the rook move if CHANGES represents a castling
maneuver."
  (let ((ply (cons (chess-pos-copy position) changes)))
    (if (null changes)
	ply
      ;; validate that `changes' can be legally applied to the given
      ;; position
      (when (member (car changes)
		    (chess-search-position position (cadr changes)
					   (chess-pos-piece position
							    (car changes))))
	;; is this a castling maneuver?
	(let ((color (chess-pos-side-to-move position)))
	  (when (and (eq (if color ?K ?k)
			 (chess-pos-piece position (car changes)))
		     (> (abs (- (chess-index-file (cadr changes))
				(chess-index-file (car changes)))) 1))
	    (let ((kingside (> (chess-index-file (cadr changes))
			       (chess-index-file (car changes)))))
	      ;; if so, add the rook moves
	      (nconc changes (if kingside
				 (list (chess-rf-to-index (if color 7 0) 7)
				       (chess-rf-to-index (if color 7 0) 5)
				       :castle)
			       (list (chess-rf-to-index (if color 7 0) 0)
				     (chess-rf-to-index (if color 7 0) 3)
				     :long-castle))))))

	(let* ((next-pos (chess-ply-next-pos ply))
	       (color (chess-pos-side-to-move next-pos)))
	  ;; is the opponent's king in check/mate or stalemate now, as
	  ;; a result of the changes?  NOTE: engines, whom we should
	  ;; trust, may already have determine if check/checkmate
	  ;; applies.
	  (let ((can-move (catch 'can-move
			    (dotimes (rank 8)
			      (dotimes (file 8)
				(let* ((to (chess-rf-to-index rank file))
				       (piece (chess-pos-piece next-pos to)))
				  (when (or (eq piece ? )
					    (if color
						(> piece ?a)
					      (< piece ?a)))
				    (if (chess-search-position next-pos
							       to color)
					(throw 'can-move t)))))))))
	    (if (chess-search-position next-pos
				       (car (chess-pos-search
					     next-pos (if color ?K ?k)))
				       (not color))
		;; yes, well is in he in checkmate?
		(if can-move
		    (nconc changes (list :check))
		  (nconc changes (list :checkmate)))
	      ;; no, but is he in stalemate?
	      (unless can-move
		(nconc changes (list :stalemate)))))

	  ;; is this a pawn move to the ultimate rank?  if so, and we
	  ;; haven't already been told, ask for the piece to promote
	  ;; it to; NOTE: 'color' has the inverse meaning at this
	  ;; point...
	  (if (and (= ?p (downcase (chess-pos-piece next-pos
						    (cadr changes))))
		   (= (if color 7 0)
		      (chess-index-rank (cadr changes))))
	      (let ((new-piece (completing-read
				"Promote pawn to queen/rook/knight/bishop? "
				chess-piece-name-table nil t "queen")))
		(setq new-piece
		      (cdr (assoc new-piece chess-piece-name-table)))
		(unless color
		  (setq new-piece (upcase new-piece)))
		(nconc changes (list :promote new-piece)))))

	;; return the annotated ply
	ply))))

(defsubst chess-ply-final-p (ply)
  "Return non-nil if this is the last ply of a game/variation."
  (chess-ply-has-keyword ply :draw :perpetual :repetition :stalemate
			 :resign :checkmate))

(defun chess-legal-plies (position)
  "Return a list of all legal plies in POSITION."
  (let ((color (chess-pos-side-to-move position)) plies)
    (dotimes (rank 8)
      (dotimes (file 8)
	(let* ((to (chess-rf-to-index rank file))
	       (piece (chess-pos-piece position to)))
	  (when (or (eq piece ? )
		    (if color
			(> piece ?a)
		      (< piece ?a)))
	    (dolist (candidate (chess-search-position position to color))
	      (push (chess-ply-create position candidate to) plies))))))
    plies))

(provide 'chess-ply)

;;; chess-ply.el ends here
