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

(require 'cl)
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

(defun chess-ply-any-keyword (ply &rest keywords)
  (catch 'found
    (dolist (keyword keywords)
      (if (memq keyword (chess-ply-changes ply))
	  (throw 'found keyword)))))

(defun chess-ply-keyword (ply keyword)
  (let ((item (memq keyword (chess-ply-changes ply))))
    (if item
	(if (memq keyword '(:which :promote))
	    (cdr item)
	  t))))

(defsubst chess-ply-source (ply)
  (let ((changes (chess-ply-changes ply)))
    (and (listp changes) (not (symbolp (car changes)))
	 (car changes))))

(defsubst chess-ply-target (ply)
  (let ((changes (chess-ply-changes ply)))
    (and (listp changes) (not (symbolp (car changes)))
	 (cadr changes))))

(defsubst chess-ply-next-pos (ply)
  (apply 'chess-pos-move (chess-pos-copy (chess-ply-pos ply))
	 (chess-ply-changes ply)))

(defconst chess-piece-name-table
  '(("queen"  . ?q)
    ("rook"   . ?r)
    ("knight" . ?n)
    ("bishop" . ?b)))

(defun chess-ply-create-castle (position &optional long)
  "Create castling changes; this function supports Fischer Random castling."
  (let* ((color (chess-pos-side-to-move position))
	 (king (car (chess-pos-search position (if color ?K ?k))))
	 (king-target (chess-rf-to-index (if color 7 0)
					 (if long 2 6)))
	 (king-file (chess-index-file king))
	 (file (if long 0 7))
	 rook)
    (while (funcall (if long '< '>) file king-file)
      (let ((index (chess-rf-to-index (if color 7 0) file)))
	(if (chess-pos-piece-p position index (if color ?R ?r))
	    (setq rook index file king-file)
	  (setq file (funcall (if long '1+ '1-) file)))))
    (if (and rook (chess-search-position position king-target
					 (if color ?K ?k)))
	(list king king-target rook
	      (chess-rf-to-index (if color 7 0) (if long 3 5))
	      (if long :long-castle :castle)))))

(defun chess-ply-create (position &rest changes)
  "Create a ply from the given POSITION by applying the suppiled CHANGES.
This function will guarantee the resulting ply is legal, and will also
annotate the ply with :check or other modifiers as necessary.  It will
also extend castling, and will prompt for a promotion piece.

Note: Do not pass in the rook move if CHANGES represents a castling
maneuver."
  (let* ((valid-p (memq :valid changes))
	 (ply (cons (chess-pos-copy position)
		    (delq :valid changes)))
	 (color (chess-pos-side-to-move position))
	 piece)
    (if (or (null changes) (symbolp (car changes)))
	ply
      ;; validate that `changes' can be legally applied to the given
      ;; position
      (when (or valid-p
		(member (car changes)
			(chess-search-position position (cadr changes)
					       (chess-pos-piece position
								(car changes)))))
	(setq piece (chess-pos-piece position (car changes)))

	;; is this a castling maneuver?
	(if (and (= piece (if color ?K ?k))
		 (not (or (memq :castle changes)
			  (memq :long-castle changes))))
	    (let* ((target (cadr changes))
		   (file (chess-index-file target))
		   (long (= 2 file))
		   new-changes)
	      (if (and (or (and (= file 6)
				(chess-pos-can-castle position
						      (if color ?K ?k)))
			   (and long
				(chess-pos-can-castle position
						      (if color ?Q ?q))))
		       (setq new-changes
			     (chess-ply-create-castle position long)))
		  (setcdr ply new-changes))))

	;; is this a pawn move to the ultimate rank?  if so, and we
	;; haven't already been told, ask for the piece to promote it to
	(if (and (= piece (if color ?P ?p))
		 (not (memq :promote changes))
		 (= (if color 0 7) (chess-index-rank (cadr changes))))
	    (let ((new-piece (completing-read
			      "Promote pawn to queen/rook/knight/bishop? "
			      chess-piece-name-table nil t "queen")))
	      (setq new-piece
		    (cdr (assoc new-piece chess-piece-name-table)))
	      (if color
		  (setq new-piece (upcase new-piece)))
	      (nconc changes (list :promote new-piece))))

	(unless (or (memq :check changes)
		    (memq :checkmate changes)
		    (memq :stalemate changes))
	  (let* ((next-pos (chess-ply-next-pos ply))
		 (next-color (not color)))
	    ;; is the opponent's king in check/mate or stalemate now, as
	    ;; a result of the changes?
	    (let ((can-move
		   (catch 'can-move
		     ;; find out if any of `color's pieces can move.  We
		     ;; start the search on the home row for that color,
		     ;; as it's likier to find a legal move faster.
		     (let ((rank (if next-color 7 0))
			   (file 0))
		       (while (funcall (if next-color '>= '<) rank
				       (if next-color 0 8))
			 (while (< file 8)
			   (let* ((to (chess-rf-to-index rank file))
				  (piece (chess-pos-piece next-pos to)))
			     (when (or (eq piece ? )
				       (if next-color
					   (> piece ?a)
					 (< piece ?a)))
			       (if (chess-search-position next-pos to next-color)
				   (throw 'can-move t))))
			   (setq file (1+ file)))
			 (setq file 0 rank (funcall (if next-color '1- '1+)
						    rank)))))))

	      ;; see if anyone from the other side is attacking the king
	      ;; in the new position
	      (if (chess-search-position next-pos
					 (car (chess-pos-search
					       next-pos (if next-color ?K ?k)))
					 (not next-color))
		  (nconc changes (list (if can-move :check :checkmate)))
		;; no, but is he in stalemate?
		(unless can-move
		  (nconc changes (list :stalemate)))))))

	;; return the annotated ply
	ply))))

(defsubst chess-ply-final-p (ply)
  "Return non-nil if this is the last ply of a game/variation."
  (chess-ply-any-keyword ply :draw :perpetual :repetition :stalemate
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
	      (push (chess-ply-create position candidate to :valid)
		    plies))))))
    plies))

(provide 'chess-ply)

;;; chess-ply.el ends here
