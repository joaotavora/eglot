;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Routines for manipulating chess plies
;;

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
(require 'chess-algebraic)

(defgroup chess-ply nil
  "Routines for manipulating chess plies."
  :group 'chess)

(defsubst chess-ply-pos (ply)
  "Returns the base position associated with PLY."
  (assert (listp ply))
  (car ply))

(defsubst chess-ply-set-pos (ply position)
  "Set the base position of PLY."
  (assert (listp ply))
  (assert (vectorp position))
  (setcar ply position))

(defsubst chess-ply-changes (ply)
  (assert (listp ply))
  (cdr ply))

(defsubst chess-ply-set-changes (ply changes)
  (assert (listp ply))
  (assert (listp changes))
  (setcdr ply changes))

(defun chess-ply-any-keyword (ply &rest keywords)
  (assert (listp ply))
  (catch 'found
    (dolist (keyword keywords)
      (if (memq keyword (chess-ply-changes ply))
	  (throw 'found keyword)))))

(defun chess-ply-keyword (ply keyword)
  (assert (listp ply))
  (assert (symbolp keyword))
  (let ((item (memq keyword (chess-ply-changes ply))))
    (if item
	(if (eq item (last (chess-ply-changes ply)))
	    t
	  (cadr item)))))

(defun chess-ply-set-keyword (ply keyword &optional value)
  (assert (listp ply))
  (assert (symbolp keyword))
  (let* ((changes (chess-ply-changes ply))
	 (item (memq keyword changes)))
    (if item
	(if value
	    (setcar (cdr item) value))
      (nconc changes (if value
			 (list keyword value)
		       (list keyword))))
    value))

(defsubst chess-ply-source (ply)
  "Returns the source square index value of PLY."
  (assert (listp ply))
  (let ((changes (chess-ply-changes ply)))
    (and (listp changes) (not (symbolp (car changes)))
	 (car changes))))

(defsubst chess-ply-target (ply)
  "Returns the target square index value of PLY."
  (assert (listp ply))
  (let ((changes (chess-ply-changes ply)))
    (and (listp changes) (not (symbolp (car changes)))
	 (cadr changes))))

(defsubst chess-ply-next-pos (ply)
  (assert (listp ply))
  (or (chess-ply-keyword ply :next-pos)
      (let ((position (apply 'chess-pos-move
			     (chess-pos-copy (chess-ply-pos ply))
			     (chess-ply-changes ply))))
	(chess-pos-set-preceding-ply position ply)
	(chess-ply-set-keyword ply :next-pos position))))

(defsubst chess-ply-to-string (ply &optional long)
  (assert (listp ply))
  (chess-ply-to-algebraic ply long))

(defsubst chess-ply-from-string (position move)
  (assert (vectorp position))
  (assert (stringp move))
  (chess-algebraic-to-ply position move))

(defconst chess-piece-name-table
  '(("queen"  . ?q)
    ("rook"   . ?r)
    ("knight" . ?n)
    ("bishop" . ?b)))

(defun chess-ply-castling-changes (position &optional long king-index)
  "Create castling changes; this function supports Fischer Random castling."
  (assert (vectorp position))
  (let* ((color (chess-pos-side-to-move position))
	 (king (or king-index (chess-pos-king-index position color)))
	 (rook (chess-pos-can-castle position (if color
						  (if long ?Q ?K)
						(if long ?q ?k))))
	 (bias (if long -1 1)) pos)
    (when rook
      (setq pos (chess-incr-index king 0 bias))
      (while (and pos (not (equal pos rook))
		  (chess-pos-piece-p position pos ? )
		  (or (and long (< (chess-index-file pos) 2))
		      (chess-pos-legal-candidates
		       position color pos (list king))))
	(setq pos (chess-incr-index pos 0 bias)))
      (if (equal pos rook)
	  (list king (chess-rf-to-index (if color 7 0) (if long 2 6))
		rook (chess-rf-to-index (if color 7 0) (if long 3 5))
		(if long :long-castle :castle))))))

(chess-message-catalog 'english
  '((pawn-promote-query . "Promote to queen? ")))

(defvar chess-ply-checking-mate nil)

(defsubst chess-ply-create* (position)
  (assert (vectorp position))
  (list position))

(defun chess-ply-create (position &optional valid-p &rest changes)
  "Create a ply from the given POSITION by applying the supplied CHANGES.
This function will guarantee the resulting ply is legal, and will also
annotate the ply with :check or other modifiers as necessary.  It will
also extend castling, and will prompt for a promotion piece.

Note: Do not pass in the rook move if CHANGES represents a castling
maneuver."
  (assert (vectorp position))
  (let* ((ply (cons position changes))
	 (color (chess-pos-side-to-move position))
	 piece)
    (if (or (null changes) (symbolp (car changes)))
	ply
      ;; validate that `changes' can be legally applied to the given
      ;; position
      (when (or valid-p
		(chess-legal-plies position :any :index (car changes)
				   :target (cadr changes)))
	(unless chess-ply-checking-mate
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
			       (chess-ply-castling-changes position long
							   (car changes))))
		    (setcdr ply new-changes))))

	  (when (= piece (if color ?P ?p))
	    ;; is this a pawn move to the ultimate rank?  if so, and
	    ;; we haven't already been told, ask for the piece to
	    ;; promote it to
	    (when (and (not (memq :promote changes))
		       (= (if color 0 7) (chess-index-rank (cadr changes))))
	      ;; jww (2002-05-15): This does not always clear ALL
	      ;; input events
	      (discard-input) (sit-for 0) (discard-input)
	      (let ((new-piece (if (yes-or-no-p
				    (chess-string 'pawn-promote-query))
				   ?Q ?N)))
		(nconc changes (list :promote (upcase new-piece)))))

	    ;; is this an en-passant capture?
	    (if (= (or (chess-pos-en-passant position) 100)
		   (or (chess-incr-index (cadr changes)
					 (if color 1 -1) 0) 200))
		(nconc changes (list :en-passant))))

	  ;; we must determine whether this ply results in a check,
	  ;; checkmate or stalemate
	  (unless (or chess-pos-always-white
		      (memq :check changes)
		      (memq :checkmate changes)
		      (memq :stalemate changes))
	    (let* ((chess-ply-checking-mate t)
		   ;; jww (2002-04-17): this is a memory waste?
		   (next-pos (chess-ply-next-pos ply))
		   (next-color (not color))
		   (king (chess-pos-king-index next-pos next-color))
		   (in-check (catch 'in-check
			       (chess-search-position next-pos king
						      (not next-color) t))))
	      ;; first, see if the moves leaves the king in check.
	      ;; This is tested by seeing if any of the opponent's
	      ;; pieces can reach the king in the position that will
	      ;; result from this ply.  If the king is in check, we
	      ;; will then test for checkmate by seeing if any of his
	      ;; subjects can move or not.  That test will also
	      ;; confirm stalemate for us.
	      (if (or in-check
		      (null (chess-legal-plies next-pos :any :index king)))
		  ;; is the opponent's king in check/mate or stalemate
		  ;; now, as a result of the changes?
		  (if (chess-legal-plies next-pos :any :color next-color)
		      (if in-check
			  (nconc changes (list (chess-pos-set-status
						next-pos :check))))
		    (nconc changes (list (chess-pos-set-status
					  next-pos
					  (if in-check
					      :checkmate
					    :stalemate)))))))))
	;; return the annotated ply
	ply))))

(defsubst chess-ply-final-p (ply)
  "Return non-nil if this is the last ply of a game/variation."
  (or (chess-ply-any-keyword ply :drawn :perpetual :repetition
			     :flag-fell :resign :aborted)
      (chess-ply-any-keyword (chess-pos-preceding-ply
			      (chess-ply-pos ply)) :stalemate :checkmate)))

(eval-when-compile
  (defvar position)
  (defvar candidate)
  (defvar color)
  (defvar plies)
  (defvar specific-target))

(defvar chess-ply-throw-if-any nil)

(defsubst chess-ply--add (rank-adj file-adj &optional pos)
  "This is totally a shortcut."
  (let ((target (or pos (chess-incr-index* candidate rank-adj file-adj))))
    (if (and (or (not specific-target)
		 (= target specific-target))
	     (chess-pos-legal-candidates position color target
					 (list candidate)))
	(if chess-ply-throw-if-any
	    (throw 'any-found t)
	  (let ((ply (chess-ply-create position t candidate target)))
	    (if ply
		(push ply plies)))))))

(defun chess-legal-plies (position &rest keywords)
  "Return a list of all legal plies in POSITION.
KEYWORDS allowed are:

  :any   return t if any piece can move at all
  :color <t or nil>
  :piece <piece character>
  :file <number 0 to 7> [can only be used if :piece is present]
  :index <coordinate index>
  :target <specific target index>
  :candidates <list of inddices>

These will constrain the plies generated to those matching the above
criteria.

NOTE: All of the returned plies will reference the same copy of the
position object passed in."
  (assert (vectorp position))
  (cond
   ((null keywords)
    (let ((plies (list t)))
      (dolist (p '(?P ?R ?N ?B ?K ?Q ?p ?r ?n ?b ?k ?q))
	(nconc plies (chess-legal-plies position :piece p)))
      (cdr plies)))
   ((memq :any keywords)
    (let ((chess-ply-throw-if-any t))
      (catch 'any-found
	(apply 'chess-legal-plies position (delq :any keywords)))))
   ((memq :color keywords)
    (let ((plies (list t))
	  (color (cadr (memq :color keywords))))
      (dolist (p '(?P ?R ?N ?B ?K ?Q))
	(nconc plies (chess-legal-plies position
					:piece (if color p
						 (downcase p)))))
      (cdr plies)))
   (t
    (let* ((piece (cadr (memq :piece keywords)))
	   (color (if piece (< piece ?a)
		    (chess-pos-side-to-move position)))
	   (specific-target (cadr (memq :target keywords)))
	   (test-piece
	    (upcase (or piece
			(chess-pos-piece position
					 (cadr (memq :index keywords))))))
	   pos plies file)
      ;; since we're looking for moves of a particular piece, do a
      ;; more focused search
      (dolist (candidate
	       (cond
		((cadr (memq :candidates keywords))
		 (cadr (memq :candidates keywords)))
		((setq pos (cadr (memq :index keywords)))
		 (list pos))
		((setq file (cadr (memq :file keywords)))
		 (let (candidates)
		   (dotimes (rank 8)
		     (setq pos (chess-rf-to-index rank file))
		     (if (chess-pos-piece-p position pos piece)
			 (push pos candidates)))
		   candidates))
		(t
		 (chess-pos-search position piece))))
	(cond
	 ;; pawn movement, which is diagonal 1 when taking, but forward
	 ;; 1 or 2 when moving (the most complex piece, actually)
	 ((= test-piece ?P)
	  (let* ((bias  (if color -1 1))
		 (ahead (chess-incr-index candidate bias 0))
		 (2ahead (chess-incr-index candidate (if color -2 2) 0)))
	    (when (chess-pos-piece-p position ahead ? )
	      (chess-ply--add bias 0 ahead)
	      (if (and (= (if color 6 1) (chess-index-rank candidate))
		       2ahead (chess-pos-piece-p position 2ahead ? ))
		  (chess-ply--add (if color -2 2) 0 2ahead)))
	    (when (setq pos (chess-incr-index candidate bias -1))
	      (if (chess-pos-piece-p position pos (not color))
		  (chess-ply--add nil nil pos))
	      ;; check for en passant capture toward queenside
	      (if (= (or (chess-pos-en-passant position) 100)
		     (or (chess-incr-index pos (if color 1 -1) 0) 200))
		  (chess-ply--add nil nil pos)))
	    (when (setq pos (chess-incr-index candidate bias 1))
	      (if (chess-pos-piece-p position pos (not color))
		  (chess-ply--add nil nil pos))
	      ;; check for en passant capture toward kingside
	      (if (= (or (chess-pos-en-passant position) 100)
		     (or (chess-incr-index pos (if color 1 -1) 0) 200))
		  (chess-ply--add nil nil pos)))))

	 ;; the rook, bishop and queen are the easiest; just look along
	 ;; rank and file and/or diagonal for the nearest pieces!
	 ((memq test-piece '(?R ?B ?Q))
	  (dolist (dir (cond
			((= test-piece ?R)
			 '(        (-1 0)
			   (0 -1)          (0 1)
				   (1 0)))
			((= test-piece ?B)
			 '((-1 -1)        (-1 1)

			   (1 -1)         (1 1)))
			((= test-piece ?Q)
			 '((-1 -1) (-1 0) (-1 1)
			   (0 -1)         (0 1)
			   (1 -1)  (1 0)  (1 1)))))
	    (setq pos (apply 'chess-incr-index candidate dir))
	    (while pos
	      (if (chess-pos-piece-p position pos ? )
		  (progn
		    (chess-ply--add nil nil pos)
		    (setq pos (apply 'chess-incr-index pos dir)))
		(if (chess-pos-piece-p position pos (not color))
		    (chess-ply--add nil nil pos))
		(setq pos nil)))

	    (when (= test-piece ?R)
	      (if (eq candidate
		      (chess-pos-can-castle position (if color ?K ?k)))
		  (let ((changes (chess-ply-castling-changes position)))
		    (if changes
			(if chess-ply-throw-if-any
			    (throw 'any-found t)
			  (push (cons position changes) plies)))))

	      (if (eq candidate
		      (chess-pos-can-castle position (if color ?Q ?q)))
		  (let ((changes (chess-ply-castling-changes position t)))
		    (if changes
			(if chess-ply-throw-if-any
			    (throw 'any-found t)
			  (push (cons position changes) plies))))))))

	 ;; the king is a trivial case of the queen, except when castling
	 ((= test-piece ?K)
	  (dolist (dir '((-1 -1) (-1 0) (-1 1)
			 (0 -1)         (0 1)
			 (1 -1)  (1 0)  (1 1)))
	    (setq pos (apply 'chess-incr-index candidate dir))
	    (if (and pos (or (chess-pos-piece-p position pos ? )
			     (chess-pos-piece-p position pos (not color))))
		(chess-ply--add nil nil pos)))

	  (if (chess-pos-can-castle position (if color ?K ?k))
	      (let ((changes (chess-ply-castling-changes position nil
							 candidate)))
		(if changes
		    (if chess-ply-throw-if-any
			(throw 'any-found t)
		      (push (cons position changes) plies)))))

	  (if (chess-pos-can-castle position (if color ?Q ?q))
	      (let ((changes (chess-ply-castling-changes position t
							 candidate)))
		(if changes
		    (if chess-ply-throw-if-any
			(throw 'any-found t)
		      (push (cons position changes) plies))))))

	 ;; the knight is a zesty little piece; there may be more than
	 ;; one, but at only one possible square in each direction
	 ((= test-piece ?N)
	  (dolist (dir '((-2 -1) (-2 1)
			 (-1 -2) (-1 2)
			 (1 -2)  (1 2)
			 (2 -1)  (2 1)))
	    ;; up the current file
	    (if (and (setq pos (apply 'chess-incr-index candidate dir))
		     (or (chess-pos-piece-p position pos ? )
			 (chess-pos-piece-p position pos (not color))))
		(chess-ply--add nil nil pos))))

	 (t (chess-error 'piece-unrecognized))))

      (delq nil plies)))))

(provide 'chess-ply)

;;; chess-ply.el ends here
