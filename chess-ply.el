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

(defun chess-ply-any-keyword (ply &rest keywords)
  (catch 'found
    (dolist (keyword keywords)
      (if (memq keyword (chess-ply-changes ply))
	  (throw 'found keyword)))))

(defun chess-ply-keyword (ply keyword)
  (let ((item (memq keyword (chess-ply-changes ply))))
    (if item
	;; these are special keywords which use a value argument;
	;; `which' is for disambiguating algebraic moves, `promote'
	;; indicates the piece to promote to, `white' is white's
	;; remaining time in seconds, and similarly for `black'
	(if (memq keyword '(:which :promote :white :black))
	    (cadr item)
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

(chess-message-catalog 'english
  '((pawn-promote-query . "Promote pawn to queen/rook/knight/bishop? ")))

(defvar chess-ply-no-promotions nil)

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

	(when (= piece (if color ?P ?p))
	  ;; is this a pawn move to the ultimate rank?  if so, and we
	  ;; haven't already been told, ask for the piece to promote
	  ;; it to
	  (if (and (not (memq :promote changes))
		   (not chess-ply-no-promotions)
		   (= (if color 0 7) (chess-index-rank (cadr changes))))
	      (let ((new-piece (completing-read
				(chess-string 'pawn-promote-query)
				chess-piece-name-table nil t "queen")))
		(setq new-piece
		      (cdr (assoc new-piece chess-piece-name-table)))
		(if color
		    (setq new-piece (upcase new-piece)))
		(nconc changes (list :promote new-piece))))

	  ;; is this an en-passant capture?
	  (if (= (or (chess-pos-en-passant position) 100)
		 (or (chess-incr-index (cadr changes) (if color 1 -1) 0) 200))
	      (nconc changes (list :en-passant))))

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

(defvar chess-ply-throw-if-any nil)

(eval-when-compile
  (defvar position)
  (defvar candidate)
  (defvar color)
  (defvar plies))
(defsubst chess-ply--add (rank-adj file-adj &optional pos)
  "This is totally a shortcut."
  (let ((ply (chess-ply-create position candidate
			       (or pos (chess-incr-index candidate
							 rank-adj file-adj)))))
    (when ply
      (if chess-ply-throw-if-any
	  (throw 'any-found t))
      (push ply plies))))

(defun chess-legal-plies (position &rest keywords)
  "Return a list of all legal plies in POSITION.
KEYWORDS allowed are:

  :any   return t if any piece can move at all
  :color <t or nil>
  :piece <piece character>
  :file <number 0 to 7> [can only be used if :piece is present]
  :index <coordinate index>

These will constrain the plies generated to those matching the above
criteria."
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
	   (test-piece
	    (upcase (or piece
			(chess-pos-piece position
					 (cadr (memq :index keywords))))))
	   (chess-ply-no-promotions t)
	   pos plies file)
      ;; since we're looking for moves of a particular piece, do a
      ;; more focused search
      (dolist (candidate
	       (cond
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
	      (chess-ply--add bias 0)
	      (if (and (= (if color 6 1) (chess-index-rank candidate))
		       (chess-pos-piece-p position 2ahead ? ))
		  (chess-ply--add (if color -2 2) 0)))
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
	    ;; up the current file
	    (setq pos (apply 'chess-incr-index candidate dir))
	    ;; jww (2002-04-11): In Fischer Random castling, the rook can
	    ;; move in wacky ways
	    (while pos
	      (if (chess-pos-piece-p position pos ? )
		  (progn
		    (chess-ply--add nil nil pos)
		    (setq pos (apply 'chess-incr-index pos dir)))
		(if (chess-pos-piece-p position pos (not color))
		    (chess-ply--add nil nil pos))
		(setq pos nil)))))

	 ;; the king is a trivial case of the queen, except when castling
	 ((= test-piece ?K)
	  (dolist (dir '((-1 -1) (-1 0) (-1 1)
			 (0 -1)         (0 1)
			 (1 -1)  (1 0)  (1 1)))
	    (setq pos (apply 'chess-incr-index candidate dir))
	    (if (and pos
		     (or (chess-pos-piece-p position pos ? )
			 (chess-pos-piece-p position pos (not color))))
		(chess-ply--add nil nil pos)))

	  (if (chess-pos-can-castle position (if color ?K ?k))
	      (chess-ply--add 0 2))
	  (if (chess-pos-can-castle position (if color ?Q ?q))
	      (chess-ply--add 0 -2)))

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
