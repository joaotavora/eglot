;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Routines for manipulating chess positions
;;
;; $Revision$

;;; Commentary:

;; A chess `position' is a vector that starts with sixty-four
;; characters, representing the 8x8 grid of a chess position.  Each
;; position may contain p, r, n, b, k, q or <space>, or any of the
;; previous letters in uppercase.  Uppercase signifies white, and
;; lowercase means black.
;;
;; Creating a new position can be done with:
;;
;;   (chess-pos-create)
;;   (chess-pos-copy POSITION)
;;
;; To setup the chess board at an aritrary position, manipulate the
;; position that has been returned to you, or use a position input
;; module.

;; Once you have a chess position, there are several things you can do
;; with i.  First of all, a coordinate system of octal indices is
;; used, where ?\044 signifies rank 4 file 4 (i.e., "e4").  Rank is
;; numbered 0 to 7, top to bottom, and file is 0 to 7, left to right.
;; For those who wish to use ASCII coordinates, such as "e4", there
;; are two conversion functions:
;;
;;    (chess-coord-to-index STRING)
;;    (chess-index-to-coord INDEX)

;; With an octal index value, you can look up what's on a particular
;; square, or set that square's value:
;;
;;    (chess-pos-piece POSITION INDEX)
;;    (chess-pos-set-piece POSITION INDEX PIECE)
;;
;; PIECE must be one of the letters mentioned above (in upper or
;; lowercase), or a space to represent a blank square.
;;
;; To test whether a piece is at a particular position, use:
;;
;;    (chess-pos-piece-p POSITION INDEX PIECE)
;;
;; PIECE may also be t for any white piece, nil for any black piece,
;; or the symbol `any', which returns t if the square is not empty.

;; You can hunt for all occurances of a certain piece using:
;;
;;    (chess-pos-search POSITION PIECE)
;;
;; You might also try the `search' event, which employs the
;; intelligence of listening rules modules to search out your piece
;; according to legal piece movements.

;; Once you have a pair of indices, you can move a piece around:
;;
;;    (chess-pos-move POSITION FROM-INDEX TO-INDEX)
;;
;; NOTE This is not the safe way for users to move pieces around!
;; This function moves pieces DIRECTLY, without checking for legality,
;; or informing listening modules of the move.  To make an "official"
;; move, use:
;;
;;    (chess-move FROM-INDEX TO-INDEX)
;;
;; This will publish the move to all listening modules, which can then
;; handle the move event as they wish.

;;; Code:

(defgroup chess-pos nil
  "Routines for manipulating chess positions."
  :group 'chess)

(defconst chess-starting-position
  [;; the eight ranks and files of the chess position
   ?r ?n ?b ?q ?k ?b ?n ?r
   ?p ?p ?p ?p ?p ?p ?p ?p
   ?  ?  ?  ?  ?  ?  ?  ?   ; spaces are blanks!
   ?  ?  ?  ?  ?  ?  ?  ?   ; here too
   ?  ?  ?  ?  ?  ?  ?  ?   ; protect from whitespace-cleanup
   ?  ?  ?  ?  ?  ?  ?  ?   ; so have a comment afterwards
   ?P ?P ?P ?P ?P ?P ?P ?P
   ?R ?N ?B ?Q ?K ?B ?N ?R
   ;; index of pawn that can be captured en passant
   nil
   ;; can white and black castle on king or queen side?
   t t t t
   ;; is the side to move in: `check', `checkmate', `stalemate'
   nil
   ;; which color is it to move next?
   t
   ;; list of annotations for this position.  Textual annotations are
   ;; simply that, while lists represent interesting variations.
   nil]
  "Starting position of a chess position.")

(defsubst chess-pos-piece (position index)
  "Return the piece on POSITION at INDEX."
  (aref position index))

(defsubst chess-pos-set-piece (position index piece)
  "Set the piece on POSITION at INDEX to PIECE."
  (aset position index piece))

(defsubst chess-pos-can-castle (position side)
  "Return whether the king can castle on SIDE.
SIDE must be either ?q or ?k (case determines color)."
  (aref position (+ 65 (if (< side ?a)
			(if (= side ?K) 0 1)
		      (if (= side ?k) 2 3)))))

(defsubst chess-pos-set-can-castle (position side value)
  "Set whether the king can castle on SIDE.
SIDE must be either ?q or ?k (case determines color)."
  (aset position (+ 65 (if (< side ?a)
			(if (= side ?K) 0 1)
		      (if (= side ?k) 2 3))) value))

(defsubst chess-pos-en-passant (position)
  "Return index of pawn that can be captured en passant, or nil."
  (aref position 64))

(defsubst chess-pos-set-en-passant (position index)
  "Set index of pawn that can be captured en passant."
  (aset position 64 index))

(defsubst chess-pos-status (position)
  "Return whether the side to move is in a special state.
The symbols allowed are: `check', `checkmate', `stalemate'."
  (aref position 69))

(defsubst chess-pos-set-status (position value)
  "Set whether the side to move is in a special state."
  (aset position 69 value))

(defsubst chess-pos-side-to-move (position)
  "Return the color whose move it is in POSITION."
  (aref position 70))

(defsubst chess-pos-set-side-to-move (position color)
  "Set the color whose move it is in POSITION."
  (aset position 70 color))

(defsubst chess-pos-annotations (position)
  "Return the list of annotations for this position."
  (aref position 71))

(defun chess-pos-add-annotation (position annotation)
  "Add an annotation for this position."
  (let ((ann (chess-pos-annotations position)))
    (if ann
	(nconc ann (list annotation))
      (aset position 71 (list annotation)))))

(defun chess-pos-copy (position)
  "Create a new chess position, set at the starting position.
If BLANK is non-nil, all of the squares will be empty.
The current side-to-move is always white."
  (let ((copy (make-vector 72 nil)) elem)
    (dotimes (i 71)
      (setq elem (aref position i))
      (aset copy i
	    (cond
	     ((listp elem) (copy-alist elem))
	     ((vectorp elem) (vconcat elem))
	     (t elem))))
    copy))

(defun chess-pos-create (&optional blank)
  "Create a new chess position, set at the starting position.
If BLANK is non-nil, all of the squares will be empty.
The current side-to-move is always white."
  (if blank
      (vconcat (make-vector 64 ? )
	       [nil t t t t nil t nil])
    (chess-pos-copy chess-starting-position)))

(defsubst chess-rf-to-index (rank file)
  "Convert RANK and FILE coordinates into an octal index."
  (+ (* 8 rank) file))

(defsubst chess-coord-to-index (coord)
  "Convert a COORD (ex. e2, f3) into a chess.el index."
  (+ (* 8 (- 7 (- (aref coord 1) ?1)))
     (- (aref coord 0) ?a)))

(defsubst chess-index-to-coord (index)
  "Convert a COORD (ex. e2, f3) into a chess position index."
  (concat (char-to-string (+ (mod index 8) ?a))
	  (char-to-string (+ (- 7 (/ index 8)) ?1))))

(defsubst chess-index-rank (index) (/ index 8))
(defsubst chess-index-file (index) (mod index 8))

(defun chess-add-index (index rank-move file-move)
  "Create a new INDEX from an old one, by adding rank-move and file-move."
  (let* ((rank (chess-index-rank index))
	 (file (chess-index-file index))
	 (newrank (+ rank rank-move))
	 (newfile (+ file file-move)))
    (if (and (>= newrank 0) (< newrank 8)
	     (>= newfile 0) (< newfile 8))
	(chess-rf-to-index newrank newfile))))

(defun chess-pos-piece-p (position index piece-or-color)
  "Return non-nil if at POSITION/INDEX there is the given PIECE-OR-COLOR.
If PIECE-OR-COLOR is t for white or nil for black, any piece of that
color will do."
  (let ((p (chess-pos-piece position index)))
    (cond
     ((= p ? ) (= p piece-or-color))
     ((eq piece-or-color t) (< p ?a))
     ((eq piece-or-color nil) (> p ?a))
     (t (= p piece-or-color)))))

(defun chess-pos-search (position piece-or-color)
  "Look on POSITION anywhere for PIECE-OR-COLOR, returning all coordinates.
If PIECE-OR-COLOR is t for white or nil for black, any piece of that
color will do."
  (let (found)
    (dotimes (i 64)
      (if (chess-pos-piece-p position i piece-or-color)
	  (push i found)))
    found))

(defun chess-pos-move (position &rest changes)
  "Move a piece on the POSITION directly, using the indices FROM and TO.
This function does not check any rules, it only makes sure you are not
trying to move a blank square."
  (while changes
    (unless (symbolp (car changes))
      (let* ((from (car changes))
	     (to (cadr changes))
	     (piece (chess-pos-piece position from)))
	(if (= piece ? )
	    (error "Attempted piece move from blank square %s" from))
	(chess-pos-set-piece position from ? )
	(chess-pos-set-piece position to piece)))
    (setq changes (cddr changes)))
  ;; once a piece is moved, en passant is no longer available
  (chess-pos-set-en-passant position nil)
  ;; toggle the side whose move it is
  (chess-pos-set-side-to-move position
			      (not (chess-pos-side-to-move position)))
  position)

(defun chess-search-position (position target piece)
  "Look on POSITION from TARGET for a PIECE that can move there.
This routine looks along legal paths of movement for PIECE.  It
differs from `chess-pos-search', which is a more basic function that
doesn't take piece movement into account.

If PIECE is t or nil, legal piece movements for any piece of that
color will be considered (t for white, nil for black).  Otherwise, the
case of the PIECE determines color.

The return value is a list of candidates, which means a list of
indices which indicate where a piece may have moved from."
  (let* ((color (if (char-valid-p piece)
		    (< piece ?a)
		  piece))
	 (bias (if color -1 1))
	 p pos candidates)
    (cond
     ;; if the piece is `t', it means to find the candidates resulting
     ;; from any piece movement.  This is useful for testing whether a
     ;; king is in check, for example.
     ((memq piece '(t nil))
      (setq candidates (list t))
      (dolist (p '(?P ?R ?N ?B ?K ?Q))
	(nconc candidates
	       (chess-search-position position target
				      (if piece p (downcase p)))))
      (setq candidates (cdr candidates)))

     ;; pawn movement, which is diagonal 1 when taking, but forward
     ;; 1 or 2 when moving (the most complex piece, actually)
     ((= (upcase piece) ?P)
      (let ((p (chess-pos-piece position target)))
	(if (if (= p ? )
		;; check for en passant
		(and (= (chess-index-rank target) (if color 2 5))
		     (setq pos (chess-add-index target bias 0))
		     (chess-pos-piece-p position pos (if color ?p ?P))
		     (and (chess-pos-en-passant position)
			  (= (chess-pos-en-passant position) target))
		     (setq candidates (list pos)))
	      (if color (> p ?a) (< p ?a)))
	    (let ((cands (list t)))
	      (setq pos (chess-add-index target (- bias) -1))
	      (if (and pos (chess-pos-piece-p position pos piece))
		  (nconc cands (list pos)))
	      (setq pos (chess-add-index target (- bias) 1))
	      (if (and pos (chess-pos-piece-p position pos piece))
		  (nconc cands (list pos)))
	      (if candidates
		  (nconc candidates (cdr cands))
		(setq candidates (cdr cands))))
	  (if (setq pos (chess-add-index target (- bias) 0))
	      (if (chess-pos-piece-p position pos piece)
		  (setq candidates (list pos))
		(when (and (= ?  (chess-pos-piece position pos))
			   (= (if color 4 3) (chess-index-rank target)))
		  (setq pos (chess-add-index pos (- bias) 0))
		  (if (and pos (chess-pos-piece-p position pos piece))
		      (setq candidates (list pos)))))))))

     ;; the rook, bishop and queen are the easiest; just look along
     ;; rank and file and/or diagonal for the nearest pieces!
     ((memq (upcase piece) '(?R ?B ?Q))
      (setq candidates (list t))
      (dolist (dir (cond
		    ((= (upcase piece) ?R)
		     '(        (-1 0)
		       (0 -1)          (0 1)
			       (1 0)))
		    ((= (upcase piece) ?B)
		     '((-1 -1)        (-1 1)

		       (1 -1)         (1 1)))
		    ((= (upcase piece) ?Q)
		     '((-1 -1) (-1 0) (-1 1)
			(0 -1)         (0 1)
			(1 -1)  (1 0)  (1 1)))))
	;; up the current file
	(setq pos (apply 'chess-add-index target dir))
	(while pos
	  (if (chess-pos-piece-p position pos piece)
	      (progn
		(nconc candidates (list pos))
		(setq pos nil))
	    (if (/= (chess-pos-piece position pos) ? )
		(setq pos nil)
	      (setq pos (apply 'chess-add-index pos dir))))))
      (setq candidates (cdr candidates)))

     ;; the king is a trivial case of the queen, except when castling
     ((= (upcase piece) ?K)
      (let ((dirs '((-1 -1) (-1 0) (-1 1)
		     (0 -1)         (0 1)
		     (1 -1)  (1 0)  (1 1))))
	(while dirs
	  ;; up the current file
	  (setq pos (apply 'chess-add-index target (car dirs)))
	  (if (and pos (chess-pos-piece-p position pos piece))
	      (setq candidates (list pos) dirs nil)
	    (setq dirs (cdr dirs)))))
      (let ((rank (if color 7 0)))
	;; if we can still castle, then the king and rook are in their
	;; squares; also, make sure that the user is not attempting to
	;; castle through check
	(if (and
	     (null candidates)
	     (or (and (equal target (chess-rf-to-index rank 6))
		      (= (chess-pos-piece position (chess-rf-to-index rank 4))
			 (if color ?K ?k))
		      (chess-pos-can-castle position (if color ?K ?k))
		      (setq pos (chess-rf-to-index rank 5))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-search-position position pos (not color)))
		      (setq pos (chess-rf-to-index rank 6))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-search-position position pos (not color))))
		 (and (equal target (chess-rf-to-index rank 2))
		      (= (chess-pos-piece position (chess-rf-to-index rank 4))
			 (if color ?K ?k))
		      (chess-pos-can-castle position (if color ?Q ?q))
		      (setq pos (chess-rf-to-index rank 1))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-search-position position pos (not color)))
		      (setq pos (chess-rf-to-index rank 2))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-search-position position pos (not color)))
		      (setq pos (chess-rf-to-index rank 3))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-search-position position pos (not color))))))
	    (setq candidates (list (chess-rf-to-index rank 4))))))

     ;; the knight is a zesty little piece; there may be more than
     ;; one, but at only one possible square in each direction
     ((= (upcase piece) ?N)
      (setq candidates (list t))
      (dolist (dir '((-2 -1) (-2 1)
		     (-1 -2) (-1 2)
		      (1 -2)  (1 2)
		      (2 -1)  (2 1)))
	;; up the current file
	(if (and (setq pos (apply 'chess-add-index target dir))
		 (chess-pos-piece-p position pos piece))
	    (nconc candidates (list pos))))
      (setq candidates (cdr candidates)))

     (t (error "Unrecognized piece identifier")))

    ;; prune from the discovered candidates list any moves which would
    ;; leave the king in check; castling through check has already
    ;; been eliminated.
    (if (char-valid-p piece)
	(let ((cand candidates) last-cand pos king-pos)
	  (while cand
	    ;; determine the resulting position
	    (setq pos (chess-pos-move (chess-pos-copy position)
				      (car cand) target))
	    ;; find the king (only once if the king isn't moving)
	    (if (or (null king-pos)
		    (eq (downcase piece) ?k))
		(setq king-pos (chess-pos-search pos (if color ?K ?k))))
	    ;; can anybody from the opposite side reach him?  if so,
	    ;; drop the candidate
	    (if (or (null king-pos)
		    (chess-search-position pos (car king-pos) (not color)))
		(if last-cand
		    (setcdr last-cand (cdr cand))
		  (setq candidates (cdr candidates)))
	      (setq last-cand cand))
	    (setq cand (cdr cand)))))

    ;; return the final list of candidate moves
    candidates))

(provide 'chess-pos)

;;; chess-pos.el ends here
