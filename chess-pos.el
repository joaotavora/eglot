;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Routines for manipulating chess positions
;;

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

(require 'chess-message)
(require 'chess-fen)

(defgroup chess-pos nil
  "Routines for manipulating chess positions."
  :group 'chess)

(defvar chess-pos-always-white nil
  "When set, it is assumed that white is always on move.
This is really only useful when setting up training positions.
This variable automatically becomes buffer-local when changed.")

(make-variable-buffer-local 'chess-pos-always-white)

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
   nil
   ;; where are the kings?
   60 4
   ;; an alist of epd evaluation codes and arguments
   nil]
  "Starting position of a chess position.")

(defsubst chess-pos-piece (position index)
  "Return the piece on POSITION at INDEX."
  (assert position)
  (assert (and (>= index 0) (< index 64)))
  (aref position index))

(defsubst chess-pos-king-index (position color)
  "Return the index on POSITION of the king.
If COLOR is non-nil, return the position of the white king, otherwise
return the position of the black king."
  (assert position)
  (assert (memq color '(nil t)))
  (or (aref position (if color 72 73))
      (aset position (if color 72 73)
	    (chess-pos-search position (if color ?K ?k)))))

(defsubst chess-pos-set-king-pos (position color index)
  "Set the known index of the king on POSITION for COLOR, to INDEX.
It is never necessary to call this function."
  (assert position)
  (assert (memq color '(nil t)))
  (assert (and (>= index 0) (< index 64)))
  (aset position (if color 72 73) index))

(defsubst chess-pos-set-piece (position index piece)
  "Set the piece on POSITION at INDEX to PIECE.
PIECE must be one of K Q N B R or P.  Use lowercase to set black
pieces."
  (assert position)
  (assert (and (>= index 0) (< index 64)))
  (assert (memq piece '(?  ?K ?Q ?N ?B ?R ?P ?k ?q ?n ?b ?r ?p)))
  (aset position index piece)
  (if (= piece ?K)
      (chess-pos-set-king-pos position t index)
    (if (= piece ?k)
	(chess-pos-set-king-pos position nil index))))

(defsubst chess-pos-can-castle (position side)
  "Return whether the king on POSITION can castle on SIDE.
SIDE must be either ?K for the kingside, or ?Q for the queenside (use
lowercase to query if black can castle)."
  (assert position)
  (assert (memq side '(?K ?Q ?k ?q)))
  (aref position (+ 65 (if (< side ?a)
			   (if (= side ?K) 0 1)
			 (if (= side ?k) 2 3)))))

(defsubst chess-pos-set-can-castle (position side value)
  "Set whether the king can castle on the given POSITION on SIDE.

See `chess-pos-can-castle'.

It is only necessary to call this function if setting up a position
manually.  Note that all newly created positions have full castling
priveleges set, unless the position is created blank, in which case
castling priveleges are unset.  See `chess-pos-copy'."
  (assert position)
  (assert (memq side '(?K ?Q ?k ?q)))
  (assert (memq value '(nil t)))
  (aset position (+ 65 (if (< side ?a)
			   (if (= side ?K) 0 1)
			 (if (= side ?k) 2 3))) value))

(defsubst chess-pos-en-passant (position)
  "Return the index of any pawn on POSITION that can be captured en passant.
Returns nil if en passant is unavailable."
  (assert position)
  (aref position 64))

(defsubst chess-pos-set-en-passant (position index)
  "Set the index of any pawn on POSITION that can be captured en passant."
  (assert position)
  (assert (or (eq index nil)
	      (and (>= index 0) (< index 64))))
  (aset position 64 index))

(defsubst chess-pos-status (position)
  "Return whether the side to move in the POSITION is in a special state.
nil is returned if not, otherwise one of the symbols: `check',
`checkmate', `stalemate'."
  (assert position)
  (aref position 69))

(defsubst chess-pos-set-status (position value)
  "Set whether the side to move in POSITION is in a special state.
VALUE should either be nil, to indicate that the POSITION is normal,
or one of the symbols: `check', `checkmate', `stalemate'."
  (assert position)
  (assert (or (eq value nil) (symbolp value)))
  (aset position 69 value))

(defsubst chess-pos-side-to-move (position)
  "Return the color whose move it is in POSITION."
  (assert position)
  (aref position 70))

(defsubst chess-pos-set-side-to-move (position color)
  "Set the color whose move it is in POSITION."
  (assert position)
  (assert (memq color '(nil t)))
  (aset position 70 color))

(defsubst chess-pos-annotations (position)
  "Return the list of annotations for this position."
  (assert position)
  (aref position 71))

(defsubst chess-pos-set-annotations (position annotations)
  "Return the list of annotations for this position."
  (assert position)
  (assert (listp annotations))
  (aset position 71 annotations))

(defun chess-pos-add-annotation (position annotation)
  "Add an annotation for this position."
  (assert position)
  (assert (or (stringp annotation) (listp annotation)))
  (let ((ann (chess-pos-annotations position)))
    (if ann
	(nconc ann (list annotation))
      (aset position 71 (list annotation)))))

(defsubst chess-pos-epd-alist (position)
  "Return the alist of EPD evaluations for this position."
  (assert position)
  (aref position 74))

(defsubst chess-pos-set-epd-alist (position alist)
  "Return the alist of EPD evaluations for this position."
  (assert position)
  (assert (listp alist))
  (aset position 74 alist))

(defsubst chess-pos-epd (position opcode)
  "Return the value of the given EPD OPCODE, or nil if not set."
  (assert position)
  (assert opcode)
  (let ((epd (chess-pos-epd-alist position)))
    (if epd
	(cdr (assq opcode epd)))))

(defun chess-pos-set-epd (position opcode &optional value)
  "Set the given EPD OPCODE to VALUE, or t if VALUE is not specified."
  (assert position)
  (assert opcode)
  (let* ((epd (chess-pos-epd-alist position))
	 (entry (assq opcode epd)))
    (if entry
	(setcdr entry (or value t))
      (push (cons opcode (or value t)) epd))))

(defun chess-pos-del-epd (position opcode)
  "Delete the given EPD OPCODE."
  (assert position)
  (assert opcode)
  (chess-pos-set-epd-alist position
			   (assq-delete-all opcode
					    (chess-pos-epd-alist position))))

(defsubst chess-pos-copy (position)
  "Copy the given chess POSITION.
If there are annotations or EPD opcodes set, these lists are copied as
well, so that the two positions do not share the same lists."
  (assert position)
  (let ((copy (vconcat position)) i)
    (setq i (chess-pos-annotations position))
    (if i (chess-pos-set-annotations copy (copy-alist i)))
    (setq i (chess-pos-epd-alist position))
    (if (and (not (eq i nil)) (listp i))
	(chess-pos-set-epd-alist copy (copy-alist i)))
    copy))

(defsubst chess-pos-create (&optional blank)
  "Create a new chess position, set at the starting position.
If BLANK is non-nil, all of the squares will be empty.
The current side-to-move is always white."
  (if blank
      (vconcat (make-vector 64 ? )
	       [nil nil nil nil nil nil t nil nil nil nil])
    (chess-pos-copy chess-starting-position)))

(defsubst chess-rf-to-index (rank file)
  "Convert RANK and FILE coordinates into an octal index."
  (assert (or (>= rank 0) (< rank 8)))
  (assert (or (>= file 0) (< file 8)))
  (+ (* 8 rank) file))

(defsubst chess-coord-to-index (coord)
  "Convert a COORD string into an index value."
  (assert (stringp coord))
  (assert (= (length coord) 2))
  (+ (* 8 (- 7 (- (aref coord 1) ?1)))
     (- (aref coord 0) ?a)))

(defsubst chess-index-to-coord (index)
  "Convert the chess position INDEX into a coord string."
  (assert (and (>= index 0) (< index 64)))
  (concat (char-to-string (+ (mod index 8) ?a))
	  (char-to-string (+ (- 7 (/ index 8)) ?1))))

(defsubst chess-index-rank (index)
  "Return the rank component of the given INDEX."
  (assert (and (>= index 0) (< index 64)))
  (/ index 8))

(defsubst chess-index-file (index)
  "Return the file component of the given INDEX."
  (assert (and (>= index 0) (< index 64)))
  (mod index 8))

(defsubst chess-incr-index (index rank-move file-move)
  "Create a new INDEX from an old one, by adding RANK-MOVE and FILE-MOVE."
  (assert (and (>= index 0) (< index 64)))
  (assert (and (>= rank-move -7) (<= rank-move 7)))
  (assert (and (>= file-move -7) (<= file-move 7)))
  (let ((newrank (+ (chess-index-rank index) rank-move))
	(newfile (+ (chess-index-file index) file-move)))
    (if (and (>= newrank 0) (< newrank 8)
	     (>= newfile 0) (< newfile 8))
	(chess-rf-to-index newrank newfile))))

(defsubst chess-incr-index* (index rank-move file-move)
  "Create a new INDEX from an old one, by adding RANK-MOVE and FILE-MOVE.
This differs from `chess-incr-index' by performing no safety checks,
in order to execute faster."
  (assert (and (>= index 0) (< index 64)))
  (assert (and (>= rank-move -7) (<= rank-move 7)))
  (assert (and (>= file-move -7) (<= file-move 7)))
  (chess-rf-to-index (+ (chess-index-rank index) rank-move)
		     (+ (chess-index-file index) file-move)))

(defsubst chess-pos-piece-p (position index piece-or-color)
  "Return non-nil if at POSITION/INDEX there is the given PIECE-OR-COLOR.
If PIECE-OR-COLOR is t for white or nil for black, any piece of that
color will do."
  (assert position)
  (assert (and (>= index 0) (< index 64)))
  (assert (memq piece-or-color
		'(t nil ?  ?K ?Q ?N ?B ?R ?P ?k ?q ?n ?b ?r ?p)))
  (let ((p (chess-pos-piece position index)))
    (cond
     ((= p ? ) (eq p piece-or-color))
     ((eq piece-or-color t) (< p ?a))
     ((eq piece-or-color nil) (> p ?a))
     (t (= p piece-or-color)))))

(defsubst chess-pos-search (position piece-or-color)
  "Look on POSITION anywhere for PIECE-OR-COLOR, returning all coordinates.
If PIECE-OR-COLOR is t for white or nil for black, any piece of that
color will do."
  (assert position)
  (assert (memq piece-or-color
		'(t nil ?  ?K ?Q ?N ?B ?R ?P ?k ?q ?n ?b ?r ?p)))
  (let (found)
    (dotimes (i 64)
      (if (chess-pos-piece-p position i piece-or-color)
	  (push i found)))
    found))

(defsubst chess-pos-to-string (position &optional full)
  "Convert the given POSITION into a string.
The returned string can be converted back to a position using
`chess-pos-from-string'."
  (assert position)
  (chess-pos-to-fen position full))

(defsubst chess-pos-from-string (string)
  "Convert the given STRING to a chess position.
This string should have been created by `chess-pos-to-string'."
  (assert (stringp string))
  (chess-fen-to-pos string))

(defconst chess-pos-piece-values
  '((?p . 1)
    (?n . 3)
    (?b . 3)
    (?q . 9)
    (?r . 5)
    (?k . 0)))

(defun chess-pos-material-value (position color)
  "Return the aggregate material value in POSITION for COLOR."
  (assert position)
  (assert (memq color '(nil t)))
  (let ((pieces (chess-pos-search position color))
	(value 0))
    (dolist (index pieces)
      (setq value
	    (+ value (cdr (assq (downcase (chess-pos-piece position index))
				chess-pos-piece-values)))))
    value))

(chess-message-catalog 'english
  '((move-from-blank . "Attempted piece move from blank square %s")))

(defun chess-pos-move (position &rest changes)
  "Move a piece on the POSITION directly, using the indices FROM and TO.
This function does not check any rules, it only makes sure you are not
trying to move a blank square."
  (assert position)
  (assert (listp changes))
  (assert (> (length changes) 0))
  (let ((ch changes))
    (while ch
      (if (symbolp (car ch))
	  (setq ch nil)
	(let* ((from (car ch))
	       (to (cadr ch))
	       (piece (chess-pos-piece position from)))
	  (if (= piece ? )
	      (chess-error 'move-from-blank (chess-index-to-coord from)))
	  (chess-pos-set-piece position from ? )
	  (chess-pos-set-piece position to piece))
	(setq ch (cddr ch)))))

  ;; now fix up the resulting position
  (let ((color (chess-pos-side-to-move position)))

    ;; if the move was en-passant, remove the captured pawn
    (if (memq :en-passant changes)
	(chess-pos-set-piece position
			     (chess-incr-index (cadr changes)
					       (if color 1 -1) 0) ? ))

    ;; once a piece is moved, en passant is no longer available
    (chess-pos-set-en-passant position nil)

    ;; if a king or rook moves, no more castling; also, if a pawn
    ;; jumps ahead two, mark it en-passantable
    (unless (symbolp (car changes))
      (let ((piece (downcase (chess-pos-piece position (cadr changes)))))
	(cond
	 ((= piece ?k)
	  (chess-pos-set-can-castle position (if color ?K ?k) nil)
	  (chess-pos-set-can-castle position (if color ?Q ?q) nil))

	 ((= piece ?r)
	  (let ((king (chess-pos-king-index position color)))
	    (if (and (chess-pos-can-castle position (if color ?Q ?q))
		     (< (chess-index-file (car changes)) king))
		(chess-pos-set-can-castle position (if color ?Q ?q) nil)
	      (if (and (chess-pos-can-castle position (if color ?K ?k))
		       (> (chess-index-file (car changes)) king))
		  (chess-pos-set-can-castle position (if color ?K ?k) nil)))))

	 ((and (= piece ?p)
	       (> (abs (- (chess-index-rank (cadr changes))
			  (chess-index-rank (car changes)))) 1))
	  (chess-pos-set-en-passant position (cadr changes))))))

    ;; toggle the side whose move it is
    (unless chess-pos-always-white
      (chess-pos-set-side-to-move position (not color)))

    ;; promote the piece if we were meant to
    (let ((new-piece (cadr (memq :promote changes))))
      (if new-piece
	  (chess-pos-set-piece position (cadr changes)
			       (if color
				   new-piece
				 (downcase new-piece)))))

    ;; did we leave the position in check, mate or stalemate?
    (chess-pos-set-status position nil)
    (cond
     ((memq :check changes)
      (chess-pos-set-status position :check))
     ((memq :checkmate changes)
      (chess-pos-set-status position :checkmate))
     ((memq :stalemate changes)
      (chess-pos-set-status position :stalemate)))

    ;; return the final position
    position))

(chess-message-catalog 'english
  '((piece-unrecognized . "Unrecognized piece identifier")))

(eval-when-compile
  (defvar candidates)
  (defvar check-only))

(defsubst chess--add-candidate (candidate)
  (if check-only
      (throw 'in-check t)
    (push candidate candidates)))

(defun chess-search-position (position target piece &optional check-only)
  "Look on POSITION from TARGET for a PIECE that can move there.
This routine looks along legal paths of movement for PIECE.  It
differs from `chess-pos-search', which is a more basic function that
doesn't take piece movement into account.

If PIECE is t or nil, legal piece movements for any piece of that
color will be considered (t for white, nil for black).  Otherwise, the
case of the PIECE determines color.

The return value is a list of candidates, which means a list of
indices which indicate where a piece may have moved from."
  (assert position)
  (assert (and (>= target 0) (< target 64)))
  (assert (memq piece '(t nil ?K ?Q ?N ?B ?R ?P ?k ?q ?n ?b ?r ?p)))
  (let* ((color (if (char-valid-p piece)
		    (< piece ?a)
		  piece))
	 (bias (if color -1 1))
	 (test-piece (and (char-valid-p piece)
			  (upcase piece)))
	 p pos candidates)
    (cond
     ;; if the piece is `t', it means to find the candidates resulting
     ;; from any piece movement.  This is useful for testing whether a
     ;; king is in check, for example.
     ((memq piece '(t nil))
      (dolist (p (if check-only
		     '(?P ?R ?N ?B ?Q)
		   '(?P ?R ?N ?B ?Q ?K)))
	(mapc 'chess--add-candidate
	      (chess-search-position position target
				     (if piece p (downcase p))
				     check-only))))

     ;; skip erroneous space requests
     ((= test-piece ? ))

     ;; pawn movement, which is diagonal 1 when taking, but forward
     ;; 1 or 2 when moving (the most complex piece, actually)
     ((= test-piece ?P)
      (let ((p (chess-pos-piece position target)))
	(if (if (= p ? )
		;; check for en passant
		(and (= (chess-index-rank target) (if color 2 5))
		     ;; make this fail if no en-passant is possible
		     (= (or (chess-pos-en-passant position) 100)
			(or (chess-incr-index target (if color 1 -1) 0) 200))
		     (or (and (setq pos (chess-incr-index target
							  (if color 1 -1) -1))
			      (chess-pos-piece-p position pos
						 (if color ?P ?p)))
			 (and (setq pos (chess-incr-index target
							  (if color 1 -1) 1))
			      (chess-pos-piece-p position pos
						 (if color ?P ?p)))))
	      (if color (> p ?a) (< p ?a)))
	    (progn
	      (if (and (setq pos (chess-incr-index target (- bias) -1))
		       (chess-pos-piece-p position pos piece))
		  (chess--add-candidate pos))
	      (if (and (setq pos (chess-incr-index target (- bias) 1))
		       (chess-pos-piece-p position pos piece))
		  (chess--add-candidate pos)))
	  (if (setq pos (chess-incr-index target (- bias) 0))
	      (if (chess-pos-piece-p position pos piece)
		  (chess--add-candidate pos)
		(if (and (chess-pos-piece-p position pos ? )
			 (= (if color 4 3) (chess-index-rank target))
			 (setq pos (chess-incr-index pos (- bias) 0))
			 (chess-pos-piece-p position pos piece))
		    (chess--add-candidate pos)))))))

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
	(setq pos (apply 'chess-incr-index target dir))
	;; jww (2002-04-11): In Fischer Random castling, the rook can
	;; move in wacky ways
	(while pos
	  (if (chess-pos-piece-p position pos piece)
	      (progn
		(chess--add-candidate pos)
		(setq pos nil))
	    (setq pos (and (chess-pos-piece-p position pos ? )
			   (apply 'chess-incr-index pos dir)))))))

     ;; the king is a trivial case of the queen, except when castling
     ((= test-piece ?K)
      (let ((dirs '((-1 -1) (-1 0) (-1 1)
		     (0 -1)         (0 1)
		     (1 -1)  (1 0)  (1 1))))
	(while dirs
	  ;; up the current file
	  (setq pos (apply 'chess-incr-index target (car dirs)))
	  (if (and pos (chess-pos-piece-p position pos piece))
	      (progn
		(chess--add-candidate pos)
		(setq dirs nil))
	    (setq dirs (cdr dirs)))))

      (let ((rank (if color 7 0)))
	;; if we can still castle, then the king and rook are in their
	;; squares; also, make sure that the user is not attempting to
	;; castle through check
	(if (and (null candidates)
		 (or (and (equal target (chess-rf-to-index rank 6))
			  (chess-pos-can-castle position (if color ?K ?k)))
		     (and (equal target (chess-rf-to-index rank 2))
			  (chess-pos-can-castle position (if color ?Q ?q)))))
	    (let* ((king (chess-pos-king-index position color))
		   (king-file (chess-index-file king))
		   (long (= 2 (chess-index-file target)))
		   (file (if long 1 6))
		   (legal t))
	      ;; jww (2002-04-10): this needs to be a bit more subtle
	      ;; for Fischer Random castling
	      (while (and legal (funcall (if long '< '>) file king-file))
		(setq pos (chess-rf-to-index rank file))
		(if (or (not (chess-pos-piece-p position pos ? ))
			(chess-search-position position pos (not color)
					       check-only))
		    (setq legal nil)
		  (setq file (funcall (if long '1+ '1-) file))))
	      (if legal
		  (chess--add-candidate (chess-rf-to-index rank 4)))))))

     ;; the knight is a zesty little piece; there may be more than
     ;; one, but at only one possible square in each direction
     ((= test-piece ?N)
      (dolist (dir '((-2 -1) (-2 1)
		     (-1 -2) (-1 2)
		      (1 -2)  (1 2)
		      (2 -1)  (2 1)))
	;; up the current file
	(if (and (setq pos (apply 'chess-incr-index target dir))
		 (chess-pos-piece-p position pos piece))
	    (chess--add-candidate pos))))

     (t (chess-error 'piece-unrecognized)))

    ;; prune from the discovered candidates list any moves which would
    ;; leave the king in check; castling through check has already
    ;; been eliminated.
    (if (and candidates (char-valid-p piece))
	(setq candidates
	      (chess-pos-legal-moves position color target candidates)))

    ;; return the final list of candidate moves
    candidates))

(defun chess-pos-legal-moves (position color target candidates)
  "Test if TARGET can legally be reached by any of CANDIDATES.
Return the list of candidates that can reach it.

CANDIDATES is a list of position indices which indicate the piece to
be moved, and TARGET is the index of the location to be moved to.

Note: All of the pieces specified by CANDIDATES must be of the same
type.  Also, it is the callers responsibility to ensure that the piece
can legally reach the square in question.  This function merely
assures that the resulting position is valid."
  (assert position)
  (assert (memq color '(nil t)))
  (assert (and (>= target 0) (< target 64)))
  (assert (listp candidates))
  (assert (> (length candidates) 0))
  (let ((cand candidates)
	(piece (chess-pos-piece position (car candidates)))
	other-piece last-cand king-pos)
    (while cand
      (unwind-protect
	  (progn
	    ;; determine the resulting position
	    (chess-pos-set-piece position (car cand) ? )
	    (setq other-piece (chess-pos-piece position target))
	    (chess-pos-set-piece position target piece)
	    ;; find the king (only once if the king isn't moving)
	    (if (or (null king-pos)
		    (memq piece '(?K ?k)))
		(setq king-pos (chess-pos-king-index position color)))
	    ;; can anybody from the opposite side reach him?  if so,
	    ;; drop the candidate
	    (if (catch 'in-check
		  (chess-search-position position king-pos (not color) t))
		(if last-cand
		    (setcdr last-cand (cdr cand))
		  (setq candidates (cdr candidates)))
	      (setq last-cand cand)))
	;; return the position to its original state
	(chess-pos-set-piece position target other-piece)
	(chess-pos-set-piece position (car cand) piece))
      ;; try the next candidate
      (setq cand (cdr cand)))
    candidates))

(provide 'chess-pos)

;;; chess-pos.el ends here
