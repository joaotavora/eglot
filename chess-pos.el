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
;; with it.  First of all, a coordinate system of octal indices is
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
   ?\077 ?\070 ?\007 ?\000
   ;; is the side to move in: `check', `checkmate', `stalemate'
   nil
   ;; which color is it to move next?
   t
   ;; list of annotations for this position.  Textual annotations are
   ;; simply that, while lists represent interesting variations.
   nil
   ;; where are the kings?
   60 4
   ;; a pointer to the ply which led to this position
   nil]
  "Starting position of a regular chess game.")

(chess-message-catalog 'english
  '((chess-nag-1   . "good move [traditional \"!\"]")
    (chess-nag-2   . "poor move [traditional \"?\"]")
    (chess-nag-3   . "very good move (traditional \"!!\"")
    (chess-nag-4   . "very poor move (traditional \"??\")")
    (chess-nag-5   . "speculative move (traditional \"!?\")")
    (chess-nag-6   . "questionable move (traditional \"?!\")")
    (chess-nag-7   . "forced move (all others lose quickly)")
    (chess-nag-8   . "singular move (no reasonable alternatives)")
    (chess-nag-9   . "worst move")
    (chess-nag-10  . "drawish position")
    (chess-nag-11  . "equal chances, quiet position")
    (chess-nag-12  . "equal chances, active position")
    (chess-nag-13  . "unclear position")
    (chess-nag-14  . "White has a slight advantage")
    (chess-nag-15  . "Black has a slight advantage")
    (chess-nag-16  . "White has a moderate advantage")
    (chess-nag-17  . "Black has a moderate advantage")
    (chess-nag-18  . "White has a decisive advantage")
    (chess-nag-19  . "Black has a decisive advantage")
    (chess-nag-20  . "White has a crushing advantage (Black should resign)")
    (chess-nag-21  . "Black has a crushing advantage (White should resign)")
    (chess-nag-22  . "White is in zugzwang")
    (chess-nag-23  . "Black is in zugzwang")
    (chess-nag-24  . "White has a slight space advantage")
    (chess-nag-25  . "Black has a slight space advantage")
    (chess-nag-26  . "White has a moderate space advantage")
    (chess-nag-27  . "Black has a moderate space advantage")
    (chess-nag-28  . "White has a decisive space advantage")
    (chess-nag-29  . "Black has a decisive space advantage")
    (chess-nag-30  . "White has a slight time (development) advantage")
    (chess-nag-31  . "Black has a slight time (development) advantage")
    (chess-nag-32  . "White has a moderate time (development) advantage")
    (chess-nag-33  . "Black has a moderate time (development) advantage")
    (chess-nag-34  . "White has a decisive time (development) advantage")
    (chess-nag-35  . "Black has a decisive time (development) advantage")
    (chess-nag-36  . "White has the initiative")
    (chess-nag-37  . "Black has the initiative")
    (chess-nag-38  . "White has a lasting initiative")
    (chess-nag-39  . "Black has a lasting initiative")
    (chess-nag-40  . "White has the attack")
    (chess-nag-41  . "Black has the attack")
    (chess-nag-42  . "White has insufficient compensation for material deficit")
    (chess-nag-43  . "Black has insufficient compensation for material deficit")
    (chess-nag-44  . "White has sufficient compensation for material deficit")
    (chess-nag-45  . "Black has sufficient compensation for material deficit")
    (chess-nag-46  . "White has more than adequate compensation for material deficit")
    (chess-nag-47  . "Black has more than adequate compensation for material deficit")
    (chess-nag-48  . "White has a slight center control advantage")
    (chess-nag-49  . "Black has a slight center control advantage")
    (chess-nag-50  . "White has a moderate center control advantage")
    (chess-nag-51  . "Black has a moderate center control advantage")
    (chess-nag-52  . "White has a decisive center control advantage")
    (chess-nag-53  . "Black has a decisive center control advantage")
    (chess-nag-54  . "White has a slight kingside control advantage")
    (chess-nag-55  . "Black has a slight kingside control advantage")
    (chess-nag-56  . "White has a moderate kingside control advantage")
    (chess-nag-57  . "Black has a moderate kingside control advantage")
    (chess-nag-58  . "White has a decisive kingside control advantage")
    (chess-nag-59  . "Black has a decisive kingside control advantage")
    (chess-nag-60  . "White has a slight queenside control advantage")
    (chess-nag-61  . "Black has a slight queenside control advantage")
    (chess-nag-62  . "White has a moderate queenside control advantage")
    (chess-nag-63  . "Black has a moderate queenside control advantage")
    (chess-nag-64  . "White has a decisive queenside control advantage")
    (chess-nag-65  . "Black has a decisive queenside control advantage")
    (chess-nag-66  . "White has a vulnerable first rank")
    (chess-nag-67  . "Black has a vulnerable first rank")
    (chess-nag-68  . "White has a well protected first rank")
    (chess-nag-69  . "Black has a well protected first rank")
    (chess-nag-70  . "White has a poorly protected king")
    (chess-nag-71  . "Black has a poorly protected king")
    (chess-nag-72  . "White has a well protected king")
    (chess-nag-73  . "Black has a well protected king")
    (chess-nag-74  . "White has a poorly placed king")
    (chess-nag-75  . "Black has a poorly placed king")
    (chess-nag-76  . "White has a well placed king")
    (chess-nag-77  . "Black has a well placed king")
    (chess-nag-78  . "White has a very weak pawn structure")
    (chess-nag-79  . "Black has a very weak pawn structure")
    (chess-nag-80  . "White has a moderately weak pawn structure")
    (chess-nag-81  . "Black has a moderately weak pawn structure")
    (chess-nag-82  . "White has a moderately strong pawn structure")
    (chess-nag-83  . "Black has a moderately strong pawn structure")
    (chess-nag-84  . "White has a very strong pawn structure")
    (chess-nag-85  . "Black has a very strong pawn structure")
    (chess-nag-86  . "White has poor knight placement")
    (chess-nag-87  . "Black has poor knight placement")
    (chess-nag-88  . "White has good knight placement")
    (chess-nag-89  . "Black has good knight placement")
    (chess-nag-90  . "White has poor bishop placement")
    (chess-nag-91  . "Black has poor bishop placement")
    (chess-nag-92  . "White has good bishop placement")
    (chess-nag-93  . "Black has good bishop placement")
    (chess-nag-84  . "White has poor rook placement")
    (chess-nag-85  . "Black has poor rook placement")
    (chess-nag-86  . "White has good rook placement")
    (chess-nag-87  . "Black has good rook placement")
    (chess-nag-98  . "White has poor queen placement")
    (chess-nag-99  . "Black has poor queen placement")
    (chess-nag-100 . "White has good queen placement")
    (chess-nag-101 . "Black has good queen placement")
    (chess-nag-102 . "White has poor piece coordination")
    (chess-nag-103 . "Black has poor piece coordination")
    (chess-nag-104 . "White has good piece coordination")
    (chess-nag-105 . "Black has good piece coordination")
    (chess-nag-106 . "White has played the opening very poorly")
    (chess-nag-107 . "Black has played the opening very poorly")
    (chess-nag-108 . "White has played the opening poorly")
    (chess-nag-109 . "Black has played the opening poorly")
    (chess-nag-110 . "White has played the opening well")
    (chess-nag-111 . "Black has played the opening well")
    (chess-nag-112 . "White has played the opening very well")
    (chess-nag-113 . "Black has played the opening very well")
    (chess-nag-114 . "White has played the middlegame very poorly")
    (chess-nag-115 . "Black has played the middlegame very poorly")
    (chess-nag-116 . "White has played the middlegame poorly")
    (chess-nag-117 . "Black has played the middlegame poorly")
    (chess-nag-118 . "White has played the middlegame well")
    (chess-nag-119 . "Black has played the middlegame well")
    (chess-nag-120 . "White has played the middlegame very well")
    (chess-nag-121 . "Black has played the middlegame very well")
    (chess-nag-122 . "White has played the ending very poorly")
    (chess-nag-123 . "Black has played the ending very poorly")
    (chess-nag-124 . "White has played the ending poorly")
    (chess-nag-125 . "Black has played the ending poorly")
    (chess-nag-126 . "White has played the ending well")
    (chess-nag-127 . "Black has played the ending well")
    (chess-nag-128 . "White has played the ending very well")
    (chess-nag-129 . "Black has played the ending very well")
    (chess-nag-130 . "White has slight counterplay")
    (chess-nag-131 . "Black has slight counterplay")
    (chess-nag-132 . "White has moderate counterplay")
    (chess-nag-133 . "Black has moderate counterplay")
    (chess-nag-134 . "White has decisive counterplay")
    (chess-nag-135 . "Black has decisive counterplay")
    (chess-nag-136 . "White has moderate time control pressure")
    (chess-nag-137 . "Black has moderate time control pressure")
    (chess-nag-138 . "White has severe time control pressure")
    (chess-nag-139 . "Black has severe time control pressure")))

(defsubst chess-pos-piece (position index)
  "Return the piece on POSITION at INDEX."
  (assert (vectorp position))
  (assert (and (>= index 0) (< index 64)))
  (aref position index))

(defsubst chess-pos-king-index (position color)
  "Return the index on POSITION of the king.
If COLOR is non-nil, return the position of the white king, otherwise
return the position of the black king."
  (assert (vectorp position))
  (assert (memq color '(nil t)))
  (or (aref position (if color 72 73))
      (chess-pos-set-king-index position color
				(chess-pos-search position (if color ?K ?k)))))

(defsubst chess-pos-set-king-index (position color index)
  "Set the known index of the king on POSITION for COLOR, to INDEX.
It is never necessary to call this function."
  (assert (vectorp position))
  (assert (memq color '(nil t)))
  (assert (and (>= index 0) (< index 64)))
  (aset position (if color 72 73) index))

(defsubst chess-pos-set-piece (position index piece)
  "Set the piece on POSITION at INDEX to PIECE.
PIECE must be one of K Q N B R or P.  Use lowercase to set black
pieces."
  (assert (vectorp position))
  (assert (and (>= index 0) (< index 64)))
  (assert (memq piece '(?  ?K ?Q ?N ?B ?R ?P ?k ?q ?n ?b ?r ?p)))
  (aset position index piece)
  (if (= piece ?K)
      (chess-pos-set-king-index position t index)
    (if (= piece ?k)
	(chess-pos-set-king-index position nil index))))

(defun chess-pos-can-castle (position side)
  "Return whether the king on POSITION can castle on SIDE.
SIDE must be either ?K for the kingside, or ?Q for the queenside (use
lowercase to query if black can castle)."
  (assert (vectorp position))
  (assert (memq side '(?K ?Q ?k ?q)))
  (let* ((index (+ 65 (if (< side ?a)
			  (if (= side ?K) 0 1)
			(if (= side ?k) 2 3))))
	 (value (aref position index)))
    (if (or (eq value nil) (integerp value))
	value
      (let* ((color (< side ?a))
	     (long (= ?Q (upcase side)))
	     (file (if long 0 7))
	     (king-file (chess-index-file
			 (chess-pos-king-index position color)))
	     rook)
	(while (funcall (if long '< '>) file king-file)
	  (let ((index (chess-rf-to-index (if color 7 0) file)))
	    (if (chess-pos-piece-p position index (if color ?R ?r))
		(setq rook index file king-file)
	      (setq file (funcall (if long '1+ '1-) file)))))
	(aset position index rook)))))

(defsubst chess-pos-set-can-castle (position side value)
  "Set whether the king can castle on the given POSITION on SIDE.

See `chess-pos-can-castle'.

It is only necessary to call this function if setting up a position
manually.  Note that all newly created positions have full castling
priveleges set, unless the position is created blank, in which case
castling priveleges are unset.  See `chess-pos-copy'."
  (assert (vectorp position))
  (assert (memq side '(?K ?Q ?k ?q)))
  (assert (memq value '(nil t)))
  (aset position (+ 65 (if (< side ?a)
			   (if (= side ?K) 0 1)
			 (if (= side ?k) 2 3))) value))

(defsubst chess-pos-en-passant (position)
  "Return the index of any pawn on POSITION that can be captured en passant.
Returns nil if en passant is unavailable."
  (assert (vectorp position))
  (aref position 64))

(defsubst chess-pos-set-en-passant (position index)
  "Set the index of any pawn on POSITION that can be captured en passant."
  (assert (vectorp position))
  (assert (or (eq index nil)
	      (and (>= index 0) (< index 64))))
  (aset position 64 index))

(defsubst chess-pos-status (position)
  "Return whether the side to move in the POSITION is in a special state.
nil is returned if not, otherwise one of the symbols: `check',
`checkmate', `stalemate'."
  (assert (vectorp position))
  (aref position 69))

(defsubst chess-pos-set-status (position value)
  "Set whether the side to move in POSITION is in a special state.
VALUE should either be nil, to indicate that the POSITION is normal,
or one of the symbols: `check', `checkmate', `stalemate'."
  (assert (vectorp position))
  (assert (or (eq value nil) (symbolp value)))
  (aset position 69 value))

(defsubst chess-pos-side-to-move (position)
  "Return the color whose move it is in POSITION."
  (assert (vectorp position))
  (aref position 70))

(defsubst chess-pos-set-side-to-move (position color)
  "Set the color whose move it is in POSITION."
  (assert (vectorp position))
  (assert (memq color '(nil t)))
  (aset position 70 color))

(defsubst chess-pos-annotations (position)
  "Return the list of annotations for this position."
  (assert (vectorp position))
  (aref position 71))

(defsubst chess-pos-set-annotations (position annotations)
  "Return the list of annotations for this position."
  (assert (vectorp position))
  (assert (listp annotations))
  (aset position 71 annotations))

(defun chess-pos-add-annotation (position annotation)
  "Add an annotation for this position."
  (assert (vectorp position))
  (assert (or (stringp annotation) (listp annotation)))
  (let ((ann (chess-pos-annotations position)))
    (if ann
	(nconc ann (list annotation))
      (aset position 71 (list annotation)))))

(defsubst chess-pos-epd (position opcode)
  "Return the value of the given EPD OPCODE, or nil if not set."
  (assert (vectorp position))
  (assert opcode)
  (cdr (assq opcode (chess-pos-annotations position))))

(defun chess-pos-set-epd (position opcode &optional value)
  "Set the given EPD OPCODE to VALUE, or t if VALUE is not specified."
  (assert (vectorp position))
  (assert opcode)
  (let ((entry (assq opcode (chess-pos-annotations position))))
    (if entry
	(setcdr entry (or value t))
      (chess-pos-add-annotation position (cons opcode (or value t))))))

(defun chess-pos-del-epd (position opcode)
  "Delete the given EPD OPCODE."
  (assert (vectorp position))
  (assert opcode)
  (chess-pos-set-annotations
   position (assq-delete-all opcode (chess-pos-annotations position))))

(defun chess-pos-preceding-ply (position)
  "Delete the given EPD OPCODE."
  (assert (vectorp position))
  (aref position 74))

(defun chess-pos-set-preceding-ply (position ply)
  "Delete the given EPD OPCODE."
  (assert (vectorp position))
  (assert (listp ply))
  (aset position 74 ply))

(defsubst chess-pos-copy (position)
  "Copy the given chess POSITION.
If there are annotations or EPD opcodes set, these lists are copied as
well, so that the two positions do not share the same lists."
  (assert (vectorp position))
  (let ((copy (vconcat position)) i)
    (setq i (chess-pos-annotations position))
    (if i (chess-pos-set-annotations copy (copy-alist i)))
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
  (assert (vectorp position))
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
  (assert (vectorp position))
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
  (assert (vectorp position))
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
  (assert (vectorp position))
  (assert (memq color '(nil t)))
  (let ((pieces (chess-pos-search position color))
	(value 0))
    (dolist (index pieces)
      (setq value
	    (+ value (cdr (assq (downcase (chess-pos-piece position index))
				chess-pos-piece-values)))))
    value))

(defun chess-pos-passed-pawns (position color &optional pawn-indices)
  "If COLOR has Passed Pawns in POSITION, return a list of their indices.
Optionally, if INDICES is non-nil those indices are considered as candidates.

A Pawn whose advance to the eighth rank is not blocked by an
opposing Pawn in the same file and who does not have to pass one
on an adjoining file is called a passed Pawn."
  (let ((seventh (if color 1 6)) (bias (if color -1 1)) (pawn (if color ?p ?P))
	pawns)
    (dolist (index (or pawn-indices
		       (chess-pos-search position (if color ?P ?p))) pawns)
      (if (= (chess-index-rank index) seventh)
	  (push index pawns)
	(let ((file (chess-index-file index)))
	  (if (catch 'passed-pawn
		(let ((test (chess-incr-index index (if color -1 1) 0)))
		  (while (funcall (if color '>= '<=)
				  (chess-index-rank test) seventh)
		    (if (if (and (> file 0) (< file 7))
			    (or (chess-pos-piece-p position test pawn)
				(chess-pos-piece-p
				 position (chess-incr-index test 0 1) pawn)
				(chess-pos-piece-p
				 position (chess-incr-index test 0 -1) pawn))
			  (or (chess-pos-piece-p position test pawn)
			      (chess-pos-piece-p
			       position
			       (chess-incr-index test 0 (if (zerop file) 1 -1))
			       pawn)))
			(throw 'passed-pawn nil)
		      (setq test (chess-incr-index test (if color -1 1) 0))))
		  t))
	      (push index pawns)))))))
    
(chess-message-catalog 'english
  '((move-from-blank . "Attempted piece move from blank square %s")))

(defun chess-pos-move (position &rest changes)
  "Move a piece on the POSITION directly, using the indices in CHANGES.
This function does not check any rules, it only makes sure you are not
trying to move a blank square."
  (assert (vectorp position))
  (assert (listp changes))
  (assert (> (length changes) 0))

  ;; apply the piece movements listed in `changes'
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
	  (if (and (chess-pos-can-castle position (if color ?Q ?q))
		   (= (car changes) (chess-pos-can-castle position (if color ?Q ?q))))
	      (chess-pos-set-can-castle position (if color ?Q ?q) nil)
	    (if (and (chess-pos-can-castle position (if color ?K ?k))
		     (= (car changes) (chess-pos-can-castle position
							    (if color ?K ?k))))
		(chess-pos-set-can-castle position (if color ?K ?k) nil))))

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

(defun chess-search-position (position target piece &optional
				       check-only no-castling)
  "Look on POSITION from TARGET for a PIECE that can move there.
This routine looks along legal paths of movement for PIECE.  It
differs from `chess-pos-search', which is a more basic function that
doesn't take piece movement into account.

If PIECE is t or nil, legal piece movements for any piece of that
color will be considered (t for white, nil for black).  Otherwise, the
case of the PIECE determines color.

The return value is a list of candidates, which means a list of
indices which indicate where a piece may have moved from.

If CHECK-ONLY is non-nil and PIECE is either t or nil, only consider
pieces which can give check (not the opponents king).
If NO-CASTLING is non-nil, do not consider castling moves."
  (assert (vectorp position))
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
	(while pos
	  (if (chess-pos-piece-p position pos piece)
	      (progn
		(chess--add-candidate pos)
		(setq pos nil))
	    (setq pos (and (chess-pos-piece-p position pos ? )
			   (apply 'chess-incr-index pos dir)))))

	;; test whether the rook can move to the target by castling
	(if (and (= test-piece ?R) (not no-castling))
	    (let (rook)
	      (if (and (= target (if color ?\075 ?\005))
		       (setq rook (chess-pos-can-castle position
							(if color ?K ?k)))
		       (chess-ply-castling-changes position))
		  (chess--add-candidate rook)
		(if (and (= target (if color ?\073 ?\003))
			 (setq rook (chess-pos-can-castle position
							  (if color ?Q ?q)))
			 (chess-ply-castling-changes position t))
		    (chess--add-candidate rook)))))))

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
	    (setq dirs (cdr dirs))))

	;; test whether the king can move to the target by castling
	(if (and (not no-castling)
		 (or (and (equal target (chess-rf-to-index (if color 7 0) 6))
			  (chess-pos-can-castle position (if color ?K ?k))
			  (chess-ply-castling-changes position))
		     (and (equal target (chess-rf-to-index (if color 7 0) 2))
			  (chess-pos-can-castle position (if color ?Q ?q))
			  (chess-ply-castling-changes position t))))
	    (chess--add-candidate (chess-pos-king-index position color)))))

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
	      (chess-pos-legal-candidates position color target
					  candidates)))

    ;; return the final list of candidate moves
    candidates))

(defun chess-pos-legal-candidates (position color target candidates)
  "Test if TARGET can legally be reached by any of CANDIDATES.
Return the list of candidates that can reach it.

CANDIDATES is a list of position indices which indicate the piece to
be moved, and TARGET is the index of the location to be moved to.

Note: All of the pieces specified by CANDIDATES must be of the same
type.  Also, it is the callers responsibility to ensure that the piece
can legally reach the square in question.  This function merely
assures that the resulting position is valid (the move does not leave the king
in check)."
  (assert (vectorp position))
  (assert (memq color '(nil t)))
  (assert (and (>= target 0) (< target 64)))
  (assert (listp candidates))
  (assert (> (length candidates) 0))
  (let ((cand candidates)
	(piece (chess-pos-piece position (car candidates)))
	other-piece en-passant-square last-cand king-pos)
    (while cand
      (unwind-protect
	  (progn
	    ;; determine the resulting position
	    (chess-pos-set-piece position (car cand) ? )
	    (setq other-piece (chess-pos-piece position target))
	    (chess-pos-set-piece position target piece)
	    (when (and (= piece (if color ?P ?p))
		       (chess-pos-en-passant position)
		       (= (chess-pos-en-passant position)
			  (chess-incr-index target (if color 1 -1) 0)))
	      (chess-pos-set-piece position
				   (setq en-passant-square
					 (chess-incr-index target
							   (if color 1 -1)
							   0))
				   ? ))
	    ;; find the king (only once if the king isn't moving)
	    (if (or (null king-pos)
		    (memq piece '(?K ?k)))
		(setq king-pos (chess-pos-king-index position color)))
	    ;; can anybody from the opposite side reach him?  if so,
	    ;; drop the candidate
	    (if (and king-pos
		     (catch 'in-check
		       (chess-search-position position king-pos
					      (not color) t)))
		(if last-cand
		    (setcdr last-cand (cdr cand))
		  (setq candidates (cdr candidates)))
	      (setq last-cand cand)))
	;; return the position to its original state
	(chess-pos-set-piece position target other-piece)
	(chess-pos-set-piece position (car cand) piece)
	(when en-passant-square
	  (chess-pos-set-piece position en-passant-square (if color ?p ?P))))
      ;; try the next candidate
      (setq cand (cdr cand)))
    candidates))

(provide 'chess-pos)

;;; chess-pos.el ends here
