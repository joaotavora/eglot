;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Convert a chess position to/from FEN notation
;;
;; FEN notation encodes a chess position using a simple string.  The
;; format is:
;;
;;   POSITION SIDE CASTLING EN-PASSANT
;;
;; The POSITION gives all eight ranks, by specifying a letter for each
;; piece on the position, and a number for any intervening spaces.
;; Trailing spaces need not be counted.  Uppercase letters signify
;; white, and lowercase black.  For example, if your position only had
;; a black king on d8, your POSITION string would be:
;;
;;   3k////////
;;
;; For the three spaces (a, b and c file), the black king, and then
;; all the remaining ranks (which are all empty, so their spaces can
;; be ignored).
;;
;; The SIDE is w or b, to indicate whose move it is.
;;
;; CASTLING can contain K, Q, k or q, to signify whether the white or
;; black king can still castle on the king or queen side.  EN-PASSANT
;; signifies the target sqaure of an en passant capture, such as "e3" or "a6".
;;
;; The starting chess position always looks like this:
;;
;;   rnbqkbnr/pppppppp/////PPPPPPPP/RNBQKBNR/ w KQkq -
;;
;; And in "full" mode (where all spaces are accounted for):
;;
;;   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -
;;
;; It may also have the current game sequence appended, but this
;; relate to the game, not the position.
;;

(defconst chess-fen-regexp
  "^\\([bnrqkpBNRQKP1-8]*/?\\)+ [bw] \\(-\\|[KQkq]+\\) \\(-\\|[1-8]\\)")

(defun chess-fen-to-pos (fen)
  "Convert a FEN-like notation string to a chess position."
  (assert (stringp fen))
  (let ((i 0) (l (length fen))
	(rank 0) (file 0) (c ?0)
	(position (chess-pos-create t))
	error (space-count 0))
    (setq c (aref fen i))
    (while (and (null error)
		(/= c ? ) (< i l))
      (cond
       ((= c ?/)
	(setq file 0 rank (1+ rank)))
       ((and (>= c ?1) (<= c ?9))
	(setq file (+ file (- c ?0))))
       ((memq (upcase c) '(?K ?Q ?B ?N ?R ?P))
	(chess-pos-set-piece position (chess-rf-to-index rank file) c)
	(setq file (1+ file)))
       (t
	(setq error t)))
      (setq i (1+ i) c (aref fen i)))
    (if (= (aref fen i) ? )
	(setq i (1+ i)))
    (if (memq (aref fen i) '(?b ?w))
	(progn
	  (chess-pos-set-side-to-move position (= (aref fen i) ?w))
	  (setq i (+ i 2)))
      (setq error t))
    (setq c (aref fen i))
    (while (and (null error)
		(< space-count 2) (< i l))
      (cond
       ((= c ?-))
       ((= c ? ) (setq space-count (1+ space-count)))
       ((= c ?K) (chess-pos-set-can-castle position ?K t))
       ((= c ?Q) (chess-pos-set-can-castle position ?Q t))
       ((= c ?k) (chess-pos-set-can-castle position ?k t))
       ((= c ?q) (chess-pos-set-can-castle position ?q t))
       ((and (>= c ?a) (<= c ?h))
	(chess-pos-set-en-passant
	 position
	 (let ((target (chess-coord-to-index (substring fen i (+ i 2)))))
	   (chess-incr-index target (if (= (chess-index-rank target) 2)
					1 (if (= (chess-index-rank target) 5)
					      -1 (setq error t) 0)) 0)))
	(setq i (1+ i)))
       (t
	(setq error t)))
      (setq i (1+ i) c (and (< i l) (aref fen i))))
    (unless error
      position)))

(defun chess-pos-to-fen (position &optional full)
  "Convert a chess POSITION to FEN-like notation.
If FULL is non-nil, represent trailing spaces as well."
  (assert (vectorp position))
  (let ((blank 0) (str "") output)
    (dotimes (rank 8)
      (dotimes (file 8)
	(let ((p (chess-pos-piece position (chess-rf-to-index rank file))))
	  (if (= p ? )
	      (setq blank (1+ blank))
	    (if (> blank 0)
		(setq str (concat str (int-to-string blank)) blank 0))
	    (setq str (concat str (char-to-string p))))))
      (if (and full (> blank 0))
	  (setq str (concat str (int-to-string blank))))
      (if (< rank 7) (setq blank 0 str (concat str "/"))))
    (setq str (if (chess-pos-side-to-move position)
		  (concat str " w ")
		(concat str " b ")))
    (mapc (lambda (castle)
	    (if (chess-pos-can-castle position castle)
		(setq str (concat str (string castle)) output t)))
	  '(?K ?Q ?k ?q))
    (if output
	(setq str (concat str " "))
      (setq str (concat str "- ")))
    (let ((index (chess-pos-en-passant position)))
      (if (and index
	       (let ((pawn (if (chess-pos-side-to-move position) ?P ?p)))
		 (or (and (chess-incr-index index 0 -1)
			  (eq (chess-pos-piece position (chess-incr-index
							 index 0 -1)) pawn))
		     (and (chess-incr-index index 0 1)
			  (eq (chess-pos-piece position (chess-incr-index
							 index 0 1)) pawn)))))
	  (concat str (chess-index-to-coord
		       (if (chess-pos-side-to-move position)
			   (chess-incr-index index -1 0)
			 (chess-incr-index index 1 0))))
	(concat str "-")))))

(provide 'chess-fen)

;;; chess-fen.el ends here
