;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Convert a chess position to/from FEN notation
;;
;; FEN notation encodes a chess position using a simple string.  The
;; format is:
;;
;;   POSITION SIDE FLAGS
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
;; The FLAGS can contain K, Q, k or q, to signify whether the white or
;; black king can still castle on the king or queen side.  You can
;; also have coordinates, such as e4, a5, to specify which pawns may
;; be captured by en passant.
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

(defun chess-fen-to-pos (fen)
  "Convert a FEN-like notation string to a chess position."
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
	(chess-pos-set-en-passant position (chess-coord-to-index
					    (substring fen i (+ i 2))))
	(setq i (1+ i)))
       (t
	(setq error t)))
      (setq i (1+ i) c (and (< i l) (aref fen i))))
    (unless error
      position)))

(defun chess-pos-to-fen (position &optional full)
  "Convert a chess POSITION to FEN-like notation.
If FULL is non-nil, represent trailing spaces as well."
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
      (setq blank 0 str (concat str "/")))
    (setq str (if (chess-pos-side-to-move position)
		  (concat str " w ")
		(concat str " b ")))
    (if (chess-pos-can-castle position ?K)
	(setq str (concat str "K") output t))
    (if (chess-pos-can-castle position ?Q)
	(setq str (concat str "Q") output t))
    (if (chess-pos-can-castle position ?k)
	(setq str (concat str "k") output t))
    (if (chess-pos-can-castle position ?q)
	(setq str (concat str "q") output t))
    (if output
	(setq str (concat str " "))
      (setq str (concat str "- ")))
    (let ((index (chess-pos-en-passant position)))
      (if index
	  (concat str (chess-index-to-coord index))
	(concat str "-")))))

(provide 'chess-fen)

;;; chess-fen.el ends here
