;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Standard Chess rules module
;;
;; This module implements the following events:
;;
;; `move'
;;
;; Make a move on the current board, if it is legal.
;;
;; `search'
;;
;; Pieces can be located by searching all legal paths a piece might
;; use to reach a particular square.  Thus, to find all white pawn(s)
;; that could make it to e4 (either by taking a piece, or by moving
;; there) you'd call:
;;
;;  (chess-standard-search-position POSITION (chess-coord-to-index "e4") ?P)
;;
;; This returns a list of indices specifying all white pawns that
;; could reach e4 in one move.  NOTE: The general search order is from
;; upper-left clockwise.

;; $Revision$

(defgroup chess-standard nil
  "The rules of standard chess."
  :group 'chess)

;;; Code:

(defun chess-standard-validate-ply (ply &optoinal search-func)
  "Validate the given PLY against standard chess rules."
  (let* ((pos (chess-ply-pos ply))
	 (color (chess-pos-side-to-move pos))
	 (source (car (chess-ply-changes ply)))
	 (piece (chess-pos-piece pos source))
	 (target (cadr (chess-ply-changes ply))))

    (if (eq piece ? )
	(signal 'chess-illegal
		"Cannot move from a square that is empty"))

    (if (if color
	    (> piece ?a)
	  (< piece ?a))
	(signal 'chess-illegal
		"Cannot move your opponents pieces"))

    (let ((enemy-piece (chess-pos-piece pos target)))
      (if (and (not (eq enemy-piece ? ))
	       (if color
		   (< enemy-piece ?a)
		 (> enemy-piece ?a)))
	  (signal 'chess-illegal
		  "Cannot move on top of your own pieces")))

    (unless (funcall (or search-func
			 'chess-standard-search-position) pos target piece)
      (signal 'chess-illegal "Illegal move"))))

(defun chess-standard-search-position (position target piece)
  "Look on POSITION from TARGET for a PIECE that can move there.
This routine looks along legal paths of movement for PIECE.

If PIECE is t or nil, legal piece movements for any piece of that
color will be considered (t for white, nil for black).  Otherwise, the
case of the PIECE determines color.

The return value is a list of candidates, which means a list of
indices which indicate where a piece may have moved from."
  ;; jww (2002-04-07): This function is a bit slow
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
	       (chess-standard-search-position position target
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
		      (chess-pos-can-castle position (if color ?K ?k))
		      (setq pos (chess-rf-to-index rank 5))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-standard-search-position position
							   pos (not color)))
		      (setq pos (chess-rf-to-index rank 6))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-standard-search-position position
							   pos (not color))))
		 (and (equal target (cons rank 2))
		      (chess-pos-can-castle position (if color ?Q ?q))
		      (setq pos (chess-rf-to-index rank 1))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-standard-search-position position
							   pos (not color)))
		      (setq pos (chess-rf-to-index rank 2))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-standard-search-position position
							   pos (not color)))
		      (setq pos (chess-rf-to-index rank 3))
		      (chess-pos-piece-p position pos ? )
		      (not (chess-standard-search-position position
							   pos (not color))))))
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
	    (if (chess-standard-search-position pos (car king-pos)
						(not color))
		(if last-cand
		    (setcdr last-cand (cdr cand))
		  (setq candidates (cdr candidates)))
	      (setq last-cand cand))
	    (setq cand (cdr cand)))))

    ;; return the final list of candidate moves
    candidates))

(provide 'chess-standard)

;;; chess-standard.el ends here
