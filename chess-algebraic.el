;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Convert a ply to/from standard chess algebraic notation
;;
;; A thing to deal with in chess is algebraic move notation, such as
;; Nxf3+.  (I leave description of this notation to better manuals
;; than this).  This notation is a shorthand way of representing where
;; a piece is moving from and to, by specifying the piece is involved,
;; where it's going, and whether or not a capture or check is
;; involved.
;;
;; You can convert from algebraic notation to a ply (one pair in most
;; cases, but two for a castle) using the following function (NOTE:
;; POSITION determines which side is on move (by calling
;; `chess-pos-side-to-move')):
;;
;;    (chess-algebraic-to-ply POSITION STRING)
;;
;; The function also checks if a move is legal, and will raise an
;; error if not.
;;
;; To convert from a ply to algebraic notation, use:
;;
;;    (chess-ply-to-algebraic PLY)
;;
;; Castling is determined by the movement of both a king and a rook.
;;
;; Lastly, there is a regexp for quickly checking if a string is in
;; algebraic notation or not, or searching out algebraic strings in a
;; buffer:
;;
;;    chess-algebraic-regexp
;;

(require 'chess-message)

(defconst chess-algebraic-pieces-regexp "[RNBKQ]")

(defconst chess-algebraic-regexp
  (format (concat "\\("
		    "O-O\\(-O\\)?\\|"
		    "\\(%s?\\)/?"
		    "\\([a-h]?[1-8]?\\)"
		    "\\([x-]?\\)"
		    "\\([a-h][1-8]\\)"
		    "\\(=\\(%s\\)\\)?"
		  "\\)"
		  "\\([#+]\\)?")
	  chess-algebraic-pieces-regexp
	  chess-algebraic-pieces-regexp)
  "A regular expression that matches all possible algebraic moves.
This regexp handles both long and short form.")

(defconst chess-algebraic-regexp-entire
  (concat chess-algebraic-regexp "$"))

(chess-message-catalog 'english
  '((clarify-piece     . "Clarify piece to move by rank or file")
    (could-not-clarify . "Could not determine which piece to use")
    (could-not-diff    . "Could not differentiate piece")
    (no-candidates     . "There are no candidate moves for '%s'")))

(defun chess-algebraic-to-ply (position move &optional trust)
  "Convert the algebraic notation MOVE for POSITION to a ply."
  (assert (vectorp position))
  (assert (stringp move))
  (let ((case-fold-search nil))
    (when (string-match chess-algebraic-regexp-entire move)
      (let ((color (chess-pos-side-to-move position))
	    (mate (match-string 9 move))
	    (piece (aref move 0))
	    changes long-style)
	(if (eq piece ?O)
	    (setq changes (chess-ply-castling-changes
			   position (= (length (match-string 1 move)) 5)))
	  (let ((promotion (match-string 8 move)))
	    (setq
	     changes
	     (let ((source (match-string 4 move))
		   (target (chess-coord-to-index (match-string 6 move))))
	       (if (and source (= (length source) 2))
		   (prog1
		       (list (chess-coord-to-index source) target)
		     (setq long-style t))
		 (if (= (length source) 0)
		     (setq source nil)
		   (setq source (aref source 0)))
		 (let (candidates which)
		   (unless (< piece ?a)
		     (setq source piece piece ?P))
		   ;; we must use our knowledge of how pieces can
		   ;; move, to determine which piece is meant by the
		   ;; piece indicator
		   (if (setq candidates
			     (chess-search-position position target
						    (if color piece
						      (downcase piece))
						    nil t))
		       (if (= (length candidates) 1)
			   (list (car candidates) target)
			 (if (null source)
			     (chess-error 'clarify-piece)
			   (nconc changes (list :which source))
			   (while candidates
			     (if (if (>= source ?a)
				     (eq (chess-index-file (car candidates))
					 (- source ?a))
				   (eq (chess-index-rank (car candidates))
				       (- 7 (- source ?1))))
				 (setq which (car candidates)
				       candidates nil)
			       (setq candidates (cdr candidates))))
			   (if (null which)
			       (chess-error 'could-not-clarify)
			     (list which target))))
		     (chess-error 'no-candidates move))))))
	    (if promotion
		(nconc changes (list :promote (aref promotion 0))))))

	(when changes
	  (when trust
	    (if mate
		(nconc changes (list (if (equal mate "#") :checkmate :check)))))
	  (unless long-style
	    (nconc changes (list :san move)))

	  (apply 'chess-ply-create position trust changes))))))

(defun chess-ply--move-text (ply long)
  (or
   (and (chess-ply-keyword ply :castle) "O-O")
   (and (chess-ply-keyword ply :long-castle) "O-O-O")
   (let* ((pos (chess-ply-pos ply))
	  (from (chess-ply-source ply))
	  (to (chess-ply-target ply))
	  (from-piece (chess-pos-piece pos from))
	  (color (chess-pos-side-to-move pos))
	  (rank 0) (file 0)
	  (from-rank (chess-index-rank from))
	  (from-file (chess-index-file from))
	  (differentiator (chess-ply-keyword ply :which)))
	(unless differentiator
	  (let ((candidates (chess-search-position pos to from-piece nil t)))
	    (when (> (length candidates) 1)
	      (dolist (candidate candidates)
		(if (= (/ candidate 8) from-rank)
		    (setq rank (1+ rank)))
		(if (= (mod candidate 8) from-file)
		    (setq file (1+ file))))
	      (cond
	       ((= file 1)
		(setq differentiator (+ from-file ?a)))
	       ((= rank 1)
		(setq differentiator (+ (- 7 from-rank) ?1)))
	       (t (chess-error 'could-not-diff)))
	      (chess-ply-set-keyword ply :which differentiator))))
	(concat
	 (unless (= (upcase from-piece) ?P)
	   (char-to-string (upcase from-piece)))
	 (if long
	     (chess-index-to-coord from)
	   (if differentiator
	       (prog1
		   (char-to-string differentiator)
		 (chess-ply-changes ply))
	     (if (and (not long) (= (upcase from-piece) ?P)
		      (/= (chess-index-file from)
			  (chess-index-file to)))
		 (char-to-string (+ (chess-index-file from) ?a)))))
	 (if (or (/= ?  (chess-pos-piece pos to))
		 (chess-ply-keyword ply :en-passant))
	     "x" (if long "-"))
	 (chess-index-to-coord to)
	 (let ((promote (chess-ply-keyword ply :promote)))
	   (if promote
	       (concat "=" (char-to-string promote))))
	 (if (chess-ply-keyword ply :check) "+"
	   (if (chess-ply-keyword ply :checkmate) "#"))))))

(defun chess-ply-to-algebraic (ply &optional long)
  "Convert the given PLY to algebraic notation.
If LONG is non-nil, render the move into long notation."
  (assert (listp ply))
  (or (and (not long) (chess-ply-keyword ply :san))
      (and (null (chess-ply-source ply)) "")
      (let ((move (chess-ply--move-text ply long)))
	(unless long (chess-ply-set-keyword ply :san move))
	move)))

(provide 'chess-algebraic)

;;; chess-algebraic.el ends here
