;;; chess-algebraic.el --- Convert a ply to/from standard chess algebraic notation

;; Copyright (C) 2002, 2004, 2008, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

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

;;; Code:

(require 'chess-message)
(require 'chess-ply)
(require 'chess-pos)
(require 'cl-lib)

(defconst chess-algebraic-figurine-pieces
  '((?K . #x2654) (?Q . #x2655) (?R . #x2656)
    (?B . #x2657) (?N . #x2658) (?P . #x2659)
    (?k . #x265A) (?q . #x265B) (?r . #x265C)
    (?b . #x265D) (?n . #x265E) (?p . #x265F))
  "Map internal piece representation to Unicode chess figures (as used in figurine
notation.")

(defconst chess-algebraic-regexp
  (rx (group (or (or "O-O" "O-O-O" "0-0" "0-0-0")
		 (and (optional (group (char ?N ?B ?R ?Q ?K
					     ?♔ ?♕ ?♖ ?♗ ?♘
					     ?♚ ?♛ ?♜ ?♝ ?♞)))
		      (optional (char ?/))
		      (group (optional (char "a-h")) (optional (char "1-8")))
		      (optional (group (char ?- ?x)))
		      (group (char "a-h") (char "1-8"))
		      (optional (group ?= (group (char ?N ?B ?R ?Q ?K
						       ?♔ ?♕ ?♖ ?♗ ?♘
						       ?♚ ?♛ ?♜ ?♝ ?♞)))))))
      (optional (group (char ?+ ?#))))
  "A regular expression that matches all possible algebraic moves.
This regexp matches short, long and figurine notation.")

(defconst chess-algebraic-regexp-entire (concat chess-algebraic-regexp "$"))

(defconst chess-algebraic-regexp-ws (concat chess-algebraic-regexp "\\s-"))

(chess-message-catalog 'english
  '((clarify-piece     . "Clarify piece to move by rank or file")
    (could-not-clarify . "Could not determine which piece to use")
    (could-not-diff    . "Could not differentiate piece")
    (no-candidates     . "There are no candidate moves for '%s'")
    (at-move-string    . "At algebraic move '%s': %s")))

(defun chess-algebraic-to-ply (position move &optional trust)
  "Convert (short, long or figurine) algebraic notation MOVE for POSITION to a ply."
  (cl-check-type position chess-pos)
  (cl-check-type move string)
  (let ((case-fold-search nil))
    (when (string-match chess-algebraic-regexp-entire move)
      (let ((color (chess-pos-side-to-move position))
	    (mate (match-string 8 move))
	    (piece (aref move 0))
	    changes type)
	(if (or (= piece ?O) (= piece ?0))
	    (setq changes (chess-ply-castling-changes
			   position (= (length (match-string 1 move)) 5)))
	  (let ((promotion (match-string 7 move)))
	    (setq
	     changes
	     (let ((source (match-string 3 move))
		   (target (chess-coord-to-index (match-string 5 move))))
	       (if (and source (= (length source) 2))
		   (prog1
		       (list (chess-coord-to-index source) target)
		     (setq type :lan))
		 (if (= (length source) 0)
		     (setq source nil)
		   (setq source (aref source 0)))
		 (let (candidates which)
		   (when (and (not type) (< piece ?a))
		     (setq type :san))
		   (when (rassq piece chess-algebraic-figurine-pieces)
		     (unless type (setq type :fan))
		     (setq piece (upcase
				  (car (rassq piece chess-algebraic-figurine-pieces)))))
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
			   (while candidates
			     (if (if (>= source ?a)
				     (= (chess-index-file (car candidates))
					(chess-file-from-char source))
				   (= (chess-index-rank (car candidates))
				      (chess-rank-from-char source)))
				 (setq which (car candidates)
				       candidates nil)
			       (setq candidates (cdr candidates))))
			   (if (null which)
			       (chess-error 'could-not-clarify)
			     (list which target))))
		     (chess-error 'no-candidates move))))))

	    (when promotion
	      (nconc changes
		     (list :promote
			   (upcase (or (car (rassq (aref promotion 0)
						   chess-algebraic-figurine-pieces))
				       (aref promotion 0))))))))

	(when changes
	  (if (and trust mate)
	      (nconc changes (list (if (string-equal mate "#")
				       :checkmate
				     :check))))
	  ;; If we know the notation type by now, remember the string so that
	  ;; we do not need to re-generate it later on.
	  (when type
	    (cl-check-type type keyword)
	    (nconc changes (list type move)))

	  (condition-case err
	      (apply 'chess-ply-create position trust changes)
	    (error
	     (chess-error 'at-move-string
			  move (error-message-string err)))))))))

(defun chess-ply-to-algebraic (ply &optional type)
  "Convert the given PLY to algebraic notation.
Optional argument TYPE specifies the kind of algebraic notation to generate.
`:san' (the default) generates short (or standard) algebraic notation
\(like \"Nc3\").  `:lan' generates long algebraic notation (like \"Nb1-c3\".
`:fan' generates figurine algebraic notation (like \"♘c3\".
Finally, `:numeric' generates ICCF numeric notation (like \"2133\"."
  (cl-check-type ply (and list (not null)))
  (cl-check-type type (member nil :san :fan :lan :numeric))
  (unless type (setq type :san))
  (or (chess-ply-keyword ply type)
      (and (null (chess-ply-source ply)) "")
      (chess-ply-set-keyword
       ply type
       (or
	(and (eq type :numeric)
	     (apply
	      #'string
	      (+ (chess-index-file (chess-ply-source ply)) ?1)
	      (+ (chess-index-rank (logxor (chess-ply-source ply) #o70)) ?1)
	      (+ (chess-index-file (chess-ply-target ply)) ?1)
	      (+ (chess-index-rank (logxor (chess-ply-target ply) #o70)) ?1)
	      (when (chess-ply-keyword ply :promote)
		(list (+ (cl-position (chess-ply-keyword ply :promote)
				      '(?Q ?R ?B ?N)) ?1)))))
	(and (chess-ply-keyword ply :castle) "O-O")
	(and (chess-ply-keyword ply :long-castle) "O-O-O")
	(let* ((pos (chess-ply-pos ply))
	       (from (chess-ply-source ply))
	       (to (chess-ply-target ply))
	       (from-piece (chess-pos-piece pos from))
	       (rank 0) (file 0)
	       (from-rank (chess-index-rank from))
	       (from-file (chess-index-file from))
	       (differentiator (chess-ply-keyword ply :which)))
	  (unless differentiator
	    (let ((candidates (chess-search-position pos to from-piece nil t)))
	      (when (> (length candidates) 1)
		(dolist (candidate candidates)
		  (when (= (chess-index-rank candidate) from-rank)
		    (setq rank (1+ rank)))
		  (when (= (chess-index-file candidate) from-file)
		    (setq file (1+ file))))
		(cond ((= file 1) (setq differentiator (chess-file-to-char from-file)))
		      ((= rank 1) (setq differentiator (chess-rank-to-char from-rank)))
		      (t (chess-error 'could-not-diff)))
		(chess-ply-set-keyword ply :which differentiator))))
	  (concat
	   (unless (= (upcase from-piece) ?P)
	     (char-to-string
	      (cond ((memq type '(:san :lan)) (upcase from-piece))
		    ((eq type :fan)
		     (cdr (assq from-piece chess-algebraic-figurine-pieces))))))
	   (cond
	    ((eq type :lan) (chess-index-to-coord from))
	    (differentiator (char-to-string differentiator))
	    ((and (not (eq type :lan)) (= (upcase from-piece) ?P)
		  (/= from-file (chess-index-file to)))
	     (char-to-string (chess-file-to-char from-file))))
	   (if (or (/= ?  (chess-pos-piece pos to))
		   (chess-ply-keyword ply :en-passant))
	       "x" (if (eq type :lan) "-"))
	   (chess-index-to-coord to)
	   (let ((promote (chess-ply-keyword ply :promote)))
	     (if promote
		 (concat "=" (char-to-string
			      (cond ((eq type :fan)
				     (cdr (assq (if (chess-pos-side-to-move pos)
						    promote
						  (downcase promote))
						chess-algebraic-figurine-pieces)))
				    (t promote))))))
	   (if (chess-ply-keyword ply :check) "+"
	     (if (chess-ply-keyword ply :checkmate) "#"))))))))

(provide 'chess-algebraic)

;;; chess-algebraic.el ends here
