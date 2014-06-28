;;; chess-plain.el --- Plain ASCII chess display

;; Copyright (C) 2002-2005, 2014  Free Software Foundation, Inc.

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

;; This chess display module presents a very compact plain character-based
;; view of the chessboard.  Contrary to the classic chess-ics1, it does not draw
;; a border around squares.
;;
;; The characters used to display individual pieces can be customized,
;; see `chess-plain-piece-chars' for a number of suggestions.  Other aspects of
;; the chessboard are also customizable, see customization group `chess-plain'.

;;; Code:

(require 'chess-display)

(defgroup chess-plain nil
  "A minimal, customizable ASCII display."
  :group 'chess-display
  :link '(custom-manual "(chess)Plain ASCII diagram displays"))

(defcustom chess-plain-border-style [?+ ?- ?+ ?| ?| ?+ ?- ?+]
  "If non-nil, a vector describing the border characters."
  :group 'chess-plain
  :type '(choice (const :tag "No border" nil)
		 (vector :tag "Plain ASCII"
			 (const :value ?+ :tag "Upper left corner: +")
			 (const :value ?- :tag "Upper border: -")
			 (const :value ?+ :tag "Upper right corner: +")
			 (const :value ?| :tag "Left border: |")
			 (const :value ?| :tag "Right border: |")
			 (const :value ?+ :tag "Lower left corrner: +")
			 (const :value ?- :tag "Lower border: -")
			 (const :value ?+ :tag "Lower right corner: +"))
		 (vector :tag "Unicode box drawing characters"
			 (const :value ?┌ :tag "Upper left corner: ┌")
			 (const :value ?╶ :tag "Upper border: ╶")
			 (const :value ?┐ :tag "Upper right corner: ┐")
			 (const :value ?╷ :tag "Left border: ╷")
			 (const :value ?╷ :tag "Right border: ╷")
			 (const :value ?└ :tag "Lower left corrner: └")
			 (const :value ?╶ :tag "Lower border: ╶")
			 (const :value ?┘ :tag "Lower right corner: ┘"))
		 (vector :tag "Custom"
			 (character :tag "Upper left corner")
			 (character :tag "Upper border")
			 (character :tag "Upper right corner")
			 (character :tag "Left border")
			 (character :tag "Right border")
			 (character :tag "Lower left corner")
			 (character :tag "Lower border")
			 (character :tag "Lower right corner"))))

(defcustom chess-plain-black-square-char ?.
  "Character used to indicate empty black squares."
  :group 'chess-plain
  :type 'character)

(defcustom chess-plain-white-square-char ?.
  "Character used to indicate empty white squares."
  :group 'chess-plain
  :type 'character)

(defcustom chess-plain-piece-chars '((?K . ?K)
				     (?Q . ?Q)
				     (?R . ?R)
				     (?B . ?B)
				     (?N . ?N)
				     (?P . ?P)
				     (?k . ?k)
				     (?q . ?q)
				     (?r . ?r)
				     (?b . ?b)
				     (?n . ?n)
				     (?p . ?p))
  "Alist of pieces and their corresponding characters.
Characters defined here should make sense in respect to the current setting
of `chess-plain-upcase-indicates'."
  :group 'chess-plain
  :type '(choice (list :tag "White has uppercase english letters and black has lowercase english letters"
		       (const :tag "White King: K"   (?K . ?K))
		       (const :tag "White Queen: Q"  (?Q . ?Q))
		       (const :tag "White Rook: R"   (?R . ?R))
		       (const :tag "White Bishop: B" (?B . ?B))
		       (const :tag "White Knight: N" (?N . ?N))
		       (const :tag "White Pawn: P"   (?P . ?P))
		       (const :tag "Black King: k"   (?k . ?k))
		       (const :tag "Black Queen: q"  (?q . ?q))
		       (const :tag "Black Rook: r"   (?r . ?r))
		       (const :tag "Black Bishop: b" (?b . ?b))
		       (const :tag "Black Knight: n" (?n . ?n))
		       (const :tag "Black Pawn: p"   (?p . ?p)))
		 (list :tag "White has uppercase german letters and black has lowercase german letters"
		       (const :tag "White King: K"   (?K . ?K))
		       (const :tag "White Queen: D"  (?Q . ?D))
		       (const :tag "White Rook: T"   (?R . ?T))
		       (const :tag "White Bishop: L" (?B . ?L))
		       (const :tag "White Knight: S" (?N . ?S))
		       (const :tag "White Pawn: B"   (?P . ?B))
		       (const :tag "Black King: k"   (?k . ?k))
		       (const :tag "Black Queen: d"  (?q . ?d))
		       (const :tag "Black Rook: t"   (?r . ?t))
		       (const :tag "Black Bishop: l" (?b . ?l))
		       (const :tag "Black Knight: s" (?n . ?s))
		       (const :tag "Black Pawn: b"   (?p . ?b)))
		 (list :tag "White has english letters and black has german letters"
		       (const :tag "White King: K"   (?K . ?K))
		       (const :tag "White Queen: Q"  (?Q . ?Q))
		       (const :tag "White Rook: R"   (?R . ?R))
		       (const :tag "White Bishop: B" (?B . ?B))
		       (const :tag "White Knight: N" (?N . ?N))
		       (const :tag "White Pawn: P"   (?P . ?P))
		       (const :tag "Black King: J"   (?k . ?J))
		       (const :tag "Black Queen: D"  (?q . ?D))
		       (const :tag "Black Rook: T"   (?r . ?T))
		       (const :tag "Black Bishop: L" (?b . ?L))
		       (const :tag "Black Knight: S" (?n . ?S))
		       (const :tag "Black Pawn: X"   (?p . ?X)))
		 (list :tag "White has german letters and black has english letters"
		       (const :tag "White King: J"   (?K . ?J))
		       (const :tag "White Queen: D"  (?Q . ?D))
		       (const :tag "White Rook: T"   (?R . ?T))
		       (const :tag "White Bishop: L" (?B . ?L))
		       (const :tag "White Knight: S" (?N . ?S))
		       (const :tag "White Pawn: X"   (?P . ?X))
		       (const :tag "Black King: K"   (?k . ?K))
		       (const :tag "Black Queen: Q"  (?q . ?Q))
		       (const :tag "Black Rook: R"   (?r . ?R))
		       (const :tag "Black Bishop: B" (?b . ?B))
		       (const :tag "Black Knight: N" (?n . ?N))
		       (const :tag "Black Pawn: P"   (?p . ?P)))
		 (list :tag "Unicode figure pieces"
		       (const :tag "White King: ♔"   (?K . ?♔))
		       (const :tag "White Queen: ♕"  (?Q . ?♕))
		       (const :tag "White Rook: ♖"   (?R . ?♖))
		       (const :tag "White Bishop: ♗" (?B . ?♗))
		       (const :tag "White Knight: ♘" (?N . ?♘))
		       (const :tag "White Pawn: ♙"   (?P . ?♙))
		       (const :tag "Black King: ♚"   (?k . ?♚))
		       (const :tag "Black Queen: ♛"  (?q . ?♛))
		       (const :tag "Black Rook: ♜"   (?r . ?♜))
		       (const :tag "Black Bishop: ♝" (?b . ?♝))
		       (const :tag "Black Knight: ♞" (?n . ?♞))
		       (const :tag "Black Pawn: ♟"   (?p . ?♟)))
		 (list :tag "User defined"
		       (cons :format "%v"
			     (const :format "" ?K) (character :tag "White King"))
		       (cons :format "%v"
			     (const :format "" ?Q) (character :tag "White Queen"))
		       (cons :format "%v"
			     (const :format "" ?R) (character :tag "White Rook"))
		       (cons :format "%v"
			     (const :format "" ?B) (character :tag "White Bishop"))
		       (cons :format "%v"
			     (const :format "" ?N) (character :tag "White Knight"))
		       (cons :format "%v"
			     (const :format "" ?P) (character :tag "White Pawn"))
		       (cons :format "%v"
			     (const :format "" ?k) (character :tag "Black King"))
		       (cons :format "%v"
			     (const :format "" ?q) (character :tag "Black Queen"))
		       (cons :format "%v"
			     (const :format "" ?r) (character :tag "Black Rook"))
		       (cons :format "%v"
			     (const :format "" ?b) (character :tag "Black Bishop"))
		       (cons :format "%v"
			     (const :format "" ?n) (character :tag "Black Knight"))
		       (cons :format "%v"
			     (const :format "" ?p) (character :tag "Black Pawn")))
		 (function :tag "Function")))

(defcustom chess-plain-upcase-indicates 'color
  "Defines what a upcase char should indicate.
The default is 'color, meaning a upcase char is a white piece, a
lowercase char a black piece.  Possible values: 'color (default),
'square-color.  If set to 'square-color, a uppercase character
indicates a piece on a black square. (Note that you also need to
modify `chess-plain-piece-chars' to avoid real confusion.)"
  :group 'chess-plain
  :type '(choice (const :tag "Upcase indicates white piece" color)
		 (const :tag "Upcase indicates black square" square-color)))

(defcustom chess-plain-spacing 1
  "Number of spaces between files."
  :group 'chess-plain
  :type 'integer)

(defface chess-plain-black-face
  '((((class color) (background light)) (:foreground "Black"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:bold t)))
  "The face used for black pieces on the ASCII display."
  :group 'chess-plain)

(defface chess-plain-white-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Yellow"))
    (t (:bold t)))
  "The face used for white pieces on the ASCII display."
  :group 'chess-plain)

(defface chess-plain-highlight-face
  '((((class color) (background light)) (:background "#add8e6"))
    (((class color) (background dark)) (:background "#add8e6")))
  "Face to use for highlighting pieces that have been selected."
  :group 'chess-plain)

(defcustom chess-plain-popup-function 'chess-plain-popup
  "The function used to popup a chess-plain display."
  :type 'function
  :group 'chess-plain)

(defcustom chess-plain-separate-frame nil
  "If non-nil, display the chessboard in its own frame."
  :type 'boolean
  :group 'chess-plain)

;;; Code:

(defun chess-plain-customize ()
  "Show possible customisations for the plain chessboard display."
  (interactive)
  (customize-group 'chess-plain))

(defun chess-plain-handler (event &rest args)
  (cond
   ((eq event 'initialize) t)
   ((eq event 'popup) (funcall chess-plain-popup-function))
   (t (let ((handler (intern-soft (concat "chess-plain-" (symbol-name event)))))
	(when handler (apply handler args))))))

(defun chess-plain-popup ()
  (if chess-plain-separate-frame
      (chess-display-popup-in-frame 9 (* (1+ chess-plain-spacing) 8)
				    nil nil t)
    (chess-display-popup-in-window)))

(defun chess-plain-piece-text (piece rank file)
  (let ((white-square (zerop (% (+ file rank) 2))))
    (if (= piece ? )
	(if white-square
	    chess-plain-white-square-char
	  chess-plain-black-square-char)
      (let* ((pchar (cdr (assq piece chess-plain-piece-chars)))
	     (p (char-to-string
		 (if (eq chess-plain-upcase-indicates 'square-color)
		     (if white-square (downcase pchar) (upcase pchar))
		   pchar))))
	(add-text-properties 0 1 (list 'face (if (> piece ?a)
						 'chess-plain-black-face
					       'chess-plain-white-face)) p)
	p))))

(defun chess-plain-draw-square (pos piece index)
  "Draw a piece at POS on an already drawn display."
  (save-excursion
    (goto-char pos)
    (delete-char 1)
    (insert (chess-plain-piece-text piece (chess-index-rank index)
				    (chess-index-file index)))
    (add-text-properties pos (point) (list 'chess-coord index))))

(defun chess-plain-draw (position perspective)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((inverted (not perspective))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0)))
      (when chess-plain-border-style
	(insert ?  (aref chess-plain-border-style 0)
		(make-string (+ 8 (* 7 chess-plain-spacing))
			     (aref chess-plain-border-style 1))
		(aref chess-plain-border-style 2) ?\n))
      (while (if inverted (>= rank 0) (< rank 8))
	(when chess-plain-border-style
	  (insert (number-to-string (- 8 rank))
		  (aref chess-plain-border-style 3)))
	(while (if inverted (>= file 0) (< file 8))
	  (let ((piece (chess-pos-piece position
					(chess-rf-to-index rank file)))
		(begin (point)))
	    (insert (chess-plain-piece-text piece rank file))
	    (add-text-properties begin (point)
				 (list 'chess-coord
				       (chess-rf-to-index rank file)))
	    (when (if inverted (>= file 1) (< file 7))
	      (insert (make-string chess-plain-spacing ? ))))
	  (setq file (if inverted (1- file) (1+ file))))
	(when chess-plain-border-style
	  (insert (aref chess-plain-border-style 4)))
	(insert ?\n)
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (if chess-plain-border-style
	  (insert ?  (aref chess-plain-border-style 5)
		  (make-string (+ 8 (* 7 chess-plain-spacing))
			       (aref chess-plain-border-style 6))
		  (aref chess-plain-border-style 7) ?\n
		  ? ?  
		  (let ((string (if (not inverted) "abcdefgh" "hgfedcba")))
		    (mapconcat 'string (string-to-list string) 
			       (make-string chess-plain-spacing ? )))))
      (set-buffer-modified-p nil)
      (goto-char pos))))

(defun chess-plain-highlight (index &optional mode)
  (let ((pos (chess-display-index-pos nil index)))
    (put-text-property pos (1+ pos) 'face
		       (cond
			((eq mode :selected)
			 'chess-plain-highlight-face)
			(t
			 (chess-display-get-face mode))))))

(provide 'chess-plain)

;;; chess-plain.el ends here
