;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ASCII chess displays
;;
;; $Revision$

(require 'chess-display)

(defgroup chess-ascii nil
  "A set of ASCII chess displays: ics1, plain.
`ICS1' is similar to what is used by Internet chess servers.
`plain' is uses a minimum number of chars, with an optional border
and customizable piece characters."
  :group 'chess-display)

;;; Code:

;;; ICS1 style display

(defun chess-ics1-draw ()
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((position (chess-display-position nil))
	   (inverted (null (chess-display-perspective nil)))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0))
	   beg)
      (insert "\n      +---+---+---+---+---+---+---+---+\n")
      (while (if inverted (>= rank 0) (< rank 8))
	(if (/= rank (if inverted 7 0))
	    (insert "      +---+---+---+---+---+---+---+---+\n"))
	(while (if inverted (>= file 0) (< file 8))
	  (let ((piece (chess-pos-piece position
					(chess-rf-to-index rank file)))
		begin)
	    (if (= file (if inverted 7 0))
		(insert (format "    %d " (1+ (- 7 rank)))))
	    (insert "| ")
	    (setq begin (1- (point)))
	    (if (and chess-display-use-faces (/= ?  piece))
		(let ((p (char-to-string piece)))
		  (add-text-properties
		   0 1 (list 'face (if (> piece ?a)
				       'chess-display-black-face
				     'chess-display-white-face)) p)
		  (insert p))
	      (insert piece))
	    (insert ? )
	    (add-text-properties begin (point)
				 (list 'chess-coord
				       (chess-rf-to-index rank file))))
	  (setq file (if inverted (1- file) (1+ file))))
	(insert "|\n")
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (insert "      +---+---+---+---+---+---+---+---+\n")
      (if inverted
	  (insert "        h   g   f   e   d   c   b   a\n")
	(insert "        a   b   c   d   e   f   g   h\n")))
    (set-buffer-modified-p nil)
    (goto-char pos)))

(defun chess-ics1-highlight (index &optional mode)
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (goto-line (+ 2 (chess-index-rank index)))
      (forward-char (+ 8 (* 4 (chess-index-file index))))
      (skip-chars-backward "^|")
      (setq beg (point))
      (skip-chars-forward "^|")
      (put-text-property beg (point) 'face 'chess-display-highlight-face))))

;; Plain display

(defcustom chess-plain-draw-border nil
  "*Non-nil if a border should be drawn (using `chess-plain-border-chars')."
  :group 'chess-plain
  :type 'boolean)

(defcustom chess-plain-border-chars '(?+ ?- ?+ ?| ?| ?+ ?- ?+)
  "*Characters used to draw borders."
  :group 'chess-plain
  :type '(list character character character character
	       character character character character))

(defcustom chess-plain-black-square-char ?.
  "*Character used to indicate black squares."
  :group 'chess-plain
  :type 'character)

(defcustom chess-plain-white-square-char ?.
  "*Character used to indicate white squares."
  :group 'chess-plain
  :type 'character)

(defcustom chess-plain-piece-chars
  '((?K . ?K)
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
  "*Alist of pieces and their corresponding characters."
  :group 'chess-plain
  :type '(alist :key-type character :value-type character))

(defcustom chess-plain-upcase-indicates 'color
  "*Defines what a upcase char should indicate.
The default is 'color, meaning a upcase char is a white piece, a
lowercase char a black piece.  Possible values: 'color (default),
'square-color.  If set to 'square-color, a uppercase character
indicates a piece on a black square. (Note that you also need to
modify `chess-plain-piece-chars' to avoid real confusion.)"
  :group 'chess-plain
  :type '(choice (const 'color) (const 'square-color)))
	 ;; fails somehow

(defun chess-plain-draw ()
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((position (chess-display-position nil))
	   (inverted (null (chess-display-perspective nil)))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0))
	   beg)
      (if chess-plain-draw-border
	  (insert ?  (nth 0 chess-plain-border-chars)
		  (make-string 8 (nth 1 chess-plain-border-chars))
		  (nth 2 chess-plain-border-chars) ?\n))
      (while (if inverted (>= rank 0) (< rank 8))
	(if chess-plain-border-chars
	    (insert (number-to-string (- 8 rank))
		    (nth 3 chess-plain-border-chars)))
	(while (if inverted (>= file 0) (< file 8))
	  (let ((piece (chess-pos-piece position
					(chess-rf-to-index rank file)))
		(white-square (evenp (+ file rank)))
		(begin (point)))
	    (insert (if (eq piece ? )
			(if white-square
			    chess-plain-white-square-char
			  chess-plain-black-square-char)
		      (let ((what chess-plain-upcase-indicates)
			    (pchar (cdr (assq piece chess-plain-piece-chars))))
			(cond
			 ((eq what 'square-color)
			  (if white-square
			      (downcase pchar)
			    (upcase pchar)))
			 (t pchar)))))
	    (add-text-properties begin (point)
				 (list 'chess-coord
				       (chess-rf-to-index rank file))))
	  (setq file (if inverted (1- file) (1+ file))))
	(if chess-plain-draw-border
	    (insert (nth 4 chess-plain-border-chars) ?\n))
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (if chess-plain-draw-border
	  (insert ?  (nth 5 chess-plain-border-chars)
		  (make-string 8 (nth 6 chess-plain-border-chars))
		  (nth 7 chess-plain-border-chars) ?\n
		  ? ?  (if (not inverted) "abcdefgh" "hgfedcba")))
      (set-buffer-modified-p nil)
      (goto-char pos))))

(defun chess-plain-highlight (index &optional mode)
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (save-excursion
    (beginning-of-line)
    (goto-line (if chess-plain-draw-border
		   (+ 2 (chess-index-rank index))
		 (1+ (chess-index-rank index))))
    (forward-char (if chess-plain-draw-border
		      (1+ (chess-index-file index))
		    (chess-index-file index)))
    (put-text-property (point) (1+ (point)) 'face
		       'chess-display-highlight-face)))

(provide 'chess-ascii)

;;; chess-ascii.el ends here
