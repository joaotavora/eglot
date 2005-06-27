;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plain ASCII chess display
;;

(require 'chess-display)

;;; Code:

(defgroup chess-plain nil
  "A minimal, customizable ASCII display."
  :group 'chess-display)

(defcustom chess-plain-draw-border nil
  "*Non-nil if a border should be drawn (using `chess-plain-border-chars')."
  :group 'chess-plain
  :type 'boolean)

(defcustom chess-plain-border-chars '(?+ ?- ?+ ?| ?| ?+ ?- ?+)
  "*A list of Characters used to draw borders."
  :group 'chess-plain
  :type '(list (character :tag "Upper left corner")
	       (character :tag "Upper border")
	       (character :tag "Upper right corner")
	       (character :tag "Left border")
	       (character :tag "Right border")
	       (character :tag "Lower left corner")
	       (character :tag "Lowwer border")
	       (character :tag "Lower right corner")))

(defcustom chess-plain-black-square-char ?.
  "*Character used to indicate empty black squares."
  :group 'chess-plain
  :type 'character)

(defcustom chess-plain-white-square-char ?.
  "*Character used to indicate empty white squares."
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
  :type '(alist :key-type (character :tag "Internal representation")
		:value-type (character :tag "Printed representation")))

(defcustom chess-plain-upcase-indicates 'color
  "*Defines what a upcase char should indicate.
The default is 'color, meaning a upcase char is a white piece, a
lowercase char a black piece.  Possible values: 'color (default),
'square-color.  If set to 'square-color, a uppercase character
indicates a piece on a black square. (Note that you also need to
modify `chess-plain-piece-chars' to avoid real confusion.)"
  :group 'chess-plain
  :type '(choice (const color) (const square-color)))

(defcustom chess-plain-spacing 1
  "*Number of spaces between files."
  :group 'chess-plain
  :type 'integer)

(defface chess-plain-black-face
  '((((class color) (background light)) (:foreground "Black"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:bold t)))
  "*The face used for black pieces on the ASCII display."
  :group 'chess-plain)

(defface chess-plain-white-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Yellow"))
    (t (:bold t)))
  "*The face used for white pieces on the ASCII display."
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
  "*If non-nil, display the chessboard in its own frame."
  :type 'boolean
  :group 'chess-plain)

;;; Code:

(defun chess-plain-handler (event &rest args)
  (cond
   ((eq event 'initialize) t)

   ((eq event 'popup)
    (funcall chess-plain-popup-function))

   ((eq event 'draw)
    (apply 'chess-plain-draw args))

   ((eq event 'draw-square)
    (apply 'chess-plain-draw-square args))

   ((eq event 'highlight)
    (apply 'chess-plain-highlight args))))

(defun chess-plain-popup ()
  (if chess-plain-separate-frame
      (chess-display-popup-in-frame 9 (* (1+ chess-plain-spacing) 8)
				    nil nil t)
    (chess-display-popup-in-window)))

(defun chess-plain-piece-text (piece rank file)
  (let ((white-square (= (% (+ file rank) 2) 0)))
    (if (eq piece ? )
	(if white-square
	    chess-plain-white-square-char
	  chess-plain-black-square-char)
      (let* ((pchar (cdr (assq piece chess-plain-piece-chars)))
	     (p (char-to-string
		 (if (eq chess-plain-upcase-indicates 'square-color)
		     (if white-square
			 (downcase pchar)
		       (upcase pchar))
		   pchar))))
	(add-text-properties 0 1 (list 'face (if (> piece ?a)
						 'chess-plain-black-face
					       'chess-plain-white-face)) p)
	p))))

(defsubst chess-plain-draw-square (pos piece index)
  "Draw a piece image at POS on an already drawn display."
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
	   (file (if inverted 7 0)) beg)
      (if chess-plain-draw-border
	  (insert ?  (nth 0 chess-plain-border-chars)
		  (make-string (+ 8 (* 7 chess-plain-spacing))
			       (nth 1 chess-plain-border-chars))
		  (nth 2 chess-plain-border-chars) ?\n))
      (while (if inverted (>= rank 0) (< rank 8))
	(if chess-plain-draw-border
	    (insert (number-to-string (- 8 rank))
		    (nth 3 chess-plain-border-chars)))
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
	(if chess-plain-draw-border
	    (insert (nth 4 chess-plain-border-chars)))
	(insert ?\n)
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (if chess-plain-draw-border
	  (insert ?  (nth 5 chess-plain-border-chars)
		  (make-string (+ 8 (* 7 chess-plain-spacing))
			       (nth 6 chess-plain-border-chars))
		  (nth 7 chess-plain-border-chars) ?\n
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
