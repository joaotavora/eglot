;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Plain ASCII chess display
;;
;; $Revision$

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
  "*Characters used to draw borders."
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

(defcustom chess-plain-spacing 0
  "*Number of spaces between pieces."
  :group 'chess-plain
  :type 'integer)

(defcustom chess-plain-popup-function 'chess-display-popup-in-window
  "The function used to popup a chess-plain display."
  :type 'function
  :group 'chess-plain)

;;; Code:

(defun chess-plain-handler (event &rest args)
  (cond
   ((eq event 'initialize) t)
   ((eq event 'popup)
    (if chess-display-popup
	(funcall chess-plain-popup-function)))
   ((eq event 'draw)
    (apply 'chess-plain-draw args))
   ((eq event 'highlight)
    (apply 'chess-plain-highlight args))))

(defun chess-plain-draw (position perspective)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((inverted (not perspective))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0))
	   beg)
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
		  ? ?  (if (not inverted) "abcdefgh" "hgfedcba")))
      (set-buffer-modified-p nil)
      (goto-char pos))))

(defun chess-plain-highlight (index &optional mode)
  (let ((inverted (not (chess-display-perspective nil))))
    (save-excursion
      (beginning-of-line)
      (let ((rank (chess-index-rank index))
	    (file (chess-index-file index)))
	(if inverted
	    (setq rank (- 7 rank)
		  file (- 7 file)))
	(goto-line (if chess-plain-draw-border
		       (+ 2 rank)
		     (1+ rank)))
	(forward-char (if chess-plain-draw-border
			  (1+ file)
			file)))
      (put-text-property (point) (1+ (point)) 'face
			 (cond
			  ((eq mode :selected)
			   'chess-plain-highlight-face)
			  (t
			   (chess-display-get-face mode)))))))

(provide 'chess-plain)

;;; chess-plain.el ends here
