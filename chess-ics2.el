;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ICS2 style display
;; Author: Dmitry "Troydm" Geurkov (dgeurkov@gmail.com)

(require 'chess-display)
(require 'chess-engine)

(defgroup chess-ics2 nil
  "The ICS2 style based on ICS1 ASCII display."
  :group 'chess-display)

(defface chess-ics2-black-face
  '((((class color) (background light))
     (:foreground "Black" :background "color-209"))
    (((class color) (background dark))
     (:foreground "Black" :background "color-209"))
    (t (:bold t)))
  "*The face used for black pieces on the ASCII display."
  :group 'chess-ics2)


(defface chess-ics2-black-face-alt
  '((((class color) (background light))
     (:foreground "Black" :background "color-130"))
    (((class color) (background dark))
     (:foreground "Black" :background "color-130"))
    (t (:bold t)))
  "*The alternative face used for black pieces on the ASCII display."
  :group 'chess-ics2)

(defface chess-ics2-white-face
  '((((class color) (background light))
     (:foreground "White" :background "color-209"))
    (((class color) (background dark))
     (:foreground "White" :background "color-209"))
    (t (:bold t)))
  "*The face used for white pieces on the ASCII display."
  :group 'chess-ics2)

(defface chess-ics2-white-face-alt
  '((((class color) (background light))
     (:foreground "White" :background "color-130"))
    (((class color) (background dark))
     (:foreground "White" :background "color-130"))
    (t (:bold t)))
  "*The alternative face used for white pieces on the ASCII display."
  :group 'chess-ics2)

(defface chess-ics2-highlight-face
  '((((class color) (background light))
     (:foreground "Black" :background "#add8e6"))
    (((class color) (background dark))
     (:foreground "Black" :background "#add8e6")))
  "Face to use for highlighting pieces that have been selected."
  :group 'chess-ics2)

(defcustom chess-ics2-piece-chars
  '((?\040 . ?\040)
    (?k . ?♛)
    (?q . ?♚)
    (?r . ?♜)
    (?b . ?♝)
    (?n . ?♞)
    (?p . ?♟)
    (?K . ?♛)
    (?Q . ?♚)
    (?R . ?♜)
    (?B . ?♝)
    (?N . ?♞)
    (?P . ?♟))
  "*Alist of pieces and their corresponding characters."
  :group 'chess-ics2
  :type '(alist :key-type (character :tag "Internal representation")
		:value-type (character :tag "Printed representation")))


(defcustom chess-ics2-popup-function 'chess-ics2-popup
  "The function used to popup a chess-ics1 display."
  :type 'function
  :group 'chess-ics2)

(defcustom chess-ics2-separate-frame nil
  "If non-nil, display the chessboard in its own frame."
  :type 'boolean
  :group 'chess-ics2)

;;; Code:

(defun chess-ics2-handler (event &rest args)
  (cond
   ((eq event 'initialize) t)

   ((eq event 'popup)
    (funcall chess-ics2-popup-function))

   ((eq event 'draw)
    (apply 'chess-ics2-draw args))

   ((eq event 'draw-square)
    (apply 'chess-ics2-draw-square args))

   ((eq event 'highlight)
    (apply 'chess-ics2-highlight args))))

(defun chess-ics2-popup ()
  (if chess-ics2-separate-frame
      (chess-display-popup-in-frame 21 43 nil nil t)
    (chess-display-popup-in-window)))


(defsubst chess-ics2-piece-text (piece rank file)
  (let ((p (char-to-string (cdr (assq piece chess-ics2-piece-chars))))
	(a (% (+ rank file) 2)))
    (add-text-properties 0 1 (list 'face (if (> piece ?a)
					     (if (= a 0) 'chess-ics2-black-face
					     'chess-ics2-black-face-alt)
					   (if (= a 0) 'chess-ics2-white-face
					     'chess-ics2-white-face-alt))) p)
    p))

(defun chess-ics2-piece-text-draw (piece rank file)
  (insert (chess-ics2-piece-text piece rank file))
  (insert (chess-ics2-piece-text ?  rank file)))

(defsubst chess-ics2-draw-square (pos piece index)
  "Draw a piece image at point on an already drawn display."
  (save-excursion
    (let ((inhibit-redisplay t))
      (message (concat (int-to-string pos) " " (int-to-string index) " "
                       (char-to-string piece)))
      (if (= (% (+ (/ index 8) pos) 2) 1)
      (goto-char pos) (goto-char (1- pos)))
      (delete-char 2)
      (insert (chess-ics2-piece-text piece (/ index 8) index))
      (insert (chess-ics2-piece-text ?  (/ index 8) index))
      (add-text-properties pos (point) (list 'chess-coord index)))))

(defun chess-ics2-draw (position perspective)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((inverted (not perspective))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0)) beg)
      (insert "\n\n")
      (while (if inverted (>= rank 0) (< rank 8))
	(while (if inverted (>= file 0) (< file 8))
	  (let ((piece (chess-pos-piece position
					(chess-rf-to-index rank file)))
		begin)
	    (if (= file (if inverted 7 0))
		(insert (format "    %d " (1+ (- 7 rank)))))
	    (setq begin (point))
	    (chess-ics2-piece-text-draw piece rank file)
	    (add-text-properties begin (point)
				 (list 'chess-coord
				       (chess-rf-to-index rank file))))
	  (setq file (if inverted (1- file) (1+ file))))
	(insert "\n")
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (if inverted
	  (insert "      h g f e d c b a\n\n")
	(insert "      a b c d e f g h\n\n")))
    (set-buffer-modified-p nil)
    (goto-char pos)))

(defun chess-ics2-highlight (index &optional mode)
  (let ((pos (chess-display-index-pos nil index)))
    (put-text-property pos (save-excursion
			     (goto-char (+ pos 2))
			     (point))
		       'face (cond
			      ((eq mode :selected)
			       'chess-ics2-highlight-face)
			      (t
			       (chess-display-get-face mode))))))

(defun chess-ics2-debug-position (&optional position)
  "This is a debugging function, and not meant from general use."
  (interactive)
  (let ((pos (or position (chess-engine-position nil))))
    (with-current-buffer (get-buffer-create "*scratch*")
      (chess-ics2-draw pos t)
      (funcall chess-ics2-popup-function))))

(provide 'chess-ics2)

;;; chess-ics2.el ends here
