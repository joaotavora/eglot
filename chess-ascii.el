;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ASCII chess displays
;;
;; $Revision$

(require 'chess-display)

(defgroup chess-ascii nil
  "A standard set of ASCII chess displays."
  :group 'chess-display)

(defcustom chess-ascii-style 'ics1
  :type '(radio (const ics1)
		(const plain))
  :group 'chess-ascii)

;;; Code:

;;;###autoload
(defun chess-ascii (session buffer event &rest args)
  "Handle any commands being sent to this instance of this module."
  (if (not (eq event 'initialize))
      (apply 'chess-display session buffer event args)
    (chess-display session buffer event
		   (cond
		    ((eq chess-ascii-style 'ics1)
		     'chess-ics1-draw 'chess-ics1-highlight)
		    ((eq chess-ascii-style 'plain)
		     'chess-plain-draw 'chess-plain-highlight)))))

(defun chess-ics1-draw ()
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((position chess-display-position)
	   (inverted (null chess-display-perspective))
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

;; jww (2002-03-28): this no longer takes a `pos' arg!
(defun chess-ics1-highlight (pos index &optional mode)
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let (beg end)
    (save-excursion
      (goto-char pos)
      (skip-chars-backward "^|")
      (setq beg (point))
      (skip-chars-forward "^|")
      (put-text-property beg (point) 'face 'chess-display-highlight-face))))

(defun chess-plain-draw ()
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((position chess-display-position)
	   (inverted (null chess-display-perspective))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0))
	   beg)
      (while (if inverted (>= rank 0) (< rank 8))
	(while (if inverted (>= file 0) (< file 8))
	  (let ((piece (chess-pos-piece position
					(chess-rf-to-index rank file)))
		(begin (point)))
	    (insert (if (eq piece ? ) ?. piece))
	    (add-text-properties begin (point)
				 (list 'chess-coord
				       (chess-rf-to-index rank file))))
	  (setq file (if inverted (1- file) (1+ file))))
	(insert ?\n)
	(setq file (if inverted 7 0)
	      rank (if inverted (1- rank) (1+ rank))))
      (set-buffer-modified-p nil)
      (goto-char pos))))

;; jww (2002-03-28): this no longer takes a `pos' arg!
(defun chess-plain-highlight (pos index &optional mode)
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (put-text-property pos (1+ pos) 'face 'chess-display-highlight-face))

(provide 'chess-ascii)

;;; chess-ascii.el ends here
