;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ICS1 style display
;;
;; $Revision$

(require 'chess-display)

;;; Code:

(defun chess-debug-position (&optional position)
  "This is a debugging function, and not meant from general use."
  (interactive)
  (let ((pos (or position (chess-engine-position nil))))
    (with-current-buffer (get-buffer-create "*scratch*")
      (chess-ics1-draw pos))))

(defun chess-ics1-draw (&optional disppos)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((position (or disppos (chess-display-position nil)))
	   (inverted (and (null disppos)
			  (null (chess-display-perspective nil))))
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

(provide 'chess-ics1)

;;; chess-ics1.el ends here
