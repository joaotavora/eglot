;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ICS1 style display
;;
;; $Revision$

(require 'chess-display)

(defgroup chess-ics1 nil
  "The ICS1 style ASCII display."
  :group 'chess-display)

(defcustom chess-ics1-popup t
  "If non-nil, popup the chessboard display whenever the opponent moves."
  :type 'boolean
  :group 'chess-ics1)

(defcustom chess-ics1-popup-function 'chess-display-popup-in-window
  "The function used to popup a chess-ics1 display."
  :type 'function
  :group 'chess-ics1)

;;; Code:

(defun chess-ics1-handler (event &rest args)
  (cond
   ((eq event 'popup)
    (if chess-ics1-popup
	(funcall chess-ics1-popup-function)))
   ((eq event 'draw)
    (apply 'chess-ics1-draw args))
   ((eq event 'highlight)
    (apply 'chess-ics1-highlight args))))

(defun chess-ics1-draw (position perspective)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((inverted (not perspective))
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
	  (insert "        h   g   f   e   d   c   b   a\n\n")
	(insert "        a   b   c   d   e   f   g   h\n\n")))
    (set-buffer-modified-p nil)
    (goto-char pos)))

(defun chess-ics1-highlight (index &optional mode)
  (if (null (get-buffer-window (current-buffer) t))
      (pop-to-buffer (current-buffer)))
  (let ((inverted (not (chess-display-perspective nil)))
	beg end)
    (save-excursion
      (goto-char (point-min))
      (let ((rank (chess-index-rank index))
	    (file (chess-index-file index)))
	(goto-line (+ 3 (* 2 (if inverted (- 7 rank) rank))))
	(forward-char (+ 8 (* 4 (if inverted (- 7 file) file)))))
      (skip-chars-backward "^|")
      (setq beg (point))
      (skip-chars-forward "^|")
      (put-text-property beg (point) 'face 'chess-display-highlight-face))))

(defun chess-debug-position (&optional position)
  "This is a debugging function, and not meant from general use."
  (interactive)
  (let ((pos (or position (chess-engine-position nil))))
    (with-current-buffer (get-buffer-create "*scratch*")
      (chess-ics1-draw pos t)
      (funcall chess-ics1-popup-function))))

(provide 'chess-ics1)

;;; chess-ics1.el ends here
