;;; chess-ics1.el --- Classic ICS1 style chessboard display

;; Copyright (C) 2002, 2005, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
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

;;; Code:

(require 'chess-display)

(defgroup chess-ics1 nil
  "The ICS1 style ASCII display."
  :group 'chess-display)

(defface chess-ics1-black-face
  '((((class color) (background light)) (:foreground "Green"))
    (((class color) (background dark)) (:foreground "Green"))
    (t (:bold t)))
  "*The face used for black pieces on the ASCII display."
  :group 'chess-ics1)

(defface chess-ics1-white-face
  '((((class color) (background light)) (:foreground "Yellow"))
    (((class color) (background dark)) (:foreground "Yellow"))
    (t (:bold t)))
  "*The face used for white pieces on the ASCII display."
  :group 'chess-ics1)

(defface chess-ics1-highlight-face
  '((((class color) (background light)) (:background "#add8e6"))
    (((class color) (background dark)) (:background "#add8e6")))
  "Face to use for highlighting pieces that have been selected."
  :group 'chess-ics1)

(defcustom chess-ics1-popup-function 'chess-ics1-popup
  "The function used to popup a chess-ics1 display."
  :type 'function
  :group 'chess-ics1)

(defcustom chess-ics1-separate-frame nil
  "If non-nil, display the chessboard in its own frame."
  :type 'boolean
  :group 'chess-ics1)

;;; Code:

(defun chess-ics1-handler (event &rest args)
  (cond
   ((eq event 'initialize) t)

   ((eq event 'popup)
    (funcall chess-ics1-popup-function))

   ((eq event 'draw)
    (apply 'chess-ics1-draw args))

   ((eq event 'draw-square)
    (apply 'chess-ics1-draw-square args))

   ((eq event 'highlight)
    (apply 'chess-ics1-highlight args))))

(defun chess-ics1-popup ()
  (if chess-ics1-separate-frame
      (chess-display-popup-in-frame 21 43 nil nil t)
    (chess-display-popup-in-window)))

(defsubst chess-ics1-piece-text (piece)
  (let ((p (char-to-string piece)))
    (add-text-properties 0 1 (list 'face (if (> piece ?a)
					     'chess-ics1-black-face
					   'chess-ics1-white-face)) p)
    p))

(defsubst chess-ics1-draw-square (pos piece index)
  "Draw a piece image at point on an already drawn display."
  (save-excursion
    (let ((inhibit-redisplay t))
      (goto-char pos)
      (delete-char 3)
      (insert ?  (chess-ics1-piece-text piece) ? )
      (add-text-properties pos (point) (list 'chess-coord index)))))

(defun chess-ics1-draw (position perspective)
  "Draw the given POSITION from PERSPECTIVE's point of view.
PERSPECTIVE is t for white or nil for black."
  (let ((inhibit-redisplay t)
	(pos (point)))
    (erase-buffer)
    (let* ((inverted (not perspective))
	   (rank (if inverted 7 0))
	   (file (if inverted 7 0)))
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
	    (insert (chess-ics1-piece-text piece) ? )
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
  (let ((pos (chess-display-index-pos nil index)))
    (put-text-property pos (save-excursion
			     (goto-char pos)
			     (skip-chars-forward "^|")
			     (point))
		       'face (cond
			      ((eq mode :selected)
			       'chess-ics1-highlight-face)
			      (t
			       (chess-display-get-face mode))))))

(defun chess-debug-position (&optional position)
  "This is a debugging function, and not meant from general use."
  (interactive)
  (let ((pos (or position (chess-display-position nil))))
    (with-current-buffer (get-buffer-create "*scratch*")
      (chess-ics1-draw pos t)
      (funcall chess-ics1-popup-function))))

(provide 'chess-ics1)

;;; chess-ics1.el ends here
