;;; chess-epd.el --- Extended Position Description Format

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; EPD is the "Extended Position Description" format.  It is a standard for
;; describing chess positions along with an extended set of structured
;; attribute values using the ASCII character set.  It is intended for data and
;; command interchange among chessplaying programs.  It is also intended
;; for the representation of portable opening library repositories and for
;; problem test suites.

;; A single EPD record uses one text line of variable length composed of
;; four data fields followed by zero or more operations.  A text file
;; composed exclusively of EPD data records should have a file name with
;; the suffix ".epd".

;;; Code:

(require 'chess-fen)
(require 'chess-ply)
(require 'chess-pos)
(require 'chess-var)

(defun chess-epd-annotation-to-string (annotation)
  (let ((opcode (car annotation))
	(value (cdr annotation)))
    (cond
     ((or (eq opcode 'am) (eq opcode 'bm))
      (assert (consp value))
      (format "%S %s;"
	      opcode (mapconcat #'chess-ply-to-string value " ")))
     ((eq opcode 'ce)
      (assert (integerp value))
      (format "%S %d;" opcode value))
     ((or (eq opcode 'pv) (eq opcode 'sv))
      (format "%S %s;"
	      opcode (chess-var-to-algebraic value)))
     (t
      (format "%S%s;" opcode (if (eq value t) "" (format " %s" value)))))))

(defun chess-pos-to-epd (position)
  "Convert a chess POSITION to a string representation in extended
position description format."
  (assert position)
  (concat (chess-pos-to-fen position)
	  (when (consp (chess-pos-annotations position))
	    (concat " "
		    (mapconcat #'chess-epd-annotation-to-string
			       (chess-pos-annotations position)
			       " ")))))

(defun chess-epd-to-pos (&optional string)
  "Convert extended position description to a chess position.
If STRING is not specified, look for an EPD string in the current buffer,
and advance point after the correctly parsed position."
  (if (stringp string)
      (with-temp-buffer
	(insert string)
	(chess-epd-parse))
    (chess-epd-parse)))

(defun chess-epd-read-file (file)
  "Return a list of positions contained in FILE."
  (let ((positions (list t)) pos)
    (with-temp-buffer
      (insert-file-literally file)
      (goto-char (point-min))
      (while (setq pos (chess-epd-parse))
	(nconc positions (list pos))))
    (cdr positions)))

(defsubst chess-game-to-epd (game &optional to-string index)
  (if to-string
      (chess-pos-to-epd (chess-game-pos game index))
    (insert (chess-pos-to-epd (chess-game-pos game index)) ?\n)))

(defsubst chess-epd-to-game (&optional string)
  (chess-game-create (chess-epd-to-pos string)))

(defun chess-epd-parse ()
  (when (re-search-forward chess-fen-regexp nil t)
    (let ((pos (chess-fen-to-pos (match-string 0))))
      (while (= 1 (skip-chars-forward " "))
	(if (looking-at "[A-Za-z]")
	    (let ((opcode (intern (buffer-substring
				   (point) (+ (point) (skip-chars-forward
						       "A-Za-z0-9_"))))))
	      (if (= 1 (skip-chars-forward ";"))
		  (chess-pos-set-epd pos opcode)
		(if (= (skip-chars-forward " ") 1)
		    (let ((val (buffer-substring
				(point) (prog1
					    (+ (point)
					       (skip-chars-forward "^;"))
					  (skip-chars-forward ";")))))
		      (chess-pos-set-epd
		       pos opcode
		       (cond
			((or (eq opcode 'am) (eq opcode 'bm))
			 (mapcar (lambda (move)
				   (chess-ply-from-string pos move))
				 (split-string val " ")))
			((eq opcode 'ce)
			 (read val))
			((or (eq opcode 'pm) (eq opcode 'sm)) ;predicted/supplied move
			 (chess-ply-from-string pos val))
			((or (eq opcode 'pv) (eq opcode 'sv)) ; predicted/supplied variation
			 (let ((var (chess-var-create pos)))
			   (mapc (lambda (ply)
				   (let ((changes (chess-ply-from-string
						   (chess-var-pos var) ply)))
				     (if changes
					 (chess-var-move var changes)
				       (error "Unable to convert ply '%s'" ply))))
				 (split-string val " "))
			   var))
			(t val))))
		  (error "Illegal char following identifier"))))
	  (error "Illegal Identifier")))
      (skip-chars-forward "\n")
      pos)))

(provide 'chess-epd)
;;; chess-epd.el ends here
