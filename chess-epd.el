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
;; 

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
  (let (positions pos)
    (with-temp-buffer
      (insert-file-literally file)
      (goto-char (point-min))
      (while (setq pos (chess-epd-parse))
	(setq positions (cons pos positions))))
    positions))

(defun chess-epd-parse ()
  (when (re-search-forward
	 "\\([bnrqkpBNRQKP1-8]*/?\\)+ [bw] \\(-\\|[KQkq]+\\) \\(-\\|[1-8]\\)"
	 nil t)
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
				   (chess-var-move var
						   (chess-ply-from-string
						    (chess-var-pos var) ply)))
				 (split-string val " "))
			   var))
			(t val))))
		  (error "Illegal char following identifier"))))
	  (error "Illegal Identifier")))
      (skip-chars-forward "\n")
      pos)))

(provide 'chess-epd)
;;; chess-epd.el ends here
