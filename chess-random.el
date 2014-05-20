;;; chess-random.el --- Generate Fischer Random style positions

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Just call `chess-fischer-random-position' to generate such a
;; position.

;;; Code:

(require 'chess-pos)

(defvar pieces-vector [?r ?n ?b ?q ?k ?b ?n ?r])

(defun chess-shuffle-vector (vector)
  "Randomly permute the elements of VECTOR (all permutations equally likely)"
  (let ((i 0)
	j
	temp
	(len (length vector)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (aref vector i))
      (aset vector i (aref vector j))
      (aset vector j temp)
      (setq i (1+ i))))
  vector)

;;;###autoload
(defun chess-fischer-random-position ()
  "Generate a Fischer Random style position."
  (let (pieces position)
    (while (null position)
      (setq pieces (chess-shuffle-vector pieces-vector))
      (let (first-bishop first-rook king)
	(catch 'retry
	  (dotimes (i 8)
	    (let ((piece (aref pieces i)))
	      (cond
	       ((= ?b piece)
		(if first-bishop
		    (if (= (mod i 2) first-bishop)
			(throw 'retry t))
		  (setq first-bishop (mod i 2))))
	       ((= ?k piece)
		(if (null first-rook)
		    (throw 'retry t))
		(setq king i))
	       ((= ?r piece)
		(if first-rook
		    (if (null king)
			(throw 'retry t))
		  (setq first-rook i))))))
	  (setq position (chess-pos-create)))))

    ;; set the home row pieces
    (dotimes (i 8)
      (chess-pos-set-piece position (chess-rf-to-index 0 i)
			   (aref pieces i))
      (chess-pos-set-piece position (chess-rf-to-index 7 i)
			   (upcase (aref pieces i))))

    position))

(provide 'chess-random)

;;; chess-random.el ends here
