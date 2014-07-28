;;; chess-input.el --- Keyboard entry of algebraic notation, using shortcut notation

;; Copyright (C) 2002, 2005, 2014 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

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

;; This scheme was adapted from the way SCID (<http://scid.sourceforge.net/>),
;; by Shane Hudson, behaves.  It is based on standard algebraic notation.
;; You do not need to type all characters from the corresponding SAN of a move,
;; chess-input will automatically pick the move once it is unambiguous.
;;
;; Additionally, optional characters from SAN are treated as such.
;; You do not need to type x or =, although you can, if you prefer to.
;; For instance, "bxc8=N#" can be selected by typing `b c 8 n'.

;;; Code:

(require 'chess-algebraic)
(require 'chess-ply)
(require 'chess-pos)

(defvar chess-input-move-string "")
(defvar chess-input-moves-pos nil)
(defvar chess-input-moves nil)
(defvar chess-input-position-function nil)
(defvar chess-input-move-function nil)

(make-variable-buffer-local 'chess-input-move-string)
(make-variable-buffer-local 'chess-input-moves-pos)
(make-variable-buffer-local 'chess-input-moves)
(make-variable-buffer-local 'chess-input-position-function)
(make-variable-buffer-local 'chess-input-move-function)

(defgroup chess-input nil
  "Move input related otpions."
  :group 'chess)

(defcustom chess-input-notation-type :san
  "Define the notation type to use for move input."
  :group 'chess-input
  :type '(choice (const :tag "Standard (short) algebraic notation" :san)
		 (const :tag "Numeric notation" :numeric)))

(defun chess-input-test-move (ply)
  "Return the given PLY if it matches the user's current input."
  (let* ((move (chess-ply-to-algebraic ply chess-input-notation-type))
	 (i 0) (x 0) (l (length move))
	 (xl (length chess-input-move-string)))
    (unless (or (and (equal (downcase chess-input-move-string) "ok")
		     (chess-ply-keyword ply :castle))
		(and (equal (downcase chess-input-move-string) "oq")
		     (chess-ply-keyword ply :long-castle)))
      (while (and (< i l) (< x xl))
	(let ((move-char (aref move i))
	      (entry-char (aref chess-input-move-string x)))
	  (cond ((or (and (= move-char ?x) (/= entry-char ?x))
		     (and (= move-char ?=) (/= entry-char ?=)))
		 (setq i (1+ i)))
		((/= entry-char (if (< entry-char ?a)
				    move-char
				  (downcase move-char)))
		 (setq ply nil i l))
		(t (setq i (1+ i) x (1+ x)))))))
    ply))

(defvar chess-display-highlight-legal nil)
(declare-function chess-display-redraw "chess-display" (&optional display))
(declare-function chess-display-highlight "chess-display" (display &rest args))

(defun chess-input-display-moves (&optional move-list)
  (unless move-list
    (setq move-list
	  (delq nil (mapcar #'chess-input-test-move (cdr chess-input-moves)))))
  (when chess-display-highlight-legal
    (chess-display-redraw nil))
  (when (> (length chess-input-move-string) 0)
    (when chess-display-highlight-legal
      (apply #'chess-display-highlight
	     nil (delete-dups (mapcar #'chess-ply-target move-list))))
    (message "[%s] %s" chess-input-move-string
	     (mapconcat (lambda (ply)
			  (chess-ply-to-algebraic ply chess-input-notation-type))
			move-list " "))))

(defun chess-input-shortcut-delete ()
  (interactive)
  (when (and chess-input-move-string
	     (stringp chess-input-move-string)
	     (> (length chess-input-move-string) 0))
    (setq chess-input-move-string
	  (substring chess-input-move-string 0 (1- (length chess-input-move-string))))
    (chess-input-display-moves)))

(defun chess-input-shortcut (&optional display-only)
  (interactive)
  (let* ((position (funcall chess-input-position-function))
	 (color (chess-pos-side-to-move position))
	 char)
    (unless (memq last-command '(chess-input-shortcut
				 chess-input-shortcut-delete))
      (setq chess-input-move-string nil))
    (unless display-only
      (setq chess-input-move-string
	    (concat chess-input-move-string
		    (char-to-string last-command-event))))
    (unless (and chess-input-moves
		 (eq position chess-input-moves-pos)
		 (or (> (length chess-input-move-string) 1)
		     (eq (car chess-input-moves) last-command-event)))
      (setq char (if (eq (downcase last-command-event) ?o)
		     ?k
		   last-command-event))
      (if (or (memq (upcase char) '(?K ?Q ?N ?B ?R ?P))
	      (and (>= char ?a) (<= char ?h))
	      (and (>= char ?1) (<= char ?8)))
	  (setq chess-input-moves-pos position
		chess-input-moves
		(cons
		 char
		 (sort
		  (cond ((eq char ?b)
			 (nconc (chess-legal-plies
				 position :piece (if color ?P ?p) :file 1)
				(chess-legal-plies
				 position :piece (if color ?B ?b))))
			((and (>= char ?a) (<= char ?h))
			 (chess-legal-plies
			  position :piece (if color ?P ?p)
			  :file (chess-file-from-char char)))
			((and (>= char ?1) (<= char ?8))
			 (chess-legal-plies
			  position :color color :file (- char ?1)))
			(t (chess-legal-plies
			    position :piece (if color
						(upcase char)
					      (downcase char)))))
		  (lambda (left right)
		    (string-lessp (chess-ply-to-algebraic left)
				  (chess-ply-to-algebraic right)))))))))
  (let ((moves (delq nil (mapcar #'chess-input-test-move
				 (cdr chess-input-moves)))))
    (cond ((or (= (length moves) 1)
	       ;; if there is an exact match except for case, it must be an
	       ;; abiguity between a bishop and a b-pawn move.  In this
	       ;; case, always take the b-pawn move; to select the bishop
	       ;; move, use B to begin the keyboard shortcut
	       (and (= (length moves) 2)
		    (string= (downcase (chess-ply-to-algebraic (car moves)))
			     (downcase (chess-ply-to-algebraic (cadr moves))))
		    (setq moves (cdr moves))))
	   (funcall chess-input-move-function nil (car moves))
	   (when chess-display-highlight-legal
	     (chess-display-redraw nil))
	   (setq chess-input-move-string nil
		 chess-input-moves nil
		 chess-input-moves-pos nil))

	  ((null moves)
	   (chess-input-shortcut-delete))

	  (t (chess-input-display-moves moves)))))

(provide 'chess-input)

;;; chess-input.el ends here
