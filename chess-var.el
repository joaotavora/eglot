;;; chess-var.el --- Manipulate variations

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

;; A chess variations is a simple list of plies.  This module provides
;; an abstraction layer for applications.

;;; Code:

(require 'chess-algebraic)
(require 'chess-ply)
(eval-when-compile (require 'cl-lib))

(defsubst chess-var-plies (var)
  "Return the plies of VAR."
  (cl-assert var)
  var)

(defsubst chess-var-pos (var &optional index)
  "Return the position related to VAR's INDEX ply."
  (cl-assert var)
  (chess-ply-pos (chess-var-ply var index)))

(defsubst chess-var-index (var)
  "Return the VAR's current position index."
  (cl-assert var)
  (1- (length (chess-var-plies var))))

(defsubst chess-var-seq (var)
  "Return the current VAR sequence."
  (cl-assert var)
  (1+ (/ (chess-var-index var) 2)))

(defsubst chess-var-side-to-move (var &optional index)
  "Return the color whose move it is in VAR at INDEX (or at the last position
of the variation if INDEX is nil)."
  (cl-assert var)
  (chess-pos-side-to-move (chess-var-pos var index)))

(defun chess-var-ply (var &optional index)
  "Return VAR's INDEXth ply."
  (cl-assert var)
  (if index
      (nth index (chess-var-plies var))
    (car (last (chess-var-plies var)))))

(defun chess-var-add-ply (var ply)
  "Add to VAR the given PLY."
  (cl-assert var)
  (cl-assert (listp ply))
  (let ((plies (chess-var-plies var)))
    (cl-assert plies)
    (nconc plies (list ply))))

(defsubst chess-var-create (&optional position)
  "Create a new chess variation object.
Optionally use the given starting POSITION."
  (list (chess-ply-create* (or position chess-starting-position))))

(defun chess-var-move (var ply)
  "Make a move in the current VAR by applying the changes of PLY.
This creates a new position and adds it to the main variation.
The 'changes' of the last ply reflect whether the var is currently in
progress (nil), if it is drawn, resigned, mate, etc."
  (cl-assert var)
  (cl-assert (listp ply))
  (let ((current-ply (chess-var-ply var))
	(changes (chess-ply-changes ply)))
    (if (chess-ply-final-p current-ply)
	(chess-error 'add-to-completed))
    (cl-assert (eq (chess-ply-pos ply) (chess-ply-pos current-ply)))
    (chess-ply-set-changes current-ply changes)
    (chess-var-add-ply var (chess-ply-create*
			    (chess-ply-next-pos current-ply)))))

(defun chess-var-to-algebraic (var &optional type)
  "Reveal the plies of VAR by converting them to algebraic notation.
Optional argument TYPE defines the type of algebraic notation to use
(`:san', `:lan' or `:fan'."
  (mapconcat (lambda (ply)
	       (chess-ply-to-algebraic ply type))
	     (if (chess-ply-final-p (chess-var-ply var))
		 (chess-var-plies var)
	       (butlast (chess-var-plies var)))
	     " "))

(provide 'chess-var)

;;; chess-var.el ends here
