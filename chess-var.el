;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manipulate variations (which are really just lists of plies)
;;

;;; Commentary:

;; A chess variations is a simple list of plies.  This module provides
;; an abstraction layer for applications.

(require 'chess-ply)

(defsubst chess-var-plies (var)
  "Return the plies of VAR."
  (assert var)
  var)

(defsubst chess-var-pos (var &optional index)
  "Return the position related to VAR's INDEX ply."
  (assert var)
  (chess-ply-pos (chess-var-ply var index)))

(defsubst chess-var-index (var)
  "Return the VAR's current position index."
  (assert var)
  (1- (length (chess-var-plies var))))

(defsubst chess-var-seq (var)
  "Return the current VAR sequence."
  (assert var)
  (1+ (/ (chess-var-index var) 2)))

(defsubst chess-var-side-to-move (var &optional index)
  "Return the color whose move it is in VAR at INDEX (or at the last position
of the variation if INDEX is nil)."
  (assert var)
  (chess-pos-side-to-move (chess-var-pos var index)))

(defun chess-var-ply (var &optional index)
  "Return VAR's INDEXth ply."
  (assert var)
  (if index
      (nth index (chess-var-plies var))
    (car (last (chess-var-plies var)))))

(defun chess-var-add-ply (var ply)
  "Return the position related to VAR's INDEX position."
  (assert var)
  (assert (listp ply))
  (let ((plies (chess-var-plies var)))
    (assert plies)
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
  (assert var)
  (assert (listp ply))
  (let ((current-ply (chess-var-ply var))
	(changes (chess-ply-changes ply))
	(position (chess-ply-pos ply)))
    (if (chess-ply-final-p current-ply)
	(chess-error 'add-to-completed))
    (assert (eq position (chess-ply-pos current-ply)))
    (chess-ply-set-changes current-ply changes)
    (chess-var-add-ply var (chess-ply-create*
			    (chess-ply-next-pos current-ply)))))

(defun chess-var-to-algebraic (var &optional long)
  "Reveal the plies of VAR by converting them to algebraic
notation."
  (mapconcat (lambda (ply)
	       (chess-ply-to-algebraic ply long))
	     (if (chess-ply-final-p (chess-var-ply var))
		 (chess-var-plies var)
	       (reverse (cdr (reverse (chess-var-plies var)))))
	     " "))

(provide 'chess-var)

;;; chess-var.el ends here
