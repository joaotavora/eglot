;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manipulate variations (which are really just lists of plies)
;;

;;; Commentary:

;; A chess variations is a simple list of plies.  This module provides
;; an abstraction layer for applications.

(require 'chess-ply)

(defsubst chess-var-plies (var)
  "Return the tags alist associated with VAR."
  var)

(defsubst chess-var-pos (var &optional index)
  "Return the position related to VAR's INDEX position."
  (chess-ply-pos (chess-var-ply var index)))

(defsubst chess-var-index (var)
  "Return the VAR's current position index."
  (1- (length (chess-var-plies var))))

(defsubst chess-var-seq (var)
  "Return the current VAR sequence."
  (1+ (/ (chess-var-index var) 2)))

(defsubst chess-var-side-to-move (var)
  (chess-pos-side-to-move (chess-var-pos var)))

(defun chess-var-ply (var &optional index)
  "Return the position related to VAR's INDEX position."
  (if index
      (nth index (chess-var-plies var))
    (car (last (chess-var-plies var)))))

(defun chess-var-add-ply (var ply)
  "Return the position related to VAR's INDEX position."
  (let ((plies (chess-var-plies var)))
    (assert plies)
    (nconc plies (list ply))))

(defsubst chess-var-create (&optional position)
  "Create a new chess variation object.
Optionally use the given starting POSITION.
SEARCH-FUNC specifies the function used to test the legality of moves.
TAGS is the starting set of var tags (which can always be changed
later using the various tag-related methods)."
  (list (chess-ply-create* (or position chess-starting-position))))

(defun chess-var-move (var ply)
  "Make a move in the current VAR, from FROM to TO.
This creates a new position and adds it to the main variation.
The 'changes' of the last ply reflect whether the var is currently in
progress (nil), if it is drawn, resigned, mate, etc."
  (let ((current-ply (chess-var-ply var))
	(changes (chess-ply-changes ply))
	(position (chess-ply-pos ply)))
    (if (chess-ply-final-p current-ply)
	(chess-error 'add-to-completed))
    (assert (eq position (chess-ply-pos current-ply)))
    (chess-ply-set-changes current-ply changes)
    (chess-var-add-ply var (chess-ply-create*
			    (chess-ply-next-pos current-ply)))))

(provide 'chess-var)

;;; chess-var.el ends here
