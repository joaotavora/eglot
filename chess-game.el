;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maintain a chess game that is being played or viewed
;;
;; $Revision$

;;; Commentary:

;; A chess game is represented by a set of tags that describe the
;; game, and a list of plies representing the main variation.

(require 'chess-ply)

(defconst chess-game-default-tags
  (list '("Event" . "Computer chess game")
	'("Round" . "-")
	(cons "Site" (system-name))
	'("White" . "?")
	'("Black" . "?")
	'("Result" . "*")
	'("TimeControl" . "-")))

(defsubst chess-game-tags (game)
  "Return the tags alist associated with GAME."
  (car game))

(defsubst chess-game-set-tags (game tags)
  "Return the tags alist associated with GAME."
  (setcar game tags))

(defsubst chess-game-plies (game)
  "Return the tags alist associated with GAME."
  (cdr game))

(defsubst chess-game-set-plies (game plies)
  "Return the tags alist associated with GAME."
  (setcdr game plies))

(defsubst chess-game-tag (game tag)
  "Return the value for TAG in GAME."
  (let ((tags (chess-game-tags game)))
    (and tags (cdr (assoc tag tags)))))

(defun chess-game-set-tag (game tag value)
  "Set a TAG for GAME to VALUE."
  (let ((tags (chess-game-tags game)))
    (if (null tags)
	(setcar game (list (cons tag value)))
      (let ((entry (assoc tag tags)))
	(if entry
	    (setcdr entry value)
	  (nconc (car game)
		 (list (cons tag value))))))))

(defun chess-game-del-tag (game tag)
  "Set a TAG for GAME to VALUE."
  (setcar game (assq-delete-all tag (chess-game-tags game))))

(defsubst chess-game-index (game)
  "Return the GAME's current position index."
  (length (cdr game)))

(defsubst chess-game-seq (game)
  "Return the current GAME sequence."
  (1+ (/ (chess-game-index game) 2)))

(defsubst chess-game-side-to-move (game)
  (chess-pos-side-to-move (chess-game-pos game)))

(defun chess-game-ply (game &optional index)
  "Return the position related to GAME's INDEX position."
  (if index
      (nth index (cdr game))
    (car (last (cdr game)))))

(defun chess-game-pos (game &optional index)
  "Return the position related to GAME's INDEX position."
  (car (chess-game-ply game index)))

(defun chess-game-create (&optional tags position)
  "Create a new chess game object.
If TAGS is non-nil, it is a list of cons cell that define starting
tags to use.  If POSITION is non-nil, the game starts at that
position."
  (let ((game (cons nil nil)))
    (dolist (tag chess-game-default-tags)
      (chess-game-set-tag game (car tag) (cdr tag)))
    (chess-game-set-tag game "Date" (format-time-string "%Y.%m.%d"))
    (dolist (tag tags)
      (chess-game-set-tag game (car tag) (cdr tag)))
    (setcdr game (list (chess-ply-create
			(or position (chess-pos-create)))))
    game))

(defun chess-game-move (game ply)
  "Make a move in the current GAME, from FROM to TO.
This creates a new position and adds it to the main variation.
The 'changes' of the last ply reflect whether the game is currently in
progress (nil), if it is drawn, resigned, mate, etc."
  (let ((current-ply (chess-game-ply game))
	(changes (chess-ply-changes ply)))
    (assert (equal (chess-ply-pos current-ply) (chess-ply-pos ply)))
    (chess-ply-set-changes current-ply changes)
    (cond
     ((or (memq ':draw changes)
	  (memq ':perpetual changes)
	  (memq ':repetition changes)
	  (memq ':stalemate changes))
      (chess-game-set-tag game "Result" "1/2-1/2"))
     ((or (memq ':resign changes)
	  (memq ':checkmate changes))
      (chess-game-set-tag game "Result"
			  (if (chess-pos-side-to-move (chess-game-pos game))
			      "0-1" "1-0")))
     (t (nconc (cdr game)
	       (list (chess-ply-create
		      (chess-ply-next-pos current-ply))))))))

(provide 'chess-game)

;;; chess-game.el ends here
