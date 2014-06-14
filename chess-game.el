;;; chess-game.el --- Maintain a chess game that is being played or viewed

;; Copyright (C) 2002, 2004, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
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

;;; Commentary:

;; A chess game is represented by a set of tags that describe the
;; game, and a list of plies representing the main variation.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'chess-ply)

(defvar chess-game-inhibit-events nil)

(defconst chess-game-default-tags
  `(("Event"	   . "Computer chess game")
    ("Round"	   . "-")
    ("Site"	   . ,(system-name))
    ("White"	   . "?")
    ("Black"	   . "?")
    ("Result"	   . "*")
    ("TimeControl" . "-")))

(defsubst chess-game-hooks (game)
  "Return the event hooks associated with GAME."
  (cl-assert game)
  (car game))

(defsubst chess-game-set-hooks (game hooks)
  "Set the event hooks associated with GAME."
  (cl-assert game)
  (cl-assert (or hooks (eq hooks nil)))
  (setcar game hooks))

(defun chess-game-add-hook (game function &optional data prepend)
  "Add to GAME an event hook FUNCTION."
  (cl-assert game)
  (cl-assert function)
  (let ((hooks (chess-game-hooks game)))
    (if (null hooks)
	(chess-game-set-hooks game (list (cons function data)))
      (if prepend
	  (chess-game-set-hooks game (cons (cons function data) hooks))
	(nconc hooks (list (cons function data)))))))

(defun chess-game-remove-hook (game function &optional data)
  "Remove from GAME all event hooks that match FUNCTION.
If DATA is specified, only remove those hooks whose associated data
matches."
  (cl-assert game)
  (cl-assert function)
  (let* ((hooks (chess-game-hooks game))
	 (h hooks) last-hook)
    (while h
      (if (and (eq (caar h) function)
	       (or (null data)
		   (eq data (cdar h))))
	  (if last-hook
	      (setcdr last-hook (cdr h))
	    (setq hooks (cdr h)))
	(setq last-hook h))
      (setq h (cdr h)))
    (chess-game-set-hooks game hooks)))

(defsubst chess-game-run-hooks (game &rest args)
  "Run the event hooks of GAME and pass ARGS."
  (cl-assert game)
  (unless chess-game-inhibit-events
    (let (result)
      (dolist (hook (chess-game-hooks game) result)
	(setq result (apply (car hook) game (cdr hook) args))))))

(defsubst chess-game-tags (game)
  "Return the tags alist associated with GAME."
  (cl-assert game)
  (cadr game))

(defsubst chess-game-set-tags (game tags)
  "Set the tags alist associated with GAME.
After the TAGS alist was set the 'set-tags event is triggered."
  (cl-assert game)
  (cl-assert (or tags (eq tags nil)))
  (setcar (cdr game) tags)
  (chess-game-run-hooks game 'set-tags))

(defsubst chess-game-tag (game tag)
  "Return the value for TAG in GAME."
  (cl-assert game)
  (cl-assert tag)
  (let ((tags (chess-game-tags game)))
    (and tags (cdr (assoc tag tags)))))

(defun chess-game-set-tag (game tag value)
  "Set TAG for GAME to VALUE."
  (cl-assert game)
  (cl-assert tag)
  (cl-assert value)
  (let ((tags (chess-game-tags game)))
    (if (null tags)
	(chess-game-set-tags game (list (cons tag value)))
      (let ((entry (assoc tag tags)))
	(if entry
	    (setcdr entry value)
	  (nconc tags (list (cons tag value)))))))
  (chess-game-run-hooks game 'set-tag tag))

(defsubst chess-game-del-tag (game tag)
  "Delete TAG from GAME."
  (cl-assert game)
  (cl-assert tag)
  (chess-game-set-tags game (assq-delete-all tag (chess-game-tags game)))
  (chess-game-run-hooks game 'delete-tag tag))


(defsubst chess-game-data-alist (game)
  "Return the data alist associated with GAME."
  (cl-assert game)
  (nth 2 game))

(defsubst chess-game-set-data-alist (game value)
  "Set the data alist associated with GAME."
  (cl-assert game)
  (setcar (nthcdr 2 game) value))

(defun chess-game-set-data (game key value)
  "Set GAME data KEY to VALUE."
  (cl-assert game)
  (cl-check-type key symbol)
  (let* ((alist (chess-game-data-alist game))
	 (cell (assq key alist)))
    (if cell
	(setcdr cell value)
      (if (null alist)
	  (setcar (nthcdr 2 game) (list (cons key value)))
	(push (cons key value) alist)
	(setcar (nthcdr 2 game) alist)))
    (chess-game-run-hooks game 'set-data key)
    value))

(defun chess-game-data (game key)
  "Return the value of GAME data KEY."
  (cl-assert game)
  (cl-check-type key symbol)
  (let ((alist (chess-game-data-alist game)))
    (if alist
	(cdr (assq key alist)))))

(defun chess-game-del-data (game key)
  "Delete KEY from GAME's data alist."
  (cl-assert game)
  (cl-check-type key symbol)
  (let ((alist (chess-game-data-alist game)))
    (if alist
	(assq-delete-all key alist))))


(defsubst chess-game-plies (game)
  "Return the main variation of GAME as a list of plies."
  (cl-assert game)
  (nth 3 game))

(defalias 'chess-game-main-var 'chess-game-plies)

(defsubst chess-game-set-plies (game plies)
  "Set the list of plies which represents the main variation of GAME."
  (cl-assert game)
  (setcdr (nthcdr 2 game) (if plies (list plies) nil))
  (chess-game-run-hooks game 'setup-game game))

(defsubst chess-game-set-start-position (game position)
  "Set the initial POSITION of GAME."
  (cl-assert game)
  (cl-assert (vectorp position))
  (chess-game-set-plies game (list (chess-ply-create* position))))

(defsubst chess-game-pos (game &optional index)
  "Return the current position of GAME or a position of a given INDEX."
  (cl-assert game)
  (chess-ply-pos (chess-game-ply game index)))

(defun chess-game-status (game &optional index)
  "Return a symbol, such as :checkmate, :resign, etc.
This conveys the status of the game at the given INDEX."
  (cl-assert game)
  (or (chess-pos-status (chess-game-pos game index))
      (chess-ply-final-p (chess-game-ply game index))))

(defsubst chess-game-index (game)
  "Return the GAME's current position index."
  (cl-assert game)
  (1- (length (chess-game-plies game))))

(defsubst chess-game-seq (game)
  "Return the current GAME sequence."
  (cl-assert game)
  (/ (+ 2 (chess-game-index game)) 2))

(defsubst chess-game-side-to-move (game &optional index)
  "Return the color whose move it is in GAME at INDEX (or at the last position
if INDEX is nil)."
  (cl-assert game)
  (chess-pos-side-to-move (chess-game-pos game index)))

(defun chess-game-ply (game &optional index)
  "Return a ply of GAME.
If INDEX is non-nil, the last played ply is returned."
  (cl-assert game)
  (if index
      (nth index (chess-game-plies game))
    (car (last (chess-game-plies game)))))

(defun chess-game-add-ply (game ply)
  "Add PLY to the main variation of GAME."
  (cl-assert game)
  (cl-check-type ply listp)
  (let ((plies (chess-game-plies game)))
    (if plies
	(nconc plies (list ply))
      (let ((chess-game-inhibit-events t))
	(chess-game-set-plies game (list ply))))))

(chess-message-catalog 'english
  '((undo-limit-reached . "Cannot undo further")
    (add-to-completed	. "Cannot add moves to a completed game")))

(defun chess-game-undo (game count)
  "Undo the last COUNT plies of GAME."
  (cl-assert game)
  (cl-check-type count (integer 0))
  (if (> count (chess-game-index game))
      (chess-error 'undo-limit-reached))
  (let ((chess-game-inhibit-events t))
    (chess-game-set-plies game (nbutlast (chess-game-plies game) count)))
  (chess-game-run-hooks game 'post-undo count))

(defun chess-game-strip-annotations (game)
  "Strip all annotations from the given GAME."
  (cl-assert game)
  (dotimes (i (chess-game-index game))
    (let ((position (chess-game-pos game i)))
      (chess-pos-set-annotations position nil))))

(defsubst chess-game-over-p (game)
  "Return non-nil if GAME is at a final positionn."
  (cl-assert game)
  (let ((last-ply (car (last (nth 3 game) 2))))
    (and last-ply (chess-ply-final-p last-ply))))


(defsubst chess-game-copy-game (game new-game)
  (cl-assert game)
  (cl-assert new-game)
  (chess-game-set-tags game (chess-game-tags new-game))
  (chess-game-set-plies game (chess-game-plies new-game)))

(defun chess-game-create (&optional position tags)
  "Create a new chess game object.
Optionally use the given starting POSITION (see also
`chess-game-set-start-position').
TAGS is the starting set of game tags (which can always be changed
later using the various tag-related methods)."
  (let ((game (list nil tags nil
		    (list (chess-ply-create* (or position
						 chess-starting-position))))))
    (dolist (tag (cons (cons "Date" (format-time-string "%Y.%m.%d"))
		       chess-game-default-tags))
      (unless (chess-game-tag game (car tag))
	(chess-game-set-tag game (car tag) (cdr tag))))
    game))

(defun chess-game-move (game ply)
  "Make a move in the current GAME using PLY.
This creates a new position and adds it to the main variation.
The 'changes' of the last ply reflect whether the game is currently in
progress (nil), if it is drawn, resigned, mate, etc."
  (cl-assert game)
  (cl-assert (listp ply))
  (let ((current-ply (chess-game-ply game))
	(position (chess-ply-pos ply))
	(changes (chess-ply-changes ply)))

    (cl-assert current-ply)
    (cl-assert (and position (eq position (chess-ply-pos current-ply))))
    (cl-assert changes)

    (if (chess-ply-final-p current-ply)
	(chess-error 'add-to-completed))

    (chess-ply-set-changes current-ply changes)
    (unless (chess-ply-any-keyword ply :drawn :perpetual :repetition
				   :resign :aborted :flag-fell)
      (chess-game-add-ply game (chess-ply-create*
				(chess-ply-next-pos current-ply))))

    (let ((long (> (length changes) 2)))
      (cond
       ((and long (chess-ply-any-keyword ply :resign :checkmate))
	(let ((color (chess-game-side-to-move game)))
	  (if (chess-ply-any-keyword ply :resign :flag-fell)
	      (chess-game-set-tag game "Result" (if color "0-1" "1-0"))
	    (chess-game-set-tag game "Result" (if color "1-0" "0-1")))))
       ((and long (chess-ply-any-keyword ply :drawn :perpetual :repetition
					 :stalemate))
	(chess-game-set-tag game "Result" "1/2-1/2"))))

    (if (chess-ply-keyword ply :resign)
	(chess-game-run-hooks game 'resign)
      (chess-game-run-hooks game 'move current-ply))))

(defsubst chess-game-end (game keyword)
  "End GAME, by resignation, draw, etc."
  (chess-game-move game (list (chess-game-pos game) keyword)))

(provide 'chess-game)

;;; chess-game.el ends here
