;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maintain a chess game that is being played or viewed
;;
;; $Revision$

;;; Commentary:

;; A chess game is represented by a set of tags that describe the
;; game, and a list of plies representing the main variation.

(require 'chess-ply)
(require 'chess-algebraic)

(defconst chess-game-default-tags
  `(("Event"	   . "Computer chess game")
    ("Round"	   . "-")
    ("Site"	   . ,(system-name))
    ("White"	   . "?")
    ("Black"	   . "?")
    ("Result"	   . "*")
    ("TimeControl" . "-")))

(defsubst chess-game-tags (game)
  "Return the tags alist associated with GAME."
  (car game))

(defsubst chess-game-set-tags (game tags)
  "Return the tags alist associated with GAME."
  (setcar game tags))

(defsubst chess-game-plies (game)
  "Return the tags alist associated with GAME."
  (cddr game))

(defsubst chess-game-set-plies (game plies)
  "Return the tags alist associated with GAME."
  (setcdr (cdr game) plies))

(defsubst chess-game-validation-func (game)
  "Return the tags alist associated with GAME."
  (car (cadr game)))

(defsubst chess-game-set-validation-func (game func)
  "Return the tags alist associated with GAME."
  (setcar (cadr game) func))

(defsubst chess-game-search-func (game)
  "Return the tags alist associated with GAME."
  (cdr (cadr game)))

(defsubst chess-game-set-search-func (game func)
  "Return the tags alist associated with GAME."
  (setcdr (cadr game) func))

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
  (length (chess-game-plies game)))

(defsubst chess-game-seq (game)
  "Return the current GAME sequence."
  (1+ (/ (chess-game-index game) 2)))

(defsubst chess-game-side-to-move (game)
  (chess-pos-side-to-move (chess-game-pos game)))

(defun chess-game-ply (game &optional index)
  "Return the position related to GAME's INDEX position."
  (if index
      (nth index (chess-game-plies game))
    (car (last (chess-game-plies game)))))

(defsubst chess-game-add-ply (game ply)
  "Return the position related to GAME's INDEX position."
  (nconc (chess-game-plies game) (list ply)))

(defun chess-game-pos (game &optional index)
  "Return the position related to GAME's INDEX position."
  (car (chess-game-ply game index)))

(defun chess-game-create (&rest keywords)
  "Create a new chess game object.
Keywords may be specified to customize the game object.  The supported
keywords are:

  :position POS          ; set the start position
  :search   FUNC         ; function used to search chess positions
  :validate FUNC         ; function used to validate chess moves
  :tags     ALIST"
  (let ((game (list (cdr (assq ':tags keywords))
		    (cons (or (cdr (assq ':validate keywords))
			      chess-standard-validate)
			  (or (cdr (assq ':search keywords))
			      chess-standard-search)))))
    (dolist (tag (cons (cons "Date" (format-time-string "%Y.%m.%d"))
		       chess-game-default-tags))
      (unless (chess-game-tag game (car tag))
	(chess-game-set-tag game (car tag) (cdr tag))))
    (chess-game-add-ply game (chess-ply-create
			      (or (cdr (assq ':position keywords))
				  (chess-pos-create))))
    game))

(defun chess-game-move (game ply)
  "Make a move in the current GAME, from FROM to TO.
This creates a new position and adds it to the main variation.
The 'changes' of the last ply reflect whether the game is currently in
progress (nil), if it is drawn, resigned, mate, etc."
  (let ((current-ply (chess-game-ply game))
	(changes (chess-ply-changes ply)))
    (unless (equal (chess-ply-pos current-ply)
		   (chess-ply-pos ply))
      (error "Positions do not match"))
    (funcall (chess-game-validation-func game) ply)
    (chess-ply-set-changes current-ply changes)
    (cond
     ((or (memq ':draw changes)
	  (memq ':perpetual changes)
	  (memq ':repetition changes)
	  (memq ':stalemate changes))
      (chess-game-set-tag game "Result" "1/2-1/2"))
     ((or (memq ':resign changes)
	  (memq ':checkmate changes))
      (chess-game-set-tag game "Result" (if (chess-game-side-to-move game)
					    "0-1" "1-0")))
     (t
      (chess-game-add-ply game (chess-ply-create
				(chess-ply-next-pos current-ply)))))))

;; A few convenience functions

(defsubst chess-game-legal-plies (game)
  "Return all legal plies from GAME's current position."
  (chess-legal-plies (chess-game-pos game)
		     (chess-game-search-func game)))

(defsubst chess-game-algebraic-to-ply (game move)
  (chess-algebraic-to-ply (chess-game-pos game) move
			  (chess-game-search-func game)))

(defsubst chess-game-ply-to-algebraic (game &optional ply long)
  (chess-ply-to-algebraic (or ply (chess-game-ply game)) long
			  (chess-game-search-func game)))

(provide 'chess-game)

;;; chess-game.el ends here
