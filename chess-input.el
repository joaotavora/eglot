;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyboard entry of algebraic notation, using shortcut notation
;;
;; This scheme was adapted from the way SCID
;; (http://scid.sourceforge.net), by Shane Hudson, behaves.  It's the
;; only way to move your pieces around!
;;

(defvar chess-move-string "")
(defvar chess-legal-moves-pos nil)
(defvar chess-legal-moves nil)
(defvar chess-input-move-function nil)

(make-variable-buffer-local 'chess-move-string)
(make-variable-buffer-local 'chess-legal-moves-pos)
(make-variable-buffer-local 'chess-legal-moves)
(make-variable-buffer-local 'chess-input-move-function)

(chess-message-catalog 'english
  '((not-your-move . "It is not your turn to move")
    (game-is-over  . "This game is over")))

(defun chess-keyboard-test-move (move-ply)
  "Return the given MOVE if it matches the user's current input."
  (let* ((move (cdr move-ply))
	 (i 0) (x 0) (l (length move))
	 (xl (length chess-move-string))
	 (match t))
    (unless (or (and (equal (downcase chess-move-string) "ok")
		     (string-match "\\`O-O[+#]?\\'" move))
		(and (equal (downcase chess-move-string) "oq")
		     (string-match "\\`O-O-O[+#]?\\'" move)))
      (while (and (< i l) (< x xl))
	(let ((move-char (aref move i))
	      (entry-char (aref chess-move-string x)))
	  (if (and (= move-char ?x)
		   (/= entry-char ?x))
	      (setq i (1+ i))
	    (if (/= entry-char (if (< entry-char ?a)
				   move-char
				 (downcase move-char)))
		(setq match nil i l)
	      (setq i (1+ i) x (1+ x)))))))
    (if match
	move-ply)))

(defsubst chess-keyboard-display-moves (&optional move-list)
  (if (> (length chess-move-string) 0)
      (message "[%s] %s" chess-move-string
	       (mapconcat 'cdr
			  (or move-list
			      (delq nil (mapcar 'chess-keyboard-test-move
						(cdr chess-legal-moves))))
			  " "))))

(defun chess-keyboard-shortcut-delete ()
  (interactive)
  (when (and chess-move-string
	     (stringp chess-move-string)
	     (> (length chess-move-string) 0))
    (setq chess-move-string
	  (substring chess-move-string 0 (1- (length chess-move-string))))
    (chess-keyboard-display-moves)))

(defun chess-keyboard-shortcut (&optional display-only)
  (interactive)
  (let* ((position (chess-display-position nil))
	 (color (chess-pos-side-to-move position))
	 char)
    (chess-display-assert-can-move position)
    (unless (memq last-command '(chess-keyboard-shortcut
				 chess-keyboard-shortcut-delete))
      (setq chess-move-string nil))
    (unless display-only
      (setq chess-move-string
	    (concat chess-move-string (char-to-string last-command-char))))
    (unless (and chess-legal-moves
		 (eq position chess-legal-moves-pos)
		 (or (> (length chess-move-string) 1)
		     (eq (car chess-legal-moves) last-command-char)))
      (setq char (if (eq (downcase last-command-char) ?o) ?k
		   last-command-char)
	    chess-legal-moves-pos position
	    chess-legal-moves
	    (cons char
		  (sort
		   (mapcar
		    (function
		     (lambda (ply)
		       (cons ply (chess-ply-to-algebraic ply))))
		    (if (eq char ?b)
			(append (chess-legal-plies
				 position :piece (if color ?P ?p) :file 1)
				(chess-legal-plies
				 position :piece (if color ?B ?b)))
		      (if (and (>= char ?a)
			       (<= char ?h))
			  (chess-legal-plies position
					     :piece (if color ?P ?p)
					     :file (- char ?a))
			(chess-legal-plies position
					   :piece (if color
						      (upcase char)
						    (downcase char))))))
		   (function
		    (lambda (left right)
		      (string-lessp (cdr left) (cdr right)))))))))
  (let ((moves (delq nil (mapcar 'chess-keyboard-test-move
				 (cdr chess-legal-moves)))))
    (cond
     ((or (= (length moves) 1)
	  ;; if there is an exact match except for case, it must be an
	  ;; abiguity between a bishop and a b-pawn move.  In this
	  ;; case, always take the b-pawn move; to select the bishop
	  ;; move, use B to begin the keyboard shortcut
	  (and (= (length moves) 2)
	       (string= (downcase (cdr (car moves)))
			(downcase (cdr (cadr moves))))
	       (setq moves (cdr moves))))
      (funcall chess-input-move-function nil (caar moves))
      (setq chess-move-string nil
	    chess-legal-moves nil
	    chess-legal-moves-pos nil))
     ((null moves)
      (chess-keyboard-shortcut-delete))
     (t
      (chess-keyboard-display-moves moves)))))

(provide 'chess-input)

;;; chess-input.el ends here
