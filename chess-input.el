;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Keyboard entry of algebraic notation, using shortcut notation
;;
;; This scheme was adapted from the way SCID
;; (http://scid.sourceforge.net), by Shane Hudson, behaves.  It's the
;; only way to move your pieces around!
;;

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

(defun chess-input-test-move (move-ply)
  "Return the given MOVE if it matches the user's current input."
  (let* ((move (cdr move-ply))
	 (i 0) (x 0) (l (length move))
	 (xl (length chess-input-move-string))
	 (match t))
    (unless (or (and (equal (downcase chess-input-move-string) "ok")
		     (string-match "\\`O-O[+#]?\\'" move))
		(and (equal (downcase chess-input-move-string) "oq")
		     (string-match "\\`O-O-O[+#]?\\'" move)))
      (while (and (< i l) (< x xl))
	(let ((move-char (aref move i))
	      (entry-char (aref chess-input-move-string x)))
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

(defsubst chess-input-display-moves (&optional move-list)
  (if (> (length chess-input-move-string) 0)
      (message "[%s] %s" chess-input-move-string
	       (mapconcat 'cdr
			  (or move-list
			      (delq nil (mapcar 'chess-input-test-move
						(cdr chess-input-moves))))
			  " "))))

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
		    (char-to-string last-command-char))))
    (unless (and chess-input-moves
		 (eq position chess-input-moves-pos)
		 (or (> (length chess-input-move-string) 1)
		     (eq (car chess-input-moves) last-command-char)))
      (setq char (if (eq (downcase last-command-char) ?o) ?k
		   last-command-char))
      (if (or (memq (upcase char) '(?K ?Q ?N ?B ?R ?P))
	      (and (>= char ?a) (<= char ?h)))
	  (setq chess-input-moves-pos position
		chess-input-moves
		(cons
		 char
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
		     (string-lessp (cdr left) (cdr right))))))))))
  (let ((moves (delq nil (mapcar 'chess-input-test-move
				 (cdr chess-input-moves)))))
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
      (setq chess-input-move-string nil
	    chess-input-moves nil
	    chess-input-moves-pos nil))
     ((null moves)
      (chess-input-shortcut-delete))
     (t
      (chess-input-display-moves moves)))))

(provide 'chess-input)

;;; chess-input.el ends here
