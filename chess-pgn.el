;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Convert a chess game to/from PGN notation
;;
;; $Revision$

(require 'chess-game)
(require 'chess-pos)
(require 'chess-algebraic)
(require 'chess-fen)

(defvar chess-pgn-fill-column 60)

(chess-message-catalog 'english
  '((pgn-read-error  . "Error reading move: %s")
    (pgn-parse-error . "Error parsing PGN syntax")))

(defun chess-pgn-read-plies (game position &optional top-level)
  (let ((plies (list t)) prevpos done)
    (catch 'done
      (while (not (or done (eobp)))
	(cond
	 ((looking-at "[1-9][0-9]*\\.[. ]*")
	  (goto-char (match-end 0)))

	 ((looking-at chess-algebraic-regexp)
	  (goto-char (match-end 0))
	  (setq prevpos position)
	  (let* ((move (match-string 0))
		 (ply (chess-algebraic-to-ply position (match-string 0))))
	    (unless ply
	      (chess-error 'pgn-read-error move))
	    (setq position (chess-ply-next-pos ply))
	    (nconc plies (list ply))))

	 ((and top-level
	       (looking-at "\\(\\*\\|1-0\\|0-1\\|1/2-1/2\\)"))
	  (goto-char (match-end 0))
	  (chess-game-set-tag game "Result" (match-string-no-properties 0))
	  (nconc plies (list (chess-ply-create
			      (chess-ply-next-pos (car (last plies))))))
	  (setq done t))

	 ((looking-at "{")
	  (forward-char)
	  (let ((begin (point)))
	    (search-forward "}")
	    (forward-char)
	    (chess-pos-add-annotation prevpos (buffer-substring-no-properties
					       begin (- (point) 2)))))
	 ((looking-at "(")
	  (forward-char)
	  (skip-chars-forward " \t\n")
	  (chess-pos-add-annotation
	   prevpos (chess-pgn-read-plies game (chess-pos-copy prevpos))))

	 ((and (not top-level)
	       (looking-at ")"))
	  (forward-char)
	  (setq done t))

	 (t
	  (nconc plies (list (chess-ply-create
			      (chess-ply-next-pos (car (last plies))))))
	  (throw 'done t)))
	(skip-chars-forward " \t\n")))
    (cdr plies)))

(defun chess-pgn-to-game (&optional string)
  "Convert PGN notation at point into a chess game."
  (if string
      (with-temp-buffer
	(insert string)
	(chess-pgn-parse))
    (chess-pgn-parse)))

(defun chess-pgn-parse ()
  (when (or (looking-at "\\[")
	    (and (search-forward "[" nil t)
		 (goto-char (match-beginning 0))))
    (let ((game (chess-game-create)))
      (chess-game-set-tags game nil)
      (while (looking-at "\\[\\(\\S-+\\)\\s-+\\(\".+?\"\\)\\][ \t\n]+")
	(chess-game-set-tag game (match-string-no-properties 1)
			    (read (match-string-no-properties 2)))
	(goto-char (match-end 0)))
      (let ((fen (chess-game-tag game "FEN")))
	(chess-game-set-plies
	 game (or (chess-pgn-read-plies
		   game (if fen
			    (chess-fen-to-pos fen)
			  (chess-pos-copy chess-starting-position)) t)
		  ;; set the starting position to the FEN string
		  (list (chess-ply-create (if fen
					      (chess-fen-to-pos fen)
					    chess-starting-position))))))
      game)))

(defun chess-pgn-insert-annotations (game index ply)
  (dolist (ann (chess-pos-annotations (chess-ply-pos ply)))
    (if (stringp ann)
	(insert "\n{" ann "}")
      (assert (listp ann))
      (chess-pgn-insert-plies game index ann))))

(defun chess-pgn-insert-plies (game index plies &optional
				     for-black indented no-annotations)
  "NYI: Still have to implement INDENTED argument."
  (while plies
    (unless for-black
      (when (chess-ply-changes (car plies))
	(if (> (current-column) chess-pgn-fill-column)
	    (insert ?\n))
	(insert (format "%d. %s" index (chess-ply-to-algebraic (car plies))))
	(unless no-annotations
	  (chess-pgn-insert-annotations game index (car plies))))
      (setq plies (cdr plies) index (1+ index)))
    (when plies
      (when (chess-ply-changes (car plies))
	(when for-black
	  (if (> (current-column) chess-pgn-fill-column)
	      (insert ?\n))
	  (insert (format "%d. ..." index))
	  (setq for-black nil))
	(insert (format " %s" (chess-ply-to-algebraic (car plies))))
	(unless no-annotations
	  (chess-pgn-insert-annotations game index (car plies))))
      (setq plies (cdr plies)))
    (if plies
	(insert ? ))))

(defvar chess-pgn-tag-order
  '("Event" "Site" "Date" "Round"
    "White" "WhiteElo" "Black" "BlackElo"
    "Result" "TimeControl"))

(defun chess-game-to-pgn (game &optional indented to-string)
  "Convert a chess GAME to PGN notation.
If INDENTED is non-nil, indent the move texts."
  (if to-string
      (with-temp-buffer
	(chess-insert-pgn game indented)
	(buffer-string))
    (chess-insert-pgn game indented)))

(defun chess-insert-pgn (game &optional indented)
  (let ((fen (chess-game-tag game "FEN"))
	(first-pos (chess-ply-pos (chess-game-ply game 0))))
    (when (and fen (not (equal fen (chess-pos-to-fen first-pos))))
      (chess-game-del-tag game "FEN")
      (setq fen nil))
    (if (and (not fen)
	     (not (equal chess-starting-position first-pos)))
	(chess-game-set-tag game "FEN" (chess-pos-to-fen first-pos))))
  (dolist (tag (sort (copy-alist (chess-game-tags game))
		     (function
		      (lambda (left right)
			(setq left (car left) right (car right))
			(let ((l-idx (position left chess-pgn-tag-order
					       :test 'equal))
			      (r-idx (position right chess-pgn-tag-order
					       :test 'equal)))
			  (cond
			   ((and l-idx (not r-idx)) t)
			   ((and (not l-idx) r-idx) nil)
			   ((and l-idx r-idx) (< l-idx r-idx))
			   (t (string-lessp left right))))))))
    (insert (format "[%s \"%s\"]\n" (car tag) (cdr tag))))
  (insert ?\n)
  (let ((begin (point)))
    (chess-pgn-insert-plies game 1 (chess-game-plies game))
    (insert (or (chess-game-tag game "Result") "*") ?\n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chess-pgn-mode for editing and browsing PGN files.
;;

(require 'chess-database)
(require 'chess-file)

(defvar chess-pgn-database)
(defvar chess-pgn-display)
(defvar chess-pgn-current-game)
(defvar chess-pgn-current-index)

(make-variable-buffer-local 'chess-pgn-database)
(make-variable-buffer-local 'chess-pgn-display)
(make-variable-buffer-local 'chess-pgn-current-game)
(make-variable-buffer-local 'chess-pgn-current-index)

;;;###autoload
(define-derived-mode chess-pgn-mode text-mode "PGN"
  "A mode for editing chess PGN files."
  (setq comment-start "{"
	comment-end "}")

  (modify-syntax-entry ?\{ "<")
  (modify-syntax-entry ?\} ">")
  (modify-syntax-entry ?\" "\"")

  (if (fboundp 'font-lock-mode)
      (font-lock-mode 1))

  (let ((map (current-local-map)))
    (define-key map [??] 'describe-mode)
    (define-key map [?T] 'text-mode)
    (define-key map [return] 'chess-pgn-show-position)
    (define-key map [mouse-1] 'chess-pgn-mouse-show-position)
    (define-key map [(control ?m)] 'chess-pgn-move)

    (when (require 'pcomplete nil t)
      (set (make-variable-buffer-local 'pcomplete-default-completion-function)
	   'chess-pgn-completions)
      (set (make-variable-buffer-local 'pcomplete-command-completion-function)
	   'chess-pgn-completions)
      (set (make-variable-buffer-local 'pcomplete-parse-arguments-function)
	   'chess-pgn-current-word)
      (define-key map [tab] 'chess-pgn-complete-move))))

(defalias 'pgn-mode 'chess-pgn-mode)

(defvar chess-pgn-bold-face 'bold)

(defconst chess-pgn-move-regexp
  (concat "[^0-9]\\(\\([1-9][0-9]*\\)\\.\\s-+"
	  "\\(\\.\\.\\.\\|" chess-algebraic-regexp "\\)"
	  "\\(\\s-+\\(" chess-algebraic-regexp "\\)\\)?\\)"))

(font-lock-add-keywords 'chess-pgn-mode
  (list (list "\\[\\(\\S-+\\)\\s-+\".*\"\\]" 1 'font-lock-keyword-face)
	(cons chess-pgn-move-regexp
	      'chess-pgn-bold-face)
	(cons "\\(1-0\\|0-1\\|\\*\\)$" 'font-lock-warning-face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pgn\\'" . chess-pgn-mode))

(eval-after-load "mm-decode"
  '(unless (fboundp 'mm-display-pgn-inline)
     (defun mm-display-pgn-inline (handle)
       (mm-display-inline-fontify handle 'chess-pgn-mode))
     (push '("application/x-chess-pgn" mm-display-pgn-inline identity)
	   mm-inline-media-tests)
     (push "application/x-chess-pgn" mm-inlined-types)
     (push "application/x-chess-pgn" mm-automatic-display)))

(defun chess-pgn-completions ()
  "Return a list of possible completions for the current move."
  (let ((position (chess-game-pos chess-pgn-current-game
				  chess-pgn-current-index)))
    (while (pcomplete-here
	    (mapcar 'chess-ply-to-algebraic
		    (chess-legal-plies position :color
				       (chess-pos-side-to-move position)))))))

(defun chess-pgn-current-word ()
  (let ((here (point)))
    (if (setq chess-pgn-current-index (chess-pgn-index))
	(save-restriction
	  (narrow-to-region (match-beginning 3) here)
	  (pcomplete-parse-buffer-arguments)))))

(defun chess-pgn-complete-move ()
  (interactive)
  (save-restriction
    (narrow-to-region (point-min) (point))
    (chess-pgn-read-game))
  (if (eq last-command 'chess-pgn-complete-move)
      (setq last-command 'pcomplete))
  (call-interactively 'pcomplete))

(defun chess-pgn-index ()
  "Return the move index associated with point."
  (save-excursion
    (if (re-search-backward chess-pgn-move-regexp nil t)
	(let* ((index (string-to-int (match-string 2)))
	       (first-move (match-string 3))
	       (second-move (match-string 14))
	       (ply (1+ (* 2 (1- index)))))
	  (if second-move
	      (setq ply (1+ ply)))
	  ply))))

(defun chess-pgn-read-game ()
  ;; load a database to represent this file if not already up
  (unless chess-pgn-database
    (setq chess-pgn-database
	  (chess-database-open 'chess-file buffer-file-name)))

  ;; a hack for speed's sake to read the current game text
  (save-excursion
    (let ((locations chess-file-locations)
	  (here (point))
	  last-location index)
      (while locations
	(if (> (car locations) here)
	    (setq locations nil)
	  (setq last-location locations
		locations (cdr locations))))
      (setq index (if last-location
		      (1- (length last-location))
		    0))
      (when (or (null chess-pgn-current-game)
		(/= index (chess-game-data chess-pgn-current-game
					   'database-index)))
	(setq chess-pgn-current-game
	      (chess-database-read chess-pgn-database index))))))

(defun chess-pgn-create-display ()
  "Return the move index associated with point."
  ;; now find what position we're at in the game
  (save-excursion
    (when chess-pgn-current-game
      (let ((index (chess-pgn-index)))
	(if (or (and (or (null chess-pgn-display)
			 (not (buffer-live-p chess-pgn-display)))
		     (let ((chess-game-inhibit-events t))
		       (setq chess-pgn-display (chess-create-display))))
		(/= (chess-game-data chess-pgn-current-game 'database-index)
		    (chess-game-data (chess-display-game chess-pgn-display)
				     'database-index)))
	    (progn
	      (chess-display-disable-popup chess-pgn-display)
	      (chess-display-set-game chess-pgn-display
				      chess-pgn-current-game index)
	      (chess-game-set-tag (chess-display-game chess-pgn-display)
				  'database-index
				  (chess-game-data chess-pgn-current-game
						   'database-index)))
	  (chess-display-set-index chess-pgn-display index))))))

(defun chess-pgn-show-position ()
  (interactive)
  (chess-pgn-read-game)
  (chess-pgn-create-display))

(defun chess-pgn-mouse-show-position (event)
  (interactive "e")
  (if (fboundp 'event-window)		; XEmacs
      (progn
	(set-buffer (window-buffer (event-window event)))
	(and (event-point event) (goto-char (event-point event))))
    (progn
      (set-buffer (window-buffer (posn-window (event-start event))))
      (goto-char (posn-point (event-start event)))))
  (chess-pgn-show-position))

(defun chess-pgn-move ()
  "Make a move from a PGN buffer."
  (interactive)
  (let ((end (point))
	coords move)
    (save-excursion
      (skip-chars-backward "^ ")
      (setq move (buffer-substring-no-properties (point) end)
	    coords (chess-algebraic-to-ply chess-display-position move))
      ;; it will just get reinserted again
      (delete-region (point) end))))

(defun chess-pgn-insert-move (move &optional color sequence)
  "Insert an algebraic move description into a PGN buffer.
If move is the symbol `wait', it means reflect that we are now waiting
for the opponent to make his move.  If move is the symbol `ready', it
means our opponent is now waiting for us to move our move.  Otherwise,
move should be a string representing the algebraic notation for the
move."
  (while (= (char-before) ?.)
    (delete-backward-char 1))
  (cond
   ((eq move 'wait)
    (insert "..."))
   ((eq move 'ready) t)
   (t
    (if (= (char-syntax (char-before)) ? )
	(insert move))
    (if color
	(move-to-column 11 t)
      (insert ?\n (format "%d.  " (1+ sequence))))))
  (let ((wind (get-buffer-window (current-buffer))))
    (if wind
	(set-window-point wind (point)))))

(provide 'chess-pgn)

;;; chess-pgn.el ends here
