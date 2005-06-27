;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Chessboard display style using graphical images
;;

;;; Commentary:

;; In addition to what all displays offer, the images display adds a
;; few commands:
;;
;;   ^  increase the size of the display (if larger pieces exist)
;;   V  decrease the size of the display (if smaller pieces exist)
;;   D  use pieces from another directory
;;
;; When using pieces from another directory, they will be loaded and
;; displayed immediately, allowing you to easily browse among
;; different piece sets if you have them (such as the ZIICS set, see
;; the xboard man page).

(require 'chess-display)

(defgroup chess-images nil
  "Module for drawing a chess-display using graphical images."
  :group 'chess-display)

(defvar chess-images-cache nil)
(defvar chess-images-size nil)
(defvar chess-images-sizes nil)

(make-variable-buffer-local 'chess-images-cache)
(make-variable-buffer-local 'chess-images-size)
(make-variable-buffer-local 'chess-images-sizes)

(defun chess-images-clear-image-cache (sym value)
  (set sym value)
  (setq chess-images-cache nil))

(defcustom chess-images-separate-frame (display-multi-frame-p)
  "If non-nil, display the chessboard in its own frame."
  :type 'boolean
  :group 'chess-images)

(defcustom chess-images-directory
  (if (file-directory-p "/usr/share/games/xboard/pixmaps")
      "/usr/share/games/xboard/pixmaps"
    (expand-file-name "pieces/xboard"
		      (file-name-directory
		       (or load-file-name buffer-file-name))))
  "Directory containing the chess piece bitmap images.
You are free to use your own chess pieces, of any size.  By default, I
assume you have xboard installed, or can go get the pixmaps that come
with xboard.  I am not an artist, and have not taken the time to draw
my own set of pieces.

If you want to draw your own images, each piece must be named
COLOR-PIECE.EXT, where COLOR is either black or white, and PIECE is
one of rook, knight, bishop, queen, king or pawn.

At the moment only XPM has been tested, and I'm afraid it's probably
the only one that will work.  ImageMagick can be used to convert other
graphics formats into XPM for you.

Each piece must define the background color to use the symbolic color
name \"background\", so that the chess program can use the same pieces
for all squares.  If you want really custom pieces, you can use the
symbolic colors dark_square, light_square and dark_piece and
light_piece."
  :type 'directory
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-default-size nil
  "The default pixel width to use for chess pieces.
If this width is not available, then next smallest will be chosen.
If there is none smaller, then the best size available will be chosen.
If `chess-images-default-size' is nil (the default), then the best
width for the current display is calculated used."
  :type '(choice integer (const :tag "Best fit" nil))
  :group 'chess-images)

(defcustom chess-images-background-image "blank"
  "The name of the file used for background squares.
This file is optional.  If there is no file available by this name, a
solid color square will be created and used.  This option exists so
that specialized squares may be used such as marble tiles, etc."
  :type 'file
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-border-color (cdr (assq 'background-color
						(frame-parameters)))
  "Color to use for the border around pieces."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-dark-color
  (if (display-color-p) "#77a26d" "gray60")
  "Color to use for \"dark\" background squares."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-light-color
  (if (display-color-p) "#c8c365" "gray80")
  "Color to use for \"light\" background squares."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-black-color
  (if (display-color-p) "#202020" "gray0")
  "Color to use for \"black\" pieces."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-white-color
  (if (display-color-p) "#ffffcc" "gray100")
  "Color to use for \"white\" pieces."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-highlight-color
  (if (display-color-p) "#add8e6" "gray90")
  "Color to use for highlighting pieces that have been selected."
  :type 'color
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-extension "xpm"
  "The file extension used for chess display bitmaps."
  :type 'file
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-border-width 2
  "This defines the width of the border that surrounds each piece."
  :type '(choice integer (const :tag "No border" nil))
  :set 'chess-images-clear-image-cache
  :group 'chess-images)

(defcustom chess-images-popup-function 'chess-images-popup
  "The function used to popup a chess-images display.
The current-buffer is set to the display buffer when this function is
called."
  :type 'function
  :group 'chess-images)

;;; Code:

(defconst chess-images-piece-names
  '((?r "rook"   0)
    (?n "knight" 1)
    (?b "bishop" 2)
    (?q "queen"  3)
    (?k "king"   4)
    (?p "pawn"   5))
  "The names and index values of the different pieces.")

(chess-message-catalog 'english
  '((no-images-fallback . "Could not find any suitable or properly sized chess images")))

(defun chess-images-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (when (display-graphic-p)
      (chess-images-initialize)
      (or chess-images-size
	  (ignore
	   (chess-message 'no-images-fallback)))))

   ((eq event 'popup)
    (funcall chess-images-popup-function))

   ((eq event 'draw)
    (apply 'chess-images-draw args))

   ((eq event 'draw-square)
    (apply 'chess-images-draw-square args))

   ((eq event 'highlight)
    (apply 'chess-images-highlight args))

   ((eq event 'start-edit)
    (setq cursor-type t))

   ((eq event 'end-edit)
    (setq cursor-type nil))))

(defun chess-images-determine-size ()
  (let ((display (and (stringp chess-images-separate-frame)
		      chess-images-separate-frame)))
    (setq cursor-type nil
	  chess-images-cache nil
	  chess-images-size (chess-images-best-size
			     (- (if display
				    (x-display-pixel-height display)
				  (display-pixel-height))
				;; On Macs and Windows, account for
				;; the Start/Status bar
				(if (memq window-system '(mac windows w32))
				    80 20))
			     (- (if display
				    (x-display-pixel-width display)
				  (display-pixel-width)) 20)))))

(defun chess-images-initialize ()
  (let ((map (current-local-map)))
    (define-key map [?^] 'chess-images-increase-size)
    (define-key map [?V] 'chess-images-decrease-size)
    (define-key map [?P] 'chess-images-set-directory))
  (chess-images-determine-size))

(chess-message-catalog 'english
  '((no-images . "Cannot find any piece images; check `chess-images-directory'")))

(defun chess-images-popup ()
  (unless chess-images-size
    (chess-error 'no-images))
  (if chess-images-separate-frame
      (let* ((size (float (+ (* (or chess-images-border-width 0) 8)
			     (* chess-images-size 8))))
	     (max-char-height (ceiling (/ size (frame-char-height))))
	     (max-char-width  (ceiling (/ size (frame-char-width))))
	     (display (and (stringp chess-images-separate-frame)
			   chess-images-separate-frame)))
	;; create the frame whenever necessary
	(chess-display-popup-in-frame (+ max-char-height 2)
				      max-char-width
				      (cdr (assq 'font (frame-parameters)))))
    (chess-display-popup-in-window)))

(defun chess-images-piece-image (piece rank file)
  "Return the image used for PIECE at RANK and FILE.
Rank and file are important because the colors of the squares on the
chess board are light or dark depending on location."
  (let ((square-color (% (+ file rank) 2))) ; 0 is white
    (if (= piece ? )
	(aref chess-images-cache (- 3 square-color))
      (aref (aref (aref chess-images-cache
			(if (> piece ?a) 0 1))
		  (if (= square-color 0) 1 0))
	    (nth 2 (assq (downcase piece)
			 chess-images-piece-names))))))

(defsubst chess-images-draw-square (pos piece index)
  "Draw a piece image at point on an already drawn display."
  (put-text-property pos (1+ pos) 'display
		     (chess-images-piece-image piece (chess-index-rank index)
					       (chess-index-file index))))

(defun chess-images-draw (position perspective)
  "Draw the current chess display position."
  (let* ((inhibit-redisplay t)
	 (inverted (not perspective))
	 (rank (if inverted 7 0))
	 (file (if inverted 7 0))
	 (pos (point)) new beg)
    (unless chess-images-cache
      (chess-images-init-cache)
      (erase-buffer))
    (unless (setq new (= (point-min) (point-max)))
      (goto-char (point-min)))
    (while (if inverted (>= rank 0) (< rank 8))
      (while (if inverted (>= file 0) (< file 8))
	(let* ((piece (chess-pos-piece position
				       (chess-rf-to-index rank file)))
	       (image (chess-images-piece-image piece rank file)))
	  (if (not new)
	      (progn
		(put-text-property (point) (1+ (point)) 'display image)
		(unless (= (1+ (point)) (point-max))
		  (forward-char 2)))
	    (setq beg (point))
	    (insert-image image)
	    (if (= file (if inverted 0 7))
		(unless (= rank (if inverted 0 7))
		  (insert ?\n))
	      (insert-image (aref chess-images-cache 5)))
	    (add-text-properties
	     beg (point) (list 'intangible (chess-rf-to-index rank file)
			       'rear-nonsticky '(intangible)
			       'chess-coord (chess-rf-to-index rank file)))))
	(setq file (if inverted (1- file) (1+ file))))
      (setq file (if inverted 7 0)
	    rank (if inverted (1- rank) (1+ rank))))
    (set-buffer-modified-p nil)
    (goto-char pos)))

(defun chess-images-highlight (index &optional mode)
  "Highlight the piece on the board at INDEX, using the given MODE.
Common modes are:
  `selected'    show that the piece has been selected for movement.
  `unselected'  show that the piece has been unselected."
  (let* ((inverted (not (chess-display-perspective nil)))
	 (pos (chess-display-index-pos nil index))
	 (highlight (copy-alist (get-text-property pos 'display))))
    (setcar (last highlight)
	    (list (cons "light_square" (if (eq mode :selected)
					   chess-images-highlight-color
					 mode))
		  (cons "dark_square" (if (eq mode :selected)
					  chess-images-highlight-color
					mode))
		  (cons "background" (if (eq mode :selected)
					 chess-images-highlight-color
				       mode))))
    (put-text-property pos (1+ pos) 'display highlight)))

(chess-message-catalog 'english
  '((redrawing-frame . "Redrawing chess display with different size...")
    (redrawing-frame-done . "Redrawing chess display with different size...done")))

(defun chess-images-change-size (size)
  (let* ((buffer (current-buffer))
	 (window (get-buffer-window buffer))
	 (frame (and window (window-frame window))))
    (setq chess-images-size size
	  chess-images-cache nil)
    (if frame
	(delete-frame frame t))
    (chess-message 'redrawing-frame)
    (chess-display-update buffer t)
    (chess-display-popup buffer)
    (chess-message 'redrawing-frame-done)))

(defun chess-images-resize ()
  "Resize the chessboard based on the frame or window's new size."
  (chess-images-determine-size)
  (if chess-images-size
      (chess-images-change-size chess-images-size)
    (chess-message 'no-images-fallback)))

(defun chess-images-alter-size (test)
  (let ((sizes chess-images-sizes))
    (if (eq test '<)
	(setq sizes (reverse sizes)))
    (while sizes
      (if (funcall test (car sizes) chess-images-size)
	  (progn
	    (chess-images-change-size (car sizes))
	    (setq sizes nil))
	(setq sizes (cdr sizes))))))

(defun chess-images-increase-size ()
  "Increase the size of the pieces on the board."
  (interactive)
  (chess-images-alter-size '>))

(defun chess-images-decrease-size ()
  "Increase the size of the pieces on the board."
  (interactive)
  (chess-images-alter-size '<))

(defun chess-images-sizes ()
  "Return the set of available sizes for the current piece set.
They are returned in ascending order, or nil for no sizes available."
  (let ((file (expand-file-name (format "black-rook.%s"
					chess-images-extension)
				chess-images-directory)))
    (if (file-readable-p file)
	(with-temp-buffer
	  (insert-file-contents-literally file)
	  (re-search-forward "\"\\([0-9]+\\)")
	  (setq chess-images-sizes (list (string-to-int (match-string 1)))))
      (let (sizes)
	(dolist (file (directory-files chess-images-directory nil
				       (format "rdd[0-9]+\\.%s"
					       chess-images-extension)))
	  (if (string-match "rdd\\([0-9]+\\)\\." file)
	      (push (string-to-int (match-string 1 file)) sizes)))
	(setq chess-images-sizes (sort sizes '<))))))

(defun chess-images-best-size (&optional height width)
  "Return the piece size that works best for a window of HEIGHT."
  (let* ((size (or chess-images-default-size
		   (min (- (/ (or height (frame-pixel-height)) 8)
			   (or chess-images-border-width 0))
			(- (/ (or width (frame-pixel-width)) 8)
			   (or chess-images-border-width 0)))))
	 (sizes (chess-images-sizes))
	 (last (car sizes)))
    (while sizes
      (if (> (car sizes) size)
	  (setq sizes nil)
	(setq last (car sizes)
	      sizes (cdr sizes))))
    (or last (and chess-images-default-size
		  (let (chess-images-default-size)
		    (chess-images-best-size height width))))))

(defun chess-images-set-directory (directory)
  "Increase the size of the pieces on the board."
  (interactive "DUse chess pieces in: ")
  (setq chess-images-directory directory
	chess-images-sizes (chess-images-sizes)
	chess-images-size (chess-images-best-size)
	chess-images-cache nil)
  (chess-images-alter-size '=))

(defun chess-images-create-xpm (height &optional width)
  (with-temp-buffer
    (insert "/* XPM */\n")
    (insert "static char *chessdotel[] = {\n")
    (insert "/* columns rows colors chars-per-pixel */\n")
    (insert (format "\"%d %d 2 1\",\n" (or width height) height))
    (insert "\"  c red s void\",\n")
    (insert "\". c red s background\",\n")
    (insert "/* pixels */\n")
    (dotimes (i height)
      (insert ?\" (make-string (or width height) ?.) ?\" ?, ?\n))
    (delete-backward-char 2)
    (insert "\n};\n")
    (buffer-string)))

(defun chess-images-hack-xpm (file add-height color)
  "Hack an XPM to append ADD-HEIGHT rows of COLOR.
This is necessary for bizzare Emacs reasons."
  (with-temp-buffer
    (if (string-match "\\`/\\* XPM \\*/" file)
	(insert file)
      (insert-file-contents-literally file))
    (goto-char (point-min))
    (if (re-search-forward (concat "\"\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-+"
				   "\\([0-9]+\\)\\s-+\\([0-9]+\\)\"") nil t)
	(let* ((width (string-to-int (match-string 1)))
	       (height (string-to-int (match-string 2)))
	       (colors (string-to-int (match-string 3)))
	       (chars-per-color (string-to-int (match-string 4)))
	       (color-char (make-string chars-per-color ?~)))
	  (replace-match (int-to-string (+ height add-height)) t t nil 2)
	  (unless
	      (save-match-data
		(save-excursion
		  (if (re-search-forward
		       (format "^\"\\(..\\)\\s-*c\\s-+%s" color) nil t)
		      (setq color-char
			    (substring (match-string 1) 0 chars-per-color)))))
	    (replace-match (int-to-string (1+ colors)) t t nil 3)
	    (end-of-line)
	    (insert "\n\"" color-char "\tc " color "\","))
	  (beginning-of-line)
	  (forward-line (1+ colors))
	  (while (looking-at "/\\*")
	    (forward-line))
	  (dotimes (i add-height)
	    (insert "\"")
	    (dotimes (j width)
	      (insert color-char))
	    (insert "\",\n"))))
    (buffer-string)))

(defsubst chess-images-create-image (file background &optional foreground)
  "Create an Emacs image object, for insertion on the board."
  (let ((syms (list (nth background
			 `(("dark_square"  . ,chess-images-dark-color)
			   ("light_square" . ,chess-images-light-color)))
		    (nth background
			 `(("background"   . ,chess-images-dark-color)
			   ("background"   . ,chess-images-light-color))))))
    (if foreground
	(nconc syms
	       (list (nth foreground
			  `(("dark_piece"   . ,chess-images-black-color)
			    ("light_piece"  . ,chess-images-white-color))))))
    (if chess-images-border-width
	(create-image
	 (chess-images-hack-xpm file chess-images-border-width
				chess-images-border-color)
	 nil t :color-symbols syms)
      (create-image file nil (string-match "\\`/\\* XPM \\*/" file)
		    :color-symbols syms))))

(chess-message-catalog 'english
  '((piece-images-loading . "Loading chess piece images...")
    (piece-images-loaded  . "Loading chess piece images...done")))

(defun chess-images-init-cache ()
  "Initialize the display image cache."
  (chess-message 'piece-images-loading)

  ;; Make a vector of two vectors of 6-item vectors: each piece of
  ;; each color on each color square; and lastly two slots for the
  ;; blank squares
  (setq chess-images-cache
	(vector (vector (make-vector 6 nil)
			(make-vector 6 nil))
		(vector (make-vector 6 nil)
			(make-vector 6 nil))
		nil nil nil nil))

  (let* ((colors '("black" "white"))
	 (backgrounds (list chess-images-dark-color
			    chess-images-light-color))
	 (piece-colors (list chess-images-black-color
			     chess-images-white-color))
	 blank name image-data)
    (dotimes (c 2)
      (dotimes (b 2)
	(dolist (piece chess-images-piece-names)
	  (let ((file (expand-file-name
		       (format "%s-%s.%s" (nth c colors) (nth 1 piece)
			       chess-images-extension)
		       chess-images-directory)))
	    (if (file-readable-p file)
		(aset (aref (aref chess-images-cache c) b)
		      (nth 2 piece)
		      (chess-images-create-image file b c))
	      ;; try loading an xboard format file
	      (setq file (expand-file-name
			  (format "%c%c%c%d.%s" (car piece)
				  (if (= c 0) ?d ?l)
				  (if (= b 0) ?d ?l) chess-images-size
				  chess-images-extension)
			  chess-images-directory))
	      (aset (aref (aref chess-images-cache c) b)
		    (nth 2 piece)
		    (chess-images-create-image file b c)))))))
    (cond
     ((file-readable-p
       (setq blank (expand-file-name
		    (format "%s.%s" chess-images-background-image
			    chess-images-extension)
		    chess-images-directory)))
      (aset chess-images-cache 2 (chess-images-create-image blank 0))
      (aset chess-images-cache 3 (chess-images-create-image blank 1)))
     ;; try loading an xboard format file
     ((file-readable-p
       (setq name (format "dsq%d.%s" chess-images-size
			  chess-images-extension)
	     blank (expand-file-name name chess-images-directory)))
      (aset chess-images-cache 2 (chess-images-create-image blank 0))
      (aset name 0 ?l)
      (setq blank (expand-file-name name chess-images-directory))
      (aset chess-images-cache 3 (chess-images-create-image blank 1)))
     ;; if all else fails, create one
     (t
      (setq image-data
	    (cond
	     ((string= chess-images-extension "xpm")
	      (chess-images-create-xpm chess-images-size))))
      (aset chess-images-cache 2 (chess-images-create-image image-data 0))
      (aset chess-images-cache 3 (chess-images-create-image image-data 1))))

    (when chess-images-border-width
      (aset chess-images-cache 4
	    (create-image
	     (chess-images-create-xpm
	      chess-images-border-width
	      (+ (* 8 chess-images-size)
		 (* 9 chess-images-border-width)))
	     nil t :color-symbols
	     (list (cons "background" chess-images-border-color))))
      (aset chess-images-cache 5
	    (create-image
	     (chess-images-create-xpm chess-images-size
				      chess-images-border-width)
	     nil t :color-symbols
	     (list (cons "background" chess-images-border-color))))))

  ;; let the garbage collector know we're through here
  (garbage-collect)
  (chess-message 'piece-images-loaded))

(provide 'chess-images)

;;; chess-images.el ends here
