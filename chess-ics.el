;;; chess-ics.el --- An engine for interacting with Internet Chess Servers

;; Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

;; Author: John Wiegley
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile (require 'cl))

(require 'comint)
(require 'chess)
(require 'chess-network)
(require 'chess-pos)

(defgroup chess-ics nil
  "Engine for interacting with Internet Chess Servers."
  :group 'chess-engine)

(defcustom chess-ics-server-list '(("freechess.org" 5000)
				   ("chess.unix-ag.uni-kl.de" 5000)
				   ("chess.mds.mdh.se" 5000)
				   ("chessclub.com" 5000))
  "A list of servers to connect to.
The format of each entry is:

  (SERVER PORT [HANDLE] [PASSWORD-OR-FILENAME] [HELPER] [HELPER ARGS...])"
  :type '(repeat (list (string :tag "Server")
		       (integer :tag "Port")
		       (choice (const :tag "Login as guest" nil)
			       (string :tag "Handle"))
		       (choice (const :tag "No password or ask" nil)
			       (string :tag "Password")
			       (file :tag "Filename"))
		       (choice (const :tag "Direct connection" nil)
			       (file :tag "Command"))
		       (choice (const :tag "No arguments" nil)
			       (repeat string))))
  :group 'chess-ics)

(defvar chess-ics-server nil
  "The ICS server name of this connection.")
(make-variable-buffer-local 'chess-ics-server)

(defvar chess-ics-handle nil
  "The ICS handle of this connection.")
(make-variable-buffer-local 'chess-ics-handle)

(defvar chess-ics-password nil
  "Password to use to identify to the server.")
(make-variable-buffer-local 'chess-ics-password)

(defvar chess-ics-handling-login nil
  "Non-nil if we are currently handling the ICS login sequence.")
(make-variable-buffer-local 'chess-ics-handling-login)

(defvar chess-ics-movelist-game-number nil
  "If we are about to receive a movelist, this variable is set to the
game number.")
(make-variable-buffer-local 'chess-ics-movelist-game-number)

(defvar chess-ics-movelist-game nil
  "If we are receiving a movelist, this variable is set to the game object.")
(make-variable-buffer-local 'chess-ics-movelist-game)

(defsubst chess-ics-send (string &optional buffer)
  "Send STRING to the ICS server."
  (comint-send-string (get-buffer-process (or buffer (current-buffer)))
		      (concat string "\n")))

(chess-message-catalog 'english
  '((ics-server-prompt . "Connect to chess server: ")
    (ics-connecting    . "Connecting to Internet Chess Server '%s'...")
    (ics-connected     . "Connecting to Internet Chess Server '%s'...done")
    (ics-anon-login    . "Logging in on Internet Chess Server '%s' as anonymous user...")
    (ics-logging-in    . "Logging in on Internet Chess Server '%s' as '%s'...")
    (ics-logged-in     . "Logging in on Internet Chess Server '%s' as '%s'...done")
    (challenge-whom    . "Whom would you like challenge? ")
    (failed-ics-parse  . "Failed to parse ICS move string (%s): %s")))

(defvar chess-ics-matcher-alist
  (list
   (cons "\\(ogin\\|name\\):"
	 (function
	  (lambda ()
	    (if (string= "guest" chess-ics-handle)
		(chess-message 'ics-anon-login chess-ics-server)
	      (chess-message
	       'ics-logging-in chess-ics-server chess-ics-handle))
	    (chess-ics-send chess-ics-handle)
	    'once)))
   (cons "[Pp]assword:"
	 (function
	  (lambda ()
	    (when chess-ics-handling-login
	      (chess-ics-send chess-ics-password))
	    'once)))
   (cons "\\(Logging you in as\\|Your name for this session will be\\) \"\\([^\"]+\\)\""
	 (function
	  (lambda ()
	    (setq chess-ics-handle (match-string 2))
	    'once)))
   (cons "Press return to enter the server as"
	 (function
	  (lambda ()
	    (chess-ics-send "")
	    'once)))
   (cons "%\\s-*$"
	 (function
	  (lambda ()
	    (chess-ics-send (concat
			     (format "set interface emacs-chess %s\n"
				     chess-version)
			     "set style 12\nset bell 0"))
	    (setq chess-ics-handling-login nil)
	    (chess-message 'ics-logged-in chess-ics-server chess-ics-handle)
	    'once)))
   (cons "\\(\\S-+\\) (\\([0-9+-]+\\)) seeking \\([1-9][0-9]*\\) \\([0-9]+\\) \\(.+\\) (\"\\([^\"]+\\)\" to respond)\\s-*$"
	 'chess-ics-handle-seek)
   (cons "^\\([A-Za-z]+\\)\\((\*)\\|(B)\\|(CA?)\\|(T[DM]?)\\|(SR)\\|(FM)\\|(W?\\(GM\\|IM\\))\\)*(\\([0-9]+\\)): .+$"
	 (function
	  (lambda ()
	    (let ((fill-prefix (make-string
				(- (match-end 1) (match-beginning 1)) ? )))
	      (goto-char (match-beginning 0))
	      (save-excursion
		(while (and (forward-line -1)
			    (or (looking-at "^[ \t]*$")
				(looking-at "^[^% \t\n\r]+%\\s-*$")))
		  (delete-region (match-beginning 0) (1+ (match-end 0)))))
	      (save-excursion
		(while (and (forward-line 1)
			    (looking-at "^\\\\\\s-+"))
		  (delete-region (1- (match-beginning 0)) (match-end 0))))
	      (when (> (- (line-end-position) (line-beginning-position))
		       fill-column)
		(fill-region (point) (line-end-position)))))))
   (cons "{Game \\([0-9]+\\) (\\(\\S-+\\) vs\\. \\(\\S-+\\)) Creating [^ ]+ \\([^ ]+\\).*}"
	 (function
	  (lambda ()
	    (let ((game-number (string-to-int (match-string 1)))
		  (white (match-string-no-properties 2))
		  (black (match-string-no-properties 3)))
	      (message "Creating game %d (%s vs. %s)" game-number white black)
	      (chess-ics-game game-number :White white :Black black)))))
   (cons "^<10>$" (function (lambda () (chess-ics-send "style 12\nrefresh"))))
   (cons "^Game \\([0-9]+\\): \\S-+ backs up \\([0-9]+\\) moves.$"
	 (function
	  (lambda ()
	    (chess-game-undo (chess-ics-game (string-to-int (match-string 1)))
			     (string-to-int (match-string 2))))))
   (cons "<12>\\s-+\\(\\([BKNPQRbknpqr-]\\{8\\} \\)\\{8\\}[BW] .+\\)$"
	 #'chess-ics-handle-ics12)
   (cons "Removing game \\([0-9]+\\) from observation list.$"
	 (function
	  (lambda ()
	    (chess-game-run-hooks
	     (chess-ics-game (string-to-int (match-string 1))) 'destroy))))
   (cons "^Movelist for game \\([0-9]+\\):$"
	 (function
	  (lambda ()
	    (if (or chess-ics-movelist-game-number
		    chess-ics-movelist-game)
		(message "[movelist] left-over movelist-game[-number]")
	      (setq chess-ics-movelist-game-number
		    (string-to-int (match-string 1)))))))
   (cons "^Move\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-*$"
	 (function
	  (lambda ()
	    (if (not chess-ics-movelist-game-number)
		(message "[movelist] no gamenumber but header seen")
	      (setq chess-ics-movelist-game
		    (chess-ics-game chess-ics-movelist-game-number
				    :White (match-string 1)
				    :Black (match-string 2)))
	      (chess-game-set-start-position
	       chess-ics-movelist-game chess-starting-position))
	    t)))
   ;; Movelist item
   (cons (concat "^\\s-*\\([0-9]+\\)\\.\\s-+\\(" chess-algebraic-regexp "\\)"
		 "\\s-+\\(([0-9][0-9]?:[0-9][0-9])\\)\\s-*"
		 "\\(\\(" chess-algebraic-regexp "\\)\\s-+"
		 "\\(([0-9][0-9]?:[0-9][0-9])\\)\\s-*\\)?$")
	 #'chess-ics-handle-movelist-item)
   (cons "\\s-+{Still in progress}\\s-+\\*$"
	 (function
	  (lambda ()
	    (if (integerp chess-ics-movelist-game-number)
		(setq chess-ics-movelist-game-number nil
		      chess-ics-movelist-game nil)
	      (message "[movelist] end of movelist seen where no game known about")))))
   (cons "\\S-+ would like to take back \\([0-9]+\\) half move(s)."
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'undo
		     (string-to-int (match-string 1))))))
   (cons "The game has been aborted on move [^.]+\\."
	 (function
	  (lambda ()
	    (let ((chess-engine-pending-offer 'abort))
	      (funcall chess-engine-response-handler 'accept)))))
   (cons "\\S-+ accepts the takeback request\\."
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'accept))))
   (cons ;; resign announcement
    "{Game \\([0-9]+\\) (\\(\\S-+\\) vs\\. \\(\\S-+\\)) \\(\\S-+\\) resigns}"
    (function
     (lambda ()
       (let ((chess-engine-handling-event t)
	     (opponent-p (not (string= chess-ics-handle (match-string 4))))
	     (game (chess-ics-game (string-to-int (match-string 1))
				   :White (match-string 2)
				   :Black (match-string 3))))
	 (with-current-buffer (chess-game-data game 'engine)
	   (if opponent-p
	       (funcall chess-engine-response-handler 'resign)
	     (unless (chess-game-status game)
	       (chess-game-end game :resign))))
	 t))))
   (cons "\\(\\S-+\\) forfeits on time}"
	 (function
	  (lambda ()
	    (if (string= (match-string 1) chess-engine-opponent-name)
		(funcall chess-engine-response-handler 'flag-fell)
	      (funcall chess-engine-response-handler 'call-flag t)))))
   (cons "Illegal move (\\([^)]+\\))\\."
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'illegal
		     (match-string 1)))))
   (cons "Challenge: \\(\\S-+\\) \\S-+ \\S-+ \\S-+ .+"
	 (function
	  (lambda ()
	    (let ((opponent (match-string 1)))
	      (if (y-or-n-p (chess-string 'want-to-play opponent))
		  (chess-ics-send (concat "accept " opponent))
		(chess-ics-send "decline match")))))))
  "An alist of regular expressions to use to scan ICS server output.
The car of each element is the regexp to try, and the cdr is a function
to run whenever the regexp matches.")

(defvar chess-ics-sessions nil
  "A list of chess-sessions spawned from an Internet Chess Server connection.
See `chess-ics-game'.")
(make-variable-buffer-local 'chess-ics-sessions)

(defun chess-ics-game (game-number &rest tags)
  "Either create, or retrieve an existing game object with GAME-NUMBER."
  (assert (integerp game-number))
  (assert (or (evenp (length tags)) (eq (car tags) t)))
  (or
   ;; First try to find a game which matches the constraints in TAGS
   (catch 'ics-game
     (let ((sessions chess-ics-sessions))
       (while sessions
	 (if (not (buffer-live-p (caar sessions)))
	     (message "Found dead engine session in `chess-ics-sessions'")
	   (let ((game (chess-engine-game (caar sessions)))
		 (tag-pairs tags))
	     (when (= game-number (chess-game-data game 'ics-game-number))
	       (if (or (null tags) (eq (car tags) t))
		   (throw 'ics-game game)
		 (while tag-pairs
		   (assert (symbolp (car tag-pairs)))
		   (let ((tag (substring (symbol-name (car tag-pairs)) 1))
			 (val (cadr tag-pairs)))
		     (assert (stringp val))
		     (if (string= (chess-game-tag game tag) val)
			 (setq tag-pairs (cddr tag-pairs))
		       (if (not (string= (chess-game-tag game tag) "?"))
			   (error "Game %d %s %s != %s"
				  game-number tag (chess-game-tag game tag) val)
			 ;; Update tag and proceed
			 (chess-game-set-tag game tag val)
			 (setq tags (cddr tags))))))
		 (throw 'ics-game game)))))
	 (setq sessions (cdr sessions)))))
   ;; if we are allowed to, create a new session for this game number
   (unless (eq (car tags) t)
     (push (let (chess-engine-handling-event)
	     (chess-session 'chess-ics))
	   chess-ics-sessions)
     (assert (caar chess-ics-sessions))
     (let ((game (chess-engine-game (caar chess-ics-sessions))))
       (chess-game-set-data game 'ics-game-number game-number)
       (chess-game-set-data game 'ics-buffer (current-buffer))
       (chess-game-set-tag game "Site" chess-ics-server)
       (while tags
	 (assert (keywordp (car tags)))
	 (chess-game-set-tag
	  game (substring (symbol-name (car tags)) 1) (cadr tags))
	 (setq tags (cddr tags)))
       game))))

(defun chess-ics-handle-movelist-item ()
  ;; TBD: time taken per ply
  (let ((chess-engine-handling-event t)
	(seq (string-to-int (match-string 1)))
	(wmove (match-string 2))
	(bmove (match-string 14))
	(game chess-ics-movelist-game))
    (when (and game
	       (chess-pos-side-to-move (chess-game-pos game))
	       (= (chess-game-seq game) seq))
      (chess-game-set-data game 'my-color nil)
      (chess-game-move
       game (chess-algebraic-to-ply (chess-game-pos game) wmove))
      (when bmove
	(chess-game-set-data game 'my-color t)
	(chess-game-move
	 game (chess-algebraic-to-ply (chess-game-pos game) bmove))))
    t))

;; ICS12 format (with artificial line breaks):
;;
;; <12> rnbqkbnr pppppppp -------- -------- \
;;      -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 \
;;      65 jwiegley GuestZYNJ 1 5 0 39 39 300 300 1 P/e2-e4 (0:00) e4 0 0 0

(defun chess-ics-handle-ics12 ()
  "Handle an ICS12 format string."
  (let* ((chess-engine-handling-event t)
	 (begin (match-beginning 0))
	 (end (match-end 0))
	 (parts (split-string (match-string 1) " "))
	 (position (let ((pos (chess-pos-create t)))
		     (assert (= (length parts) 32))
		     (dotimes (r 8)
		       (let ((rank (pop parts)))
			 (dotimes (f 8)
			   (unless (= (aref rank f) ?-)
			     (chess-pos-set-piece
			      pos (chess-rf-to-index r f) (aref rank f))))))
		     (chess-pos-set-side-to-move pos (string= (pop parts) "W"))
		     (let ((file (string-to-int (pop parts))))
		       (when (>= file 0)
			 (chess-pos-set-en-passant
			  pos (chess-rf-to-index
			       (if (chess-pos-side-to-move pos) 3 4) file))))
		     (mapc (lambda (castle)
			     (if (string= (pop parts) "1")
				 (chess-pos-set-can-castle pos castle t)))
			   '(?K ?Q ?k ?q))
		     ;; the number of moves made since the last irreversible
		     ;; move.  (0 if last move was irreversible.  If the value
		     ;; is >= 100, the game can be declared a draw due to the
		     ;; 50 move rule.)
		     (setq parts (cdr parts))
		     (assert (= (length parts) 17))
		     pos))
	 (game (chess-ics-game (string-to-int (pop parts))
			       :White (pop parts) :Black (pop parts)))
	 (status
	  ;; my relation to this game:
	  ;; -3 isolated position, such as for "ref 3" or the "sposition"
	  ;;    command
	  ;; -2 I am observing game being examined
	  ;;  2 I am the examiner of this game
	  ;; -1 I am playing, it is my opponent's move
	  ;;  1 I am playing and it is my move
	  ;;  0 I am observing a game being played
	  (string-to-int (pop parts))))
    (when (or (= status 2) (= status -2) (= status 0))
      (chess-game-set-data game 'my-color (chess-pos-side-to-move position)))
    ;; initial time and increment (in seconds) of the match
    (chess-game-set-tag
     game "TimeControl" (format "%s/%s" (pop parts) (pop parts)))
    ;; material values for each side
    (let ((centipawn (* 100 (- (string-to-int (pop parts))
			       (string-to-int (pop parts))))))
      (chess-pos-set-epd position 'ce (if (chess-pos-side-to-move position)
					  centipawn (- centipawn))))
    ;; White's and Black's remaining time
    (mapc (lambda (d) (chess-game-set-data game d (string-to-int (pop parts))))
	  '(white-remaining black-remaining))
    (let* ((seq (prog1 (string-to-int (pop parts))
		  ;; move in long alegebraic notation
		  (setq parts (cdr parts))
		  ;; time taken to make previous move "(min:sec)".
		  (setq parts (cdr parts))))
	   (index (- (* seq 2) (if (chess-pos-side-to-move position) 2 1)))
	   (move (prog1 (unless (string= (car parts) "none")
			  (case (aref (car parts) (1- (length (car parts))))
			    (?+ (chess-pos-set-status position :check))
			    (?# (chess-pos-set-status position :checkmate)
				(chess-pos-set-epd position 'ce 32767)))
			  ;; jww (2002-04-30): what about stalemate?  do I need to
			  ;; calculate this each time?
			  (when nil
			    (chess-pos-set-status position :stalemate))
			  (car parts))
		   (setq parts (cdr parts)))))
      ;; flip field for board orientation: 1 = Black at bottom, 0 =
      ;; White at bottom.
      (setq parts (cdr parts))

      ;; jww (2002-04-18): what do these two mean?
      (setq parts (cdr parts))
      (setq parts (cdr parts))

      (unwind-protect
	  (if move
	      (if (progn (setq error 'comparing-index)
			 (= (1- index) (chess-game-index game)))
		  (let ((ply (progn (setq error 'converting-ply)
				    (chess-algebraic-to-ply
				     (chess-game-pos game) move t))))
		    ;; each move gives the _position occurring after the ply_
		    (if (progn (setq error 'comparing-colors)
			       (eq (chess-pos-side-to-move position)
				   (chess-game-data game 'my-color)))
			(setq error 'applying-opponent-move)
		      (setq error 'applying-my-move))
		    ;; save us from generating a position we already have
		    (chess-ply-set-keyword ply :next-pos position)
		    (chess-pos-set-preceding-ply position ply)
		    ;; apply the move
		    (chess-game-move game ply)
		    (setq error nil))
		(if (= index (chess-game-index game))
		    (setq error nil) ; Ignore a "refresh" command
		  (when (and (> index (1+ (chess-game-index game)))
			     (= 1 (chess-game-seq game)))
		    ;; we lack a complete game, try to get it via the movelist
		    (chess-ics-send
		     (format "moves %d"
			     (chess-game-data game 'ics-game-number))))))
	    ;; no preceeding ply supplied, so this is a starting position
	    (let ((chess-game-inhibit-events t)
		  (color (chess-pos-side-to-move position))
		  plies)
	      (when (or (= 1 status) (= -1 status))
		(chess-game-set-data game 'my-color (if (= 1 status)
							color
						      (not color)))
		(chess-game-set-data game 'active t))
	      (setq error 'setting-start-position)
	      (chess-game-set-start-position game position))
	    (setq error 'orienting-board)
	    (chess-game-run-hooks game 'orient)
	    (setq error nil))
	(if error
	    (chess-message 'failed-ics-parse error
			   (buffer-substring-no-properties begin end)))
	(unless error
	  (goto-char begin)
	  (delete-region begin end)
	  (save-excursion
	    (while (and (forward-line -1)
			(or (looking-at "^[ \t]*$")
			    (looking-at "^[^% \t\n\r]+%\\s-*$")))
	      (delete-region (match-beginning 0) (1+ (match-end 0)))))
	  ;; we need to counter the forward-line in chess-engine-filter
	  (unless error
	    (forward-line -1))))
      t)))

(defface chess-ics-seek-button '((((type pc) (class color))
				  (:foreground "lightblue"))
				 (t :underline t))
  "Default face used for seek buttons."
  :group 'chess-ics)

(defvar chess-ics-seek-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'chess-ics-push-seek-button)
    (define-key map [mouse-2] 'chess-ics-push-seek-button)
    map)
  "Keymap used by seek buttons.")

(defun chess-ics-push-seek-button (&optional pos)
  "Perform the action specified by a button at location POS.
POS may be either a buffer position or a mouse-event.
POS defaults to point, except when `push-button' is invoked
interactively as the result of a mouse-event, in which case, the
mouse event is used.
If there's no button at POS, do nothing and return nil, otherwise
return t."
  (interactive
   (list (if (integerp last-command-event) (point) last-command-event)))
  (if (and (not (integerp pos)) (eventp pos))
      ;; POS is a mouse event; switch to the proper window/buffer
      (let ((posn (event-start pos)))
        (with-current-buffer (window-buffer (posn-window posn))
          (push-button (posn-point posn) t)))
    ;; POS is just normal position
    (let ((command (get-char-property pos 'ics-seek-command)))
      (when (stringp command)
	(chess-ics-send command)
	t))))

(defun chess-ics-handle-seek ()
  (goto-char (match-beginning 0))
  (add-text-properties
   (match-beginning 6) (match-end 6)
   (list 'face 'chess-ics-seek-button
	 'mouse-face 'highlight
	 'keymap chess-ics-seek-button-map
	 'ics-seek-command (buffer-substring (match-beginning 6) (match-end 6))))
  (save-excursion
    (while (and (forward-line -1)
		(or (looking-at "^[ \t]*$")
		    (looking-at "^[^% \t\n\r]+%\\s-*$")))
      (delete-region (match-beginning 0) (1+ (match-end 0))))))

;;;###autoload
(defun chess-ics (server port &optional handle password-or-filename
			 helper &rest helper-args)
  "Connect to an Internet Chess Server."
  (interactive
   (let ((args (if (= (length chess-ics-server-list) 1)
		   (car chess-ics-server-list)
		 (assoc (completing-read (chess-string 'ics-server-prompt)
					 chess-ics-server-list
					 nil t (caar chess-ics-server-list))
			chess-ics-server-list))))
     (if (and (nth 2 args) (not (nth 3 args)))
	 (append (list (nth 0 args) (nth 1 args) (nth 2 args)
		       (read-passwd "Password: ")
		       (nth 4 args))
		 (nthcdr 5 args))
       args)))
  (unless handle
    (setq handle "guest"))
  (chess-message 'ics-connecting server)
  (let ((buf (if helper
		 (apply 'make-comint "chess-ics" helper nil helper-args)
	       (make-comint "chess-ics" (cons server port)))))
    (chess-message 'ics-connected server)
    (set-buffer buf)
    (setq chess-ics-server server
	  chess-ics-handle handle
	  chess-ics-password
	  (if (and password-or-filename
		   (file-readable-p password-or-filename))
	      (with-temp-buffer
		(insert-file-contents password-or-filename)
		(buffer-string))
	    password-or-filename)
	  chess-ics-handling-login t
	  chess-engine-regexp-alist (copy-alist chess-ics-matcher-alist)
	  comint-prompt-regexp "^[^%\n]*% *"
	  comint-scroll-show-maximum-output t)
    (add-hook 'comint-output-filter-functions 'chess-engine-filter t t)
    (let ((ntimes 50))
      (while (and chess-ics-handling-login
		  (> (setq ntimes (1- ntimes)) 0))
	(accept-process-output (get-buffer-process (current-buffer)) 0 100)))
    (switch-to-buffer buf)))

(defun chess-ics-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize))

     ((eq event 'ready)
      (chess-game-run-hooks game 'announce-autosave))

     ((eq event 'busy))			; ICS will inform them

     ((eq event 'match)
      (setq chess-engine-pending-offer 'match)
      (chess-engine-send
       nil (format "match %s\n"
		   (read-string (chess-string 'challenge-whom)))))

     ;; this handler is taken from chess-common; we need to send long
     ;; algebraic notation to the ICS server, not short
     ((eq event 'move)
      (chess-ics-send
       (if (chess-ply-any-keyword (car args) :castle :long-castle)
	   (chess-ply-to-algebraic (car args))
	 (concat (chess-index-to-coord
		  (chess-ply-source (car args))) "-"
		  (chess-index-to-coord
		   (chess-ply-target (car args)))))
       (chess-game-data game 'ics-buffer))
      (if (chess-game-over-p game)
	  (chess-game-set-data game 'active nil)))

     ((eq event 'flag-fell)
      (chess-common-handler game 'flag-fell))

     ((eq event 'forward)
      (chess-ics-send "forward" (chess-game-data game 'ics-buffer)))
     (t
      (apply 'chess-network-handler game event args)))))

(provide 'chess-ics)

;;; chess-ics.el ends here
