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

(eval-when-compile (require 'rx))

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



(defcustom chess-ics-initial-commands
  (list
   (list "freechess.org"
	 "iset defprompt 1"  ; So we can't be supprised by a user setting
	 (format "set interface emacs-chess %s" chess-version)
	 "iset seekremove 1" ; For real-time sought display
	 "iset startpos 1"   ; Sends initial position before movelist
	 "set style 12"      ; So we can parse the board "easily"
	 "set bell 0")       ; We have our own way of announcing events
   (list "chessclub.com"
	 (format "/set-quietly interface emacs-chess %s" chess-version)
	 "/set-quietly style 12"      ; So we can parse the board "easily"
	 "/set-quietly bell 0")
   (list nil
	 (format "set interface emacs-chess %s" chess-version)
	 "set style 12"      ; So we can parse the board "easily"
	 "set bell 0"))
  "A list of commands to send automatically upon successful login.
The format is (SERVER COMMANDS...) where SERVER is either the server-name
\(see `chess-ics-server-list') or nil, which is the default to use for all
servers which do not have a specialized entry in this list.  COMMAND is a
string which should be sent (newline characters will be added automatically.)"
  :group 'chess-ics
  :type '(repeat
	  (list :tag "Initialisation for"
		(choice (string :tag "Server Name") (const :tag "Default" nil))
		(repeat :inline t (string :tag "Command")))))

(defcustom chess-ics-prompt-regexp "\\(?:[0-2][0-9]:[0-6][0-9]_\\)?[af]ics% $"
  "*Regexp which matches an ICS prompt."
  :group 'chess-ics
  :type 'regexp)

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

(defvar chess-ics-server-type 'FICS
  "The type of chss server we are about to connect too.
Possible values are currently FICS (the default, and best supported)
and ICC.")
(make-variable-buffer-local 'chess-ics-server-type)

(defcustom chess-ics-icc-datagrams '(22 23 26 33 50 51 56 110 111)
  "*A list of datagrams to request when connecting to ICC."
  :group 'chess-ics
  :type '(repeat (choice (const :tag "DG_SEND_MOVES" 24)
			 (const :tag "DG_KIBITZ" 26)
			 (const :tag "DG_MOVE_ALGEBRAIC" 33)
			 (const :tag "DG_SEEK" 50)
			 (const :tag "DG_SEEK_REMOVED" 51)
			 (const :tag "DG_MSEC" 56)
			 (const :tag "DG_POSITION_BEGIN" 101)
			 (const :tag "DG_POSITION_BEGIN2" 110)
			 (const :tag "DG_PAST_MOVE" 111))))

(defvar chess-ics-movelist-game-number nil
  "If we are about to receive a movelist, this variable is set to the
game number.")
(make-variable-buffer-local 'chess-ics-movelist-game-number)

(defvar chess-ics-movelist-game nil
  "If we are receiving a movelist, this variable is set to the game object.")
(make-variable-buffer-local 'chess-ics-movelist-game)

(defvar chess-ics-movelist-start-position chess-starting-position
  "The starting position to use upon receiving of a movelist.
It is possible to configure certain servers to automatically send a
style12 board before sending a movelist, to allow retrieval of
the movelist for a non-standard game (one which does not start at the
standard position).  In those cases, this variable should be set to nil.")
(make-variable-buffer-local 'chess-ics-movelist-start-position)

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
    (failed-ics-parse  . "Failed to parse ICS move string (%s): ")))

(defconst chess-ics-style12-regexp
  (rx (and "<12> " (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
 	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
 	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
 	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
 	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
	   (group (repeat 8 (in "-pnbrqkPNBRQK"))) " "
 	   (group (in "BW")) ? (group (and (? ?-) (in "0-7"))) ? 
 	   (group (and (? ?-) digit)) ? (group (and (? ?-) digit)) ? 
  	   (group (and (? ?-) digit)) ? (group (and (? ?-) digit)) ? 
 	   (group (+ digit)) " "
 	   (group (+ digit)) " "
  	   (group (+ (not (in " ")))) " " (group (+ (not (in " ")))) " "
  	   (group (and (? ?-) digit)) " "
 	   (group (+ digit)) " " (group (+ digit)) " "
 	   (group (+ digit)) " " (group (+ digit)) " "
 	   (group (and (? ?-) (+ digit))) " "
 	   (group (and (? ?-) (+ digit))) " "
 	   (group (+ digit)) " "
 	   (group (+ (not (in " ")))) " "
 	   "(" (group (+ (not (in " )")))) ") "
 	   (group (+ (not (in " ")))) " "
 	   (group (and (? ?-) digit))
	   (optional (and " " (group (and (? ?-) digit)) " "
			  (group (and (? ?-) (+ digit)))))))
  "A regular expression matching a style12 board string.")

(defvar chess-ics-matcher-alist
  (list
   (cons "www.chessclub.com"
	 (function
	  (lambda ()
	    (when chess-ics-handling-login
	      (setq chess-ics-server-type 'ICC
		    comint-preoutput-filter-functions
		    '(chess-icc-preoutput-filter)))
	    'once)))
   (cons "\\(ogin\\|name\\):"
	 (function
	  (lambda ()
	    (when (eq chess-ics-server-type 'ICC)
	      (chess-ics-send
	       (format "level2settings=%s"
		       (let ((str (make-string
				   (1+ (apply 'max chess-ics-icc-datagrams))
				   ?0)))
			 (dolist (dg chess-ics-icc-datagrams str)
			   (aset str dg ?1))))))
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
	    (chess-ics-send
	     (mapconcat 'identity
			(cdr
			 (or
			  (assoc chess-ics-server chess-ics-initial-commands)
			  (assoc nil chess-ics-initial-commands))) "\n"))
	    (setq chess-ics-handling-login nil)
	    (chess-message 'ics-logged-in chess-ics-server chess-ics-handle)
	    'once)))
   (cons "fics%\\s-+startpos set.$"
	 (function
	  (lambda ()
	    (setq chess-ics-movelist-start-position nil)
	    'once)))
   (cons (concat "^Game [0-9]+: \\S-+ moves: " chess-algebraic-regexp-entire)
	 (function
	  (lambda ()
	    (save-excursion
	      (while (and (forward-line -1)
			  (or (looking-at "^[ \t]*$")
			      (looking-at
			       (concat "^" chess-ics-prompt-regexp))))
		(delete-region (match-beginning 0) (1+ (match-end 0)))))
	    t)))
   (cons "^\\([A-Za-z0-9]+\\)\\((\\*)\\|(B)\\|(CA?)\\|(H)\\|(DM)\\|(T[DM]?)\\|(SR)\\|(FM)\\|(W?[GI]M)\\|(U)\\|([0-9-]+)\\)*\\((\\([0-9]+\\))\\| tells you\\| s-shouts\\|\\[\\([0-9]+\\)\\] kibitzes\\): \\(.+\\)$"
	 (function
	  (lambda ()
	    (let ((fill-prefix (make-string
				(- (match-end 1) (match-beginning 1)) ? ))
		  (game-num (match-string 5))
		  (text-begin (match-beginning 6)))
	      (goto-char (match-beginning 0))
	      (save-excursion
		(while (and (forward-line 1)
			    (looking-at "^\\\\\\s-+"))
		  (delete-region (1- (match-beginning 0)) (match-end 0))))
	      (when game-num
		(chess-game-run-hooks
		 (chess-ics-game (string-to-int game-num))
		 'kibitz (buffer-substring text-begin (line-end-position))))
	      (when (> (- (line-end-position) (line-beginning-position))
		       fill-column)
		(save-excursion
		 (fill-region (point) (line-end-position))))
	      (save-excursion
		(while (and (forward-line -1)
			    (or (looking-at "^[ \t]*$")
				(looking-at "^[af]ics%\\s-*$")))
		  (delete-region (match-beginning 0) (1+ (match-end 0)))))))))
   (cons "{Game \\([0-9]+\\) (\\(\\S-+\\) vs\\. \\(\\S-+\\)) Creating [^ ]+ \\([^ ]+\\).*}"
	 (function
	  (lambda ()
	    (let ((game-number (string-to-int (match-string 1)))
		  (white (match-string-no-properties 2))
		  (black (match-string-no-properties 3)))
	      (message "Creating game %d (%s vs. %s)" game-number white black)
	      (chess-ics-game game-number :White white :Black black)))))
   (cons "^<10>$" (function (lambda () (chess-ics-send "style 12\nrefresh"))))
   (cons "^Game \\([0-9]+\\): \\S-+ backs up \\([0-9]+\\).$"
	 (function
	  (lambda ()
	    (chess-game-undo (chess-ics-game (string-to-int (match-string 1)))
			     (string-to-int (match-string 2))))))
   (cons chess-ics-style12-regexp #'chess-ics-handle-style12)
   (cons "Removing game \\([0-9]+\\) from observation list.$"
	 (function
	  (lambda ()
	    (chess-ics-game-destroy (string-to-int (match-string 1))))))
   (cons "You are no longer examining game \\([0-9]+\\).$"
	 (function
	  (lambda ()
	    (chess-ics-game-destroy (string-to-int (match-string 1))))))
   (cons "^Movelist for game \\([0-9]+\\):$"
	 (function
	  (lambda ()
	    (if (or chess-ics-movelist-game-number
		    chess-ics-movelist-game)
		(message "[movelist] left-over movelist-game[-number]")
	      (setq chess-ics-movelist-game-number
		    (string-to-int (match-string 1)))))))
   (cons "^Move\\s-+\\*?\\(\\S-+\\)\\s-+\\*?\\(\\S-+\\)\\s-*$"
	 (function
	  (lambda ()
	    (if (not chess-ics-movelist-game-number)
		(progn
		  (goto-char (match-beginning 0))
		  (insert "(no game# known) "))
	      (setq chess-ics-movelist-game
		    (chess-ics-game chess-ics-movelist-game-number
				    :White (match-string 1)
				    :Black (match-string 2)))
	      (when chess-ics-movelist-start-position
	      (chess-game-set-start-position
	       chess-ics-movelist-game chess-ics-movelist-start-position)))
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
			   (message "Game %d %s %s != %s"
				  game-number tag (chess-game-tag game tag) val))
		       ;; Update tag and proceed
		       (chess-game-set-tag game tag val)
		       (setq tags (cddr tags)))))
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

(defvar last-triggers nil)

(defun chess-ics-game-destroy (game-number &rest tags)
  (let ((sessions chess-ics-sessions)
	last-session)
    (while sessions
      (if (not (buffer-live-p (caar sessions)))
	  (message "Found dead engine session in `chess-ics-sessions'")
	(let ((game (chess-display-game (cadar sessions)))
	      (tag-pairs tags)
	      (found t))
	  (when (= game-number (chess-game-data game 'ics-game-number))
	    (if (null tags)
		(progn
		  (chess-display-destroy (cadar sessions))
		  (if last-session
		      (setcdr last-session (cdr sessions))
		    (setq chess-ics-sessions (cdr sessions))))
	      (while (and tag-pairs found)
		(assert (symbolp (car tag-pairs)))
		(let ((tag (substring (symbol-name (car tag-pairs)) 1))
		      (val (cadr tag-pairs)))
		  (assert (stringp val))
		  (if (string= (chess-game-tag game tag) val)
		      (setq tag-pairs (cddr tag-pairs))
		    (setq found nil))))
	      (if (not found)
		  (error "Game not found")
		(chess-engine-destroy (cadar sessions))
		(if last-session
		    (setcdr last-session (cdr sessions))
		  (setq chess-ics-sessions (cdr sessions))))))))
      (setq last-triggers sessions
	    sessions (cdr sessions)))))

(defun chess-ics-handle-movelist-item ()
  ;; TBD: time taken per ply
  (let ((chess-engine-handling-event t)
	(seq (string-to-int (match-string 1)))
	(wmove (match-string 2))
	(bmove (match-string 14))
	(game chess-ics-movelist-game))
    (when game
      (if (/= (chess-game-seq game) seq)
	  (progn
	    (goto-char (match-beginning 0))
	    (insert (format "SeqNr. unmatched (%d): " seq)))
	(when (chess-pos-side-to-move (chess-game-pos game))
	  (chess-game-move
	   game (chess-algebraic-to-ply (chess-game-pos game) wmove))
	  (when bmove
	    (chess-game-move
	     game (chess-algebraic-to-ply (chess-game-pos game) bmove))))))
    t))

;; ICS style12 format (with artificial line breaks):
;;
;; <12> rnbqkbnr pppppppp -------- -------- \
;;      -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 \
;;      65 jwiegley GuestZYNJ 1 5 0 39 39 300 300 1 P/e2-e4 (0:00) e4 0 0 0

(defun chess-ics-handle-style12 ()
  "Handle an ICS Style12 board string."
  (let* ((chess-engine-handling-event t)
	 (begin (match-beginning 0))
	 (end (match-end 0))
	 (position (let ((pos (chess-pos-create t)))
		     (dotimes (r 8)
		       (let ((rank (match-string (1+ r))))
			 (dotimes (f 8)
			   (unless (= (aref rank f) ?-)
			     (chess-pos-set-piece
			      pos (chess-rf-to-index r f) (aref rank f))))))
		     (chess-pos-set-side-to-move pos (string= (match-string 9) "W"))
		     (let ((file (string-to-int (match-string 10))))
		       (when (>= file 0)
			 (chess-pos-set-en-passant
			  pos (chess-rf-to-index
			       (if (chess-pos-side-to-move pos) 3 4) file))))
		     (mapc (lambda (info)
			     (if (string= (match-string (cdr info)) "1")
				 (chess-pos-set-can-castle pos (car info) t)))
			   '((?K . 11) (?Q . 12) (?k . 13) (?q . 14))) pos))
	 (game (save-match-data
		 (chess-ics-game (string-to-int (match-string 16))
				 :White (match-string 17)
				 :Black (match-string 18))))
	 (status
	  ;; my relation to this game:
	  ;; -3 isolated position, such as for "ref 3" or the "sposition"
	  ;;    command
	  ;; -2 I am observing game being examined
	  ;;  2 I am the examiner of this game
	  ;; -1 I am playing, it is my opponent's move
	  ;;  1 I am playing and it is my move
	  ;;  0 I am observing a game being played
	  (string-to-int (match-string 19))))
    (when (or (= status 2) (= status -2) (= status 0))
      (chess-game-set-data game 'my-color (chess-pos-side-to-move position)))
    ;; initial time and increment (in seconds) of the match
    (chess-game-set-tag
     game "TimeControl" (format "%s/%s" (match-string 20) (match-string 21)))
    ;; material values for each side
    (let ((centipawn (* 100 (- (string-to-int (match-string 22))
			       (string-to-int (match-string 23))))))
      (chess-pos-set-epd position 'ce (if (chess-pos-side-to-move position)
					  centipawn (- centipawn))))
    ;; White's and Black's remaining time
    (chess-game-set-data game 'white-remaining (string-to-int (match-string 24)))
    (chess-game-set-data game 'black-remaining (string-to-int (match-string 25)))
    (let ((index (- (* (string-to-int (match-string 26)) 2)
		    (if (eq (chess-game-data game 'black-moved-first) t)
			(if (chess-pos-side-to-move position) 3 2)
		      (if (chess-pos-side-to-move position) 2 1))))
	  (move (unless (string= (match-string 29) "none")
		  (case (aref (match-string 29) (1- (length (match-string 29))))
		    (?+ (chess-pos-set-status position :check))
		    (?# (chess-pos-set-status position :checkmate)
			(chess-pos-set-epd position 'ce 32767)))
		  ;; jww (2002-04-30): what about stalemate?  do I need to
		  ;; calculate this each time?
		  (when nil
		    (chess-pos-set-status position :stalemate))
		  (match-string 29)))
	  error)
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
		    (setq error 'refresh) ; Ignore a "refresh" command
		  (if (and (> index (1+ (chess-game-index game)))
			     (= 1 (chess-game-seq game)))
		    ;; we lack a complete game, try to get it via the movelist
		    (progn
		      (setq error nil)
		      (chess-ics-send
		       (format "moves %d"
			       (chess-game-data game 'ics-game-number))))
		    (setq error
			  (format "comparing-index (%d:%d)" index (chess-game-index game))))))
	    ;; no preceeding ply supplied, so this is a starting position
	    (let ((chess-game-inhibit-events t)
		  (color (chess-pos-side-to-move position))
		  plies)
	      (when (or (= 1 status) (= -1 status))
		(chess-game-set-data game 'my-color (if (= 1 status)
							color (not color)))
		(chess-game-set-data game 'active t))
	      (setq error 'setting-start-position)
	      (chess-game-set-start-position game position)
	      (chess-game-set-data game 'black-moved-first (not color)))
	    (setq error 'orienting-board)
	    (chess-game-run-hooks game 'orient)
	    (setq error nil))
	(goto-char begin)
	(if error
	    (insert (chess-string 'failed-ics-parse error))
	  (delete-region begin end)
	  (save-excursion
	    (while (and (forward-line -1)
			(or (looking-at "^[ \t]*$")
			    (looking-at "^[^% \t\n\r]+%\\s-*$")))
	      (delete-region (match-beginning 0) (1+ (match-end 0)))))
	  ;; we need to counter the forward-line in chess-engine-filter
	  (forward-line -1)))
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

(defvar chess-ics-sought-parent-buffer nil
  "Contains the buffer from which this seektable originates.")
(make-variable-buffer-local 'chess-ics-sought-parent-buffer)

(defun chess-ics-sought-accept (&optional pos)
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
    (let ((command (get-char-property pos 'ics-command)))
      (when (stringp command)
	(chess-ics-send command chess-ics-sought-parent-buffer)
	t))))

(defvar chess-ics-popup-sought t
  "*If non-nil, display the sought buffer automatically.")

(defcustom chess-ics-sought-buffer-name "*chess-ics-sought*"
  "*The name of the buffer which accumulates seek ads."
  :group 'chess-ics
  :type 'string)

(defvar chess-ics-sought-sort-state nil
  "Determines the order for seek ads in the sought buffer.
If nil, do not sort entries, i.e., keep the order of arrival.")
(make-variable-buffer-local 'chess-ics-sought-sort-state)

(defvar chess-ics-sought-sort-direction nil
  "Determines the direction of sorting for seek ads in the sought buffer.
If nil, ads are sorted in ascending order, if non-nil, they are sorted in
descending order.")
(make-variable-buffer-local 'chess-ics-sought-sort-direction)

(defun chess-ics-sought-sort ()
  (case chess-ics-sought-sort-state
   (id     (sort-numeric-fields 1 (point-min) (point-max)))
   (player (sort-fields 2 (point-min) (point-max)))
   (rating (sort-numeric-fields 3 (point-min) (point-max)))
   (time   (sort-numeric-fields 5 (point-min) (point-max)))
   (inc    (sort-numeric-fields 6 (point-min) (point-max))))
  (and chess-ics-sought-sort-state
       chess-ics-sought-sort-direction
       (reverse-region (point-min) (point-max))))

(defun chess-ics-sought-toggle-sort-state ()
  (interactive)
  (setq chess-ics-sought-sort-state
	(case chess-ics-sought-sort-state
	  ((id) 'player)
	  ((player) 'rating)
	  ((rating) 'time)
	  ((time) 'inc)
	  ((inc) nil)
	  ((nil) 'id)))
  (message "Sorting ads by %s..."
	   (case chess-ics-sought-sort-state
	     ((id) "ID")
	     ((player) "player name")
	     ((rating) "rating (ascending)")
	     ((reverse-rating) "rating (descending)")
	     ((time) "initial time")
	     ((inc) "time increment")
	     ((nil) "arrival")))
  (chess-ics-sought-sort))

(defun chess-ics-sought-toggle-sort-direction ()
  (interactive)
  (message "Sorting %sscending direction..."
	   (if (setq chess-ics-sought-sort-direction
		     (not chess-ics-sought-sort-direction))
	       "de" "a"))
  (chess-ics-sought-sort))

(defcustom chess-ics-sought-mode-line-format
  '("-" mode-line-mule-info mode-line-modified mode-line-frame-identification
   "   "
   global-mode-string
   "   %[("
   (:eval (mode-line-mode-name))
   minor-mode-alist
   "%n"
   ")%]--"
   (:eval (format "[%d ads displayed]" (count-lines (point-min) (point-max))))
   "-%-")
  "Mode line data for ICS sought mode."
  :group 'chess-ics
  :type 'sexp)

(defvar chess-ics-sought-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'chess-ics-sought-accept)
    (define-key map [mouse-2] 'chess-ics-sought-accept)
    (define-key map [??] 'describe-mode)
    (define-key map [?s] 'chess-ics-sought-toggle-sort-state)
    (define-key map [? ] 'chess-ics-sought-toggle-sort-direction)
    map)
  "Keymap for `chess-ics-sought-mode'.")

(define-derived-mode chess-ics-sought-mode fundamental-mode "Seek Ads"
  "A mode for displaying ICS game seek advertisments."
  (let ((map (current-local-map)))
    (define-key map "\r" 'chess-ics-sought-accept)
    (define-key map [mouse-2] 'chess-ics-sought-accept)
    (define-key map [return] 'chess-ics-sought-accept)
    (define-key map [??] 'describe-mode)
    (define-key map [?s] 'chess-ics-sought-toggle-sort-state)
    (define-key map [? ] 'chess-ics-sought-toggle-sort-direction)
    (define-key map [?n] 'next-line)
    (define-key map [?p] 'previous-line))
  (setq sort-fold-case t
	mode-line-format chess-ics-sought-mode-line-format
	header-line-format
	'((3 . "ID") " "
	  (20 "Player") " "
	  (4 . "Elo") " "
	  "Rated" " "
	  (7 . "  Time") " "
	  "Variant%-")))

(defun chess-ics-sought-add (id name rating rated time inc variant
			     ics-buffer cmd)
  (setq id (concat id (make-string (- 4 (length id)) ? )))
  (setq name (concat name (make-string (- 20 (length name)) ? )))
  (setq variant (concat variant (make-string (- 25 (length variant)) ? )))
  (with-current-buffer
      (or (get-buffer chess-ics-sought-buffer-name)
	  (with-current-buffer (get-buffer-create
				chess-ics-sought-buffer-name)
	    (chess-ics-sought-mode)
	    (setq chess-ics-sought-parent-buffer ics-buffer)
	    (and chess-ics-popup-sought (display-buffer (current-buffer)))
	    (current-buffer)))
    (let ((here (point)))
      (when (re-search-forward (concat "^" (regexp-quote id) " ") nil t)
	(goto-char (line-beginning-position))
	(delete-region (point) (1+ (line-end-position))))
      (goto-char (point-min))
      (let ((beg (point)))
	(insert (format "%s %s %4d %4s  %3d/%3d %s"
			id name rating rated time inc variant))
	(add-text-properties
	 beg (point)
	 (list 'rear-nonsticky t
	       'mouse-face 'highlight
	       'ics-command cmd))
	(insert "\n"))
      (chess-ics-sought-sort)
      (goto-char here))))

(defun chess-ics-seeking (string)
  (while (string-match
	  (concat "[\n\r]+\\(\\S-+\\) (\\([0-9+ -]+\\)) seeking \\([a-z]\\S-+ \\)?\\([0-9]+\\) \\([0-9]+\\) \\(\\(un\\)?rated\\) \\([^(]*\\)(\"\\([^\"]+\\)\" to respond)\\s-*[\n\r]+"
		  chess-ics-prompt-regexp)
	  string)
    (let* ((pre (substring string 0 (match-beginning 0)))
	   (post (substring string (match-end 0))))
      (chess-ics-sought-add (substring (match-string 9 string) 5)
			    (match-string 1 string)
			    (string-to-int (match-string 2 string))
			    (if (string= (match-string 6 string) "rated")
				"yes" "no")
			    (string-to-int (match-string 4 string))
			    (string-to-int (match-string 5 string))
			    (concat
			     (if (match-string 3 string)
				 (concat (match-string 3 string) " ") "")
			     (match-string 8 string))
			    (current-buffer)
			    (match-string 9 string))
      (setq string (concat pre post))))
  string)

(defun chess-ics-ads-removed (string)
  "Look for Seek ad removal announcements in the output stream.
This function should be put on `comint-preoutput-filter-functions'."
  (let (ids)
    (while (string-match
	    (concat "[\n\r]+Ads removed: \\([0-9 ]+\\)\\s-*[\n\r]+"
		    chess-ics-prompt-regexp)
	    string)
      (setq ids (append (save-match-data
			  (split-string (match-string 1 string) " +")) ids))
      (setq string (concat (substring string 0 (match-beginning 0))
			   (substring string (match-end 0)))))
    (when ids
      (let ((buf (get-buffer chess-ics-sought-buffer-name)))
	(when (buffer-live-p buf)
	  (with-current-buffer buf
	    (let ((here (point)))
	      (while ids
		(goto-char (point-min))
		(when (re-search-forward (concat "^" (car ids) " ") nil t)
		  (delete-region (line-beginning-position)
				 (1+ (line-end-position))))
		(setq ids (cdr ids)))
	      (goto-char here)))))))
  string)

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
    (make-variable-buffer-local 'comint-preoutput-filter-functions)
    (setq comint-preoutput-filter-functions
	  '(chess-ics-ads-removed chess-ics-seeking))
    (let ((ntimes 50))
      (while (and chess-ics-handling-login
		  (> (setq ntimes (1- ntimes)) 0))
	(accept-process-output (get-buffer-process (current-buffer)) 0 100)))
    (switch-to-buffer buf)))

(defvar chess-icc-unprocessed nil)
(defun chess-icc-datagram-handler (string)
  (if (not (string-match "^\\([0-9]+\\) \\(.*\\)$" string))
      (format "\nUnknown datagram format: %s\n" string)
    (let ((chess-engine-handling-event t)
	  (dg (string-to-int (match-string 1 string)))
	  (args (match-string 2 string)))
      (cond
       ((and (or (= dg 22) (= dg 23))
	     (string-match "\\([0-9]+\\) \\([1-9][0-9]*\\)" args))
	(chess-game-undo (chess-ics-game (string-to-int (match-string 1 args)))
			 (string-to-int (match-string 2 args)))
	"")
       ((and (or (= dg 101) (= dg 110))
	     (string-match "\\([0-9]+\\) {\\(.+\\) \\(?:[0-9]+\\) \\(?:[0-9]+\\)} \\([0-9]+\\)" args))
	(let ((pos (chess-fen-to-pos (match-string 2 args))))
	  (chess-game-set-start-position
	   (chess-ics-game (string-to-int (match-string 1 args))) pos))
	"")
       ((and (or (= dg 24) (= dg 111))
	     (string-match "^\\([0-9]+\\) \\(.+\\)$" args))
	(let* ((move (match-string 2 args))
	       (game (chess-ics-game (string-to-int (match-string 1 args))))
	       (pos (chess-game-pos game))
	       (ply (chess-algebraic-to-ply pos move)))
	  (chess-game-move game ply)
	  ""))
       ((and (= dg 26)
	     (string-match "^\\([0-9]+\\) \\(\\S-+\\) {\\([^}]*\\)} \\([01]\\) {\\(.*\\)}"
			   args))
	(let ((game-number (match-string 1 args))
	      (action (if (string= (match-string 4 args) "1")
			  "kibitzes" "whispers"))
	      (name (match-string 2 args))
	      (titles (match-string 3 args))
	      (text (match-string 5 args)))
	  (setq name
		(concat name
			(mapconcat (lambda (title)
				     (concat "(" title ")"))
				   (split-string titles " ") "")))
	  (format "\n%s[%s] %s: %s\n" name game-number action text)))
       ((and (= dg 56)
	     (string-match "^\\([0-9]+\\) \\([WB]\\) \\([0-9]+\\) \\([01]\\)"
			   args))
	(let ((sec (/ (string-to-int (match-string 3 args)) 1000))
	      (color (if (string= (match-string 2 args) "W")
			 'white-remaining 'black-remaining))
	      (game (chess-ics-game (string-to-int (match-string 1 args)))))
	  (chess-game-set-data game color sec))
	"")
       ((and (= dg 50)
	     (string-match "^\\([0-9]+\\) \\(\\S-+\\) {\\([^}]*\\)} \\([0-9]+\\) \\([0-2]\\) \\([0-9]+\\) \\(\\S-+\\) \\([0-9]+\\) \\([0-9]+\\) \\([01]\\) \\(-?[01]\\) \\([0-9]+\\) \\([0-9]+\\) \\([01]\\) \\([01]\\) {\\([^}]*\\)}" args))
	(chess-ics-sought-add
	 (match-string 1 args)
	 (concat (match-string 2 args)
		 (if (not (string= (match-string 3 args) ""))
		     (format "(%s)" (match-string 3 args))
		   ""))
	 (string-to-int (match-string 4 args))
	 (if (string= (match-string 10 args) "1") "yes" "no")
	 (string-to-int (match-string 8 args))
	 (string-to-int (match-string 9 args))
	 (concat (match-string 7 args)
		 (if (not (string= (match-string 6 args) "0"))
		     (concat " " (match-string 6 args)) "")
		 (if (string= (match-string 14 args) "0")
		     " m" "")
		 (if (string= (match-string 15 args) "1")
		     " f" ""))
	 (current-buffer)
	 (concat "play " (match-string 1 args)))
	"")
       ((= dg 51)
	(let ((id (car (split-string args " ")))
	      (buf (get-buffer chess-ics-sought-buffer-name)))
	  (when (buffer-live-p buf)
	    (with-current-buffer buf
	      (let ((here (point)))
		(goto-char (point-min))
		(when (re-search-forward (concat "^" id " ") nil t)
		  (delete-region (line-beginning-position)
				 (1+ (line-end-position))))
		(goto-char here)))))
	"")
       (t
	(format "\nIgnoring datagram DG%03d: %s\n" dg args))))))
(defun chess-icc-preoutput-filter (string)
  (if chess-icc-unprocessed
      (let ((string (concat chess-icc-unprocessed string)))
	(if (string-match ")" string)
	    (let ((newstr (unwind-protect
			      (chess-icc-datagram-handler
			       (substring string 0 (match-beginning 0)))
			    (setq chess-icc-unprocessed nil))))
	      (chess-icc-preoutput-filter (concat (or newstr "")
						  (substring string
							     (match-end 0)))))
	  (setq chess-icc-unprocessed string)
	  ""))
    (if (string-match "(" string)
	(let ((pre (substring string 0 (match-beginning 0)))
	      (substr (substring string (match-end 0))))
	  (if (string-match ")" substr)
	      (let ((post (substring substr (match-end 0)))
		    (newstr (chess-icc-datagram-handler
			     (substring substr 0 (match-beginning 0)))))
		(chess-icc-preoutput-filter (concat pre newstr post)))
	    (setq chess-icc-unprocessed substr)
	    pre))
      string)))

(defun chess-ics-icc-preoutput-filter (string)
  (while (string-match "(\\([0-9]+\\) \\(.*?\\))" string)
    (let ((dg (string-to-int (match-string 1 string)))
	  (args (match-string 2 string))
	  (pre (substring string 0 (match-beginning 0)))
	  (post (substring string (match-end 0))))
      (cond
       ((and (or (= dg 101) (= dg 110))
	     (string-match "\\([0-9]+\\) {\\(.+\\) \\(?:[0-9]+\\) \\(?:[0-9]+\\)} \\([0-9]+\\)" args))
	(let ((pos (chess-fen-to-pos (match-string 2 args))))
	  (chess-game-set-start-position
	   (chess-ics-game (string-to-int (match-string 1 args))) pos))
	(setq string (concat pre post)))
       ((and (or (= dg 24) (= dg 111))
	     (string-match "\\([0-9]+\\) \\(.+\\)$" args))
	(let* ((chess-engine-handling-event t)
	       (move (match-string 2 args))
	       (game (chess-ics-game (string-to-int (match-string 1 args))))
	       (pos (chess-game-pos game))
	       (ply (chess-algebraic-to-ply pos move)))
	  (if ply
	      (chess-game-move game ply)
	    (setq pre (format "%s\nunable to apply move %s\n" pre move))))
	(setq string (concat pre post)))
       ((and (= dg 26)
	     (string-match "\\([0-9]+\\) \\(\\S-+\\) {\\([^}]*\\)} \\([01]\\) {\\(.*\\)}"
			   args))
	(let ((game-number (match-string 1 args))
	      (action (if (string= (match-string 4 args) "1")
			  "kibitzes" "whispers"))
	      (name (match-string 2 args))
	      (titles (match-string 3 args))
	      (text (match-string 5 args)))
	  (setq name
		(concat name
			(mapconcat (lambda (title)
				     (concat "(" title ")"))
				   (split-string titles " ") "")))
	  (setq string
		(format "%s\n%s[%s] %s: %s\n%s"
			pre name game-number action text post))))
       ((and (= dg 56)
	     (string-match "\\([0-9]+\\) \\([WB]\\) \\([0-9]+\\) \\([01]\\)"
			   args))
	(let ((sec (/ (string-to-int (match-string 3 args)) 1000))
	      (color (if (string= (match-string 2 args) "W")
			 'white-remaining 'black-remaining))
	      (game (chess-ics-game (string-to-int (match-string 1 args)))))
	  (chess-game-set-data game color sec))
	(setq string (concat pre post)))
       ((and (= dg 50)
	     (string-match "\\([0-9]+\\) \\(\\S-+\\) {\\([^}]*\\)} \\([0-9]+\\) \\([0-2]\\) \\([0-9]+\\) \\(\\S-+\\) \\([0-9]+\\) \\([0-9]+\\) \\([01]\\) \\(-?[01]\\) \\([0-9]+\\) \\([0-9]+\\) \\([01]\\) \\([01]\\) {\\([^}]*\\)}" args))
	(chess-ics-sought-add
	 (match-string 1 args)
	 (concat (match-string 2 args)
		 (if (not (string= (match-string 3 args) ""))
		     (format "(%s)" (match-string 3 args))
		   ""))
	 (string-to-int (match-string 4 args))
	 (if (string= (match-string 10 args) "1")
	     "yes" "no")
	 (string-to-int (match-string 8 args))
	 (string-to-int (match-string 9 args))
	 (concat (match-string 7 args)
		 (if (not (string= (match-string 6 args) "0"))
		     (concat " " (match-string 6 args)) "")
		 (if (string= (match-string 14 args) "0")
		     " m" "")
		 (if (string= (match-string 15 args) "1")
		     " f" ""))
	 (current-buffer)
	 (concat "play " (match-string 1 args)))
	(setq string (concat pre post)))
       ((= dg 51)
	(let ((id (car (split-string args " ")))
	      (buf (get-buffer chess-ics-sought-buffer-name)))
	  (when (buffer-live-p buf)
	    (with-current-buffer buf
	      (let ((here (point)))
		(goto-char (point-min))
		(when (re-search-forward (concat "^" id " ") nil t)
		  (delete-region (line-beginning-position)
				 (1+ (line-end-position))))
		(goto-char here)))))
	(setq string (concat pre post)))
       (t
	(message "Ignoring Datagram %03d: %s" dg args)
	(setq string (concat pre post))))))
  string)

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

     ((eq event 'undo)
      (chess-ics-send (format "takeback %d" (car args))
		      (chess-game-data game 'ics-buffer)))

     ((eq event 'abort)
      (chess-ics-send "abort" (chess-game-data game 'ics-buffer)))

     ((eq event 'call-flag)
      (chess-ics-send "flag" (chess-game-data game 'ics-buffer)))

     ((eq event 'draw)
      (chess-ics-send "draw" (chess-game-data game 'ics-buffer)))

     ((eq event 'resign)
      (chess-ics-send "resign" (chess-game-data game 'ics-buffer)))

     (t
      (apply 'chess-network-handler game event args)))))

(provide 'chess-ics)

;;; chess-ics.el ends here
