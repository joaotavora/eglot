;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An engine for interacting with Internet Chess Servers
;;
;; jww (2002-04-23): This module has only been tested on FICS.
;;

(require 'comint)
(require 'chess-network)

(defgroup chess-ics nil
  "Engine for interacting with Internet Chess Servers."
  :group 'chess-engine)

(defcustom chess-ics-server-list
  '(("freechess.org" 5000))
  "A list of servers to connect to.
The format of each entry is:

  (SERVER PORT [HANDLE] [PASSWORD-OR-FILENAME] [HELPER] [HELPER ARGS...])"
  :type '(repeat (list (string :tag "Server")
		       (integer :tag "Port")
		       (string :tag "Handle")
		       (choice (const :tag "No password or ask" nil)
			       (string :tag "Password")
			       (file :tag "Filename"))
		       (choice (const :tag "Direct connection" nil)
			       (file :tag "Command"))
		       (choice (const :tag "No arguments" nil)
			       (repeat string))))
  :group 'chess-ics)

(defvar chess-ics-handle)
(defvar chess-ics-prompt)

(make-variable-buffer-local 'chess-ics-handle)
(make-variable-buffer-local 'chess-ics-prompt)

;; ICS12 format (with artificial line breaks):
;;
;; <12> rnbqkbnr pppppppp -------- -------- \
;;      -------- -------- PPPPPPPP RNBQKBNR W \
;;      -1 1 1 1 1 0 65 jwiegley GuestZYNJ \
;;      1 5 0 39 39 300 300 1 P/e2-e4 (0:00) e4 0 0 0

(defun chess-ics12-parse (string)
  "Parse an ICS12 format string, and return a list of its info.
The list is comprised of: the ply the string represents, who is white,
who is black."
  (let ((parts (split-string string " "))
	(position (chess-pos-create t))
	white black white-time black-time move)

    (assert (= (length parts) 32))

    ;; first, handle the layout of the position
    (dotimes (i 8)
      (dotimes (j 8)
	(let ((piece (aref (car parts) j)))
	  (unless (= piece ?-)
	    (chess-pos-set-piece position (chess-rf-to-index i j)
				 piece))))
      (setq parts (cdr parts)))

    ;; next, the "side to move"
    (chess-pos-set-side-to-move position (string= (car parts) "W"))
    (setq parts (cdr parts))

    ;; -1 if the previous move was NOT a double pawn push, otherwise
    ;; the chess board file (numbered 0--7 for a--h) in which the
    ;; double push was made
    (let ((index (string-to-number (car parts))))
      (when (>= index 0)
	(chess-pos-set-en-passant
	 position (chess-rf-to-index
		   (if (chess-pos-side-to-move position) 3 4) index))))
    (setq parts (cdr parts))

    ;; can White still castle short? (0=no, 1=yes)
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?K t))
    (setq parts (cdr parts))
    ;; can White still castle long?
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?Q t))
    (setq parts (cdr parts))
    ;; can Black still castle short?
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?k t))
    (setq parts (cdr parts))
    ;; can Black still castle long?
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?q t))
    (setq parts (cdr parts))

    ;; the number of moves made since the last irreversible move.  (0
    ;; if last move was irreversible.  If the value is >= 100, the
    ;; game can be declared a draw due to the 50 move rule.)
    (setq parts (cdr parts))

    ;; The game number
    (setq parts (cdr parts))

    ;; white player, black player
    (setq white (car parts) parts (cdr parts))
    (setq black (car parts) parts (cdr parts))

    ;; my relation to this game:
    ;; -3 isolated position, such as for "ref 3" or the "sposition"
    ;;    command
    ;; -2 I am observing game being examined
    ;;  2 I am the examiner of this game
    ;; -1 I am playing, it is my opponent's move
    ;;  1 I am playing and it is my move
    ;;  0 I am observing a game being played
    (setq parts (cdr parts))

    ;;  initial time (in seconds) of the match
    (setq parts (cdr parts))

    ;;  increment In seconds) of the match
    (setq parts (cdr parts))

    ;; material values for each side
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    ;;  White's and Black's remaining time
    (setq white-time (string-to-number (car parts)))
    (setq parts (cdr parts))
    (setq black-time (string-to-number (car parts)))
    (setq parts (cdr parts))

    ;; the number of the move about to be made (standard chess
    ;; numbering -- White's and Black's first moves are both 1, etc.)
    (setq parts (cdr parts))

    ;; move in long alegebraic notation
    (setq parts (cdr parts))

    ;; time taken to make previous move "(min:sec)".
    (setq parts (cdr parts))

    ;; move in short algebraic notation (SAN)
    (setq move (unless (string= (car parts) "none")
		 (car parts)))
    (setq parts (cdr parts))

    ;; flip field for board orientation: 1 = Black at bottom, 0 =
    ;; White at bottom.
    (setq parts (cdr parts))

    ;; jww (2002-04-18): what do these two mean?
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    (list position move white black white-time black-time)))

(defun chess-ics-handle-move ()
  (let ((chess-engine-handling-event t)
	(begin (match-beginning 0))
	(end (match-end 0))
	(info (chess-ics12-parse (match-string 1))))
    (if (and (chess-game-data (chess-engine-game nil) 'active)
	     (> (chess-engine-index nil) 0))
	(when (and (cadr info)
		   (eq (chess-pos-side-to-move (car info))
		       (chess-game-data (chess-engine-game nil) 'my-color)))
	  (let* ((game (chess-engine-game nil))
		 (ply (chess-algebraic-to-ply
		       (chess-ply-pos (car (last (chess-game-plies game))))
		       (cadr info) t)))
	    (chess-game-set-data game 'white-remaining (nth 4 info))
	    (chess-game-set-data game 'black-remaining (nth 5 info))
	    (chess-game-move game ply))
	  (assert (equal (car info) (chess-engine-position nil))))
      (let ((chess-game-inhibit-events t) plies)
	(chess-game-set-data (chess-engine-game nil)
			     'my-color (string= (nth 2 info) chess-ics-handle))
	(chess-game-set-data (chess-engine-game nil) 'active t)
	(chess-game-set-start-position (chess-engine-game nil) (car info)))
      (chess-game-run-hooks (chess-engine-game nil) 'orient))
    (goto-char begin)
    (delete-region begin end)
    ;; we need to counter the `forward-line' in `chess-engine-filter'
    (forward-line -1)
    t))

(defvar chess-ics-regexp-alist
  (list (cons "^[^% \t\n\r]+%"
	      (function
	       (lambda ()
		 (chess-engine-send nil "set style 12\n")
		 (chess-engine-send nil "set bell 0\n")
		 'once)))
	(cons "Logging you in as \"\\([^\"]+\\)\""
	      (function
	       (lambda ()
		 (setq chess-ics-handle (match-string 1))
		 'once)))
	(cons "The game has been aborted on move [^.]+\\."
	      (function
	       (lambda ()
		 (let ((chess-engine-pending-offer 'abort))
		   (funcall chess-engine-response-handler 'accept)))))
	(cons "<12> \\(.+\\)"
	      'chess-ics-handle-move)
	(cons "Challenge: \\(\\S-+\\) \\S-+ \\S-+ \\S-+ .+"
	      (function
	       (lambda ()
		 (funcall chess-engine-response-handler 'match
			  (match-string 1)))))))

(chess-message-catalog 'english
  '((ics-server-prompt . "Connect to chess server: ")
    (ics-connecting    . "Connecting to Internet Chess Server '%s'...")
    (ics-connected     . "Connecting to Internet Chess Server '%s'...done")
    (challenge-whom    . "Whom would you like challenge? ")))

(defun chess-ics-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (kill-buffer (current-buffer))
      (let ((server
	     (if (= (length chess-ics-server-list) 1)
		 (car chess-ics-server-list)
	       (assoc (completing-read (chess-string 'ics-server-prompt)
				       chess-ics-server-list
				       nil t (caar chess-ics-server-list))
		      chess-ics-server-list))))

	(chess-message 'ics-connecting (nth 0 server))

	(let ((buf (apply 'make-comint "chess-ics"
			  (if (nth 4 server)
			      (cons (nth 4 server) (nth 5 server))
			    (list (cons (nth 0 server) (nth 1 server)))))))

	  (chess-message 'ics-connected (nth 0 server))

	  (display-buffer buf)
	  (set-buffer buf)

	  (add-hook 'comint-output-filter-functions 'chess-engine-filter t t)

	  (setq comint-prompt-regexp "^[^%\n]*% *"
		comint-scroll-show-maximum-output t)

	  (let ((proc (get-buffer-process (current-buffer))))
	    (if (null (nth 2 server))
		(comint-send-string proc "guest\n")
	      (setq chess-ics-handle (nth 2 server))
	      (comint-send-string proc (concat chess-ics-handle "\n"))
	      (let ((pass (or (nth 3 server)
			      (read-passwd "Password: "))))
		(if (file-readable-p pass)
		    (setq pass (with-temp-buffer
				 (insert-file-contents file)
				 (buffer-string))))
		(comint-send-string proc (concat pass "\n")))))))
      t)

     ((eq event 'match)
      (setq chess-engine-pending-offer 'match)
      (chess-engine-send
       nil (format "match %s\n"
		   (read-string (chess-string 'challenge-whom)))))

     ((eq event 'send)
      (comint-send-string (get-buffer-process (current-buffer))
			  (car args)))

     (t
      (apply 'chess-network-handler game event args)))))

(provide 'chess-ics)

;;; chess-ics.el ends here
