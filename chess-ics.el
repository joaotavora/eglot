;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An engine for interacting with Internet Chess Servers
;;
;; $Revision$

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
		       (choice (const :tag "No password or ask" nil)
			       (string :tag "Password")
			       (file :tag "Filename"))
		       (choice (const :tag "Direct connection" nil)
			       (file :tag "Command"))
		       (choice (const :tag "No arguments" nil)
			       (repeat string))))
  :group 'chess-ics)

(defvar chess-ics-handle)
(defvar chess-ics-ensure-ics12 nil)

(make-variable-buffer-local 'chess-ics-handle)
(make-variable-buffer-local 'chess-ics-ensure-ics12)

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
	white black move)

    (assert (= (length parts) 32))

    ;; first, handle the layout of the position
    (dotimes (i 8)
      (dotimes (j 8)
	(let ((piece (aref (car parts) j)))
	  (unless (= piece ?-)
	    (chess-pos-set-piece position (chess-rf-to-index i j)
				 piece))))
      (setq parts (cdr parts)))

    ;; next, the "side to move
    (chess-pos-set-side-to-move position (string= (car parts) "W"))
    (setq parts (cdr parts))

    ;; unknown
    (setq parts (cdr parts))

    ;; castling rights?
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?K t))
    (setq parts (cdr parts))
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?Q t))
    (setq parts (cdr parts))
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?k t))
    (setq parts (cdr parts))
    (if (string= (car parts) "1")
	(chess-pos-set-can-castle position ?q t))
    (setq parts (cdr parts))

    ;; jww (2002-04-11): How is check indicated?

    ;; unknown
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    ;; white player, black player
    (setq white (car parts))
    (setq parts (cdr parts))
    (setq black (car parts))
    (setq parts (cdr parts))

    ;; unknown
    (setq parts (cdr parts))
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    ;; material values for each side
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    ;; starting time each side
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    ;; unknown
    (setq parts (cdr parts))

    ;; move in elaborated notation
    (setq parts (cdr parts))

    ;; time elapsed
    (setq parts (cdr parts))

    ;; move in algebraic notation
    (setq move (unless (string= (car parts) "none")
		 (car parts)))
    (setq parts (cdr parts))

    ;; unknown
    (setq parts (cdr parts))
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    (list position move white black)))

(defun chess-ics-handle-move ()
  (let ((chess-engine-handling-event t)
	(begin (match-beginning 1))
	(end (match-end 1))
	(info (chess-ics12-parse (match-string 3))))
    (if (and (chess-game-data chess-engine-game 'active)
	     (> (chess-game-index chess-engine-game) 0))
	(when (and (cadr info)
		   (eq (chess-pos-side-to-move (car info))
		       (chess-game-data chess-engine-game 'my-color)))
	  (chess-game-move chess-engine-game
			   (chess-algebraic-to-ply
			    (chess-ply-pos
			     (car (last (chess-game-plies chess-engine-game))))
			    (cadr info) t))
	  (assert (equal (car info) (chess-engine-position nil))))
      (let ((chess-game-inhibit-events t) plies)
	(chess-game-set-data chess-engine-game
			     'my-color (string= (nth 2 info) chess-ics-handle))
	(chess-game-set-data chess-engine-game 'active t)
	(chess-game-set-start-position chess-engine-game (car info)))
      (chess-game-run-hooks chess-engine-game 'orient))
    (delete-region begin end)
    t))

(defvar chess-ics-regexp-alist
  (list (cons "\\(\\(\n*fics%\n*\\)?<12> \\(.+\\)\\)\n"
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

(defun chess-ics-handler (event &rest args)
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

      (chess-message 'ics-connecting (car server))

      (let ((buf (apply 'make-comint "chess-ics"
			(if (nth 3 server)
			    (cons (nth 4 server) (nth 5 server))
			  (list (cons (nth 0 server) (nth 1 server)))))))

	(chess-message 'ics-connected (car server))

	(display-buffer buf)
	(set-buffer buf)

	(add-hook 'comint-output-filter-functions 'chess-ics-filter t t)
	(set (make-local-variable 'comint-preoutput-filter-functions)
	     '(chess-ics-strip))

	(if (nth 2 server)
	    (progn
	      (setq chess-ics-handle (nth 2 server))
	      (comint-send-string (concat chess-ics-handle "\n"))
	      (let ((pass (nth 3 server)))
		(when pass
		  (if (file-readable-p pass)
		      (setq pass (with-temp-buffer
				   (insert-file-contents file)
				   (buffer-string))))
		  (comint-send-string (concat pass "\n")))))
	  ;; jww (2002-04-13): Have to parse out the allocated Guest
	  ;; name from the output
	  (comint-send-string "guest\n\n"))))

      nil)

   ((eq event 'match)
    (setq chess-engine-pending-offer 'match)
    (chess-engine-send
     nil (format "match %s\n"
		 (read-string (chess-string 'challenge-whom)))))

   ((eq event 'move)
    (unless chess-ics-ensure-ics12
      (chess-engine-send nil "set style 12\n")
      (setq chess-ics-ensure-ics12 t))
    (chess-network-handler 'move (car args)))

   ((eq event 'send)
    (comint-send-string (get-buffer-process (current-buffer))
			(car args)))

   (t
    (apply 'chess-network-handler event args))))

(defun chess-ics-filter (string)
  (save-excursion
    (if chess-engine-last-pos
	(goto-char chess-engine-last-pos)
      (goto-char (point-min)))
    (unwind-protect
	(while (and (not (eobp))
		    (/= (line-end-position) (point-max)))
	  (let ((triggers chess-ics-regexp-alist))
	    (while triggers
	      ;; this could be accelerated by joining
	      ;; together the regexps
	      (if (and (looking-at (concat "[^\n\r]*" (caar triggers)))
		       (funcall (cdar triggers)))
		  (setq triggers nil)
		(setq triggers (cdr triggers)))))
	  (forward-line))
      (setq chess-engine-last-pos (point)))))

(defun chess-ics-strip (string)
  (while (string-match "[\r\a]" string)
    (setq string (replace-match "" t t string)))
  string)

(provide 'chess-ics)

;;; chess-ics.el ends here
