;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An engine for interacting with Internet Chess Servers
;;
;; $Revision$

(require 'chess-network)
(require 'ics)

(defgroup chess-ics nil
  "Engine for interacting with Internet Chess Servers."
  :group 'chess-engine)

(defcustom chess-ics-server "freechess.org"
  "The default ICS server to connect to."
  :type 'string
  :group 'chess-ics)

(defcustom chess-ics-port 5000
  "The port to use when connecting to `chess-ics-server'."
  :type 'integer
  :group 'chess-ics)

(defcustom chess-ics-handle "jwiegley"
  "The default handle used when logging into `chess-ics-server'."
  :type 'string
  :group 'chess-ics)

(defvar chess-ics-ensure-ics12 nil)
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
	white black ply)

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
	(info (chess-ics12-parse (match-string 3)))
	(game (chess-engine-game nil)))
    (assert game)
    (unwind-protect
	(if (and (chess-game-data game 'active)
		 (> (chess-game-index game) 0))
	    (if (and (cadr info)
		     (eq (chess-pos-side-to-move (car info))
			 (chess-game-data game 'my-color)))
		(let ((last-ply (last (chess-game-plies game))))
		  (setcar last-ply
			  (chess-algebraic-to-ply (chess-ply-pos (car last-ply))
						  (cadr info) t))
		  (chess-game-add-ply game (chess-ply-create (car info)))
		  (chess-game-run-hooks game 'update)))
	  (let ((chess-game-inhibit-events t) plies)
	    (chess-game-set-data game 'my-color (string= (nth 2 info)
							 chess-ics-handle))
	    (chess-game-set-data game 'active t)
	    (chess-game-set-start-position game (car info)))
	  (chess-game-run-hooks game 'orient))
      (delete-region begin end))
    t))

(defvar chess-ics-regexp-alist
  (list (cons "\\(\\(\n*fics%\n*\\)?<12> \\(.+\\)\\)\n"
	      'chess-ics-handle-move)
	(cons "Challenge: \\(\\S-+\\) \\S-+ \\S-+ \\S-+ .+"
	      (function
	       (lambda ()
		 (funcall chess-engine-response-handler 'match
			  (match-string 1)))))))

(defun chess-ics-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (message "Connecting to Internet Chess Server '%s'..." chess-ics-server)

    (let ((buf (make-comint "chess-ics"
			    (cons chess-ics-server chess-ics-port))))
      (message "Connecting to Internet Chess Server '%s'...done"
	       chess-ics-server)

      (display-buffer buf)
      (set-buffer buf)

      (add-hook 'comint-output-filter-functions 'chess-ics-filter t t)
      (set (make-local-variable 'comint-preoutput-filter-functions)
	   '(chess-ics-strip))
      nil))

   ((eq event 'match)
    (setq chess-engine-pending-offer 'match)
    (chess-engine-send nil (format "match %s\n"
				   (read-string "Whom would you like challenge? "))))

   ((eq event 'move)
    (unless chess-ics-ensure-ics12
      (chess-engine-send nil "set style 12\n")
      (setq chess-ics-ensure-ics12 t))
    (chess-network-handler 'move (car args)))

   ((eq event 'send)
    (message "sending string: %s" (car args))
    (comint-send-string (get-buffer-process (current-buffer))
			(car args)))

   (t
    (apply 'chess-network-handler event args))))

(defun chess-ics-filter (string)
  (save-excursion
    (if chess-engine-last-pos
	(goto-char chess-engine-last-pos)
      (goto-char (point-min)))
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
    (setq chess-engine-last-pos (point))))

(defun chess-ics-strip (string)
  (while (string-match "[\r\a]" string)
    (setq string (replace-match "" t t string)))
  string)

(provide 'chess-ics)

;;; chess-ics.el ends here
