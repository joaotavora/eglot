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

    (assert (= (length parts) 31))

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
    (setq ply (if (string= (car parts) "none")
		  (chess-ply-create position)
		(chess-algebraic-to-ply position (substring (car parts) 2))))
    (setq parts (cdr parts))

    ;; time elapsed
    (setq parts (cdr parts))

    ;; move in algebraic notation
    (setq parts (cdr parts))

    ;; unknown
    (setq parts (cdr parts))
    (setq parts (cdr parts))
    (setq parts (cdr parts))

    (list ply white black)))

(defun chess-ics-handle-move ()
  (let ((begin (match-beginning 1))
	(end (match-end 1))
	(info (chess-ics12-parse (match-string 2)))
	(game (chess-engine-game nil)))
    (assert game)
    (if (> (chess-game-index game) 0)
	(if (eq (chess-pos-side-to-move (chess-ply-pos (car info)))
		(chess-pos-side-to-move (chess-engine-position nil)))
	    (chess-engine-do-move (car info)))
      (chess-engine-set-start-position nil (chess-ply-pos (car info))
				       (string= (cadr info) ics-handle)))
    (delete-region begin end)
    t))

(defvar chess-ics-regexp-alist
  (list (cons "\\(<12> \\(.+\\)\\)" 'chess-ics-handle-move)
	(cons "Challenge: \\(\\S-+\\) \\S-+ \\S-+ \\S-+ .+"
	      (function
	       (lambda ()
		 (funcall chess-engine-response-handler 'connect
			  (match-string 1)))))))

(defun chess-ics-handler (event &rest args)
  (cond
   ((eq event 'initialize)
    (let* ((old-buffer (current-buffer))
	   (address-or-alias (read-from-minibuffer
			      "ICS Server address or alias: "))
	   (server-info-list (cdr (assoc address-or-alias
					 ics-servers-alist)))
	   (ics-address (or (car (cdr server-info-list))
			    address-or-alias))
	   (ics-connect-method (or (car (nthcdr 3 server-info-list))
				   ics-default-connect-method))
	   (server-name (or (car server-info-list)
			    address-or-alias))
	   (ics-port (or (car (nthcdr 2 server-info-list))
			 (read-from-minibuffer "ICS port: "
					       ics-default-port)))
	   (handle (read-from-minibuffer "ICS Handle: "
					 ics-default-handle))
	   (proc (concat server-name ":" handle))
	   (buffer (concat "*" proc "*")))

      (setq ics-handle handle)

      (if (comint-check-proc buffer)
	  (set-buffer buffer)
	(run-hooks 'ics-pre-connect-hook)
	(set-buffer (make-comint proc (cons ics-address ics-port)))
	(run-hooks 'ics-post-connect-hook)
	(ics-mode))

      (set (make-variable-buffer-local 'ics-last-command-time)
	   (ics-current-time))
      (set (make-variable-buffer-local 'ics-idle-p) nil)
      (set (make-variable-buffer-local 'ics-interface-variable-set) nil)
      (set (make-variable-buffer-local 'ics-wakeup-last-alarm-time)
	   (ics-current-time))
      (set (make-variable-buffer-local 'ics-last-highlight-end) nil)
      (set (make-variable-buffer-local 'ics-last-add-buttons-end) nil)

      (add-hook 'comint-output-filter-functions 'chess-ics-filter t t)
      (set (make-local-variable 'comint-preoutput-filter-functions)
	   '(chess-ics-strip-cr))

      (display-buffer buffer)
      (kill-buffer old-buffer)

      nil))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n"))

   ((eq event 'move)
    (unless chess-ics-ensure-ics12
      (comint-send-string (get-buffer-process (current-buffer))
			  "set style 12\n")
      (setq chess-ics-ensure-ics12 t))
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))

   ((eq event 'accept)
    (chess-engine-send nil "accept\n"))

   ((eq event 'decline)
    (chess-engine-send nil "decline\n"))

   ((eq event 'resign)
    (chess-engine-send nil "resign\n"))

   ((eq event 'send)
    (comint-send-string (get-buffer-process (current-buffer)) (car args)))))

(defun chess-ics-filter (string)
  (save-excursion
    (if chess-engine-last-pos
	(goto-char chess-engine-last-pos)
      (goto-char (point-min)))
    (beginning-of-line)
    (while (not (eobp))
      (let ((triggers chess-ics-regexp-alist))
	(while triggers
	  ;; this could be accelerated by joining together the
	  ;; regexps
	  (if (and (looking-at (concat "[^\n\r]*" (caar triggers)))
		   (funcall (cdar triggers)))
	      (setq triggers nil)
	    (setq triggers (cdr triggers)))))
      (forward-line))
    (setq chess-engine-last-pos (point))))

(defun chess-ics-strip-cr (string)
  (while (string-match "\r" string)
    (setq string (replace-match "" t t string)))
  string)

(provide 'chess-ics)

;;; chess-ics.el ends here
