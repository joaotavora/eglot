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

;; ICS12 format:
;; <12> rnbqkbnr pppppppp -------- -------- -------- -------- PPPPPPPP RNBQKBNR W -1 1 1 1 1 0 65 jwiegley GuestZYNJ 1 5 0 39 39 300 300 1 none (0:00) none 0 0 0

(defun chess-ics-handle-move ()
  (let ((begin (match-beginning 1))
	(end (match-end 1))
	(color (string= (match-string 2) "W"))
	(white (match-string 3))
	(move (match-string 4)))
    (if (and (not (string= white ics-handle))
	     (= 0 (chess-game-index (chess-engine-game nil))))
	(chess-game-run-hooks (chess-engine-game nil) 'pass)
      (if (eq color (chess-pos-side-to-move
		     (chess-engine-position nil)))
	  (funcall chess-engine-response-handler
		   'move move))
      (delete-region begin end))))

(defvar chess-ics-regexp-alist
  (list
   (cons (concat "\\(<12> \\S-+ \\S-+ \\S-+ \\S-+ \\S-+ \\S-+ \\S-+ \\S-+ "
		 "\\([BW]\\) [-0-9]+ "
		 "[-0-9]+ [-0-9]+ [-0-9]+ [-0-9]+ [-0-9]+ "
		 "[-0-9]+ \\(\\S-+\\) \\S-+ "
		 "[-0-9]+ [-0-9]+ [-0-9]+ "
		 "[-0-9]+ [-0-9]+ [-0-9]+ [-0-9]+ "
		 "[-0-9]+ \\S-+ \\S-+ \\(\\S-+\\)\\)")
	 'chess-ics-handle-move)
   (cons "You accept the match offer from \\([^\\.]+\\)."
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
    (ignore-errors
      (chess-engine-send nil "quit\n")))

   ((eq event 'move)
    (unless chess-ics-ensure-ics12
      (comint-send-string (get-buffer-process (current-buffer))
			  "set style 12\n")
      (setq chess-ics-ensure-ics12 t))
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))

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
	  (if (looking-at (concat "[^\n\r]*" (caar triggers)))
	      (progn
		(funcall (cdar triggers))
		(setq triggers nil))
	    (setq triggers (cdr triggers)))))
      (forward-line))
    (setq chess-engine-last-pos (point))))

(defun chess-ics-strip-cr (string)
  (while (string-match "\r" string)
    (setq string (replace-match "" t t string)))
  string)

(provide 'chess-ics)

;;; chess-ics.el ends here
