;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This transport uses an IRC bot to send/receive moves.
;;
;; $Revision$

(require 'chess-network)

(defgroup chess-irc nil
  "Use an IRC bot for sending/receiving moves."
  :group 'chess-engine)

(defcustom chess-irc-host "irc.openprojects.net"
  "The IRC host to connect your chess-irc engine to."
  :type 'string
  :group 'chess-irc)

(defcustom chess-irc-port 6667
  "The port of the IRC host specified by `chess-irc-host'."
  :type 'string
  :group 'chess-irc)

(defcustom chess-irc-nick (user-login-name)
  "The nick you wish to use for sending/receiving IRC chess moves."
  :type 'string
  :group 'chess-irc)

(defvar chess-irc-regexp-alist
  (append chess-network-regexp-alist
	  (list (cons ".+"
		      (function
		       (lambda ()
			 (message "Your opponent says: %s"
				  (match-string 0))))))))

(defvar chess-irc-process)
(defvar chess-irc-engine)
(defvar chess-irc-opponent)
(defvar chess-irc-working nil)
(defvar chess-irc-last-pos nil)

(make-variable-buffer-local 'chess-irc-process)
(make-variable-buffer-local 'chess-irc-engine)
(make-variable-buffer-local 'chess-irc-opponent)
(make-variable-buffer-local 'chess-irc-working)
(make-variable-buffer-local 'chess-irc-last-pos)

(defun chess-irc-handler (event &rest args)
  "This is an example of a generic transport engine."
  (cond
   ((eq event 'initialize)
    (message "Connecting to IRC server '%s:%d'..."
	     chess-irc-host chess-irc-port)
    (let ((engine (current-buffer)) proc)
      (with-current-buffer (generate-new-buffer " *chess-irc*")
	(setq chess-irc-engine engine
	      proc (open-network-stream "*chess-irc*" (current-buffer)
					chess-irc-host chess-irc-port))
	(message "Connected, now logging in as '%s'..." chess-irc-nick)
	(when (and proc (eq (process-status proc) 'open))
	  (process-send-string proc (format "USER %s 0 * :%s\n"
					    (user-login-name)
					    (user-full-name)))
	  (process-send-string proc (format "NICK %s\n" chess-irc-nick))
	  (set-process-filter proc 'chess-irc-filter)
	  (set-process-buffer proc (current-buffer))
	  (set-marker (process-mark proc) (point))
	  (message "Now waiting for 'name USER' via /msg, or `M-x chess-irc-engage'")))
      (setq chess-irc-process proc))
    nil)

   ((eq event 'shutdown)
    (ignore-errors
      (chess-engine-send nil "quit"))
    (ignore-errors
      (process-send-string chess-irc-process "QUIT :Goodbye\n"))
    (ignore-errors
      (kill-buffer (process-buffer chess-irc-process))))

   ((eq event 'send)
    (process-send-string chess-irc-process
			 (format "PRIVMSG %s :%s\n"
				 chess-irc-opponent (car args))))
   (t
    (apply 'chess-network-handler event args))))

(defun chess-irc-engage (nick)
  "Begin playing with another chess-irc user with the given NICK.
NOTE: This function is meant to be called from a display buffer!"
  (interactive "sYour opponent's IRC nick: ")
  (with-current-buffer
      (cdr (assq 'chess-engine-event-handler
		 (chess-game-hooks (chess-display-game nil))))
    (setq chess-irc-opponent nick)
    (chess-engine-send nil (format "name %s\n" (user-full-name)))))

;; This filter translates IRC syntax into basic chess-network protocol
(defun chess-irc-filter (proc string)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(let ((moving (= (point) (process-mark proc))))
	  (save-excursion
	    ;; Insert the text, advancing the marker.
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
	(unless chess-irc-working
	  (setq chess-irc-working t)
	  (unwind-protect
	      (progn
		(if chess-irc-last-pos
		    (goto-char chess-irc-last-pos)
		  (goto-char (point-min)))
		(beginning-of-line)
		(while (not (eobp))
		  (cond
		   ((looking-at
		     ":\\([^ \t\n!]+\\)!\\S-+ PRIVMSG \\(\\S-+\\) :\\(.+\\)")
		    (let ((sender (match-string 1))
			  (target (match-string 2))
			  (msg (match-string 3)))
		    (with-current-buffer chess-irc-engine
		      (when (and (string= chess-irc-nick target)
				 (or (null chess-irc-opponent)
				     (string= chess-irc-opponent sender)))
			(unless chess-irc-opponent
			  (setq chess-irc-opponent sender))
			(chess-engine-submit nil (concat msg "\n")))))))
		  (forward-line)))
	    (setq chess-irc-last-pos (point)
		  chess-irc-working nil)))))))

(provide 'chess-irc)

;;; chess-irc.el ends here
