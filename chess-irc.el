;;; chess-irc.el --- This transport uses an IRC bot to send/receive moves.

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'chess-network)

(defgroup chess-irc nil
  "Use an IRC bot for sending/receiving moves."
  :group 'chess-engine)

(defcustom chess-irc-server "irc.openprojects.net"
  "The IRC host to connect your chess-irc engine to."
  :type 'string
  :group 'chess-irc)

(defcustom chess-irc-port 6667
  "The port of the IRC host specified by `chess-irc-server'."
  :type 'string
  :group 'chess-irc)

(defcustom chess-irc-nick (user-login-name)
  "The nick you wish to use for sending/receiving IRC chess moves."
  :type 'string
  :group 'chess-irc)

;;; Code:

(chess-message-catalog 'english
  '((opponent-says  . "Your opponent says: %s")
    (irc-connecting . "Connecting to IRC server '%s:%d'...")
    (irc-logging-in . "Connected, now logging in as '%s'...")
    (irc-waiting    . "Now waiting for 'name USER' via /msg, or `M-x chess-irc-engage'")
    (irc-challenge  . "IRC nick of user to challenge: ")))

(defvar chess-irc-regexp-alist
  (append chess-network-regexp-alist
	  (list (cons ".+"
		      (function
		       (lambda ()
			 (chess-message 'opponent-says
					(match-string 0))))))))

(defvar chess-irc-process)
(defvar chess-irc-engine)
(defvar chess-irc-opponent)
(defvar chess-irc-working nil)
(defvar chess-irc-last-pos nil)
(defvar chess-irc-use-ctcp nil)

(make-variable-buffer-local 'chess-irc-process)
(make-variable-buffer-local 'chess-irc-engine)
(make-variable-buffer-local 'chess-irc-opponent)
(make-variable-buffer-local 'chess-irc-working)
(make-variable-buffer-local 'chess-irc-last-pos)
(make-variable-buffer-local 'chess-irc-use-ctcp)

(defun chess-irc-handler (game event &rest args)
  "This is an example of a generic transport engine."
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (chess-message 'irc-connecting chess-irc-server chess-irc-port)
      (let ((engine (current-buffer)) proc)
	(with-current-buffer (generate-new-buffer " *chess-irc*")
	  (setq chess-irc-engine engine
		proc (open-network-stream "*chess-irc*" (current-buffer)
					  chess-irc-server chess-irc-port))
	  (chess-message 'irc-logging-in chess-irc-nick)
	  (when (and proc (processp proc)
		     (eq (process-status proc) 'open))
	    (process-send-string proc (format "USER %s 0 * :%s\n"
					      (user-login-name)
					      chess-full-name))
	    (process-send-string proc (format "NICK %s\n" chess-irc-nick))
	    (set-process-filter proc 'chess-irc-filter)
	    (set-process-buffer proc (current-buffer))
	    (set-marker (process-mark proc) (point))
	    (chess-message 'irc-waiting)))
	(setq chess-irc-process proc))
      t)

     ((eq event 'match)
      (setq chess-irc-opponent (read-string (chess-string 'irc-challenge)))
      (chess-network-handler 'match chess-irc-opponent))

     ((eq event 'destroy)
      (chess-engine-send nil "quit")
      (process-send-string chess-irc-process "QUIT :Goodbye\n")
      (kill-buffer (process-buffer chess-irc-process)))

     ((eq event 'send)
      (process-send-string chess-irc-process
			   (if chess-irc-use-ctcp
			       (format "PRIVMSG %s :\C-aCHESS %s\C-a\n"
				       chess-irc-opponent (car args))
			     (format "PRIVMSG %s :%s\n"
				     chess-irc-opponent (car args)))))
     (t
      (apply 'chess-network-handler game event args)))))

;; This filter translates IRC syntax into basic chess-network protocol
(defun chess-irc-filter (proc string)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(let ((moving (= (point) (process-mark proc))))
	  (save-excursion
	    ;; Insert the text, advancing the marker.
	    (goto-char (process-mark proc))
	    (while (string-match "\r" string)
	      (setq string (replace-match "" t t string)))
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
		     ":\\([^ \t\n!]+\\)!\\S-+ PRIVMSG \\(\\S-+\\) :\\(\C-aCHESS \\)?\\(.+\\)\C-a?\n")
		    (let ((sender (match-string 1))
			  (target (match-string 2))
			  (ctcp (match-string 3))
			  (msg (match-string 4)))
		    (with-current-buffer chess-irc-engine
		      (when (and (string= chess-irc-nick target)
				 (or (null chess-irc-opponent)
				     (string= chess-irc-opponent sender)))
			(unless chess-irc-opponent
			  (setq chess-irc-opponent sender))
			(if (and (not chess-irc-use-ctcp)
				 ctcp (> (length ctcp) 0))
			  (setq chess-irc-use-ctcp t))
			(chess-engine-submit nil (concat msg "\n")))))))
		  (forward-line)))
	    (setq chess-irc-last-pos (point)
		  chess-irc-working nil)))))))

(provide 'chess-irc)

;;; chess-irc.el ends here
