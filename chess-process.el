;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Obtain movements and other information from a subprocess
;;
;; $Revision$

;;; Commentary:

(require 'chess-session)
(require 'chess-game)
(require 'chess-algebraic)

(defgroup chess-process nil
  "Code for reading movements and other commands from a subprocess."
  :group 'chess)

(defvar chess-process-command-or-host nil
  "What type of process is it?
This could be a string, naming a command to run, in which case it is a
local connection.
It could be a cons cell, giving the (HOST . PORT) of a network connection.
It could be a Lisp function, which will be called to establish
whatever type of connection it wishes, so long as it returns a buffer
related to the resulting process.")

(make-variable-buffer-local 'chess-process-command-or-host)

(defvar chess-process-arguments nil
  "If `chess-process-where' is a string or Lisp function, pass these args.")
(defvar chess-process-game)
(defvar chess-process-last-pos)
(defvar chess-process-working nil)

(make-variable-buffer-local 'chess-process-arguments)
(make-variable-buffer-local 'chess-process-game)
(make-variable-buffer-local 'chess-process-last-pos)
(make-variable-buffer-local 'chess-process-working)

(defvar chess-process-triggers
  (list (list
	 (concat "\\s-*\\(white\\|black\\)\\s-*([0-9]+):\\s-+\\("
		 chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda (color move)
	    (if (if (chess-game-side-to-move chess-process-game)
		    (string= (downcase color) "white")
		  (string= (downcase color) "black"))
		(chess-session-event
		 chess-current-session 'move
		 (chess-algebraic-to-ply
		  (chess-game-pos chess-process-game) move)))))
	 1 2)
	'(".*Illegal move:\\s-*\\(.*\\)"
	  (signal 'chess-illegal (match-string 1)))
	'(".+?\015" (replace-match "")))
  "A list of regexps and the commands that they trigger.
The alist should be of the form:

   ((REGEXP COMMAND ARG1-GROUP ARG2-GROUP ...) ...)

Where the ARG*-GROUP entries specify which parenthesis groups in the
regexp demarcate those arguments.  Anything more complicated than this
must be handled by modules that derive from this module.")

(make-variable-buffer-local 'chess-process-triggers)

;;; Code:

(defun chess-process (session buffer event &rest args)
  "Handle any commands being sent to this instance of this module."
  (cond
   ((eq event 'initialize)
    (let ((buf (generate-new-buffer " *chess-process*")))
      (with-current-buffer buf
	(setq chess-process-triggers (nth 0 args)
	      chess-process-command-or-host (nth 1 args)
	      chess-process-arguments (nthcdr 2 args))
	(let ((proc
	       (if (stringp chess-process-command-or-host)
		   (prog2
		       (message "Starting chess program '%s'..."
				chess-process-command-or-host)
		       (apply 'start-process "chess-process"
			      (current-buffer)
			      chess-process-command-or-host
			      chess-process-arguments)
		     (message "Starting chess program '%s'...done"
			      chess-process-command-or-host))
		 (prog2
		     (message "Connecting to host %s:%d..."
			      (car chess-process-command-or-host)
			      (cdr chess-process-command-or-host))
		     (open-network-stream
		      "chess-process" (current-buffer)
		      (car chess-process-command-or-host)
		      (cdr chess-process-command-or-host))
		   (message "Connecting to host %s:%d...done"
			    (car chess-process-command-or-host)
			    (cdr chess-process-command-or-host))))))
	  (unless (and proc (memq (process-status proc) '(run open)))
	    (error "Failed to start chess process"))
	  (set-process-filter proc 'chess-process-filter))
	buf)))
   ((eq event 'shutdown)
    (when (buffer-live-p buffer)
      (ignore-errors
	(process-send-string (get-buffer-process buffer) "quit\n"))
      (kill-buffer buffer)))
   (t
    (ignore
     (with-current-buffer buffer
       (let (cmdstr)
	 (cond
	  ((eq event 'setup)
	   (setq chess-process-game (car args)
		 chess-process-last-pos (point-min)))
	  ((eq event 'move)
	   (setq cmdstr (concat (chess-ply-to-algebraic (car args)) "\n")))
	  ((eq event 'pass)
	   (setq cmdstr "go\n")))
	 (if (and cmdstr (not chess-process-working))
	     (process-send-string (get-buffer-process (current-buffer))
				  cmdstr))))))))

(defun chess-process-filter (proc string)
  "Process filter for receiving text from a chess process."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(let ((moving (= (point) (process-mark proc))))
	  (save-excursion
	    ;; Insert the text, advancing the process marker.
	    (goto-char (process-mark proc))
	    (insert string)
	    (set-marker (process-mark proc) (point)))
	  (if moving (goto-char (process-mark proc))))
	(unless chess-process-working
	  (setq chess-process-working t)
	  (unwind-protect
	      (progn
		(goto-char chess-process-last-pos)
		(beginning-of-line)
		(while (not (eobp))
		  (condition-case err
		      (let ((triggers chess-process-triggers))
			(while triggers
			  ;; this could be accelerated by joining
			  ;; together the regexps
			  (when (looking-at (caar triggers))
			    (let ((command (nth 1 (car triggers)))
				  (args (mapcar 'match-string
						(nthcdr 2 (car triggers)))))
			      (cond
			       ((functionp command)
				(apply command args))
			       ((symbolp command)
				(chess-session-event chess-current-session
						     command args))
			       (t (eval command)))))
			  (setq triggers (cdr triggers))))
		    (chess-illegal (error-message-string err)))
		  (forward-line)))
	    (setq chess-process-last-pos (point)
		  chess-process-working nil)))))))

(provide 'chess-process)

;;; chess-process.el ends here
