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

(defvar chess-process-triggers nil
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
    (if (buffer-live-p buffer)
	(kill-buffer buffer)))
   (t
    (ignore
     (with-current-buffer buffer
       (cond
	((eq event 'setup)
	 (setq chess-process-game (car args)
	       chess-process-last-pos (point-min)))))))))

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

(defun chess-process-let (forms)
  `(let ((str (progn ,@forms)))
     (if (stringp str)
	 (ignore
	  (process-send-string (get-buffer-process (current-buffer))
			       (concat str "\n")))
       str)))

(defun chess-process-insert-forms (event)
  (if (assq event forms)
      (chess-process-let
       (prog1
	   (cdr (assq event forms))
	 (setq forms (assq-delete-all event forms))))))

(defmacro define-chess-engine (name ignored triggers &rest forms)
  "Define a chess engine.
NAME is an unquoted symbol name that denotes the engine.  This name is
used as the default string for the chess engine's external command
name.
TRIGGERS is a list of process triggers, which fire when the output
from the process matches certain regexps.  See
`chess-process-triggers' for more information.
FORMS is an alist of event symbols, and forms to evaluate when such an
event is received by the module.  If these forms return a string, this
string will be sent to the engine process.
See the file chess-engines.el for code examples."
  (let ((namestr (symbol-name name)))
    `(progn
       (defcustom ,(intern (concat "chess-" namestr "-command"))
	 (and (require 'executable)
	      (executable-find ,namestr))
	 ,(concat "The name of the " namestr " program.")
	 :type 'file
	 :group 'chess-process)

       (defun ,(intern (concat "chess-" namestr))
	 (session buffer event &rest args)
	 (cond
	  ((eq event 'initialize)
	   (with-current-buffer
	       (chess-process session buffer event ,triggers
			      ,(intern (concat "chess-" namestr "-command")))
	     ,(chess-process-insert-forms 'init)
	     (current-buffer)))
	  ((eq event 'shutdown)
	   (when (buffer-live-p buffer)
	     (ignore-errors
	       ,(chess-process-insert-forms 'shutdown))
	     (kill-buffer buffer)))
	  (t
	   (ignore
	    (with-current-buffer buffer
	      (cond
	       ((eq event 'setup)
		(apply 'chess-process session buffer event args)
		,(chess-process-insert-forms 'setup))
	       ,@(mapcar
		  (function
		   (lambda (entry)
		     `((eq event (quote ,(car entry)))
		       ,(chess-process-let (cdr entry))))) forms)
	       (t
		(apply 'chess-process session buffer event args)))))))))))

(provide 'chess-process)

;;; chess-process.el ends here
