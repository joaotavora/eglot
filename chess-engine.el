;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Obtain movements and other information from a subprocess
;;
;; $Revision$

;;; Commentary:

(require 'chess-session)
(require 'chess-game)

(defgroup chess-engine nil
  "Code for reading movements and other commands from a subprocess."
  :group 'chess)

(defvar chess-engine-regexp-alist nil)
(defvar chess-engine-event-handler nil)
(defvar chess-engine-response-handler nil)
(defvar chess-engine-session nil)
(defvar chess-engine-position nil)
(defvar chess-engine-game nil)
(defvar chess-engine-search-function nil)

(make-variable-buffer-local 'chess-engine-regexp-alist)
(make-variable-buffer-local 'chess-engine-event-handler)
(make-variable-buffer-local 'chess-engine-response-handler)
(make-variable-buffer-local 'chess-engine-session)
(make-variable-buffer-local 'chess-engine-position)
(make-variable-buffer-local 'chess-engine-game)
(make-variable-buffer-local 'chess-engine-search-function)

(defvar chess-engine-last-pos nil)
(defvar chess-engine-working nil)

(make-variable-buffer-local 'chess-engine-last-pos)
(make-variable-buffer-local 'chess-engine-working)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User interface
;;

(defmacro chess-with-current-buffer (buffer &rest body)
  `(let ((buf ,buffer))
     (if buf
	 (with-current-buffer buf
	   ,@body)
       ,@body)))

(defun chess-engine-default-handler (event &rest args)
  (cond
   ((eq event 'move)
    (cond
     ((chess-engine-session nil)
      (apply 'chess-session-event (chess-engine-session nil) event args))
     ((chess-engine-game nil)
      (chess-game-move (chess-engine-game nil) (car args)))
     (t
      (apply 'chess-pos-move (chess-ply-pos (car args))
	     (chess-ply-changes (car args))))))))

(defun chess-engine-create (module &optional user-handler session search-func)
  (let ((regexp-alist (intern-soft (concat (symbol-name module)
					   "-regexp-alist")))
	(handler (intern-soft (concat (symbol-name module) "-handler"))))
    (with-current-buffer (generate-new-buffer " *chess-engine*")
      (setq chess-engine-regexp-alist (symbol-value regexp-alist)
	    chess-engine-event-handler handler
	    chess-engine-response-handler (or 'chess-engine-default-handler
					      user-handler))
      (let ((proc (funcall handler 'initialize)))
	(unless (and proc (memq (process-status proc) '(run open)))
	  (error "Failed to start chess engine process"))
	(set-process-buffer proc (current-buffer))
	(set-process-filter proc 'chess-engine-filter))
      (chess-engine-set-game nil (chess-game-create nil search-func))
      (current-buffer))))

(defun chess-engine-destroy (engine)
  (let ((buf (or display (current-buffer))))
    (if (buffer-live-p buf)
	(kill-buffer buf))))

(defun chess-engine-command (engine event &rest args)
  (chess-with-current-buffer engine
    (apply chess-engine-event-handler event args)))

(defun chess-engine-search-function (engine)
  (chess-with-current-buffer engine
    chess-engine-search-function))

(defun chess-engine-set-search-function (engine search-func)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-search-function chess-engine-game)
      (or chess-engine-search-function
	  'chess-standard-search-position))))

(defun chess-engine-session (engine)
  (chess-with-current-buffer engine
    chess-engine-session))

(defun chess-engine-set-option (engine option value)
  (chess-with-current-buffer engine
    ))

(defun chess-engine-option (engine option) 'ponder 'search-depth 'wall-clock
  (chess-with-current-buffer engine
    ))

(defun chess-engine-set-position (engine position)
  (chess-with-current-buffer engine
    (setq chess-engine-game nil
	  chess-engine-position position)
    (chess-engine-command nil 'setup position)))

(defun chess-engine-position (engine)
  (chess-with-current-buffer engine
    (or (and chess-engine-game
	     (chess-game-pos chess-engine-game))
	chess-engine-position)))

(defun chess-engine-set-game (engine game)
  (chess-with-current-buffer engine
    (setq chess-engine-game game
	  chess-engine-position nil)
    (chess-engine-command nil 'setup (chess-game-pos game))))

(defun chess-engine-game (engine)
  (chess-with-current-buffer engine
    chess-engine-game))

(defun chess-engine-index (engine)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-index chess-engine-game))))

(defun chess-engine-move (engine ply)
  (chess-with-current-buffer engine
    (cond
     (chess-engine-game
      (chess-game-move chess-engine-game ply))
     (chess-engine-position
      (apply 'chess-pos-move ply)))
    (chess-engine-command engine 'move ply)))

(defun chess-engine-pass (engine ply)
  (chess-with-current-buffer engine
    (chess-engine-command engine 'pass)))

(defun chess-engine-send (engine string)
  (chess-with-current-buffer engine
    (process-send-string (get-buffer-process (current-buffer)) string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Module method
;;

;;;###autoload
(defun chess-engine (session buffer event &rest args)
  "Handle any commands being sent to this instance of this module."
  (if (eq event 'initialize)
      (chess-engine-create (car args) 'chess-engine-session-callback session)
    (ignore
     (cond
      ((eq event 'shutdown)
       (chess-engine-destroy engine))

      ((eq event 'setup)
       (chess-engine-set-game engine (car args)))

      ((eq event 'pass)
       (chess-engine-pass engine))))))

(defun chess-engine-filter (proc string)
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
	(unless chess-engine-working
	  (setq chess-engine-working t)
	  (unwind-protect
	      (progn
		(if chess-engine-last-pos
		    (goto-char chess-engine-last-pos)
		  (goto-char (point-min)))
		(beginning-of-line)
		(while (not (eobp))
		  (condition-case err
		      (let ((triggers chess-engine-regexp-alist))
			(while triggers
			  ;; this could be accelerated by joining
			  ;; together the regexps
			  (if (looking-at (caar triggers))
			      (funcall (cdar triggers)))
			  (setq triggers (cdr triggers))))
		    (chess-illegal (error-message-string err)))
		  (forward-line)))
	    (setq chess-engine-last-pos (point)
		  chess-engine-working nil)))))))

(provide 'chess-engine)

;;; chess-engine.el ends here
