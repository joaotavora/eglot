;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Obtain movements and other information from an engine
;;
;; $Revision$

;;; Commentary:

(require 'chess-game)

(defgroup chess-engine nil
  "Code for reading movements and other commands from an engine."
  :group 'chess)

(defvar chess-engine-regexp-alist nil)
(defvar chess-engine-event-handler nil)
(defvar chess-engine-response-handler nil)
(defvar chess-engine-current-marker nil)
(defvar chess-engine-position nil)
(defvar chess-engine-game nil)

(make-variable-buffer-local 'chess-engine-regexp-alist)
(make-variable-buffer-local 'chess-engine-event-handler)
(make-variable-buffer-local 'chess-engine-response-handler)
(make-variable-buffer-local 'chess-engine-current-marker)
(make-variable-buffer-local 'chess-engine-position)
(make-variable-buffer-local 'chess-engine-game)

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

(defun chess-engine-do-move (ply)
  (cond
   (chess-engine-game
    (chess-game-move chess-engine-game ply))
   (chess-engine-position
    (setq chess-engine-position (chess-ply-next-pos ply)))))

(defun chess-engine-default-handler (event &rest args)
  (cond
   ((eq event 'move)
    (chess-engine-do-move (car args)))

   ((eq event 'pass)
    (message "Your opponent has passed the first move to you"))

   ((eq event 'connect)
    (message "Your opponent, %s, is now ready to play" (car args)))

   ((eq event 'quit)
    (message "Your opponent has quit playing"))

   ((eq event 'setup)
    (let* ((position (chess-fen-to-pos (car args)))
	   (ply (chess-ply-create position)))
      (chess-game-set-plies (chess-engine-game nil) (list ply))))))

(defun chess-engine-create (module &optional user-handler &rest args)
  (let ((regexp-alist (intern-soft (concat (symbol-name module)
					   "-regexp-alist")))
	(handler (intern-soft (concat (symbol-name module) "-handler"))))
    (with-current-buffer (generate-new-buffer " *chess-engine*")
      (setq chess-engine-regexp-alist (symbol-value regexp-alist)
	    chess-engine-event-handler handler
	    chess-engine-response-handler (or user-handler
					      'chess-engine-default-handler))
      (let ((proc (apply handler 'initialize args)))
	(when (processp proc)
	  (unless (memq (process-status proc) '(run open))
	    (error "Failed to start chess engine process"))
	  (set-process-buffer proc (current-buffer))
	  (set-process-filter proc 'chess-engine-filter))
	(setq chess-engine-current-marker (point-marker)))
      (current-buffer))))

(defun chess-engine-destroy (engine)
  (let ((buf (or engine (current-buffer))))
    (if (buffer-live-p buf)
	(kill-buffer buf))))

(defun chess-engine-command (engine event &rest args)
  (chess-with-current-buffer engine
    (apply chess-engine-event-handler event args)))

;; 'ponder
;; 'search-depth
;; 'wall-clock

(defun chess-engine-set-option (engine option value)
  (chess-with-current-buffer engine
    ))

(defun chess-engine-option (engine option) 'ponder 'search-depth 'wall-clock
  (chess-with-current-buffer engine
    ))

(defun chess-engine-set-position (engine position)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-engine-detach-game nil))
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
    (if chess-engine-game
	(chess-engine-detach-game nil))
    (setq chess-engine-game game
	  chess-engine-position nil)
    (chess-game-add-hook game 'chess-engine-event-handler engine)
    (chess-engine-command nil 'setup (chess-game-pos game))))

(defun chess-engine-detach-game (engine)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-remove-hook chess-engine-game
				'chess-engine-event-handler))))

(defun chess-engine-game (engine)
  (chess-with-current-buffer engine
    chess-engine-game))

(defun chess-engine-index (engine)
  (chess-with-current-buffer engine
    (if chess-engine-game
	(chess-game-index chess-engine-game))))

(defun chess-engine-move (engine ply)
  (chess-with-current-buffer engine
    (chess-engine-do-move ply)
    (chess-engine-command engine 'move ply)))

(defun chess-engine-pass (engine)
  (chess-with-current-buffer engine
    (chess-engine-command engine 'pass)))

(defun chess-engine-send (engine string)
  "Send the given STRING to ENGINE."
  (chess-with-current-buffer engine
    (let ((proc (get-buffer-process (current-buffer))))
      (if proc
	  (if (memq (process-status proc) '(run open))
	      (process-send-string proc string)
	    (error "The engine you were using is no longer running"))
	(chess-engine-command nil 'send string)))))

(defun chess-engine-submit (engine string)
  "Submit the given STRING, so ENGINE sees it in its input stream."
  (chess-with-current-buffer engine
    (let ((proc (get-buffer-process (current-buffer))))
      (if (and (processp proc)
	       (not (memq (process-status proc) '(run open))))
	  (error "The engine you were using is no longer running"))
      (chess-engine-filter nil string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Primary event handler
;;

(defun chess-engine-event-handler (game engine event &rest args)
  "Handle any commands being sent to this instance of this module."
  (with-current-buffer engine
    (assert (eq game (chess-engine-game nil)))
    (apply chess-engine-event-handler event args)
    (cond
     ((eq event 'shutdown)
      (chess-engine-destroy engine))

     ((eq event 'setup)
      (chess-engine-set-game engine (car args))))))

(defun chess-engine-filter (proc string)
  "Filter for receiving text for an engine from an outside source."
  (let ((buf (if (processp proc)
		 (process-buffer proc)
	       (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(let ((moving (= (point) chess-engine-current-marker)))
	  (save-excursion
	    ;; Insert the text, advancing the marker.
	    (goto-char chess-engine-current-marker)
	    (insert string)
	    (set-marker chess-engine-current-marker (point)))
	  (if moving (goto-char chess-engine-current-marker)))
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
