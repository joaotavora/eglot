;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Manage a chess playing/viewing session
;;
;; $Revision$

;;; Commentary:

(defun chess-session-create ()
  (cons nil nil))

(defun chess-session-add-listener (session listener &optional front)
  (if (or front (not (cdr session)))
      (setcdr session (cons (cons listener nil)
			    (cdr session)))
    (nconc session (list (cons listener nil)))))

(defun chess-session-remove-listener (session listener)
  (setcdr session (delq (assq listener (cdr session))
			(cdr session))))

(defun chess-session-data (session sym)
  (cdr (assq sym (car session))))

(defun chess-session-set-data (session sym value)
  (let ((entry (assq sym (car session))))
    (if entry
	(setcdr entry value)
      (setcar session (cons (cons sym value)
			    (car session))))))

(defun chess-session-del-data (session sym)
  (setcar session (delq (assq sym (car session))
			(car session))))

(defun chess-session-event (session event &rest args)
  (let ((listeners (cdr session)) result)
    (while (and (or (eq event 'initialize)
		    (null result)) listeners)
      (setq result (apply (caar listeners) session (cdar listeners)
			  event args))
      (if (eq event 'initialize)
	  (setcdr (car listeners) result))
      (setq listeners (cdr listeners)))
    result))

(provide 'chess-session)

;;; chess-session.el ends here
