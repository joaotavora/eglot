;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements a chess clock
;;

(defvar chess-clock-last-time nil)
(defvar chess-clock-timer nil)

(make-variable-buffer-local 'chess-clock-last-time)
(make-variable-buffer-local 'chess-clock-timer)

(defsubst chess-clock-add-seconds (time seconds)
  "To TIME, add SECONDS.  Return result as a time value."
  (let* ((secint (truncate seconds))
	 (hi (/ secint 65536))
	 (lo (% secint 65536))
	 (calc (+ (cadr time) lo)))
    (if (< calc 65536)
	(list (+ (car time) hi) calc)
      (list (+ (car time) (1+ hi)) (% calc 65536)))))

(defsubst chess-clock-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst chess-clock-time-diff (t1 t2)
  "Return the difference in seconds between T1 and T2."
  (- (chess-clock-time-to-seconds t1)
     (chess-clock-time-to-seconds t2)))

(defun chess-clock-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (unless (chess-game-data game 'white-remaining)
      (chess-game-set-data game 'white-remaining (float (or (car args) 0))))
    (unless (chess-game-data game 'black-remaining)
      (chess-game-set-data game 'black-remaining (float (or (car args) 0))))
    (setq chess-clock-timer
	  (run-with-timer 0 1 'chess-clock-tick-tock (current-buffer)))
    t)

   ((eq event 'post-undo)
    (let ((last-ply (car (last (chess-game-plies game) 2))))
      (chess-game-set-data game 'white-remaining
			   (chess-ply-keyword last-ply :white))
      (chess-game-set-data game 'black-remaining
			   (chess-ply-keyword last-ply :black))))

   ((eq event 'move)
    (when (> (chess-game-index game) 0)
      (let ((last-ply (car (last (chess-game-plies game) 2))))
	(nconc last-ply
	       (list :white (chess-game-data game 'white-remaining)
		     :black (chess-game-data game 'black-remaining))))))

   ((eq event 'destroy)
    (cancel-timer chess-clock-timer))))

(defvar chess-clock-tick-tocking nil)

(defun chess-clock-tick-tock (module)
  (unless chess-clock-tick-tocking
    (let ((chess-clock-tick-tocking t))
      (with-current-buffer module
	(let ((last-time chess-clock-last-time) counter)
	  (setq chess-clock-last-time (current-time))
	  (when (> (chess-game-index chess-module-game) 0)
	    (if (chess-pos-side-to-move (chess-game-pos chess-module-game))
		(setq counter 'white-remaining)
	      (setq counter 'black-remaining))
	    (chess-game-set-data
	     chess-module-game counter
	     (- (chess-game-data chess-module-game counter)
		(chess-clock-time-diff chess-clock-last-time last-time))))))
      (force-mode-line-update))))

(provide 'chess-clock)

;;; chess-clock.el ends here
