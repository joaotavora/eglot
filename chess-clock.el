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
    (unless (or (null (car args))
		(chess-game-data game 'white-remaining))
      (chess-game-set-data game 'white-remaining (float (car args)))
      (chess-game-set-data game 'black-remaining (float (car args))))
    t)

   ((eq event 'post-undo)
    (let* ((last-ply (car (last (chess-game-plies game) 2)))
	   (white (chess-ply-keyword last-ply :white))
	   (black (chess-ply-keyword last-ply :black)))
      (when (and white black)
	(chess-game-set-data game 'white-remaining white)
	(chess-game-set-data game 'black-remaining black))))

   ((eq event 'move)
    (let ((white (chess-game-data game 'white-remaining))
	  (black (chess-game-data game 'black-remaining)))
      (when (and white black
		 (chess-game-data game 'active)
		 (> (chess-game-index game) 0))
	(unless chess-clock-timer
	  (setq chess-clock-timer
		(run-with-timer 0 1 'chess-clock-tick-tock
				(current-buffer))))
	(let ((last-ply (car (last (chess-game-plies game) 2))))
	  (chess-ply-set-keyword last-ply :white white)
	  (chess-ply-set-keyword last-ply :black black))))

    (if (chess-game-over-p game)
	(chess-clock-handler game 'destroy)))

   ((eq event 'set-data)
    (if (and (eq (car args) 'active)
	     (not (chess-game-data game 'active)))
	(chess-clock-handler game 'destroy)))

   ((eq event 'destroy)
    (if chess-clock-timer
      (cancel-timer chess-clock-timer)
      (setq chess-clock-timer nil)))))

(defvar chess-clock-tick-tocking nil)

(defun chess-clock-tick-tock (module)
  (unless chess-clock-tick-tocking
    (let ((chess-clock-tick-tocking t))
      (with-current-buffer module
	(let ((last-time chess-clock-last-time)
	      (chess-game-inhibit-events t)
	      counter)
	  (setq chess-clock-last-time (current-time))
	  (when (and (> (chess-game-index chess-module-game) 0)
		     (not (chess-game-status chess-module-game)))
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
