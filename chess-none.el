;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A null engine, used when two humans play each on the same display.
;;
;; $Revision$

(require 'chess-engine)

(defun chess-none-handler (event &rest args)
  "Initialize the network chess engine."
  (cond
   ((eq event 'send))

   ((eq event 'ready)
    (and (chess-engine-game nil)
	 (chess-game-set-data (chess-engine-game nil) 'active t)))

   ((memq event '(resign abort))
    (and (chess-engine-game nil)
	 (chess-engine-set-start-position nil)))

   ((eq event 'undo)
    (if (chess-engine-game nil)
	(chess-game-undo (chess-engine-game nil) (car args))))))

(provide 'chess-none)

;;; chess-none.el ends here
