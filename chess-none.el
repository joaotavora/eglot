;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A null engine, used when two humans play each on the same display.
;;
;; $Revision$

(require 'chess-engine)

(defun chess-none-handler (event &rest args)
  "An empty chess engine, used for fielding key events.
This is only useful when two humans are playing each other, in which
case this engine will do the job of accepting undos, handling
resignations, etc."
  (cond
   ((eq event 'initialize) t)

   ((memq event '(resign abort))
    (chess-engine-set-position nil))

   ((eq event 'undo)
    (chess-game-undo chess-engine-game (car args)))))

(provide 'chess-none)

;;; chess-none.el ends here
