;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A null engine, used when two humans play against each other on the
;; same display.
;;

(require 'chess-engine)

(defvar chess-none-regexp-alist nil)

(defun chess-none-handler (game event &rest args)
  "An empty chess engine, used for fielding key events.
This is only useful when two humans are playing each other, in which
case this engine will do the job of accepting undos, handling
resignations, etc."
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize) t)

     ((memq event '(resign abort))
      (chess-engine-set-position nil))

     ((eq event 'undo)
      (chess-game-undo game (car args))))))

(provide 'chess-none)

;;; chess-none.el ends here
