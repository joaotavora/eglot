;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A special kind of display that merely autosaves the game
;;

(require 'chess-game)

(chess-message-catalog 'english
  '((queen-would-take . "The queen would take your knight!")
    (congratulations  . "Congratulations!")
    (knight-1-done    . "Goal: take all the pawns, without letting the queen take your knight")))

(defun chess-tutorial-knight-1 (game ignore event &rest args)
  (if (eq event 'move)
      (let ((position (chess-game-pos game)))
	(if (null (chess-pos-search position ?p))
	    (chess-message 'congratulations)
	  (when (chess-search-position
		 position (car (chess-pos-search position ?N)) ?q)
	    (chess-game-run-hooks chess-module-game 'undo 1)
	    (chess-display-update nil)
	    (chess-error 'queen-would-take))))))

(defun chess-tutorial ()
  (interactive)
  (let* (chess-default-modules
	 (display (chess-create-display)))
    (with-current-buffer display
      (chess-game-set-start-position
       (chess-display-game nil)
       (chess-fen-to-pos "8/3p1p/2p3p/4q/2p3p/3p1p/8/N w - -"))
      (chess-game-add-hook (chess-display-game nil) 'chess-tutorial-knight-1)
      (setq chess-pos-always-white t)
      (chess-display-popup nil)
      (chess-message 'knight-1-done))))

(provide 'chess-tutorial)

;;; chess-tutorial.el ends here
