;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Play against popular chess engines
;;
;; $Revision$

(require 'chess-process)

(define-chess-engine crafty (&rest args)
  (list (list
	 (concat "\\s-*\\(White\\|Black\\)\\s-*([0-9]+):\\s-+\\("
		 chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda (color move)
	    (if (string= (if (chess-game-side-to-move chess-process-game)
			     "White" "Black")
			 color)
		(chess-session-event
		 chess-current-session 'move
		 (chess-algebraic-to-ply
		  (chess-game-pos chess-process-game) move)))))
	 1 2)
	'("Illegal move:\\s-*\\(.*\\)"
	  (signal 'chess-illegal (match-string 1))))
  (init (concat "display nogeneral\n"
		"display nochanges\n"
		"display noextstats\n"
		"display nohashstats\n"
		"display nomoves\n"
		"display nonodes\n"
		"display noply1\n"
		"display nostats\n"
		"display notime\n"
		"display novariation\n"
		"alarm off\n"
		"ansi off"))
  (shutdown "quit")
  (move (chess-game-ply-to-algebraic chess-process-game (car args)))
  (pass "go"))

(define-chess-engine gnuchess (&rest args)
  (list (list
	 (concat "My move is : \\(" chess-algebraic-regexp "\\)")
	 (function
	  (lambda (move)
	    (chess-session-event chess-current-session 'move
				 (chess-algebraic-to-ply
				  (chess-game-pos chess-process-game) move))))
	 1)
	'("Illegal move:\\s-*\\(.*\\)"
	  (signal 'chess-illegal (match-string 1))))
  (shutdown "quit")
  (move (chess-game-ply-to-algebraic chess-process-game (car args)))
  (pass "go"))

;;; chess-engines.el ends here
