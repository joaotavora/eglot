;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scheme to verbally announce moves
;;
;; $Revision$

(require 'chess-game)

(chess-message-catalog 'english
  '((queen	  . "queen")
    (king	  . "king")
    (bishop	  . "bishop")
    (knight	  . "knight")
    (rook	  . "rook")
    (pawn	  . "pawn")
    (short-castle . "short castle")
    (long-castle  . "long castle")
    (check	  . "check")
    (checkmate	  . "checkmate")
    (stalemate	  . "stalemate")
    (piece-moves  . "%s to %s")
    (piece-takes  . "%s takes %s at %s")))

(defvar chess-announce-names
  '((?q . queen)
    (?k . king)
    (?b . bishop)
    (?n . knight)
    (?r . rook)
    (?p . pawn)))

(autoload 'festival-start-process "festival")
(autoload 'festival-kill-process "festival")

(defvar chess-announce-functions
  (if (and (executable-find "festival")
	   (not (featurep 'emacspeak)))
      (if (fboundp 'festival-say-string)
	  '(festival-start-process festival-say-string festival-kill-process)
	'(ignore chess-announce-festival ignore))
    '(ignore message ignore))
  "These three functions are used to for announcing moves.
The first is called one start of the announcer.  The second is called
with the string to announce each time.  The third is called to
shutdown the announcer process, if necessary.")

(defun chess-announce-available-p () t)

(defun chess-announce-for-game (game)
  "Announce the opponent's moves in GAME."
  (funcall (nth 0 chess-announce-functions))
  (chess-game-add-hook game 'chess-announce-handler))

(defun chess-announce-handler (game ignore event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (cond
   ((eq event 'shutdown)
    (funcall (nth 2 chess-announce-functions)))

   ((eq event 'move)
    (let* ((ply (chess-game-ply game (1- (chess-game-index game))))
	   (pos (chess-ply-pos ply)))
      (unless (eq (chess-game-data game 'my-color)
		  (chess-pos-side-to-move pos))
	(let* ((source (chess-ply-source ply))
	       (target (chess-ply-target ply))
	       (s-piece (chess-pos-piece pos source))
	       (t-piece (chess-pos-piece pos target))
	       (which (chess-ply-keyword ply :which))
	       text)
	  (if which
	      (setq which (char-to-string which)))
	  (cond
	   ((chess-ply-keyword ply :castle)
	    (setq text (chess-string 'short-castle)))
	   ((chess-ply-keyword ply :long-castle)
	    (setq text (chess-string 'long-castle)))
	   ((= t-piece ? )
	    (setq text
		  (concat which
			  (chess-string 'piece-moves
					(chess-string
					 (cdr (assq (downcase s-piece)
						    chess-announce-names)))
					(chess-index-to-coord target)))))
	   (t
	    (setq text
		  (concat which
			  (chess-string 'piece-takes
					(chess-string
					 (cdr (assq (downcase s-piece)
						    chess-announce-names)))
					(chess-string
					 (cdr (assq (downcase t-piece)
						    chess-announce-names)))
					(chess-index-to-coord target))))))

	  (if (chess-ply-keyword ply :check)
	      (setq text (concat text ", " (chess-string 'check))))
	  (if (chess-ply-keyword ply :checkmate)
	      (setq text (concat text ", " (chess-string 'checkmate))))
	  (if (chess-ply-keyword ply :stalemate)
	      (setq text (concat text ", " (chess-string 'stalemate))))

	  (funcall (nth 1 chess-announce-functions) text)))))))

(defun chess-announce-festival (text)
  "Announce the given text using festival.
This is less efficient than festival.el, which should be installed if
possible.  Debian installs it automatically when you apt-get install
festival."
  (let ((proc (start-process "announce" nil "/usr/bin/festival" "--tts")))
    (when (and proc (eq (process-status proc) 'run))
      (process-send-string proc (concat text "\n"))
      (process-send-eof proc))))

(provide 'chess-announce)

;;; chess-announce.el ends here
