;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Scheme to verbally announce moves
;;
;; $Revision$

(require 'chess-game)

(defvar chess-announce-names
  '((?q . "queen")
    (?k . "king")
    (?b . "bishop")
    (?n . "knight")
    (?r . "rook")
    (?p . "pawn")))

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
	       text)
	  (cond
	   ((chess-ply-has-keyword ply :castle)
	    (setq text "kingside castle"))
	   ((chess-ply-has-keyword :long-castle)
	    (setq text "queenside castle"))
	   ((= t-piece ? )
	    (setq text (concat (cdr (assq (downcase s-piece)
					  chess-announce-names))
			       " to "
			       (chess-index-to-coord target))))
	   (t
	    (setq text (concat (cdr (assq (downcase s-piece)
					  chess-announce-names))
			       " takes at "
			       (chess-index-to-coord target)))))
	  (if (chess-ply-has-keyword :check)
	      (setq text (concat text ", check")))
	  (if (chess-ply-has-keyword :checkmate)
	      (setq text (concat text ", checkmate")))
	  (if (chess-ply-has-keyword :stalemate)
	      (setq text (concat text ", stalemate")))

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
