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

(defvar chess-announce-function 'chess-announce-festival
  "The function to call for announcing moves audially.
It is passed the string of English text to announce.")

(defun chess-announce-for-game (game perspective)
  "Announce moves in GAME on behalf of PERSPECTIVE.
This means that if PERSPECTIVE is t (for white), only black's moves
will be announced."
  (chess-game-add-hook game 'chess-announce-event-handler perspective))

(defun chess-announce-change-perspective (game perspective)
  "Change the announce perspective in GAME to PERSPECTIVE."
  (let ((cell (assq 'chess-announce-event-handler (chess-game-hooks game))))
    (if cell
	(setcdr cell perspective))))

(defun chess-announce-event-handler (game perspective event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (cond
   ((memq event '(move game-over))
    (let* ((ply (chess-game-ply game (1- (chess-game-index game))))
	   (pos (chess-ply-pos ply)))
      (unless (eq perspective (chess-pos-side-to-move pos))
	(let* ((changes (chess-ply-changes ply))
	       (source (car changes))
	       (target (cadr changes))
	       (s-piece (chess-pos-piece pos source))
	       (t-piece (chess-pos-piece pos target))
	       text)
	  (cond
	   ((memq :castle changes)
	    (setq text "kingside castle"))
	   ((memq :long-castle changes)
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
	  (if (memq :check changes)
	      (setq text (concat text ", check")))
	  (if (memq :checkmate changes)
	      (setq text (concat text ", checkmate")))
	  (if (memq :stalemate changes)
	      (setq text (concat text ", stalemate")))

	  (funcall chess-announce-function text)))))))

(defun chess-announce-festival (text)
  "Announce the given text using festival."
  (let ((proc (start-process "announce" nil "/usr/bin/festival" "--tts")))
    (when (and proc (eq (process-status proc) 'run))
      (process-send-string proc (concat text "\n"))
      (process-send-eof proc))))
