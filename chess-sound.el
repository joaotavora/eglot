;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is very similar to chess-announce, except it uses specific
;; .WAV files instead of text-to-speech.
;;
;; $Revision$

(require 'chess-game)

(defgroup chess-sound nil
  "Code to play specific sounds when announcing chess moves."
  :group 'chess)

(defcustom chess-sound-directory
  (expand-file-name "sounds"
		    (file-name-directory
		     (or load-file-name buffer-file-name)))
  "The directory where chess sounds can be found."
  :type 'directory
  :group 'chess-sound)

(defcustom chess-sound-play-function (if (fboundp 'play-sound-file)
					 'play-sound-file
				       'chess-sound-play)
  "Non-nil if chess-sound should play sounds ."
  :type 'file
  :group 'chess-sound)

(defcustom chess-sound-program (or (executable-find "esdplay")
				   (executable-find "play"))
  "Program used to play sounds, if `play-sound-file' does not exist."
  :type 'file
  :group 'chess-sound)

(defcustom chess-sound-args nil
  "Additional args to pass to `chess-sound-program', before the .WAV file."
  :type '(repeat string)
  :group 'chess-sound)

(defun chess-sound-available-p ()
  (and (file-directory-p chess-sound-directory)
       (file-readable-p (expand-file-name "tap.wav"
					  chess-sound-directory))
       (or (eq chess-sound-play-function 'play-sound-file)
	   (file-executable-p chess-sound-program))))

(defun chess-sound-for-game (game)
  "Announce the opponent's moves in GAME."
  (chess-game-add-hook game 'chess-sound-handler))

(defun chess-sound (ch)
  (let ((file
	 (cond
	  ((stringp ch)
	   (format "%s.wav" ch))
	  ((memq ch '(?\# ?\+ ?k ?q ?b ?n ?r ?p ?x))
	   (format "%c_.wav" ch))
	  (t
	   (format "%s.wav" (chess-index-to-coord ch))))))
    (funcall chess-sound-play-function
	     (expand-file-name file chess-sound-directory))))

(defun chess-sound-play (file)
  (apply 'call-process chess-sound-program
	 nil nil nil (append chess-sound-args (list file))))

(defun chess-sound-handler (game ignore event &rest args)
  "This display module presents a standard chessboard.
See `chess-display-type' for the different kinds of displays."
  (when (eq event 'move)
    (let* ((ply (chess-game-ply game (1- (chess-game-index game))))
	   (pos (chess-ply-pos ply)))
      (if (eq (chess-game-data game 'my-color)
	      (chess-pos-side-to-move pos))
	  (chess-sound "tap")
	(let* ((source (chess-ply-source ply))
	       (target (chess-ply-target ply))
	       (s-piece (chess-pos-piece pos source))
	       (t-piece (chess-pos-piece pos target))
	       text)
	  (cond
	   ((chess-ply-has-keyword :castle)
	    (chess-sound "O-O"))
	   ((chess-ply-has-keyword :long-castle)
	    (chess-sound "O-O-O"))
	   ((= t-piece ? )
	    (chess-sound (downcase s-piece))
	    (chess-sound target))
	   (t
	    (chess-sound (downcase s-piece))
	    (chess-sound ?x)
	    (chess-sound (downcase t-piece))
	    (chess-sound target)))
	  (if (chess-ply-has-keyword :check)
	      (chess-sound ?+))
	  (if (chess-ply-has-keyword :checkmate)
	      (chess-sound ?#))
	  (if (chess-ply-has-keyword :stalemate)
	      (chess-sound "smate")))))))

(provide 'chess-sound)

;;; chess-sound.el ends here
