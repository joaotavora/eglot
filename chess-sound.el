;;; chess-sound.el --- Announce chess moves with pre-recorded sound files

;; Copyright (C) 2002, 2008, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is very similar to chess-announce, except it uses specific
;; .WAV files instead of text-to-speech.

;;; Code:

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
  :type 'function
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

(defcustom chess-sound-my-moves nil
  "If non-nil, plays the move.wav sound whenever you make a move."
  :type 'boolean
  :group 'chess-sound)

(defsubst chess-sound (file)
  (ignore-errors
    (funcall chess-sound-play-function
	     (expand-file-name (concat file ".wav")
			       chess-sound-directory))))

(defsubst chess-sound-play (file)
  (apply 'call-process chess-sound-program
	 nil nil nil (append chess-sound-args (list file))))

(defun chess-sound-handler (game event &rest _args)
  (cond
   ((eq event 'initialize)
    (and (file-directory-p chess-sound-directory)
	 (file-readable-p (expand-file-name "move.wav"
					    chess-sound-directory))
	 (or (eq chess-sound-play-function 'play-sound-file)
	     (and chess-sound-program
		  (file-executable-p chess-sound-program)))))

   ((eq event 'move)
    (let* ((ply (chess-game-ply game (1- (chess-game-index game))))
	   (pos (chess-ply-pos ply)))
      (if (eq (chess-game-data game 'my-color)
	      (chess-pos-side-to-move pos))
	  (if chess-sound-my-moves
	      (chess-sound "move"))
	(let* ((source (chess-ply-source ply))
	       (target (chess-ply-target ply))
	       (s-piece (and source (chess-pos-piece pos source)))
	       (t-piece (and target (chess-pos-piece pos target)))
	       (which (chess-ply-keyword ply :which)))
	  (cond
	   ((chess-ply-keyword ply :castle)
	    (chess-sound "O-O"))
	   ((chess-ply-keyword ply :long-castle)
	    (chess-sound "O-O-O"))
	   ((and s-piece t-piece (= t-piece ? ) target)
	    (if which
		(chess-sound (char-to-string which)))
	    (chess-sound (format "%c_" (downcase s-piece)))
	    (chess-sound (chess-index-to-coord target)))
	   ((and s-piece t-piece target)
	    (if which
		(chess-sound (char-to-string which)))
	    (chess-sound (format "%c_" (downcase s-piece)))
	    (chess-sound "x_")
	    (chess-sound (format "%c_" (downcase t-piece)))
	    (chess-sound (chess-index-to-coord target))))

	  (if (chess-ply-keyword ply :promote)
	      (chess-sound
	       (format "%c_" (downcase
			      (chess-ply-keyword ply :promote)))))
	  (if (chess-ply-keyword ply :en-passant)
	      (chess-sound "enpassant"))
	  (if (chess-ply-keyword ply :check)
	      (chess-sound "+_"))
	  (if (chess-ply-keyword ply :checkmate)
	      (chess-sound "#_"))
	  (if (chess-ply-keyword ply :stalemate)
	      (chess-sound "smate"))))))))

(provide 'chess-sound)

;;; chess-sound.el ends here
