;;; chess.el --- Play chess in Emacs

;; Copyright (C) 2001 John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: chess.el
;; Version: 2.0
;; Keywords: games
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: John Wiegley <johnw@gnu.org>
;; Description: Play chess in Emacs
;; URL: http://www.gci-net.com/~johnw/Emacs/packages/chess.tar.gz
;; Compatibility: Emacs20, Emacs21, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Welcome to Emacs Chess, a chess playing module for GNU Emacs.
;;
;; This program will not play chess against you; it is not a chess
;; computer.  It can use a chess computer, however, to simulate your
;; opponent's moves.  This is decided when you choose your opponent.
;; You must, of course, have that chess computer installed.  See the
;; top of chess-player.el for more information.
;;
;; To just get a chessboard up, put the following in your .emacs file:
;;
;;   (add-to-list 'load-list "<the path to Emacs Chess>")
;;
;;   (autoload 'chess "chess" "Play a game of chess" t)
;;
;; Now you can type `M-x chess', and play chess against anyone else in
;; the room with you, without having to install anything more.
;;
;; Once this is working, the next thing to do is to customize
;; `chess-use-modules'.  This is a list of functionality modules used
;; by chess.el to provide its functionality.  You can enable or
;; disable modules so that Emacs Chess better suites your tastes.
;; Those modules in turn often have configuration variables, and
;; appropriate documentation at the top of the related file.
;;
;; Emacs Chess is designed in a highly modular fashion, using loosely
;; coupled modules that respond to events on the chess board.  This
;; makes it very easy for programmers to add their own types of
;; displays, opponents, analysis programs, etc.  See the documentation
;; in chess-module.el to learn more.
;;
;; There is no documentation for this program other than what exists
;; in the source files.  This is because the source files aim at being
;; self documenting, and as chess is such a simple game, most chess
;; players aren't going to need to know much about this program in
;; particular.
;;
;; However, most people will probably be interested in reading the top
;; of chess-display.el and chess-pgn.el, which describe the user
;; interface commands available in each of those buffer types.

;;; Code:

(require 'chess-game)
(require 'chess-display)
(require 'chess-engine)
(require 'chess-pgn)

(defgroup chess nil
  "An Emacs chess playing program."
  :group 'games)

(defconst chess-version "2.0a7"
  "The version of the Emacs chess program.")

(defcustom chess-default-display (if (display-graphic-p)
				     'chess-images 'chess-ics1)
  "Default module set to be used when starting a chess session."
  :type 'sexp
  :group 'chess)

(defcustom chess-default-engine 'chess-gnuchess
  "Default engine to be used when starting a chess session."
  :type 'sexp
  :group 'chess)

(defcustom chess-announce-moves t
  "If non-nil, announce when your opponent makes a move.
This variable can also be a symbol which names a different announcing
module to use.  This happens verbally if 'festival' is installed, or
if you have sound files installed and a sound play (see
chess-sound.el).  Otherwise it just prints a message in your
minibuffer, which works well for Emacspeak users."
  :type 'boolean
  :group 'chess)

(defcustom chess-full-name (user-full-name)
  "The full name to use when playing chess."
  :type 'string
  :group 'chess)

;;;###autoload
(defun chess (&optional engine disable-popup engine-response-handler
			&rest engine-ctor-args)
  "Start a game of chess, playing against ENGINE (a module name)."
  (interactive
   (list
    (if current-prefix-arg
	(intern
	 (concat "chess-"
		 (let ((str (read-string "Engine to play against: ")))
		   (if (> (length str) 0)
		       str
		     "none"))))
      chess-default-engine)))

  (require chess-default-display)
  (let* ((my-color t)			; we start out as white always
	 (display (chess-display-create chess-default-display my-color))
	 (game (chess-game-create)))

    (when (and (eq chess-default-display 'chess-images)
	       (with-current-buffer display
		 (null chess-images-size)))
      (message "Could not find suitable chess images; using ics1 display")
      (chess-display-destroy display)
      (require 'chess-ics1)
      (setq display (chess-display-create 'chess-ics1 my-color)))

    (chess-game-set-data game 'my-color my-color)
    (if disable-popup
	(chess-display-disable-popup display))
    (chess-display-set-game display game)
    (chess-display-set-main display)

    (let ((engine-module (or engine chess-default-engine)))
      (when (and engine-module (require engine-module nil t))
	(let ((engine (apply 'chess-engine-create engine-module nil
			     engine-ctor-args)))
	  (chess-engine-set-game* engine game)
	  ;; for the sake of engines which are ready to play now, and
	  ;; which don't need connect/accept negotiation (most
	  ;; computerized engines fall into this category), we need to
	  ;; let them know we're ready to begin
	  (chess-engine-command engine 'ready))

	(when chess-announce-moves
	  (if (and (not (eq chess-announce-moves t))
		   (symbolp chess-announce-moves))
	      (let ((name (symbol-name chess-announce-moves)))
		(require chess-announce-moves)
		(if (funcall (intern (concat name "-available-p")))
		    (funcall (intern (concat name "-for-game")) game)))
	    (require 'chess-sound)
	    (if (chess-sound-available-p)
		(chess-sound-for-game game)
	      (require 'chess-announce)
	      (if (chess-announce-available-p)
		  (chess-announce-for-game game)))))))

    (chess-display-update display t)

    (cons display engine)))

(defalias 'chess-session 'chess)

;;;###autoload
(defun chess-read-pgn (&optional file)
  "Read and display a PGN game after point."
  (interactive "P")
  (if (or file (not (search-forward "[Event" nil t)))
      (setq file (read-file-name "Read a PGN game from file: ")))
  (if file
      (find-file file))
  (let ((game (chess-pgn-to-game)))
    (when game
      (require chess-default-display)
      (chess-display-set-game
       (chess-display-create chess-default-display
			     (chess-game-side-to-move game)) game))))

(defvar chess-puzzle-locations nil)

(defun chess-puzzle-next ()
  "Play the next puzzle in the collection, selected randomly."
  (interactive)
  (if chess-puzzle-locations
      (chess-puzzle (aref chess-puzzle-locations 0))))

;;;###autoload
(defun chess-puzzle (file)
  "Pick a random puzzle from FILE, and solve it against the default engine.
The spacebar in the display buffer is bound to `chess-puzzle-next',
making it easy to go on to the next puzzle once you've solved one."
  (interactive "fRead chess puzzles from: ")
  (save-excursion
    (with-current-buffer (find-file-noselect file)
      (when (or (null chess-puzzle-locations)
		(not (equal file (aref chess-puzzle-locations 0))))
	(let (locations)
	  (goto-char (point-min))
	  (while (search-forward "[Event" nil t)
	    (push (point) locations))
	  (setq chess-puzzle-locations (vector file locations nil nil)))
	(random t))
      (goto-char (nth (random (length (aref chess-puzzle-locations 1)))
		      (aref chess-puzzle-locations 1)))
      (let ((game (chess-pgn-to-game)))
	(when game
	  (require chess-default-display)
	  (let ((puzzle-display
		 (or (and (buffer-live-p (aref chess-puzzle-locations 2))
			  (aref chess-puzzle-locations 2))
		     (chess-display-create chess-default-display
					   (chess-game-side-to-move game)))))
	    (chess-display-set-game puzzle-display game)
	    (aset chess-puzzle-locations 2 puzzle-display)
	    ;; setup spacebar as a convenient way to jump to the next puzzle
	    (with-current-buffer puzzle-display
	      (define-key (current-local-map) [? ] 'chess-puzzle-next)))
	  (require chess-default-engine)
	  (let ((puzzle-engine
		 (or (and (buffer-live-p (aref chess-puzzle-locations 3))
			  (aref chess-puzzle-locations 3))
		     (chess-engine-create chess-default-engine))))
	    (chess-engine-set-game puzzle-engine game)
	    (aset chess-puzzle-locations 3 puzzle-engine)))))))

(provide 'chess)

;;; chess.el ends here
