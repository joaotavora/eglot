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
(require 'chess-database)
(require 'chess-file)

(defgroup chess nil
  "An Emacs chess playing program."
  :group 'games)

(defconst chess-version "2.0a8"
  "The version of the Emacs chess program.")

(defcustom chess-default-displays
  '((chess-images chess-ics1 chess-plain)
    (chess-sound chess-announce))
  "Default displays to be used when starting a chess session.
This is a list of display modules, all of which will be invoked.  If
any entry is itself a list, then it specifies a series of alternatives
if the first modules were not available.
Note: The very first display is marked the 'main' display, which will
popup on significant events (unless `chess-display-popup' in nil);
also, killing this main display will cause all related chess buffers
to be killed."
  :type '(repeat (choice symbol (repeat symbol)))
  :group 'chess)

(defcustom chess-default-engine
  '(chess-crafty chess-gnuchess chess-phalanx)
  "Default engine to be used when starting a chess session.
A list indicates a series of alternatives if the first engines are not
available."
  :type '(choice symbol (repeat symbol))
  :group 'chess)

(defcustom chess-full-name (user-full-name)
  "The full name to use when playing chess."
  :type 'string
  :group 'chess)

(defun chess--create-display (module game my-color first disable-popup)
  (if (require module nil t)
      (let ((display (chess-display-create game module my-color first)))
	(when display
	  (chess-game-set-data game 'my-color my-color)
	  (if disable-popup
	      (chess-display-disable-popup display))
	  (chess-display-update display t)
	  display))))

(defun chess--create-engine (module game response-handler ctor-args)
  (if (require module nil t)
      (let ((engine (apply 'chess-engine-create game module
			   response-handler ctor-args)))
	(when engine
	  ;; for the sake of engines which are ready to play now, and
	  ;; which don't need connect/accept negotiation (most
	  ;; computerized engines fall into this category), we need to
	  ;; let them know we're ready to begin
	  (chess-engine-command engine 'ready)
	  engine))))

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

  (let ((my-color t)			; we start out as white always
	(game (chess-game-create))
	(first t)
	objects)

    (dolist (module chess-default-displays)
      (let (display)
	(if (symbolp module)
	    (setq display (chess--create-display module game my-color
						 first disable-popup))
	  ;; this module is actually a list, which means keep trying
	  ;; until we find one that works
	  (while module
	    (if (setq display (chess--create-display (car module) game
						     my-color first
						     disable-popup))
		(setq module nil)
	      (setq module (cdr module)))))
	(if display
	    (push display objects)))
      (setq first nil))

    (setq objects (nreverse objects))

    (let ((module (or engine chess-default-engine)))
      (if (symbolp module)
	  (push (chess--create-engine module game
				      engine-response-handler
				      engine-ctor-args)
		objects)
	(let (engine)
	  (while module
	    (setq engine (chess--create-engine (car module) game
					       engine-response-handler
					       engine-ctor-args))
	    (if engine
		(progn
		  (push engine objects)
		  (setq module nil))
	      (setq module (cdr module))))
	  (unless engine
	    (push nil objects)))))

    objects))

(defalias 'chess-session 'chess)

(defun chess-create-display ()
  "Just make a display to use, letting chess.el decide the style."
  (cadr (chess-session 'chess-none)))

;;;###autoload
(defun chess-read-pgn (&optional file)
  "Read and display a PGN game after point."
  (interactive "P")
  (if (or file (not (search-forward "[Event " nil t)))
      (setq file (read-file-name "Read a PGN game from file: ")))
  (if file
      (find-file file))
  (let ((game (chess-pgn-to-game))
	display)
    (when game
      (setq display (chess-create-display))
      (chess-display-set-game display game))))

;;;###autoload
(defun chess-puzzle (file &optional index)
  "Pick a random puzzle from FILE, and solve it against the default engine.
The spacebar in the display buffer is bound to `chess-puzzle-next',
making it easy to go on to the next puzzle once you've solved one."
  (interactive "fRead chess puzzles from: ")
  (random t)
  (let* ((database (chess-database-open 'chess-file file))
	 (objects (and database (chess-session)))
	 (display (cadr objects)))
    (when database
      (with-current-buffer display
	;; make sure the database is closed when the display is shutdown
	(chess-game-add-hook (chess-display-game nil)
			     'chess-database-event-handler database)
	(chess-game-set-data (chess-display-game nil) 'database database)
	(define-key (current-local-map) [? ] 'chess-puzzle-next)
	(chess-puzzle-next)))))

(defun chess-puzzle-next ()
  "Play the next puzzle in the collection, selected randomly."
  (interactive)
  (let* ((database (chess-game-data chess-display-game 'database))
	 (index (random (chess-database-count database)))
	 (next-game (chess-database-read database index)))
    (if (null next-game)
	(error "Error reading game at position %d" index)
      (chess-display-set-game nil next-game 0)
      (chess-game-set-data chess-display-game 'my-color
			   (chess-pos-side-to-move
			    (chess-game-pos chess-display-game)))
      (dolist (key '(database database-index database-count))
	(chess-game-set-data chess-display-game key
			     (chess-game-data next-game key))))))

(defun chess-write-game (game file)
  "Write a chess GAME to FILE as raw Lisp."
  (let ((game-copy (copy-alist game)))
    (chess-game-set-hooks game-copy nil)
    (chess-game-set-data-alist game-copy nil)
    (with-current-buffer (find-file-noselect)
      (erase-buffer)
      (prin1 game)
      (save-buffer))))

(defun chess-read-game (file)
  "Read a chess game as raw Lisp from FILE."
  (with-current-buffer (find-file-noselect)
    (goto-char (point-min))
    (read)))

(provide 'chess)

;;; chess.el ends here
