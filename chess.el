a0 243
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
(require 'chess-session)

(require 'chess-game)
(require 'chess-display)
(require 'chess-pgn)

(defgroup chess nil
  "An Emacs chess playing program."
  :group 'games)
(defconst chess-version "2.0a1"
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

(defun chess (&optional arg)
  "Start a game of chess."
  (interactive "P")
  (let ((session (chess-session-create))
	(perspective t))		; start out as white always
    ;; setup `chess-handler' to receive all events first
    (chess-session-add-listener session 'chess-handler)
    (chess-session-set-data session 'my-color perspective)
    ;; initialize all of the modules, and setup a new game
    (chess-session-event session 'initialize)
    (chess-session-event session 'setup (chess-game-create))
    ;; create a display object linked to the session, and add it to
    ;; the event chain; it is via this object that session events will
    ;; for the most part be generated
    (require chess-default-display)
    (chess-session-add-listener session 'chess-display nil
				(chess-display-create chess-default-display
						      perspective session))
    ;; unless prefix arg is given, use `chess-default-engine' to play
    ;; against; otherwise, just create a board for play between two
    ;; people
    (unless arg
      (require chess-default-engine)
      (chess-session-add-listener session 'chess-engine nil
				  (chess-engine-create chess-default-engine
						       nil session)))))

(defun chess-handler (session window-config event &rest args)
  "React to changes on the chess board in a global Emacs way."
  (if (eq event 'initialize)
      (current-window-configuration)
    (ignore
     (cond
      ((eq event 'shutdown)
       (set-window-configuration window-config))

      ((eq event 'setup)
       (chess-session-set-data session 'current-game (car args)))

      ((eq event 'pass)
       (let ((color (not (chess-session-data session 'my-color))))
	 (message "You are now playing %s" (if color "White" "Black"))
	 (chess-session-set-data session 'my-color (not color))))

      ((eq event 'move)
       (chess-game-move (chess-session-data session 'current-game)
			(car args)))))))
	    (aset chess-puzzle-locations 3 puzzle-engine)))))))

(provide 'chess)

;;; chess.el ends here
