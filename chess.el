;;; chess.el --- Play chess in Emacs

;; Copyright (C) 2001 John Wiegley <johnw@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: chess.el
;; Version: 2.0
;; Keywords: games
;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Description: Play chess in Emacs
;; URL: http://emacs-chess.sourceforge.net/
;; Compatibility: Emacs21

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
;;   (add-to-list 'load-path "<the path to Emacs Chess>")
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

(defgroup chess nil
  "An Emacs chess playing program."
  :group 'games)

(defconst chess-version "2.0b6"
  "The version of the Emacs chess program.")

(defcustom chess-default-display
  '(chess-images chess-ics1 chess-plain)
  "Default display to be used when starting a chess session.
A list indicates a series of alternatives if the first display is
not available."
  :type '(choice symbol (repeat symbol))
  :group 'chess)

(defcustom chess-default-modules
  '((chess-sound chess-announce)
    chess-autosave
    chess-clock
    ;;chess-kibitz   jww (2002-04-30): not fully supported yet
    ;;chess-chat
    )
  "Modules to be used when starting a chess session.
A sublist indicates a series of alternatives, if the first is not
available.
These can do just about anything."
  :type '(repeat (choice symbol (repeat symbol)))
  :group 'chess)

(defcustom chess-default-engine
  '(chess-crafty chess-gnuchess chess-phalanx chess-ai)
  "Default engine to be used when starting a chess session.
A list indicates a series of alternatives if the first engine is not
available."
  :type '(choice symbol (repeat symbol))
  :group 'chess)

(defcustom chess-full-name (user-full-name)
  "The full name to use when playing chess."
  :type 'string
  :group 'chess)

(defun chess--create-display (module game my-color disable-popup)
  (let ((display (chess-display-create game module my-color)))
    (when display
      (chess-game-set-data game 'my-color my-color)
      (if disable-popup
	  (chess-display-disable-popup display))
      display)))

(defun chess--create-engine (module game response-handler ctor-args)
  (let ((engine (apply 'chess-engine-create module game
		       response-handler ctor-args)))
    (when engine
      ;; for the sake of engines which are ready to play now, and
      ;; which don't need connect/accept negotiation (most
      ;; computerized engines fall into this category), we need to
      ;; let them know we're ready to begin
      (chess-engine-command engine 'ready)
      engine)))

(defun chess-create-modules (module-list create-func &rest args)
  "Create modules from MODULE-LIST with CREATE-FUNC and ARGS.
If an element of MODULE-LIST is a sublist, treat it as alternatives."
  (let (objects)
    (dolist (module module-list)
      (let (object)
	(if (symbolp module)
	    (if (setq object (apply create-func module args))
		(push object objects))
	  ;; this module is actually a list, which means keep trying
	  ;; until we find one that works
	  (while module
	    (if (setq object (apply create-func (car module) args))
		(progn
		  (push object objects)
		  (setq module nil))
	      (setq module (cdr module)))))))
    (nreverse objects)))

(chess-message-catalog 'english
  '((no-engines-found . "Could not find any chess engines to play against; install gnuchess!")))

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

  (let ((game (chess-game-create))
	(my-color t)			; we start out as white always
	objects)

    ;; all these odd calls are so that `objects' ends up looking like:
    ;;   (ENGINE FIRST-DISPLAY...)

    (setq objects (chess-create-modules (list chess-default-display)
					'chess--create-display
					game my-color disable-popup))
    (when (car objects)
      (mapc 'chess-display-update objects)
      (chess-module-set-leader (car objects))
      (unless disable-popup
	(chess-display-popup (car objects))))

    (nconc objects (chess-create-modules chess-default-modules
					 'chess-module-create game))

    (push (unless (eq engine 'none)
	    (car ;(condition-case nil
		     (chess-create-modules (list (or engine chess-default-engine))
					   'chess--create-engine game
					   engine-response-handler
					   engine-ctor-args)
		   ;(error nil))
		     ))
	  objects)

    (unless (car objects)
      (chess-message 'no-engines-found))

    objects))

;;;###autoload
(defalias 'chess-session 'chess)

;;;###autoload
(defun chess-create-display (perspective &optional modules-too)
  "Create a display, letting the user's customization decide the style.
If MODULES-TOO is non-nil, also create and associate the modules
listed in `chess-default-modules'."
  (if modules-too
      (let ((display (cadr (chess-session 'none))))
	(chess-display-set-perspective* display perspective))
    (car (chess-create-modules (list chess-default-display)
			       'chess--create-display
			       (chess-game-create) perspective nil))))

(provide 'chess)

;;; chess.el ends here
