;;; chess-phalanx.el --- Play chess against phalanx!

;; Copyright (C) 2002, 2004  Free Software Foundation, Inc.

;; Author: John Wiegley
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games, processes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'chess-common)

(defgroup chess-phalanx nil
  "The publically available chess engine 'phalanx'."
  :group 'chess-engine
  :link '(url-link "http://dusan.freeshell.org/phalanx/"))

(defcustom chess-phalanx-path (executable-find "phalanx")
  "The path to the phalanx executable."
  :type 'file
  :group 'chess-phalanx)

(defvar chess-phalanx-regexp-alist
  (list
   (cons (concat "my move is P?\\(" chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 1) t)))))
   (cons "Illegal move:\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (error (match-string 1)))))))

(defun chess-phalanx-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-common-handler game 'initialize "phalanx")))
	(when (and proc (processp proc)
		   (eq (process-status proc) 'run))
	  (process-send-string proc "nopost\n")
	  (setq chess-engine-process proc
		chess-engine-opponent-name "Phalanx")
	  t)))

     ((eq event 'resign)
      (chess-game-set-data game 'active nil))

     (t
      (apply 'chess-common-handler game event args)))))

(provide 'chess-phalanx)

;;; chess-phalanx.el ends here
