;;; chess-none.el --- Null engine

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A null engine, used when two humans play against each other on the
;; same display.

;;; Code:

(require 'chess-engine)

(defvar chess-none-regexp-alist nil)

(defun chess-none-handler (game event &rest args)
  "An empty chess engine, used for fielding key events.
This is only useful when two humans are playing each other, in which
case this engine will do the job of accepting undos, handling
resignations, etc."
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize) t)

     ((memq event '(resign abort))
      (chess-engine-set-position nil))

     ((eq event 'undo)
      (chess-game-undo game (car args))))))

(provide 'chess-none)

;;; chess-none.el ends here
