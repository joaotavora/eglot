;;; chess-fruit.el --- Play against fruit!

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games, processes

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

;;; Code:

(require 'chess-uci)

(defgroup chess-fruit nil
  "The publically available chess engine 'fruit'."
  :group 'chess-engine
  :link '(url-link "http://www.fruitchess.com/"))

(defcustom chess-fruit-path (executable-find "fruit")
  "*The path to the fruit executable."
  :type 'file
  :group 'chess-fruit)

(defvar chess-fruit-regexp-alist chess-uci-regexp-alist
  "Patterns used to match engine output.")

(defun chess-fruit-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-uci-handler game 'initialize "fruit")))
	(when (and proc (processp proc) (eq (process-status proc) 'run))
	  (process-send-string proc "uci\n")
	  (setq chess-engine-process proc)
	  t)))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after fruit moves"))

      (apply 'chess-uci-handler game event args)))))

(provide 'chess-fruit)

;;; chess-fruit.el ends here
