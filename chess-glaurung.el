;;; chess-glaurung.el --- Play against glaurung!

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

(defgroup chess-glaurung nil
  "The publically available chess engine 'glaurung'."
  :group 'chess-engine
  :link '(custom-manual "(chess)Glaurung")
  :link '(url-link "http://www.glaurungchess.com/"))

(defcustom chess-glaurung-path (executable-find "glaurung")
  "*The path to the glaurung executable."
  :type 'file
  :group 'chess-glaurung)

(defvar chess-glaurung-regexp-alist chess-uci-regexp-alist
  "Patterns used to match engine output.")

(defun chess-glaurung-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-uci-handler game 'initialize "glaurung")))
	(when (and proc (processp proc) (eq (process-status proc) 'run))
	  (process-send-string proc "uci\n")
	  (setq chess-engine-process proc)
	  t)))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after glaurung moves"))

      (apply 'chess-uci-handler game event args)))))

(provide 'chess-glaurung)

;;; chess-glaurung.el ends here
