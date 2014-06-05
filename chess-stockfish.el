;;; chess-stockfish.el --- Play against stockfish!

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

(defgroup chess-stockfish nil
  "The publically available chess engine 'stockfish'."
  :group 'chess-engine
  :link '(custom-manual "(chess)Stockfish")
  :link '(url-link "http://www.stockfishchess.com"))

(defcustom chess-stockfish-path (executable-find "stockfish")
  "*The path to the stockfish executable."
  :type 'file
  :group 'chess-stockfish)

(defvar chess-stockfish-regexp-alist
  (append
   chess-uci-regexp-alist
   (list
    (cons (concat "^info\\s-+.*nps\\s-+\\([0-9]+\\).*pv\\s-+\\("
		  chess-uci-long-algebraic-regexp
		  "\\(\\s-+" chess-uci-long-algebraic-regexp "\\)+\\)")
	  (function
	   (lambda ()
	     (setq-local chess-stockfish-nps (string-to-number (match-string 1)))
	     (setq-local chess-stockfish-pv
			 (split-string (match-string 2) " ")))))))
  "Patterns used to match stockfish output.")

(defun chess-stockfish-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-uci-handler game 'initialize "stockfish")))
	(when (and proc (processp proc) (eq (process-status proc) 'run))
	  (process-send-string proc "uci\n")
	  (setq chess-engine-process proc)
	  t)))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after stockfish moves"))

      (apply 'chess-uci-handler game event args)))))

(provide 'chess-stockfish)

;;; chess-stockfish.el ends here
