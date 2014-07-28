;;; chess-eco.el --- Chess opening classification

;; Copyright (C) 2004, 2014  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
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

;;; Code:

(require 'chess-algebraic)
(require 'chess-fen)
(require 'chess-game)
(require 'chess-ply)
(require 'chess-pos)

(defgroup chess-eco nil
  "Chess opening classification module."
  :group 'chess)

(defcustom chess-eco-max-index 36
  "*Index at which to stop chess opening announcements."
  :group 'chess-eco
  :type 'integer)

(defvar chess-eco-hash-table
  (when (file-exists-p 
	 (expand-file-name "chess-eco.fen"
			   (file-name-directory load-file-name)))
    (with-temp-buffer
      (message "Emacs Chess: Loading ECO openings database...")
      (insert-file-contents "chess-eco.fen")
      (prog1
	  (let ((fen-data (read (current-buffer)))
		(hash (make-hash-table :size 10541 :test 'equal)))
	    (mapc (lambda (entry)
		    (puthash (car entry) (cdr entry) hash))
		  fen-data)
	    hash)
	(message "Emacs Chess: Loading ECO openings database...done"))))
  "List of well known chess opening positions.")

(defun chess-generate-fen-table ()
  "Generate chess-eco.fen from the ply lists in chess-eco.pos."
  (require 'chess-pos)
  (require 'chess-ply)
  (require 'chess-fen)
  (require 'chess-algebraic)
  (with-temp-buffer
    (insert-file-contents (car command-line-args-left))
    (let ((fen-buffer (get-buffer-create "chess-eco.fen"))
	  (pos-data (read (current-buffer))))
      (with-current-buffer fen-buffer
	(print (mapcar
		(lambda (entry)
		  (message "Preparing opening %s (%s)"
			   (car entry) (cadr entry))
		  (let ((pos (chess-pos-create)))
		    (mapc (lambda (move)
			    (apply 'chess-pos-move
				   pos (chess-ply-changes
					(chess-algebraic-to-ply pos move))))
			  (split-string (car (cddr entry)) " " t))
		    (list (chess-pos-to-fen pos) (cadr entry) (car entry))))
		pos-data)
	       (current-buffer))
	(write-file (cadr command-line-args-left))))))

(defvar chess-eco-last-opening nil)
(make-variable-buffer-local 'chess-eco-last-opening)

(defun chess-eco-classify (game)
  (when chess-eco-hash-table
    (let ((plies (chess-game-plies game))
	  found)
      (while plies
	(let* ((fen (chess-pos-to-fen (chess-ply-pos (car plies))))
	       (entry (gethash fen chess-eco-hash-table)))
	  (if entry
	      (setq found entry))
	  (setq plies (cdr plies))))
      found)))

(chess-message-catalog 'english
  '((announce-opening . "%s (ECO code %s)")))

(defun chess-eco-handler (game event &rest _args)
  "Handle for the `chess-eco' module.
If you add `chess-eco' to `chess-default-modules', this handler will
try to figure out if the current position of a game does match a
well known chess opening position."
  (cond
   ((eq event 'initialize))

   ((eq event 'post-move)
    (when (= (chess-game-index game) 1)
      (setq chess-eco-last-opening nil))
    (when (< (chess-game-index game) chess-eco-max-index)
      (let ((info (chess-eco-classify game)))
	(when (and info (not (eq info chess-eco-last-opening)))
	  (setq chess-eco-last-opening info)
	  (chess-message 'announce-opening (car info) (cadr info))))))))

(defun chess-eco-parse-scid-eco ()
  (let ((result (list t)))
    (while (re-search-forward
	    "\\([A-E][0-9][0-9]\\([a-z][0-9]?\\)?\\) \"\\([^\"]+\\)\"[\n ]+\\([^*]*\\|\n\\) +\\*"
	    nil t)
      (nconc
       result
       (list
	(list (match-string 1)
	      (match-string 3)
	      (mapconcat (lambda (move)
			   (if (string-match
				(concat
				 "\\(" chess-algebraic-regexp "\\)")
				     move)
				    (match-string 1 move)
				  move))
			      (split-string (match-string 4) "[\n ]+") " ")))))
    (cdr result)))

(provide 'chess-eco)

;;; chess-ecos.el ends here
