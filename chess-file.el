;;; chess-file.el --- Handle chess databases stored in PGN or EPD files

;; Copyright (C) 2002, 2004, 2014  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: files, games

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

;;; Commentary:

;; A game database that stores PGN or EPD format positions in a single file.
;;
;; This is basically what you expect from a file ending in .pgn or .epd.

;;; Code:

(require 'chess-fen)
(require 'chess-pgn)

(defvar chess-file-locations nil
  "A list of starting positions of individual records of this collection.")
(make-variable-buffer-local 'chess-file-locations)

(defvar chess-file-type nil
  "The file format type of this database instance (a symbol).
See `chess-file-types' for details.")
(make-variable-buffer-local 'chess-file-type)

(defvar chess-file-types
  `((pgn "^\\[Event " chess-pgn-to-game chess-game-to-pgn (?\n ?\n))
    (epd ,(concat chess-fen-regexp "\\(\\s-+.+\\);\\s-*$")
	 chess-epd-to-game chess-game-to-epd (?\n)))
  "Alist of different file types.
Entries have the form (TYPE BEGIN-REGEXP TO-GAME FROM-GAME SEPARATOR)
where TYPE is a symbol (usually either 'pgn or 'epd),
BEGIN-REGEXP is the regexp to use for matching the beginning of new records,
TO-GAME and FROM-GAME are functions to use for reading and writing a game
object from/into the buffer and SEPARATOR is a list of characters to insert
inbetween of individual records.")

(defun chess-file-handler (event &rest args)
  "Event handler for file database objects."
  (cond
   ((eq event 'open)
    (with-current-buffer (find-file-noselect (car args))
      (when (setq chess-file-type
		  (cond
		   ((or (string-match "\\.pgn\\'" (car args))
			(save-excursion (re-search-forward "^\\[Event" nil t)))
		    'pgn)
		   ((string-match "\\.epd\\'" (car args))
		    'epd)))
	(chess-file-handler 'rescan)
	(current-buffer))))

   ((eq event 'rescan)
    (save-excursion
      (goto-char (point-min))
      (setq chess-file-locations nil)
      (while (re-search-forward (nth 1 (assq chess-file-type chess-file-types))
				nil t)
	(goto-char (match-beginning 0))
	(push (point) chess-file-locations)
	(forward-char 1))
      (setq chess-file-locations (nreverse chess-file-locations))))

   ((eq event 'read-only-p)
    buffer-read-only)

   ((eq event 'filename)
    buffer-file-name)

   ((eq event 'save)
    (save-buffer))

   ((eq event 'count)
    (length chess-file-locations))

   ((eq event 'read)
    (let ((index (car args)) game)
      (when (and (>= index 0)
		 (< index (chess-file-handler 'count)))
	(goto-char (nth index chess-file-locations))
	(when (setq game (funcall (nth 2 (assq chess-file-type
					       chess-file-types))))
	  (chess-game-set-data game 'database (current-buffer))
	  (chess-game-set-data game 'database-index index)
	  (chess-game-set-data game 'database-count
			       (chess-file-handler 'count))
	  game))))

   ((eq event 'write)
    (goto-char (point-max))
    (while (memq (char-before) '(?  ?\t ?\n ?\r))
      (delete-char -1))
    (apply 'insert (nth 4 (assq chess-file-type chess-file-types)))
    (push (point) chess-file-locations)
    (funcall (nth 3 (assq chess-file-type chess-file-types)) (car args))
    (1- (chess-file-handler 'count)))

   ((eq event 'replace)
    (let ((index (or (cadr args)
		     (chess-game-data (car args) 'database-index)))
	  (count (chess-file-handler 'count)))
      (when (and (>= index 0)
		 (< index count))
	(goto-char (nth index chess-file-locations))
	(delete-region (point) (if (= (1+ index) count)
				   (point-max)
				 (nth (1+ index) chess-file-locations)))
	(funcall (nth 3 (assq chess-file-type chess-file-types)) (car args))
	(when (eq chess-file-type 'pgn) (insert ?\n)))))))

(provide 'chess-file)

;;; chess-file.el ends here
