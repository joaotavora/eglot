;;; chess-scid.el --- A game database that uses SCID for storage/retrieval

;; Copyright (C) 2002, 2004, 2008  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
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

;;; Commentary:

;; The advantage of SCID is that it's much faster than PGN, and far, far more
;; compact.
;;
;; SCID offers a Tcl-based command-line interface, tcscid, which is what
;; this module uses to query SCID databases.

;;; Code:

(require 'chess-game)
(require 'chess-message)
(require 'chess-pgn)

(chess-message-catalog 'english
  '((failed-load     . "Failed to load game %d from ChessDB")
    (failed-find-end . "Failed to locate end of game %d in ChessDB")))

(defvar chess-scid-process)

(make-variable-buffer-local 'chess-scid-process)

(defsubst chess-scid-send (string)
  (process-send-string chess-scid-process (concat string "\n")))

(defun chess-scid-get-result (command)
  (let ((here (point-max)) (iterations 10))
    (chess-scid-send command)
    (accept-process-output chess-scid-process)
    (while (and (> (setq iterations (1- iterations)) 0)
		(eobp))
      (accept-process-output chess-scid-process 1 0 t))
    (if (= here (point-max))
	(error "chess-scid: '%s' failed to produce any output" command))
    (goto-char (point-max))
    (skip-chars-backward " \t\n\r%")
    (prog1
	(buffer-substring here (point))
      (erase-buffer))))

(defun chess-scid-handler (event &rest args)
  (cond
   ((eq event 'open)
    (let* ((db-file (if (string-match "\\.sg3\\'" (car args))
			(car args)
		      (concat (car args) ".sg3")))
	   (db-base (and (string-match "\\`\\(.+\\)\\.sg3\\'" db-file)
			 (match-string 1 db-file))))
      (if (and db-base (file-readable-p db-file))
	  (let* ((buffer (generate-new-buffer " *chess-scid*"))
		 (proc (start-process "*chess-scid*" buffer
				      (or (executable-find "tcscid")
					  (executable-find "tcchessdb")))))
	    (if (and proc (eq (process-status proc) 'run))
		(with-current-buffer buffer
		  (accept-process-output proc)
		  (setq chess-scid-process proc)
		  (if (= 1 (string-to-number
			    (chess-scid-get-result
			     (format "sc_base open %s"
				     (expand-file-name db-base)))))
		      buffer
		    (kill-process proc)
		    (kill-buffer buffer)
		    nil))
	      (kill-buffer buffer)
	      nil)))))

   ((eq event 'close)
    (chess-scid-send "sc_base close\nexit")
    (while (eq (process-status chess-scid-process) 'run)
      (sit-for 0 250)))

   ((eq event 'read-only-p)
    (if (zerop (string-to-number (chess-scid-get-result "sc_base isReadOnly")))
	nil
      t))

   ((eq event 'filename)
    (chess-scid-get-result "sc_base filename"))

   ((eq event 'count)
    (string-to-number (chess-scid-get-result "sc_base numGames")))

   ((eq event 'read)
    ;; clear the buffer, since we don't need old data here any more, and it
    ;; can accumulate without bound during running of the validation tests
    (erase-buffer)
    (process-send-string chess-scid-process
			 (format "sc_game load %d\n" (1+ (car args))))
    (accept-process-output chess-scid-process)
    (let ((here (point-max))
	  (iterations 10)
	  found)
      (process-send-string chess-scid-process "sc_game pgn\n")
      (accept-process-output chess-scid-process)
      (goto-char here)
      (while (and (> (setq iterations (1- iterations)) 0)
		  (not (and (or (looking-at "\\[")
				(and (search-forward "[" nil t)
				     (goto-char (match-beginning 0))))
			    (setq found t))))
	(accept-process-output chess-scid-process 1 0 t))
      (if (not found)
	  (chess-error 'failed-load (1+ (car args)))
	(setq iterations 10 found nil here (point))
	(while (and (> (setq iterations (1- iterations)) 0)
		    (not (and (re-search-forward "\\(\\*\\|1-0\\|0-1\\|1/2-1/2\\)" nil t)
			      (goto-char (match-beginning 0))
			      (setq found t))))
	  (accept-process-output chess-scid-process 1 0 t))
	(if (not found)
	    (chess-error 'failed-find-end (1+ (car args)))
	  (goto-char here)
	  (let ((game (chess-pgn-to-game)))
	    (when game
	      (chess-game-set-data game 'database (current-buffer))
	      (chess-game-set-data game 'database-index (car args))
	      (chess-game-set-data game 'database-count (chess-scid-handler 'count))
	      game))))))

   ((and (eq event 'query)
	 (eq (car args) 'tree-search))
    (if (string=
	 (chess-scid-get-result
	  (format "sc_game import \"%s\""
		  (with-temp-buffer
		    (chess-pgn-insert-plies
		     (cadr args) 1 (chess-game-plies (cadr args)))
		    (insert " " (or (chess-game-tag (cadr args) "Result") "*"))
		    (buffer-string))))
	 "PGN text imported with no errors or warnings.")
	(let ((here (point-max)))
	  (chess-scid-send "sc_tree search")
	  (accept-process-output chess-scid-process)
	  (goto-char here)
	  (let ((lines (list t)))
	    (while
		(re-search-forward
		 (concat "\\s-*\\([1-9][0-9]*\\):\\s-+\\([^ ]+\\)\\s-+"
			 "\\([0-9]+\\):\\s-*\\([0-9.]+\\)%\\s-+"
			 "\\([0-9.]+\\)%\\s-+\\([0-9]+\\|    \\)\\s-+"
			 "\\([0-9]+\\|    \\)\\s-+\\([0-9]+\\|    \\)\\s-+"
			 "\\([0-9.]+\\)%")
		 nil t)
	      (let ((move (match-string 2))
		    (freq (string-to-number (match-string 3)))
		    (score (string-to-number (match-string 5)))
		    (avgelo (string-to-number (match-string 6)))
		    (perf (string-to-number (match-string 7)))
		    (avgyear (string-to-number (match-string 8)))
		    (draws (string-to-number (match-string 9))))
		(nconc lines
		       (list
			(append
			 (list move freq score draws)
			 (cond
			  ((and (zerop avgyear) (zerop avgelo) (zerop perf))
			   nil)
			  ((and (zerop avgelo) (zerop perf))
			   (list avgyear))
			  (t
			   (list
			    (unless (zerop avgyear) avgyear)
			    (unless (zerop avgelo) avgelo)
			    (unless (zerop perf) perf)))))))))
	    (when (re-search-forward
		   (concat "TOTAL:\\s-+\\([0-9]+\\):100.0%\\s-+"
			   "\\([0-9.]+\\)%\\s-+\\([0-9]+\\|    \\)\\s-+"
			   "\\([0-9]+\\|    \\)\\s-+\\([0-9]+\\|    \\)\\s-+"
			   "\\([0-9.]+\\)%") nil t)
	      (goto-char (point-max))
	      (append
	       (list (string-to-number (match-string 1))
		     (string-to-number (match-string 2))
		     (string-to-number (match-string 6)))
	       (cdr lines)))))
      (error "Unable to import game")))

   ((eq event 'write)
    (chess-scid-handler 'replace (car args) 0))

   ((eq event 'replace)
    (unless (chess-scid-handler 'read-only-p)
      (let ((index (or (1+ (cadr args))
		       (chess-game-data (car args) 'database-index))))
	(chess-scid-send
	 (format "sc_game import \"%s\""
		 (with-temp-buffer
		   (chess-pgn-insert-plies
		    (car args) 1 (chess-game-plies (car args)))
		   (insert (or (chess-game-tag (car args) "Result") "*"))
		   (buffer-string))))
	(dolist (tag (chess-game-tags (car args)))
	  ;; jww (2002-05-01): how do I set extra tags?
	  (unless (string= (car tag) "TimeControl")
	    (chess-scid-send
	     (concat "sc_game tags set "
		     (cond
		      ((string= (car tag) "Event")     "-event")
		      ((string= (car tag) "Site")      "-site")
		      ((string= (car tag) "Date")      "-date")
		      ((string= (car tag) "Round")     "-round")
		      ((string= (car tag) "White")     "-white")
		      ((string= (car tag) "WhiteElo")  "-whiteElo")
		      ((string= (car tag) "Black")     "-black")
		      ((string= (car tag) "BlackElo")  "-blackElo")
		      ((string= (car tag) "Result")    "-result")
		      ((string= (car tag) "ECO")       "-eco")
		      ((string= (car tag) "EventDate") "-eventdate")
		      ((string= (car tag) "Extra")     "-extra"))
		     " \"" (cdr tag) "\""))))
	(chess-scid-send (format "sc_game save %d" index)))))))

(provide 'chess-scid)

;;; chess-scid.el ends here
