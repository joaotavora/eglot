;;; chess-sjeng.el --- Play against sjeng!

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: games

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

;;; Code:

(require 'chess-common)

(defgroup chess-sjeng nil
  "The publically available chess engine 'sjeng'."
  :group 'chess-engine
  :link '(url-link "http://sjeng.sourceforge.net"))

(defcustom chess-sjeng-path (executable-find "sjeng")
  "*The path to the sjeng executable."
  :type 'file
  :group 'chess-sjeng)

(defvar chess-sjeng-evaluation nil)

(make-variable-buffer-local 'chess-sjeng-evaluation)

(defvar chess-sjeng-regexp-alist
  (list
   (cons (concat "move\\s-+\\(" chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 1) t)))))
   (cons "tellics set 1\\s-+\\(.+\\)$"
	 (function
	  (lambda ()
	    (setq chess-engine-opponent-name (match-string 1)))))
   (cons "{\\(Black\\|White\\) resigns}"
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'resign))))
   (cons "\\(Illegal move\\|unrecognized/illegal command\\):\\s-*\\(.*\\)"
	 (function
	  (lambda ()
	    (error (match-string 1)))))
   (cons "command not legal now"
	 (function
	  (lambda ()
	    (error (match-string 0)))))))

(defun chess-sjeng-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-common-handler game 'initialize "sjeng")))
	(when (and proc (processp proc)
		   (eq (process-status proc) 'run))
	  (process-send-string proc "xboard\nnew\n")
	  (setq chess-engine-process proc)
	  t)))

     ((eq event 'setup-pos)
      (chess-engine-send nil (format "setboard %s\n"
				     (chess-pos-to-string (car args)))))

     ((eq event 'move)
      (when (= 1 (chess-game-index game))
	(chess-game-set-tag game "White" chess-full-name)
	(chess-game-set-tag game "Black" chess-engine-opponent-name))

      (chess-engine-send
       nil
       (concat (chess-index-to-coord (chess-ply-source (car args)))
	       (chess-index-to-coord (chess-ply-target (car args)))
	       (if (chess-ply-keyword (car args) :promote)
		   (string (downcase (chess-ply-keyword (car args) :promote)))
		 "")
	       "\n"))
      (if (chess-game-over-p game)
	  (chess-game-set-data game 'active nil)))

     ((eq event 'setup-game)
      (let ((file (chess-with-temp-file
		      (insert (chess-game-to-string (car args)) ?\n))))
	(chess-engine-send nil (format "read %s\n" file))))

     ((eq event 'set-option)
      (cond
       ((eq (car args) 'resign)
	(if (cadr args)
	    (chess-engine-send nil "resign 9\n")
	  (chess-engine-send nil "resign -1\n")))
       ((eq (car args) 'ponder)
	(if (cadr args)
	    (chess-engine-send nil "hard\n")
	  (chess-engine-send nil "easy\n")))))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after sjeng moves"))

      (apply 'chess-common-handler game event args)))))

(provide 'chess-sjeng)

;;; chess-sjeng.el ends here
