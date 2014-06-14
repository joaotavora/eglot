;;; chess-crafty.el --- Play against crafty!

;; Copyright (C) 2002, 2004, 2014  Free Software Foundation, Inc.

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

;;; Code:

(require 'chess-common)
(require 'chess-fen)
(require 'chess-pgn)
(require 'chess-var)

(defgroup chess-crafty nil
  "The publically available chess engine 'crafty'."
  :group 'chess-engine
  :link '(custom-manual "(chess)Crafty"))

(defcustom chess-crafty-path (or (executable-find "crafty")
				 (executable-find "wcrafty"))
  "*The path to the crafty executable."
  :type 'file
  :group 'chess-crafty)

(defvar chess-crafty-evaluation nil)

(make-variable-buffer-local 'chess-crafty-evaluation)

(defvar chess-crafty-analyzing-p nil
  "Non-nil if Crafty is currently in analysis mode.")

(make-variable-buffer-local 'chess-crafty-analyzing-p)

(defvar chess-crafty-regexp-alist
  (list
   (cons (concat "move\\s-+\\(" chess-algebraic-regexp "\\)\\s-*$")
	 (function
	  (lambda ()
	    (funcall chess-engine-response-handler 'move
		     (chess-engine-convert-algebraic (match-string 1) t)))))
   (cons "total evaluation\\.+\\s-+\\([-+0-9.]+\\)"
	 (function
	  (lambda ()
	    (setq chess-crafty-evaluation
		  (string-to-number (match-string 1))))))
   (cons "tellicsnoalias kibitz Hello from\\s-+\\(.+\\)$"
	 (function
	  (lambda ()
	    (setq chess-engine-opponent-name (match-string 1)))))
   (cons "Analyze Mode: type \"exit\" to terminate.$"
	 (function
	  (lambda ()
	    (setq chess-crafty-analyzing-p t))))
   (cons (concat "\t ?\\([0-9]+\\)\\s-+"
		 "\\(-?[0-9]+\\)\\s-+\\([0-9]+\\)\\s-+\\([0-9]+\\)\\s-+"
		 "\\(" ;; The list of moves
		   "\\( *[1-9][0-9]*\\. "
		     "\\(\\.\\.\\.\\|" chess-algebraic-regexp "\\)"
		     "\\( " chess-algebraic-regexp "\\)?\\)+\\)$")
	 (function
	  (lambda ()
	    (when chess-crafty-analyzing-p
	      ;; We can translate this information to EPD opcodes
	      (let ((depth (read (match-string 1)))
		    (centipawn (read (match-string 2)))
		    (pos (chess-engine-position nil)))
		(chess-pos-set-epd pos 'acd depth)
		(chess-pos-set-epd pos 'ce centipawn)
		(chess-pos-set-epd
		 pos
		 'pv ; predicted variation
		 (save-restriction
		   (narrow-to-region (match-beginning 5) (match-end 5))
		   (let ((var (chess-var-create pos)))
		     (goto-char (point-min))
		     (while (not (eobp))
		       (cond
			((looking-at "[1-9][0-9]*\\.[ .]*")
			 (goto-char (match-end 0)))
			((looking-at chess-algebraic-regexp)
			 (goto-char (match-end 0))
			 (let ((ply (chess-algebraic-to-ply
				     (chess-var-pos var)
				     (match-string-no-properties 0))))
			   (unless ply
			     (error "unable to read move '%s'"
				    (match-string-no-properties 0)))
			   (chess-var-move var ply))))
		       (skip-chars-forward " "))
		     var))))))))
   (cons "analyze complete.$"
	 (function
	  (lambda ()
	    (setq chess-crafty-analyzing-p nil))))
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

(defun chess-crafty-handler (game event &rest args)
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      (let ((proc (chess-common-handler game 'initialize "crafty")))
	(when (and proc (processp proc)
		   (eq (process-status proc) 'run))
	  (process-send-string proc "xboard\n")
	  (setq chess-engine-process proc)
	  t)))

     ((eq event 'setup-pos)
      (chess-engine-send nil (format "setboard %s\n"
				     (chess-pos-to-fen (car args)))))

     ((eq event 'evaluate)
      (setq chess-crafty-evaluation nil)
      (chess-engine-send nil "display general\nscore\ndisplay nogeneral\n")
      (let ((limit 50))
	(while (and (null chess-crafty-evaluation)
		    (> (setq limit (1- limit)) 0))
	  (sit-for 0.1 t))
	chess-crafty-evaluation))

     ((eq event 'analyze)
      (if (car args)
	  (chess-engine-send nil "analyze\npost\n")
	(chess-engine-send nil "exit\nnopost\n")))

     ((eq event 'setup-game)
      (let ((file (chess-with-temp-file
		      (chess-insert-pgn (car args)) (insert ?\n))))
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
	  (chess-engine-send nil "easy\n")))
       ((eq (car args) 'search-depth)
	(cl-assert (and (integerp (cadr args)) (>= (cadr args) 0)))
	(chess-engine-send nil (format "sd %d\n" (cadr args))))
       ((eq (car args) 'search-time)
	(cl-assert (and (integerp (cadr args)) (> (cadr args) 0)))
	(chess-engine-send nil (format "st %d\n" (cadr args))))))

     (t
      (if (and (eq event 'undo)
	       (= 1 (mod (car args) 2)))
	  (error "Cannot undo until after crafty moves"))

      (apply 'chess-common-handler game event args)))))

(provide 'chess-crafty)

;;; chess-crafty.el ends here
