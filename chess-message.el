;;; chess-message.el --- Code shared by all chess displays

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

;;; Code:

(defgroup chess-message nil
  "Support for message catalogs in chess.el."
  :group 'chess)

(defcustom chess-message-language 'english
  "The language to use when reporting messages."
  :type 'symbol
  :group 'chess-message)

;;; Code:

(defvar chess-message-catalog nil)

(defun chess-message-catalog (catalog definitions)
  (let ((entry (assq catalog chess-message-catalog)))
    (if entry
	(dolist (def definitions)
	  (let ((str (assq (car def) (cdr entry))))
	    (if str
		(setcdr str (cdr def))
	      (setcdr entry (cons def (cdr entry))))))
      (push (cons catalog definitions) chess-message-catalog))))

(defun chess-string (key &rest arguments)
  (let* ((entry (assq chess-message-language chess-message-catalog))
	 (msg (and entry (cdr (assq key (cdr entry))))))
    (if msg
	(apply 'format msg arguments)
      (format "Message not found: %s" key))))

(defsubst chess-message (key &rest arguments)
  (message (apply 'chess-string key arguments)))

(defsubst chess-error (key &rest arguments)
  (error (apply 'chess-string key arguments)))

(put 'chess-message-catalog 'lisp-indent-function 1)

(provide 'chess-message)

;;; chess-message.el ends here
