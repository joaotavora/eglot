;;; chess-database.el --- Basic code for manipulating game databases

;; Copyright (C) 2002, 2004, 2008  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: data, games

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

(require 'chess-message)

(defgroup chess-database nil
  "Generic interface to chess database modules."
  :group 'chess)

(defcustom chess-database-modules '(chess-scid chess-file)
  "List of database modules to try when `chess-database-open' is called."
  :type '(repeat (symbol :tag "Module"))
  :group 'chess-database)

(defvar chess-database-handler nil)

(make-variable-buffer-local 'chess-database-handler)

(chess-message-catalog 'english
  '((no-such-database . "There is no such chess database module '%s'")))

(defun chess-database-do-open (module file)
  "Returns the opened database object, or nil."
  (when (require module nil t)
    (let* ((name (symbol-name module))
	   (handler (intern-soft (concat name "-handler"))))
      (unless handler
	(chess-error 'no-such-database name))
      (let ((buffer (funcall handler 'open file)))
	(when buffer
	  (with-current-buffer buffer
	    (setq chess-database-handler handler)
	    (add-hook 'kill-buffer-hook 'chess-database-close nil t)
	    (add-hook 'after-revert-hook 'chess-database-rescan nil t)
	    (current-buffer)))))))

(defun chess-database-open (file &optional module)
  "Returns the opened database object, or nil."
  (if module
      (chess-database-do-open module file)
    (let (result)
      (setq module chess-database-modules)
      (while module
	(if (setq result (chess-database-do-open (car module) file))
	    (setq module nil)
	  (setq module (cdr module))))
      result)))

(defsubst chess-database-command (database event &rest args)
  (with-current-buffer database
    (apply chess-database-handler event args)))

(defun chess-database-close (&optional database)
  (let ((buf (or database (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(remove-hook 'kill-buffer-hook 'chess-database-close t))
      (chess-database-save buf)
      (chess-database-command buf 'close)
      (kill-buffer buf))))

(defun chess-database-save (database)
  (chess-database-command database 'save))

(defun chess-database-rescan (&optional database)
  (chess-database-command database 'rescan))

(defun chess-database-count (database)
  (chess-database-command database 'count))

(defun chess-database-read-only-p (database)
  "Return non-nil if DATABASE is read only."
  (chess-database-command database 'read-only-p))

(defun chess-database-filename (database)
  "Return the filename of an already opened DATABASE."
  (chess-database-command database 'filename))

(defun chess-database-read (database index)
  "Return from DATABASE the chess game object at INDEX."
  (chess-database-command database 'read index))

(defun chess-database-write (database game)
  (chess-database-command database 'write game))

(defun chess-database-replace (database game &optional index)
  (chess-database-command database 'replace game index))

(defun chess-database-query (database &rest terms)
  "Run a query on DATABASE.
TERMS is partly dependent on the chess-database module in use.
chess-scid:
 tree-search GAME: Perform a tree search on the last position of GAME."
  (apply 'chess-database-command database 'query terms))

(provide 'chess-database)

;;; chess-database.el ends here
