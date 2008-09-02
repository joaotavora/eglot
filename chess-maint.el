;;; chess-maint.el --- code to help build chess -*- no-byte-compile: t -*-

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(provide 'chess-maint)

(defun chess-generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file
	(expand-file-name (car command-line-args-left)))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; chess-maint.el ends here
