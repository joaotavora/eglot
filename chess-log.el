;;; chess-log.el --- Log chess events, as an aid to debugging

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

(require 'chess-module)

(defgroup chess-log nil
  "Code for logging chess events."
  :group 'chess)

(defun chess-log (&rest args)
  (with-current-buffer (get-buffer-create "*Chess Log*")
    (insert (apply 'format args) ?\n)))

(provide 'chess-log)

;;; chess-log.el ends here
