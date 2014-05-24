;;; chess-chat.el --- Very much like kibitzing, but not saved.

;; Copyright (C) 2002, 2014 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Maintainer: Mario Lang <mlang@delysid.org>
;; Keywords: games

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

;; RET is used to send each chat line.

;;; Code:

(require 'chess-module)

(defvar chess-chat-input-last nil)

(make-variable-buffer-local 'chess-chat-input-last)

(define-derived-mode chess-chat-mode text-mode "Chat"
  "A mode for editing chess annotations."
  (set-buffer-modified-p nil)
  (setq chess-chat-input-last (copy-marker (point-max) t))
  (let ((map (current-local-map)))
    (define-key map [return] 'chess-chat-send)
    (define-key map [(control ?m)] 'chess-chat-send)))

(defun chess-chat-send ()
  (interactive)
  (chess-game-run-hooks chess-module-game 'chat
			(buffer-substring-no-properties
			 chess-chat-input-last (point-max)))
  (set-marker chess-chat-input-last (point-max))
  (set-buffer-modified-p nil))

(defun chess-chat-handler (_game event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (set-buffer (generate-new-buffer "*Chat*"))
    (chess-chat-mode)
    t)

   ((eq event 'switch-to-chat)
    (switch-to-buffer-other-window (current-buffer)))

   ((eq event 'chat)
    (chess-chat-handler 'switch-to-chat)
    (save-excursion
      (goto-char chess-chat-input-last)
      (insert (car args))))))

(provide 'chess-chat)

;;; chess-chat.el ends here
