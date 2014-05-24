;;; chess-kibitz.el --- Chess kibitzing, stored as annotations

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

;; Implements chess kibitzing, stored as annotations to the game being
;; viewed or played.  C-c C-c is used to save a kibitzing comment.

;;; Code:

(require 'chess-game)

(defvar chess-kibitz-input-last nil)
(defvar chess-kibitz-index nil)

(make-variable-buffer-local 'chess-kibitz-input-last)
(make-variable-buffer-local 'chess-kibitz-index)

(define-derived-mode chess-kibitz-mode text-mode "Kibitz"
  "A mode for editing chess annotations."
  (set-buffer-modified-p nil)
  (setq chess-kibitz-input-last (copy-marker (point-max) t))
  (let ((map (current-local-map)))
    (define-key map [(control ?c) (control ?c)] 'chess-kibitz-save)))

(defvar chess-module-game)

(defun chess-kibitz-save ()
  (interactive)
  (let ((ann (buffer-substring-no-properties chess-kibitz-input-last
					     (point-max))))
    (chess-game-run-hooks chess-module-game 'kibitz ann)
    (chess-pos-add-annotation (chess-game-pos chess-kibitz-index) ann))
  (set-marker chess-kibitz-input-last (point-max))
  (set-buffer-modified-p nil))

(defun chess-kibitz-show-annotations (index)
  (setq chess-kibitz-index index)
  (erase-buffer)
  (let ((position (chess-game-pos chess-module-game index))
	popup)
    (dolist (ann (chess-pos-annotations position))
      (when (stringp ann)
	(insert ann ?\n)
	(setq popup t)))
    (if popup
	(display-buffer (current-buffer)))))

(defun chess-kibitz-handler (game event &rest args)
  (cond
   ((eq event 'initialize)
    (kill-buffer (current-buffer))
    (set-buffer (generate-new-buffer "*Annotations*"))
    (chess-kibitz-mode)
    t)

   ((eq event 'switch-to-annotations)
    (switch-to-buffer-other-window (current-buffer)))

   ((eq event 'kibitz)
    (chess-kibitz-handler 'switch-to-annotations)
    (save-excursion
      (goto-char chess-kibitz-input-last)
      (insert (car args))))

   ((eq event 'set-index)
    (chess-kibitz-show-annotations (car args)))

   ((memq event '(post-undo move))
    (chess-kibitz-show-annotations (chess-game-index game)))))

(provide 'chess-kibitz)

;;; chess-kibitz.el ends here
