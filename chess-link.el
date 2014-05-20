;;; chess-link.el --- Connect two engines

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

;; A module for connecting two engines.  If one is a protocol
;; transport (like chess-network), and the other is a computing engine
;; (like chess-gnuchess), this will allow you to expose a bot over the
;; channel.

;;; Code:

(require 'chess)
(require 'chess-engine)

(defun chess-link-response-handler (event &rest args)
  "This function handles responses from the bot's computing engine."
  (let ((first-engine
	 (chess-game-data (chess-engine-game nil) 'first-engine))
	(second-engine
	 (chess-game-data (chess-engine-game nil) 'second-engine))
	return-value)
    (cond
     ((eq event 'match)
      (chess-engine-command nil 'accept)
      t)

     (t
      (let ((chess-engine-inhibit-auto-pass t))
	(setq return-value
	      (apply 'chess-engine-default-handler event args)))

      ;; but now transfer the event to the other engine
      (apply 'chess-engine-command
	     (if (eq (current-buffer) first-engine)
		 second-engine
	       first-engine) event args)

      return-value))))

(defun chess-link-connect (first-engine second-engine)
  "Connect two engines, so that they rely events back and forth."
  (chess-engine-set-response-handler first-engine
				     'chess-link-response-handler)
  (chess-engine-set-response-handler second-engine
				     'chess-link-response-handler))

;;;###autoload
(defun chess-link (first-engine-type second-engine-type)
  "Play out a game between two engines, and watch the progress.
If you want to run an engine as a bot, make the transport the first
engine, and the computer the second engine."
  (interactive "sFirst engine: \nsSecond engine: ")
  (setq first-engine-type (intern (concat "chess-" first-engine-type))
	second-engine-type (intern (concat "chess-" second-engine-type)))
  (let* ((my-color t)			; we start out as white always
	 (display (chess-create-display my-color))
	 (game (chess-display-game display)))
    (chess-game-set-data game 'my-color my-color)
    (chess-module-set-leader display)
    (chess-display-disable-popup display)
    (condition-case err
	(when (and (require first-engine-type)
		   (require second-engine-type))
	  (let ((first-engine
		 (chess-engine-create first-engine-type game))
		(second-engine
		 (chess-engine-create second-engine-type game)))

	    (chess-game-set-data game 'first-engine first-engine)
	    (chess-engine-command first-engine 'ready)

	    (chess-game-set-data game 'second-engine second-engine)
	    (chess-link-connect first-engine second-engine)
	    (chess-engine-command second-engine 'ready)

	    ;; tell the first engine to start moving
	    (chess-engine-command first-engine 'pass))

	  (chess-display-update display)
	  (chess-display-popup display))
      (error
       (chess-module-destroy display)
       (error (error-message-string err))))))

(provide 'chess-link)

;;; chess-link.el ends here
