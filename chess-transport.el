;;; chess-transport.el --- Example generic transport

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

;; An example of a generic transport engine, based on the protocol
;; used by chess-network.el.  The only parts missing are send and
;; receive.  This could be used for transmitting chess.el protocol
;; over CTCP, for example.

;;; Code:

(require 'chess-network)

(defvar chess-transport-regexp-alist chess-network-regexp-alist)

(defun chess-transport-handler (game event &rest args)
  "This is an example of a generic transport engine."
  (unless chess-engine-handling-event
    (cond
     ((eq event 'initialize)
      ;; Initialize the transport here, if necessary.  Make sure that
      ;; any housekeeping data you use is kept in buffer-local
      ;; variables.  Otherwise, multiple games played using the same
      ;; kind of transport might collide.  For example:
      ;;
      ;; (set (make-local-variable 'chess-transport-data) (car args))
      ;;
      ;; NOTE: Be sure not to return a process, or else chess-engine
      ;; will do all the transport work!
      t)

     ((eq event 'send)
      ;; Transmit the string given in `(car args)' to the outbound
      ;; transport from here
      )

     (t
      ;; Pass all other events down to chess-network
      (apply 'chess-network-handler game event args)))))

;; Call `(chess-engine-submit engine STRING)' for text that arrives
;; from the inbound transport

(provide 'chess-transport)

;;; chess-transport.el ends here
