;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An example of a generic transport engine, based on the protocol
;; used by chess-network.el.  The only parts missing are send and
;; receive.  This could be used for transmitting chess.el protocol
;; over CTCP, for example.
;;
;; NOTE: Make sure that any housekeeping data you use is kept in
;; buffer-local variables.  Otherwise, multiple games played using the
;; same kind of transport might collide.
;;
;; $Revision$

(require 'chess-network)

(defalias 'chess-network-regexp-alist 'chess-transport-regexp-alist)

(defun chess-transport-handler (event &rest args)
  "This is an example of a generic transport engine."
  (cond
   ((eq event 'send)
    ;; transmit the string given in (car args) to your outbound
    ;; transport from here
    )))

;; call (chess-engine-submit engine STRING) for text that arrives from
;; your inbound transport

(provide 'chess-transport)

;;; chess-transport.el ends here
