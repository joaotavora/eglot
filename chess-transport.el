;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; An example of a generic transport engine, based on the protocol
;; used by chess-network.el.  The only parts missing are send and
;; receive.  This could be used for transmitting chess.el protocol
;; over CTCP, for example.
;;

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
