;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define handler functions that are common to the (relatively)
;; standard chess engine communication protocol:
;;
;;   http://www.tim-mann.org/xboard/engine-intf.html
;;

(require 'chess-engine)
(require 'chess-message)

(defvar chess-common-engine-name nil)
(defvar chess-common-temp-files nil)
(make-variable-buffer-local 'chess-common-engine-name)
(make-variable-buffer-local 'chess-common-temp-files)

(defmacro chess-with-temp-file (&rest body)
  `(let ((file (make-temp-file "chess")))
     (with-temp-file file
       ,@body)
     (push file chess-common-temp-files)
     file))

(put 'chess-with-temp-file 'lisp-indent-function 1)

(chess-message-catalog 'english
  '((starting-engine	   . "Starting chess program '%s'...")
    (starting-engine-done  . "Starting chess program '%s'...done")
    (could-not-find-engine . "Cannot find %s executable; check `%s'")
    (draw-offer-declined   . "Your draw offer was declined")
    (illegal-move          . "Illegal move")
    (not-yet-implemented   . "This feature is not yet implemented")))

(defun chess-common-handler (game event &rest args)
  "Initialize the network chess engine."
  (cond
   ((eq event 'initialize)
    (let* ((name (car args))
	   (path (intern (concat "chess-" name "-path")))
	   proc)
      (chess-message 'starting-engine name)
      (unless (boundp path)
	(chess-error 'could-not-find-engine name path))
      (setq proc (start-process (concat "chess-" name)
				(current-buffer) (symbol-value path)))
      (chess-message 'starting-engine-done name)
      proc))

   ((eq event 'ready)
    (chess-game-set-data game 'active t)
    (chess-game-run-hooks game 'check-autosave))

   ((eq event 'destroy)
    (let ((proc (get-buffer-process (current-buffer))))
      (if (and (processp proc)
	       (memq (process-status proc) '(run open)))
	  (chess-engine-send nil "quit\n")))

    (dolist (file chess-common-temp-files)
      (if (file-exists-p file)
	  (delete-file file)))
    (setq chess-common-temp-files nil))

   ((eq event 'pass)
    (chess-engine-send nil "go\n"))

   ((eq event 'draw)
    (chess-message 'draw-offer-declined))

   ((eq event 'resign)
    (chess-engine-send nil "resign\n")
    (chess-game-set-data game 'active nil))

   ((eq event 'new)
    (chess-engine-send nil "new\n")
    (chess-engine-set-position nil))

   ((eq event 'force)
    (chess-error 'not-yet-implemented))

   ((eq event 'undo)
    (dotimes (i (car args))
      (chess-engine-send nil "undo\n"))
    (if (= 1 (mod (car args) 2))
	(chess-engine-send nil "go\n"))

    ;; prevent us from handling the `undo' event which this triggers
    (let ((chess-engine-handling-event t))
      (chess-game-undo game (car args))))

   ((eq event 'flag-fell)
    (chess-game-set-data game 'active nil)
    (let ((chess-game-inhibit-events t))
      (chess-game-end game :flag-fell)))

   ((eq event 'move)
    (when (= 1 (chess-game-index game))
      (chess-game-set-tag game "White" chess-full-name)
      (chess-game-set-tag game "Black" chess-engine-opponent-name))

    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n"))
    (if (chess-game-over-p game)
	(chess-game-set-data game 'active nil)))))

(provide 'chess-common)

;;; chess-common.el ends here
