;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define handler functions that are common to the (relatively)
;; standard chess engine communication protocol:
;;
;;   http://www.tim-mann.org/xboard/engine-intf.html
;;
;; $Revision$

(require 'chess-engine)

(defvar chess-common-temp-files nil)
(make-variable-buffer-local 'chess-common-temp-files)

(defmacro chess-with-temp-file (&rest body)
  `(let ((file (make-temp-file "chess")))
     (with-temp-file file
       ,@body)
     (push file chess-common-temp-files)
     file))

(put 'chess-with-temp-file 'lisp-indent-function 1)

(defun chess-common-handler (event &rest args)
  "Initialize the network chess engine."
  (cond
   ((eq event 'initialize)
    (let* ((name (car args))
	   (path (intern (concat "chess-" name "-path")))
	   proc)
      (message "Starting chess program '%s'..." name)
      (unless (boundp path)
	(error "Cannot find %s executable; check `%s'" name path))
      (setq proc (start-process (concat "chess-" name)
				(current-buffer) (symbol-value path)))
      (message "Starting chess program '%s'...done" name)
      proc))

   ((eq event 'ready)
    (chess-game-set-data chess-engine-game 'active t))

   ((eq event 'shutdown)
    (chess-engine-send nil "quit\n")
    (dolist (file chess-common-temp-files)
      (if (file-exists-p file)
	  (delete-file file)))
    (setq chess-common-temp-files nil))

   ((eq event 'pass)
    (chess-engine-send nil "go\n"))

   ((eq event 'resign)
    (chess-engine-send nil "resign\n"))

   ((eq event 'draw)
    (message "Your draw offer was declined"))

   ((memq event '(resign abort))
    (chess-engine-send nil "new\n")
    (chess-engine-set-position nil))

   ((eq event 'undo)
    (dotimes (i (car args))
      (chess-engine-send nil "undo\n"))
    (if (= 1 (mod (car args) 2))
	(chess-engine-send nil "go\n"))
    (chess-game-undo chess-engine-game (car args)))

   ((eq event 'move)
    (chess-engine-send nil (concat (chess-ply-to-algebraic (car args))
				   "\n")))))

(provide 'chess-common)

;;; chess-common.el ends here
