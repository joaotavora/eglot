;;; -*-emacs-lisp-*-

(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (chess) "chess" "chess.el" (15487 9165))
;;; Generated autoloads from chess.el

(autoload (quote chess) "chess" "\
Start a game of chess." t nil)

;;;***

;;;### (autoloads (chess-gnuchess) "chess-gnuchess" "chess-gnuchess.el"
;;;;;;  (15487 8747))
;;; Generated autoloads from chess-gnuchess.el

(autoload (quote chess-gnuchess) "chess-gnuchess" nil nil nil)

;;;***

;;;### (autoloads (chess-highlight) "chess-highlight" "chess-highlight.el"
;;;;;;  (15487 8169))
;;; Generated autoloads from chess-highlight.el

(autoload (quote chess-highlight) "chess-highlight" "\
This is the module constructor, call with a chess BOARD object.
This function returns an initialized module closure, which is a
function object that maintains the state associated with this call.
It may be called with command symbols in order to influence the
behavior of this module.  One symbol which must be accepted is
`shutdown'." nil nil)

;;;***

;;;### (autoloads (chess-ics1) "chess-ics1" "chess-ics1.el" (15487
;;;;;;  7434))
;;; Generated autoloads from chess-ics1.el

(autoload (quote chess-ics1) "chess-ics1" "\
Handle any commands being sent to this instance of this module." nil nil)

;;;***

;;;### (autoloads (chess-images) "chess-images" "chess-images.el"
;;;;;;  (15487 8784))
;;; Generated autoloads from chess-images.el

(autoload (quote chess-images) "chess-images" "\
This display module presents a standard chessboard using images." nil nil)

;;;***

;;;### (autoloads (chess-module) "chess-module" "chess-module.el"
;;;;;;  (15487 7434))
;;; Generated autoloads from chess-module.el

(autoload (quote chess-module) "chess-module" "\
This is the module constructor, call with a chess GAME object.
This function returns an initialized module closure, which is a
function object that maintains the state associated with this call.
It may be called with command symbols in order to influence the
behavior of this module.  One symbol which must be accepted is
`shutdown'." nil nil)

;;;***

;;;### (autoloads (chess-phalanx) "chess-phalanx" "chess-phalanx.el"
;;;;;;  (15487 7434))
;;; Generated autoloads from chess-phalanx.el

(autoload (quote chess-phalanx) "chess-phalanx" nil nil nil)

;;;***

;;;### (autoloads (chess-plain) "chess-plain" "chess-plain.el" (15487
;;;;;;  7434))
;;; Generated autoloads from chess-plain.el

(autoload (quote chess-plain) "chess-plain" "\
Handle any commands being sent to this instance of this module." nil nil)

;;;***

;;;### (autoloads (chess-standard) "chess-standard" "chess-standard.el"
;;;;;;  (15487 9133))
;;; Generated autoloads from chess-standard.el

(autoload (quote chess-standard) "chess-standard" nil nil nil)

;;;***

;;;### (autoloads (chess-crafty) "chess-crafty" "chess-crafty.el"
;;;;;;  (15487 7434))
;;; Generated autoloads from chess-crafty.el

(autoload (quote chess-crafty) "chess-crafty" nil nil nil)

;;;***
