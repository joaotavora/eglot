;;; -*-emacs-lisp-*-

(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (chess-crafty) "chess-crafty" "engines/chess-crafty.el"
;;;;;;  (15163 40201))
;;; Generated autoloads from engines/chess-crafty.el

(autoload (quote chess-crafty) "chess-crafty" nil nil nil)

;;;***

;;;### (autoloads (chess-gnuchess) "chess-gnuchess" "engines/chess-gnuchess.el"
;;;;;;  (15163 40201))
;;; Generated autoloads from engines/chess-gnuchess.el

(autoload (quote chess-gnuchess) "chess-gnuchess" nil nil nil)

;;;***

;;;### (autoloads (chess-highlight) "chess-highlight" "tools/chess-highlight.el"
;;;;;;  (15160 56560))
;;; Generated autoloads from tools/chess-highlight.el

(autoload (quote chess-highlight) "chess-highlight" "\
This is the module constructor, call with a chess BOARD object.
This function returns an initialized module closure, which is a
function object that maintains the state associated with this call.
It may be called with command symbols in order to influence the
behavior of this module.  One symbol which must be accepted is
`shutdown'." nil nil)

;;;***

;;;### (autoloads (chess-ics1) "chess-ics1" "displays/chess-ics1.el"
;;;;;;  (15482 53911))
;;; Generated autoloads from displays/chess-ics1.el

(autoload (quote chess-ics1) "chess-ics1" "\
Handle any commands being sent to this instance of this module." nil nil)

;;;***

;;;### (autoloads (chess-images) "chess-images" "displays/chess-images.el"
;;;;;;  (15482 53911))
;;; Generated autoloads from displays/chess-images.el

(autoload (quote chess-images) "chess-images" "\
This display module presents a standard chessboard using images." nil nil)

;;;***

;;;### (autoloads (chess-module) "chess-module" "misc/chess-module.el"
;;;;;;  (15484 30636))
;;; Generated autoloads from misc/chess-module.el

(autoload (quote chess-module) "chess-module" "\
This is the module constructor, call with a chess GAME object.
This function returns an initialized module closure, which is a
function object that maintains the state associated with this call.
It may be called with command symbols in order to influence the
behavior of this module.  One symbol which must be accepted is
`shutdown'." nil nil)

;;;***

;;;### (autoloads (chess-phalanx) "chess-phalanx" "engines/chess-phalanx.el"
;;;;;;  (15163 40201))
;;; Generated autoloads from engines/chess-phalanx.el

(autoload (quote chess-phalanx) "chess-phalanx" nil nil nil)

;;;***

;;;### (autoloads (chess-plain) "chess-plain" "displays/chess-plain.el"
;;;;;;  (15482 53911))
;;; Generated autoloads from displays/chess-plain.el

(autoload (quote chess-plain) "chess-plain" "\
Handle any commands being sent to this instance of this module." nil nil)

;;;***

;;;### (autoloads (chess-module) "chess-speak" "displays/chess-speak.el"
;;;;;;  (15478 2520))
;;; Generated autoloads from displays/chess-speak.el

(autoload (quote chess-module) "chess-speak" "\
This is the module constructor, call with a chess BOARD object.
This function returns an initialized module closure, which is a
function object that maintains the state associated with this call.
It may be called with command symbols in order to influence the
behavior of this module.  One symbol which must be accepted is
`shutdown'." nil nil)

;;;***

;;;### (autoloads (chess-standard) "chess-standard" "core/chess-standard.el"
;;;;;;  (15484 32658))
;;; Generated autoloads from core/chess-standard.el

(autoload (quote chess-standard) "chess-standard" nil nil nil)

;;;***

;;;### (autoloads (chess) "chess" "chess.el" (15486 56060))
;;; Generated autoloads from chess.el

(autoload (quote chess) "chess" "\
Start a game of chess." t nil)

;;;***
