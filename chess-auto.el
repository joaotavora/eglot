;;; -*-emacs-lisp-*-

(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (chess) "chess" "chess.el" (15535 63810))
;;; Generated autoloads from chess.el

(autoload (quote chess) "chess" "\
Start a game of chess." t nil)

;;;***
