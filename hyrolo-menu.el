;;; hyrolo-menu.el --- Pulldown and popup menus of HyRolo commands
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    28-Oct-94 at 10:59:44
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'easymenu)
(require 'hyrolo)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst infodock-hyrolo-menu
  '("Rolo"
    ["Manual"            (id-info "(hyperbole)HyRolo") t]
    "----"
     ;; Delete Rolo menu from all menubars.
    ["Remove-This-Menu"  (hui-menu-remove Rolo hyrolo-mode-map) t]
    "----"
    ["Add-Entry"         (id-tool-invoke 'hyrolo-add) t]
    ["Delete-Entry"      (id-tool-invoke 'hyrolo-kill) t]
    ["Display-Prior-Matches" (id-tool-invoke 'hyrolo-display-matches) t]
    ["Edit-Entry"        (id-tool-invoke 'hyrolo-edit) t]
    ["Edit-Rolo"         (id-tool-invoke
			  '(progn (require 'hyrolo)
				  (find-file (car hyrolo-file-list))
				  (setq buffer-read-only nil)))
     t]
    ["Insert-Entry-at-Point" (id-tool-invoke 'hyrolo-yank) t]
    ["Mail-to-Address"   (id-tool-invoke 'hyrolo-mail-to) t]
    ["Search-for-Regexp" (id-tool-invoke 'hyrolo-grep)  t]
    ["Search-for-String" (id-tool-invoke 'hyrolo-fgrep) t]
    ["Search-for-Word"   (id-tool-invoke 'hyrolo-word)  t]
    ["Sort-Entries"      (id-tool-invoke 'hyrolo-sort)  t]
    ))

(defconst hyrolo-menu-common-body
  '(
    ("Move"
     ["Scroll-Backward"     scroll-down             t]
     ["Scroll-Forward"      scroll-up               t]
     ["To-Beginning"        beginning-of-buffer     t]
     ["To-End"              end-of-buffer           t]
     "----"
     ["To-Next-Entry"          outline-next-visible-heading     t]
     ["To-Next-Same-Level"     outline-forward-same-level       t]
     ["To-Previous-Entry"      outline-previous-visible-heading t]
     ["To-Previous-Same-Level" outline-backward-same-level      t]
     ["Up-a-Level"             outline-up-heading               t]
     )
    ("Outline"
     ["Hide (Collapse)"        outline-hide-subtree             t]
     ["Show (Expand)"          outline-show-subtree             t]
     ["Show-All"               outline-show-all                 t]
     ["Show-Only-First-Line"   outline-hide-body                t]
     ))
  "The middle menu entries common to all HyRolo menus.")

;;; This definition is used by InfoDock only.
(defconst id-menubar-hyrolo
  (append
   '(("Rolo"
      ["Help"                describe-mode                  t]
      ["Manual"              (id-info "(hyperbole)Rolo Keys") t]
      "----"
      ["Toggle-Read-Only"    read-only-mode                 t]
      ["Write (Save as)"     write-file                     t]
      "----"
      ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
      ))
   '(["Edit-Entry-at-Point"  hyrolo-edit-entry         t]
     ["Mail-to-Address"      (id-tool-invoke 'hyrolo-mail-to) t])
   `,@hyrolo-menu-common-body
   '(["Next-Match"          hyrolo-next-match         t]
     ["Previous-Match"      hyrolo-previous-match     t])
   (list infodock-hyrolo-menu)
   ))

;;; This definition is used by InfoDock and XEmacs.
(defconst id-popup-hyrolo-menu
  (append
   '("Rolo"
     ["Help"                describe-mode           t]
     ["Manual"              (id-info "(hyperbole)Rolo Keys") t]
     "----"
     ["Edit-Entry-at-Point" hyrolo-edit-entry         t]
     "----"
     ["Locate-Entry-Isearch" hyrolo-locate        t]
     ["Next-Match"          hyrolo-next-match         t]
     ["Previous-Match"      hyrolo-previous-match     t]
     "----")
   `,@hyrolo-menu-common-body
   (list infodock-hyrolo-menu)
   '("----"
     ["Quit"                (id-tool-quit '(hyrolo-quit)) t])
   ))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; This definition is used only by XEmacs and Emacs.
(defun hyrolo-menubar-menu ()
  "Add a HyRolo menu to the rolo match buffer menubar."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-hyrolo-menu))
	((featurep 'xemacs)
	 (define-key hyrolo-mode-map 'button3 'hyrolo-popup-menu))
	(t ;; hyperb:emacs-p
	 (define-key hyrolo-mode-map [down-mouse-3] 'hyrolo-popup-menu)
	 (define-key hyrolo-mode-map [mouse-3] nil)))
  (unless (cond (hyperb:emacs-p
		 (global-key-binding [menu-bar Rolo]))
		((boundp 'current-menubar)
		 (car (find-menu-item current-menubar '("Rolo")))))
    (if (featurep 'xemacs) (set-buffer-menubar (copy-sequence current-menubar)))
    (easy-menu-define nil hyrolo-mode-map "Rolo Menubar Menu" id-popup-hyrolo-menu)
    ;; Force a menu-bar update.
    (force-mode-line-update)))

;;; This definition is used only by XEmacs and Emacs.
(defun hyrolo-popup-menu (event)
  "Popup the Hyperbole Rolo match buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (popup-menu id-popup-hyrolo-menu))

(cond ((featurep 'infodock)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'hyrolo-mode 'id-menubar-hyrolo))
      ((or hyperb:emacs-p (featurep 'xemacs))
       ;; Emacs or XEmacs under a window system
       (add-hook 'hyrolo-mode-hook #'hyrolo-menubar-menu)))

(provide 'hyrolo-menu)

;;; hyrolo-menu.el ends here
