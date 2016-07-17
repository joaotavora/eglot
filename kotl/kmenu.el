;;; kmenu.el --- Pulldown and popup menus for kotl-mode, the Koutliner mode
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    28-Mar-94 at 11:22:09
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'easymenu)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst kotl-menu-common-body
  (delq
   nil
   (list
    '("Edit"
      ["Set-Cell-Attribute"  kotl-mode:set-cell-attribute   t]
      "----"
      ["Demote"              kotl-mode:demote-tree
       :active t :keys "TAB"]
      ["Promote"             kotl-mode:promote-tree
       :active t :keys "M-TAB"]
      "----"
      ["Add-Child"           kotl-mode:add-child            t]
      ["Add-Cell"            kotl-mode:add-cell             t]
      ["Add-Parent"          kotl-mode:add-parent           t]
      ["Append-Cell"         kotl-mode:append-cell          t]
      ["Split-Cell"          kotl-mode:split-cell           t]
      "----"
      ["Kill-to-Cell-End"    kotl-mode:kill-contents        t]
      ["Kill-Tree"           kotl-mode:kill-tree            t]
      ["Yank"                kotl-mode:yank                 t]
      "----"
      ["Copy-After-Cell"     kotl-mode:copy-after           t]
      ["Copy-Before-Cell"    kotl-mode:copy-before          t]
      ["Move-After-Cell"     kotl-mode:move-after           t]
      ["Move-Before-Cell"    kotl-mode:move-before          t]
      "----"
      ["Fill"                kotl-mode:fill-cell            t]
      ["Fill-Paragraph"      kotl-mode:fill-paragraph       t]
      ["Set-Fill-Prefix"     kotl-mode:set-fill-prefix      t]
      "----"
      ["Tab-Key-Inserts-Spaces" kotl-mode:toggle-indent-tabs-mode
       :style toggle :selected (not kotl-mode:indent-tabs-mode)]
      ["Tab-Key-Tabs-Over" kotl-mode:toggle-tab-flag
       :style toggle :selected kotl-mode:tab-flag]
      )
    (if (boundp 'infodock-go-menu) infodock-go-menu)
    '("Jump-to"
      ["Cell"                kotl-mode:goto-cell            t]
      "----"
      ["Cell-Beginning"      kotl-mode:beginning-of-cell    t]
      ["Cell-End"            kotl-mode:end-of-cell          t]
      "----"
      ["Parent"              kotl-mode:up-level             t]
      ["Child"               kotl-mode:down-level           t]
      "----"
      ["Prev-Cell"           kotl-mode:previous-cell        t]
      ["Next-Cell"           kotl-mode:next-cell            t]
      "----"
      ["Prev-Same-Level"     kotl-mode:backward-cell        t]
      ["Next-Same-Level"     kotl-mode:forward-cell         t]
      "----"
      ["First-Sibling"       kotl-mode:first-sibling        t]
      ["Last-Sibling"        kotl-mode:last-sibling         t]
      "----"
      ["Beginning-of-Tree"   kotl-mode:beginning-of-tree    t]
      ["End-of-Tree"         kotl-mode:end-of-tree          t]
      "----"
      ["First-Cell"          kotl-mode:beginning-of-buffer  t]
      ["Last-Cell"           kotl-mode:end-of-buffer        t]
      )
    '("Label-Type"
      ["Alphanumeric (Default)"  (kview:set-label-type kview 'alpha)  t]
      ["Legal"                   (kview:set-label-type kview 'legal)  t]
      ;; ["None"                    (kview:set-label-type kview 'no)     t]
      ;; ["Partial-Alpha"           (kview:set-label-type kview 'partial-alpha) t]
      ["Permanent-Idstamp"       (kview:set-label-type kview 'id)     t]
      ;; ["Stars"                   (kview:set-label-type kview 'star) t]
      "----"
      ["Set-Label-Separator"     kview:set-label-separator  t]
      )
    '("Link"
      ["Add-at-Point"        klink:create                   t]
      )
    (if (fboundp 'infodock-options-menu) (infodock-options-menu))
    '("Tree"
      ["Copy-to-Buffer"      kotl-mode:copy-to-buffer       t]
      ["Demote"              kotl-mode:demote-tree
       :active t :keys "TAB"]
      ["Kill"                kotl-mode:kill-tree            t]
      ["Mail"                kotl-mode:mail-tree            t]
      ["Promote"             kotl-mode:promote-tree
       :active t :keys "M-TAB"]
      ["Show-Attributes"     (kotl-mode:cell-help nil 2)
       :active t :keys "C-u C-c h"]
      "----"
      ["Copy-After-Cell"     kotl-mode:copy-after           t]
      ["Copy-Before-Cell"    kotl-mode:copy-before          t]
      ["Move-After-Cell"     kotl-mode:move-after           t]
      ["Move-Before-Cell"    kotl-mode:move-before          t]
      )
    '("View"
      ["Set-View-Spec"       kvspec:activate                t]
      ["Toggle-Blank-Lines"  kvspec:toggle-blank-lines      t]
      "----"
      ["Set-Cell-Attribute"   kotl-mode:set-cell-attribute  t]
      ["Show-Cell-Attributes" (kotl-mode:cell-help)
       :active t :keys "C-c h"]
      ["All-Cells-Attributes" (kotl-mode:cell-help nil -1)
       :active t :keys "C-u -1 C-c h"]
      ["Show-Tree-Attributes" (kotl-mode:cell-help nil 2)
       :active t :keys "C-u C-c h"]
      "----"
      ["Hide (Collapse)"     kotl-mode:hide-tree            t]
      ["Hide-Levels"         kotl-mode:hide-sublevels       t]
      ["Hide-Subtree"        kotl-mode:hide-subtree         t]
      ["Overview"            kotl-mode:overview             t]
      "----"
      ["Show (Expand)"       kotl-mode:show-tree            t]
      ["Show-All"            kotl-mode:show-all             t]
      ["Show-Subtree"        kotl-mode:show-subtree         t]
      ["Show-Top-Level-Only" kotl-mode:top-cells            t]
      )
    ))
  "The middle menu entries common to all Koutliner menus.")

(defconst kotl-menu-common-preamble
  '("Koutline"
    ["All-Cells-Attributes" (kotl-mode:cell-help nil -1)
     :active t :keys "C-u -1 C-c h"]
    ["Example"             kotl-mode:example
     :active t :keys "C-h h k e"]
    ["Help"                describe-mode                  t]
    ["Manual"              (id-info "(hyperbole)Outliner") t]
    "----"
     ;; Delete Koutline menu from all menubars.
    ["Remove-This-Menu"    (hui-menu-remove Koutline kotl-mode-map) t]
    ))

(defconst kotl-menu-common-postamble
  '("----"
    ["Find (Open)"         find-file                      t]
    ["Find-Read-Only"      find-file-read-only            t]
    ["Save"                save-buffer                    t]
    ["Toggle-Read-Only"
     (if (fboundp 'vc-toggle-read-only)
	 (call-interactively 'vc-toggle-read-only)
       (call-interactively 'toggle-read-only))
     :active t
     :keys "C-x C-q"]
    ["Write (Save as)"     kfile:write                    t]
    "----"
    ["Export-to-HTML"      kexport:html                   t]
    ["Import-to-Koutline"  kimport:file                   t]
    "----"
    ["Quit"                (id-tool-quit '(kill-buffer nil))  t]
    ))

;;; This definition is used by InfoDock only.
(defconst id-menubar-kotl
  (cons
   (append kotl-menu-common-preamble
	   kotl-menu-common-postamble)
   kotl-menu-common-body))

;;; This definition is used by InfoDock, XEmacs and GNU Emacs.
(defconst id-popup-kotl-menu
   (append kotl-menu-common-preamble
	   `("----"
	     ,@ kotl-menu-common-body)
	   kotl-menu-common-postamble))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; This definition is used only by XEmacs and Emacs.
(defun kotl-menubar-menu ()
  "Add a Koutline menu to the menubar for each koutline buffer."
  (cond ((fboundp 'popup-mode-menu)
	 (setq mode-popup-menu id-popup-kotl-menu))
	(hyperb:xemacs-p
	 (define-key kotl-mode-map 'button3 'kotl-popup-menu))
	(t ;; hyperb:emacs-p
	 (define-key kotl-mode-map [down-mouse-3] 'kotl-popup-menu)
	 (define-key kotl-mode-map [mouse-3] nil)))
  (unless (cond (hyperb:emacs-p
		 (global-key-binding [menu-bar Koutline]))
		((boundp 'current-menubar)
		 (car (find-menu-item current-menubar '("Koutline")))))
    (if hyperb:xemacs-p	(set-buffer-menubar (copy-sequence current-menubar)))
    (easy-menu-define nil kotl-mode-map "Koutline Menubar Menu" id-popup-kotl-menu)
    ;; Force a menu-bar update.
    (force-mode-line-update)))

;;; This definition is used only by XEmacs and Emacs.
(defun kotl-popup-menu (event)
  "Popup the Koutline buffer menu."
  (interactive "@e")
  (mouse-set-point event)
  (popup-menu id-popup-kotl-menu))

(cond ((featurep 'infodock)
       ;; InfoDock under a window system
       (require 'id-menubars)
       (id-menubar-set 'kotl-mode 'id-menubar-kotl))
      ((or hyperb:emacs-p hyperb:xemacs-p)
       ;; Emacs or XEmacs under a window system
       (add-hook 'kotl-mode-hook #'kotl-menubar-menu)))

(provide 'kmenu)

;;; kmenu.el ends here
