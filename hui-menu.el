;;; hui-menu.el --- Menubar menu of GNU Hyperbole commands
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

(eval-and-compile (mapc #'require '(hpath hui-jmenu hyrolo-menu browse-url easymenu)))


;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defmacro hui-menu-browser (title browser-option)
  `(list
    (list ,title
	  ["Chrome (Google)"
	   (setq ,browser-option #'browse-url-chrome)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-chrome)]
	  ["Chromium"
	   (setq ,browser-option #'browse-url-chromium)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-chromium)]
	  ["Default (System wide)"
	   (setq ,browser-option #'browse-url-default-browser)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-default-browser)]
	  ["EWW (Emacs)"
	   (setq ,browser-option #'eww-browse-url)
	   :style radio
	   :selected (eq ,browser-option #'eww-browse-url)]
	  ;; Whatever browse-url-text-browser is set to, default is Lynx
	  ["Emacs Text Browser"
	   (setq ,browser-option #'browse-url-text-emacs)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-text-emacs)]
	  ["Firefox"
	   (setq ,browser-option #'browse-url-firefox)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-firefox)]
	  ["KDE"
	   (setq ,browser-option #'browse-url-kde)
	   :style radio
	   :selected (eq ,browser-option #'browse-url-kde)]
	  ["XTerm Text Browser"
	   (setq ,browser-option #'browse-url-text-xterm)
	   :style radio
	   :selected (eq ,browser-option #'browse-text-xterm)]
	  "----"
	  ["Toggle-URLs-in-New-Window"
	   (setq browse-url-new-window-flag (not browse-url-new-window-flag))
	   :style toggle
	   :selected browse-url-new-window-flag]
	  )))

;; List explicit buttons in the current buffer for menu activation.
(defun hui-menu-explicit-buttons (rest-of-menu)
  (delq nil
	(append
	 '(["Manual"   (id-info "(hyperbole)Explicit Buttons") t]
	   "----")
	 (let ((labels (ebut:list))
	       (cutoff))
	   (if labels
	       (progn
		 ;; Cutoff list if too long.
		 (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) labels))
		     (setcdr cutoff nil))
		 (delq nil
		       (append
			'("----"
			  ["Alphabetize-List"
			   (setq hui-menu-order-explicit-buttons 
				 (not hui-menu-order-explicit-buttons))
			   :style toggle :selected hui-menu-order-explicit-buttons]
			  "Activate:")
			(mapcar (lambda (label) (vector label `(ebut:act ,label) t))
				(if hui-menu-order-explicit-buttons
				    (sort labels 'string-lessp)
				  labels))
			(if cutoff '(". . ."))
			'("----" "----"))))))
	 rest-of-menu)))

(defun hui-menu-cutoff-list (lst)
  "If list LST is longer than, `hui-menu-max-list-length', then cut it off there.
Return t if cutoff, else nil."
  (let ((cutoff))
    (if (setq cutoff (nthcdr (1- hui-menu-max-list-length) lst))
	(setcdr cutoff nil))
    (if cutoff t)))

;; List existing global buttons for menu activation.
(defun hui-menu-global-buttons (rest-of-menu)
  (delq nil
	(append
	 '(["Manual" (id-info "(hyperbole)Global Buttons") t]
	   "----")
	 (let ((labels (gbut:label-list))
	       (cutoff))
	   (when labels
	     ;; Cutoff list if too long.
	     (setq cutoff (hui-menu-cutoff-list labels))
	     (delq nil (append
			'("----" "Activate:")
			(mapcar (lambda (label) (vector label `(gbut:act ,label) t))
				(sort labels 'string-lessp))
			(if cutoff '(". . ."))
			'("----" "----")))))
	 rest-of-menu)))

(defun hui-menu-key-binding-item (item-name command)
  "Return a key binding menu item string built from ITEM-NAME and COMMAND."
  (format "%s(%s)" item-name (key-description (where-is-internal command nil t))))

(defun hui-menu-key-bindings (rest-of-menu)
  (nconc
   (list
    (vector (hui-menu-key-binding-item "Action-Key         \t\t\t" 'hkey-either)       '(hui:bind-key #'hkey-either) t)        ;; {M-RET}
    (vector (hui-menu-key-binding-item "Button-Rename-Key  \t\t"   'hui:ebut-rename)   '(hui:bind-key #'hui:ebut-rename) t)    ;; {C-c C-r}
    (vector (hui-menu-key-binding-item "Drag-Emulation-Key \t\t"   'hkey-operate)      '(hui:bind-key #'hkey-operate) t)       ;; {M-o}
    (vector (hui-menu-key-binding-item "Hyperbole-Menu-Key \t"     'hyperbole)         '(hui:bind-key #'hyperbole) t)          ;; {C-h h}
    (vector (hui-menu-key-binding-item "Mark-Thing-Key     \t\t"   'hui-select-thing)  '(hui:bind-key #'hui-select-thing) t)   ;; {C-c C-m}
    (vector (hui-menu-key-binding-item "Smart-Help-Key     \t\t"   'hkey-help)         '(hui:bind-key #'hkey-help) t)          ;; {C-h A}
    (vector (hui-menu-key-binding-item "Windows-Control-Key\t"     'hycontrol-windows) '(hui:bind-key #'hycontrol-windows) t)) ;; {C-C \}
   rest-of-menu))

;; Dynamically compute submenus for Screen menu
(defun hui-menu-screen (_ignored)
  (list
   ["Manual" (id-info "(hyperbole)HyControl") t]
   "----"
   ["Frames-Control"  hycontrol-frames t]
   ["Windows-Control" hycontrol-windows t]
   "----"
   (hui-menu-of-buffers)
   (hui-menu-of-frames)
   (hui-menu-of-windows)))

(defun hui-menu-web-search ()
  ;; Pulldown menu
  (mapcar (lambda (service)
	    (vector service
		    (list #'hyperbole-web-search service nil)
		    t))
	  (mapcar 'car hyperbole-web-search-alist)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;; Ensure that this variable is defined to avert any error within
;; the Customize menu.
;; (defvar highlight-headers-follow-url-netscape-new-window nil
;;   "*Whether to make Netscape create a new window when a URL is sent to it.")

(defconst hui-menu-about
  (vector (concat "About-Hyperbole-"
		  (if (= (aref hyperb:version 0) ?0)
		      (substring hyperb:version 1)
		    hyperb:version))
	  #'(hypb:display-file-with-logo
	     (expand-file-name "HY-ABOUT" hyperb:dir))
	  t))

(defconst hui-menu-options
  (append '(["All-Hyperbole-Options" (customize-browse 'hyperbole) t]
	     "----"
	    ["Hyperbole-on-Menubar"
	     (cond ((and (boundp 'menubar-configuration)
			 (not (memq 'Hyperbole menubar-configuration)))
		    ;; Hyperbole may be included as part of the menubar but
		    ;; may be invisible due to a menubar configuration
		    ;; setting.  Invoking this item should then make it
		    ;; visible.
		    (hyperb:init-menubar))
		   ((cond (hyperb:emacs-p
			   (global-key-binding [menu-bar Hyperbole]))
			  ((boundp 'current-menubar)
			   (car (find-menu-item current-menubar '("Hyperbole")))))
		    ;; Already on the menubar, remove it.
		    (hui-menu-remove Hyperbole))
		   (t;; Add it.
		    (hyperb:init-menubar)))
	     :style toggle
	     :selected
	      (cond ((boundp 'menubar-configuration)
		     (memq 'Hyperbole menubar-configuration))
		    (hyperb:emacs-p
		     (and (global-key-binding [menu-bar Hyperbole]) t))
		    ((boundp 'current-menubar)
		     (car (find-menu-item current-menubar '("Hyperbole")))))]
	    "----"
	    ["Find-File-Accepts-URLs"
	     hpath:find-file-urls-mode
	     :style toggle
	     :selected hpath:find-file-urls-mode]
	    "----")
	  '(("Change-Key-Bindings" :filter hui-menu-key-bindings))
	  '("----")
	  (list (cons "Display-Referents-in"
		      (mapcar (lambda (sym)
				(vector
				 (capitalize (symbol-name sym))
				 `(setq hpath:display-where ',sym)
				 :style 'radio
				 :selected `(eq hpath:display-where ',sym)))
			      (mapcar 'car hpath:display-where-alist))))
	  '("----")
	  (hui-menu-browser "Display-URLs-in" browse-url-browser-function)
	  '("----")
	  (hui-menu-browser "Display-Web-Searches-in" hyperbole-web-search-browser-function)
	  '("----")
	  '(("Smart-Key-Press-at-Eol"
	     "----"
	     "----"
	     ;; This menu may be loaded by InfoDock before hsettings.el has
	     ;; defined `smart-scroll-proportional'.  Handle that case
	     ;; with a conditional.
	     ["Scrolls-a-Windowful"
	      (setq smart-scroll-proportional nil)
	      :style radio :selected (if (boundp 'smart-scroll-proportional)
					 (null smart-scroll-proportional))]
	     ["Scrolls-Proportionally"
	      (setq smart-scroll-proportional t)
	      :style radio :selected (if (boundp 'smart-scroll-proportional)
					 smart-scroll-proportional)]
	     ))
	  '("----"
	    ["Toggle-Isearch-Invisible-Text" hypb:toggle-isearch-invisible
	     :visible (boundp 'isearch-invisible)
	     :style toggle :selected (and (boundp 'isearch-invisible)
					  isearch-invisible)]
	    ["Toggle-Messaging-Explicit-Buttons" hyperbole-toggle-messaging
	     :style toggle :selected (not inhibit-hyperbole-messaging)]
	    ["Toggle-Override-Local-Keys" hkey-toggle-override-local-bindings
	     :style toggle :selected hkey-init-override-local-keys]
	    ["Toggle-Rolo-Dates" hyrolo-toggle-datestamps
	     :style toggle :selected (and (boundp 'hyrolo-add-hook)
					  (listp hyrolo-add-hook)
					  (memq 'hyrolo-set-date hyrolo-add-hook))]
	    ["Toggle-Smart-Key-Debug (HyDebug)" hkey-toggle-debug
	     :style toggle :selected hkey-debug]
	    ))
  "Untitled menu of Hyperbole options.")

(defvar infodock-hyperbole-menu nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Add Hyperbole menu to menubar.
(defun hyperbole-menubar-menu ()
  "Add to or update the Hyperbole menu on the global menubar."
  (cond ((boundp 'menubar-configuration)
	 (unless (memq 'Hyperbole menubar-configuration)
	   ;; Hyperbole may be included as part of the menubar but
	   ;; may be invisible due to a menubar configuration
	   ;; setting.  Make it visible here.
	   (if (fboundp 'customize-set-variable)
	       (customize-set-variable 'menubar-configuration
				       (cons 'Hyperbole menubar-configuration))
	     (setq menubar-configuration
		   (cons 'Hyperbole menubar-configuration))))
	 (set-menubar-dirty-flag))
	(t (let ((add-before (cond ((and (boundp 'infodock-menubar-type)
					 (eq infodock-menubar-type 'menubar-infodock))
				    "Key")
				   ((global-key-binding [menu-bar Koutline])
				    "Koutline")
				   ((global-key-binding [menu-bar OO-Browser])
				    "OO-Browser"))))
	     (add-submenu nil (infodock-hyperbole-menu t) add-before))))
  ;; Force a menu-bar update.
  (force-mode-line-update))

(defun hyperbole-popup-menu ()
  "Popup the Hyperbole menubar menu."
  (interactive)
  (popup-menu (infodock-hyperbole-menu)))

;;; Don't change this name; doing so will break the way InfoDock
;;; initializes the Hyperbole menu.
(defun infodock-hyperbole-menu (&optional rebuild-flag)
  "Return the Hyperbole menu for the global InfoDock menubar.
Uses any non-nil existing value of the menu unless optional
REBUILD-FLAG is non-nil, in which case the menu is rebuilt."
  (when (or rebuild-flag (null infodock-hyperbole-menu))
    (setq infodock-hyperbole-menu
	  (delq nil
		(list
		 (if (featurep 'xemacs) "%_Hyperbole" "Hyperbole")
		 :config 'Hyperbole
		 hui-menu-about
		 ["Demonstration"  (hypb:display-file-with-logo
				    (expand-file-name "DEMO" hyperb:dir)) t]
		 ;; Comment InfoDock manual reference out until
		 ;; InfoDock is modernized for Emacs 25.
		 ;; (if (and (boundp 'infodock-version) infodock-version)
		 ;;     ["Manual"      (id-info "(infodock)Hyperbole Menu") t]
		 ;;   ["Manual"      (id-info "(hyperbole)Top") t])
		 ["Manual"      (id-info "(hyperbole)Top") t]
		 ["What-is-New?"  (hypb:display-file-with-logo
				   (expand-file-name "HY-NEWS" hyperb:dir)) t]
		 "----"
		 ["Remove-This-Menu"
		  (progn
		    ;; Delete Hyperbole menu from all menubars.
		    (hui-menu-remove Hyperbole)
		    ;;
		    ;; Remove Hyperbole button comment from future
		    ;; outgoing mail.
		    (if (boundp 'smail:comment) (setq smail:comment nil)))
		  t]
		 "----"
		 ["Activate-Button-at-Point" hui:hbut-current-act
		  (hbut:is-p (hbut:at-p))]
		 ["Back-to-Prior-Location" (hhist:remove current-prefix-arg)
		  (and (boundp '*hhist*) *hhist*)]
		 '("Button-File"
		   ["Manual"  (id-info "(hyperbole)Button Files") t]
		   "----"
		   ["Edit-Per-Directory-File" (find-file hbmap:filename) t]
		   ["Edit-Personal-File" (find-file
					  (expand-file-name
					   hbmap:filename hbmap:dir-user)) t]
		   )
		 (cons "Customize" hui-menu-options)
		 '("Documentation"
		   ["Manual"      (id-info "(hyperbole)Top") t]
		   "----"
		   ["Demonstration"  (hypb:display-file-with-logo
				      (expand-file-name "DEMO" hyperb:dir)) t]
		   ["Glossary"    (id-info "(hyperbole)Glossary") t]
		   ["Manifest"    (find-file-read-only
				   (expand-file-name "MANIFEST" hyperb:dir)) t]
		   ["Smart-Key-Summary" (id-browse-file (hypb:hkey-help-file)) t]
		   ("Types"
		    ["Action-Types-Manual"
		     (id-info "(hyperbole)Action Types") t]
		    ["Implicit-Button-Types-Manual"
		     (id-info "(hyperbole)Implicit Buttons") t]
		    "----"
		    ["Show-Action-Types"
		     (hui:htype-help-current-window 'actypes) t]
		    ["Show-Implicit-Button-Types"
		     (hui:htype-help-current-window 'ibtypes 'no-sort) t]
		    ))
		 '("Explicit-Button"
		   :filter hui-menu-explicit-buttons
		   ["Activate" hui:hbut-act t]
		   ["Create" hui:ebut-create t]
		   ["Delete" hui:ebut-delete t]
		   ["Edit"   hui:ebut-modify t]
		   ("Help"  
		    ["Manual"   (id-info "(hyperbole)Location") t]
		    "----"
		    ["Buffer-Buttons"   (hui:hbut-report -1) t]
		    ["Current-Button"   (hui:hbut-report)    t]
		    ["Ordered-Buttons"  (hui:hbut-report 1)  t]
		    )
		   ["Modify" hui:ebut-modify t]
		   ["Rename" hui:ebut-rename t]
		   ["Search" hui:ebut-search t]
		   ["Types"
		    (hui:htype-help-current-window 'actypes) t]
		   )
		 (append
		  '("Find"
		    ["Manual"   (id-info-item "menu, Find") t]
		    "----"
		    ;; Show numbered line matches in all specified files.
		    ["Grep-Files"           hypb:rgrep t]
		    ;; Show numbered line matches for regexp in all file-based buffers.
		    ["Locate-Files"         locate t]
		    ;; Show numbered line matches for regexp in all file-based buffers.
		    ["Match-File-Buffers"   moccur t]
		    ;; Show numbered line matches for regexp from this buffer.
		    ["Occur-Here"           occur  t]
		    ;; Following point, remove all lines that match regexp.
		    ["Remove-Lines-Here"    hypb:remove-lines t]
		    ;; Following point, keep only lines that match regexp.
		    ["Save-Lines-Here"      hypb:save-lines t]
		    "----"
		    "Web-Search:")
		  (hui-menu-web-search)
		  )
		 '("Global-Button"
		   :filter hui-menu-global-buttons
		   ["Create" hui:gbut-create t]
		   ["Edit"   hui:gbut-modify t]
		   ["Help"   gbut:help t]
		   ["Modify" hui:gbut-modify t]
		   )
		 '("Implicit-Button"
		   ["Manual"   (id-info "(hyperbole)Implicit Buttons") t]
		   "----"
		   ["Activate-at-Point"    hui:hbut-current-act t]
		   ["Delete-Type"         (hui:htype-delete 'ibtypes) t]
		   ["Help"   hui:hbut-help t]
		   ["Types"  (hui:htype-help 'ibtypes 'no-sort) t]
		   )
		 (if hyperb:kotl-p
		     '("Koutliner"
		       ["Manual" (id-info "(hyperbole)Koutliner") t]
		       ["Example"   kotl-mode:example                 t]
		       "----"
		       ["Create-File"    kfile:find t]
		       ["View-File"      kfile:view t]
		       "----"
		       ["Collapse-Tree" (progn (kotl-mode:is-p)
					       (kotl-mode:hide-tree
						(kcell-view:label)))
			(eq major-mode 'kotl-mode)]
		       ["Create-Link" klink:create
			(eq major-mode 'kotl-mode)]
		       ["Expand-All-Trees" kotl-mode:show-all
			(eq major-mode 'kotl-mode)]
		       ["Expand-Tree" (progn (kotl-mode:is-p)
					     (kotl-mode:show-tree
					      (kcell-view:label)))
			(eq major-mode 'kotl-mode)]
		       ["Show-Top-Level-Only" kotl-mode:hide-body
			(eq major-mode 'kotl-mode)]
		       ))
		 '("Mail-Lists"
		   ["Manual" (id-info "(hyperbole)Suggestion or Bug Reporting")
		    t]
		   "----"
		   ["Mail-to-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config)) t]
		   ["Mail-to-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config)) t]
		   "----"
		   ["Join-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users-join@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   ["Join-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole-join@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   "----"
		   ["Leave-Hyperbole-Users-List"
		    (hmail:compose "hyperbole-users-leave@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   ["Leave-Hyperbole-Bug-Report-List"
		    (hmail:compose "bug-hyperbole-leave@gnu.org" nil
				   "Just send the message; subject and body are ignored.") t]
		   )
		 infodock-hyrolo-menu
		 '("Screen (HyControl)" :filter hui-menu-screen)
		 hui-menu-hywconfig)))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hui-menu-max-list-length 24
  "Positive integer that caps the length of a Hyperbole dynamic menu lists.")

(defvar hui-menu-order-explicit-buttons t
  "When non-nil (default), explicit button menu list is lexicographically ordered.
Otherwise, explicit buttons are listed in their order of appearance within
the current buffer.")

(provide 'hui-menu)

;;; hui-menu.el ends here
