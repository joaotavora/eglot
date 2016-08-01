;;; hui-mini.el --- Single line command menus for GNU Hyperbole
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Oct-91 at 20:13:17
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hypb)
(require 'browse-url)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hui:menu-select          "\C-m"
  "*Character string which selects the Hyperbole menu item at point.")
(defvar hui:menu-quit            "Q"
  "*Upper case character string which quits selecting from a Hyperbole menu item.")
(defvar hui:menu-abort           "\C-g"
  "*Same function as 'hui:menu-quit'.")
(defvar hui:menu-top             "\C-t"
  "*Character string which returns to top Hyperbole menu.")

(defvar hui:menu-p nil
  "Non-nil iff the Hyperbole minibuffer menu is active.")

(defvar hui:menus nil
  "Hyperbole minibuffer command menus.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; Used as the autoloaded main entry point to Hyperbole.  The "hyperbole"
;;; file is loaded when this is invoked.
;;; This command brings up a series of menus of Hyperbole commands.
;;;###autoload
(defun hyperbole (&optional menu menu-list doc-flag help-string-flag)
  "Invokes the Hyperbole minibuffer menu when not already active.
\\[hyperbole] runs this.  Non-interactively, returns t if a menu is
displayed by this call, else nil (e.g. when already in a Hyperbole
mini-menu).

Two optional arguments may be given to invoke alternative menus.
MENU (a symbol) specifies the menu to invoke from MENU-LIST, (a
Hyperbole menu list structure).  MENU defaults to 'hyperbole and MENU-LIST
to `hui:menus'.  See `hui:menus' definition for the format of the menu list
structure.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."

  (interactive (list nil nil (if current-prefix-arg t) (if current-prefix-arg t)))
  (if (and hui:menu-p (> (minibuffer-depth) 0))
      (progn (beep) nil)
    (unwind-protect
	(progn
	  (hyperb:init-menubar)
	  (setq hui:menu-p t)
	  (hui:menu-act (or menu 'hyperbole) menu-list doc-flag help-string-flag)
	  t)
      (setq hui:menu-p nil))))

(defun hui:menu-act (menu &optional menu-list doc-flag help-string-flag)
  "Prompts user with Hyperbole MENU (a symbol) and performs selected item.
Optional second argument MENU-LIST is a Hyperbole menu list structure from
which to extract MENU.  It defaults to `hui:menus'.  See its definition for
the menu list structure.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."

  (let ((set-menu '(or (and menu (symbolp menu)
			    (setq menu-alist
				  (cdr (assq menu (or menu-list hui:menus)))))
		       (hypb:error "(hui:menu-act): Invalid menu symbol arg: `%s'"
			      menu)))
	(show-menu t)
	(rtn)
	menu-alist act-form)
    (while (and show-menu (eval set-menu))
      (cond ((and (consp (setq act-form (hui:menu-select menu-alist doc-flag help-string-flag)))
		  (cdr act-form)
		  (symbolp (cdr act-form)))
	     ;; Display another menu
	     (setq menu (cdr act-form)))
	    (act-form
	     (let ((prefix-arg current-prefix-arg))
	       (cond ((symbolp act-form)
		      (unless (eq act-form t)
			(setq show-menu nil
			      rtn (call-interactively act-form))))
		     ((stringp act-form)
		      (if (or doc-flag help-string-flag)
			  (setq show-menu nil
				rtn act-form)
			(hui:menu-help act-form)
			;; Loop and show menu again.
			))
		     (t (setq show-menu nil
			      rtn (eval act-form))))))
	    (t (setq show-menu nil))))
    rtn))

(defun hui:menu-backward-item ()
  "Move point back to the previous start of a selectable minibuffer menu item.  If on the first item, move to the last."
  (interactive)
  (if (save-excursion (not (search-backward ">" nil t)))
      (goto-char (point-max)))
  (if (and (re-search-backward "[ \t]+[^> \t\n]" nil t)
	   (save-excursion (search-backward ">" nil t)))
      (skip-chars-forward " \t")
    (goto-char (point-min))
    (hui:menu-backward-item)))

(defun hui:menu-doc (key-sequence &optional help-string-flag)
  "Returns formatted documentation for a normalized Hyperbole minibuffer menu KEY-SEQUENCE.
With optional HELP-STRING-FLAG, instead returns the one line help string for the key sequence."
  (when (and (stringp key-sequence)
	     (not (eq key-sequence ""))
	     (kbd-key:hyperbole-mini-menu-key-p key-sequence))
    (let ((hargs:reading-p 'hmenu-help))
      (setq unread-command-events
	    (nconc unread-command-events
		   (mapcar 'identity (substring key-sequence
						(length (or (car (where-is-internal #'hyperbole)) "\C-hh"))))))
      (prog1 (hui:menu-act 'hyperbole nil t help-string-flag)
	;; Ignore any keys past the first menu item activation.
	(discard-input)))))

(defun hui:menu-enter (&optional char-str)
  "Uses CHAR-STR or last input character as minibuffer argument."
  (interactive)
  (let ((input (or char-str (aref (recent-keys) (1- (length (recent-keys)))))))
    (cond (hyperb:emacs-p
	   (and (not (integerp input))
		(eventp input)
		(setq input (event-basic-type input))))
	  ((featurep 'xemacs)
	   (if (eventp input)
	       (setq input (event-to-character input)))))
    (if (or (symbolp input)
	    (and (integerp input)
		 (= input ?\r)))
	(setq input (hargs:at-p)))
    (erase-buffer)
    (or (symbolp input) (null input) (insert input)))
  (exit-minibuffer))

(defun hui:menu-forward-item ()
  "Move point to the next selectable minibuffer menu item.  If on the last item, move to the first."
  ;; First skip past menu name/description prompt, if need be.
  (interactive)
  (if (save-excursion (not (search-backward ">" nil t)))
      (search-forward ">" nil t))
  (if (re-search-forward "[ \t]+[^> \t\n]" nil t)
      (backward-char 1)
    (goto-char (point-min))
    (if (search-forward ">" nil t)
	(hui:menu-forward-item))))

(defun hui:menu-help (help-str)
  "Displays HELP-STR in a small window at the bottom of the selected frame.  HELP-STR must be a string."
  (let* ((window-min-height 2)
	 (owind (selected-window))
	 (buf-name (hypb:help-buf-name "Menu")))
    (unwind-protect
	(progn
	  (save-window-excursion
	    (hkey-help-show buf-name)) ;; Needed to save wconfig.
	  (if (eq (selected-window) (minibuffer-window))
	      (other-window 1))
	  (and (= (window-top-line) 0)
	       (< (- (frame-height) (window-height)) 2)
	       (split-window-vertically nil))
	  (select-window (hui:bottom-window))
	  (switch-to-buffer (get-buffer-create buf-name))
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "\n" help-str)
	  (set-buffer-modified-p nil)
	  (let ((neg-shrink-amount (- (+ 3 (hypb:char-count ?\n help-str)))))
	    (if (window-resizable-p (selected-window) neg-shrink-amount)
		(shrink-window (+ (window-height) neg-shrink-amount)))))
      (if (eq owind (minibuffer-window))
	  (select-window owind)))))

(defun hui:menu-xemacs (&optional menu menu-list)
  "Returns an XEmacs menu built from a Hyperbole minibuffer menu.
Optional MENU (a symbol) specifies a specific submenu of optional MENU-LIST.
a Hyperbole menu list structure.  Otherwise, all menus are used.
MENU defaults to 'hyperbole and MENU-LIST to `hui:menus'.  See `hui:menus'
definition for the format of the menu list structure."
  (mapcar
   (lambda (entry)
     (or (consp entry) 
	 (error "(hui:menu-xemacs): Invalid menu entry: %s" entry))
     (let ((label (car entry))
	   (content (car (cdr entry))))
       (cond ((null content) (hypb:replace-match-string ">$" label "" t))
	     ((and (consp content) (eq (car content) 'menu))
	      (hui:menu-xemacs (cdr content)))
	     (t (vector label content 't)))))
   (cdr (assq (or menu 'hyperbole) (or menu-list hui:menus)))))

(defun hui:menu-select (menu-alist &optional doc-flag help-string-flag)
  "Prompts user to choose the first character of any item from MENU-ALIST.
Case is not significant.  If chosen by direct selection with the Assist Key,
returns any help string for item, else returns the action form for the item.

Two additional optional arguments may be given when documentation for
a menu item should be shown rather than display of a menu.  DOC-FLAG
non-nil means show documentation for any item that is selected by the
user.  HELP-STRING-FLAG non-nil means show only the first line of the
documentation, not the full text."
  (let* ((menu-line (hui:menu-line menu-alist))
	 (set:equal-op 'eq)
	 (select-char (string-to-char hui:menu-select))
	 (quit-char (string-to-char hui:menu-quit))
	 (abort-char (string-to-char hui:menu-abort))
	 (top-char  (string-to-char hui:menu-top))
	 (item-keys (mapcar (lambda (item) (aref item 0))
			    (mapcar 'car (cdr menu-alist))))
	 ;; 0 matches an empty string return, no selection
	 (keys (apply 'list 0 1 select-char quit-char abort-char
		      top-char item-keys))
	 (key 0)
	 (hargs:reading-p 'hmenu)
	 sublist)
    (while (not (memq (setq key (upcase
				 (string-to-char
				  (read-from-minibuffer
				   "" menu-line hui:menu-mode-map))))
		      keys))
      (beep)
      (setq hargs:reading-p 'hmenu)
      (discard-input))
    ;; Here, the minibuffer has been exited, and `key' has been set to either:
    ;;   a menu item initial character code;
    ;;   a menu command character code;
    ;;   1 for in the menu prefix area;
    ;;   0 for at the end of the menu.
    (cond ((or (eq key 0) (eq key quit-char)) nil)
	  ((eq key abort-char) (beep) nil)
	  ((or (eq key 1) (eq key top-char)) '(menu . hyperbole))
	  ((and (eq key select-char)
		(save-excursion
		  (if (search-backward " " nil t)
		      (progn (skip-chars-forward " ")
			     (setq key (following-char))
			     nil)  ;; Drop through.
		    t))))
	  (t (hui:menu-item key doc-flag help-string-flag nil menu-alist)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(if (fboundp 'window-lowest-p)
    (defun hui:bottom-window ()
      "Return a window that is at the bottom of the selected frame."
      (let ((winds (hypb:window-list 'no-mini))
	    (window))
	(while (and (not window) winds)
	  (if (window-lowest-p (car winds))
	      (setq window (car winds))
	    (setq winds (cdr winds))))
	window))
  (defun hui:bottom-window ()
    "Return a window that is at the bottom of the selected frame."
    (let* ((winds (hypb:window-list 'no-mini))
	   (bot-list (mapcar
		      (lambda (wind)
			(nth 3 (window-edges wind)))
		      winds))
	   (bot (apply 'max bot-list)))
      (nth (- (length winds) (length (memq bot bot-list))) winds))))

(defun hui:menu-item (key doc-flag help-string-flag &optional menu menu-alist)
"Returns either the action or the documentation for a Hyperbole minibuffer menu item KEY.
If DOC-FLAG is non-nil, returns the fully formatted documentation unless
HELP-STRING-FLAG is non-nil, in which case only the first line of
documentation is returned.  If both are nil, the action form for the
item is returned.

Two additional optional arguments determine the items from which key
selects, MENU and MENU-ALIST are Hyperbole minibuffer menu internal
constructs.  If not given, the top-level Hyperbole menu is used."
  (unless menu-alist
    (setq menu-alist (or (cdr (assq (or (and (symbolp menu) menu) 'hyperbole)
				    hui:menus))
			 (hypb:error "(hui:menu-item): Invalid menu symbol arg: `%s'"
				     menu))))
  (let ((item-keys (mapcar (lambda (item) (aref item 0))
			    (mapcar 'car (cdr menu-alist))))
	 sublist)
    (if (setq sublist (memq key item-keys))
	(let* ((label-act-help-list
		(nth (- (1+ (length item-keys)) (length sublist))
		     menu-alist))
	       (act-form (car (cdr label-act-help-list))))
	  (if (or (eq hargs:reading-p 'hmenu-help)
		  (and doc-flag
		       ;; Not another menu to display
		       (not (and (listp act-form) (atom (car act-form)) (atom (cdr act-form))))))
	      (let ((help-str (or (car (cdr (cdr label-act-help-list)))
				  "No help documentation for this item.")))
		(if help-string-flag
		    help-str
		  (concat (car label-act-help-list) "\n  "
			  help-str "\n    Action: "
			  (prin1-to-string act-form))))
	    act-form)))))

(defun hui:menu-line (menu-alist)
  "Returns a menu line string built from MENU-ALIST."
  (let ((menu-prompt (concat (car (car menu-alist)) "  "))
	(menu-items (mapconcat 'car (cdr menu-alist) "  "))
	menu-line)
    (setq menu-line (concat menu-prompt menu-items))
    ;; Narrow menu by changing 2 spaces to 1 if too wide for current frame.
    (if (>= (length menu-line) (1- (frame-width)))
	(concat menu-prompt (mapconcat 'car (cdr menu-alist) " "))
      menu-line)))

(defun hui:menu-web-search ()
  (let ((web-mini-menu
	 (cons 'web
	       (cons '("Web>")
		     (mapcar (lambda (service)
			       (list service
				     (list #'hyperbole-web-search service)
				     (format "Search %s" service)))
			     (mapcar 'car hyperbole-web-search-alist))))))
    web-mini-menu))

(defun hui-search-web ()
  "Prompt for a web search engine and search term and then perform the search."
  (interactive)
  (hyperbole 'web))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

;; Hyperbole menu mode is suitable only for specially formatted data.
(put 'hui:menu-mode 'mode-class 'special)

(defvar hui:menu-mode-map nil
  "Keymap containing Hyperbole minibuffer menu commands.")
(if hui:menu-mode-map
    nil
  (setq hui:menu-mode-map (make-keymap))
  (suppress-keymap hui:menu-mode-map)
  (define-key hui:menu-mode-map hui:menu-quit   #'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-abort  #'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-top    #'hui:menu-enter)
  (define-key hui:menu-mode-map hui:menu-select #'hui:menu-enter)
  (define-key hui:menu-mode-map "\M-b"          #'hui:menu-backward-item)
  (define-key hui:menu-mode-map "\M-f"          #'hui:menu-forward-item)
  (define-key hui:menu-mode-map "\C-i"          #'hui:menu-forward-item)  ;; TAB
  (define-key hui:menu-mode-map [backtab]       #'hui:menu-backward-item) ;; Shift-TAB
  (define-key hui:menu-mode-map "\M-\C-i"       #'hui:menu-backward-item) ;; M-TAB
  ;;
  ;; This next binding is necessary since the default button1 binding under
  ;; XEmacs, mouse-track, is broken under XEmacs V19.8.
  (and (featurep 'xemacs) window-system
       (define-key hui:menu-mode-map 'button1 'mouse-set-point))
  (let ((i 32))
    (while (<= i 126)
      (define-key hui:menu-mode-map (char-to-string i) 'hui:menu-enter)
      (setq i (1+ i)))))

;;; ************************************************************************
;;; Hyperbole Minibuffer Menus
;;; ************************************************************************

(setq
 hui:menus
 (delq nil
 (list (cons
	'hyperbole
	(append
	 (let ((version (if (= (aref hyperb:version 0) ?0)
			    (substring hyperb:version 1)
			  hyperb:version)))
	   (list (list (concat "Hy" version ">"))))
	 (delq nil
	       (list
		'("Act"         hui:hbut-act
		  "Activates button at point or prompts for explicit button.")
		'("Butfile/"    (menu . butfile)
		  "Quick access button files menus.")
		'("Cust/"       (menu . cust)
		  "Customizes Hyperbole by setting major options.")
		'("Doc/"        (menu . doc)
		  "Quick access to Hyperbole documentation.")
		'("Ebut/"       (menu . ebut)
		  "Explicit button commands.")
		'("Find/"       (menu . find)
		  "Find matching line commands.")
		'("Gbut/"       (menu . gbut)
		  "Global button commands.")
		'("Hist"        (hhist:remove current-prefix-arg)
		  "Jumps back to location prior to last Hyperbole button follow.")
		'("Ibut/"       (menu . ibut)
		  "Implicit button and button type commands.")
		(if hyperb:kotl-p
		    '("Kotl/"   (menu . otl)
		      "Autonumbered outlining and hyperlink capabilities."))
		'("Msg/"        (menu . msg)
		  "Mail and News messaging capabilities.")
		'("Rolo/"       (menu . hyrolo)
		  "Hierarchical, multi-file rolo lookup and edit commands.")
		'("Screen/"     (menu . screen)
		  "Screen display management commands.")
		'("Win/"        (menu . win)
		  "Window configuration management commands.")
		))))
       '(butfile .
	 (("Butfile>")
	  ("DirFile"      (find-file hbmap:filename)
	   "Edits directory-specific button file.")
	  ("Info"
	   (id-info "(hyperbole)Button Files")
	   "Displays manual section on button files.") 
	  ("PersonalFile" (find-file
			    (expand-file-name hbmap:filename hbmap:dir-user))
	   "Edits user-specific button file.")
	  ))
       '(cust .
         (("Cust>")
	  ("All-Options" (customize-browse 'hyperbole)
	   "Display tree of Hyperbole customizable options by group.")
	  ("Debug-Toggle" hkey-toggle-debug
	   "Toggle display of Smart Key context after each press, for debugging.")
	  ("Find-File-URLs" hpath:find-file-urls-mode
	   "Toggle find-file support for ftp and www URLs.")
	  ("Isearch-Invisible" hypb:toggle-isearch-invisible
	   "Toggle whether isearch searches invisible text or not.")
	  ("KeyBindings/" (menu . cust-keys) "Rebinds global Hyperbole keys.")
	  ("Msg-Toggle-Ebuts" hyperbole-toggle-messaging
	   "Toggle Hyperbole support for explicit buttons in mail and news buffers.")
	  ("Override-Local-Keys" hkey-toggle-override-local-bindings
	   "Toggle whether conflicting local key bindings are overridden by Hyperbole.")
	  ("Referents/" (menu . cust-referents)
	   "Sets where Hyperbole button referents are displayed.")
	  ("Smart-Key-at-Eol/" (menu . cust-eol)
	   "Sets how scrolling via end of line presses works.")
	  ("Toggle-Rolo-Dates" hyrolo-toggle-datestamps
	   "Toggle whether date stamps are updated when rolo entries are edited.")
	  ("URL-Display/" (menu . cust-urls) "Sets where URLs are displayed.")))
       '(cust-eol .
         (("Smart Key press at eol scrolls>")
	  ("Proportionally" (setq smart-scroll-proportional t))
	  ("Windowful"      (setq smart-scroll-proportional nil))))
       '(cust-keys .
         (("Change Keys>")
	  ("ActionKey"   (hui:bind-key #'hkey-either))            ;; {M-RET}
	  ("ButRename"   (hui:bind-key #'hui:ebut-rename))        ;; {C-c C-r}
	  ("DragKey"     (hui:bind-key #'hkey-operate))           ;; {M-o}
	  ("HypbMenu"    (hui:bind-key #'hyperbole))              ;; {C-h h}
	  ("MarkThing"   (hui:bind-key #'hui-select-thing))       ;; {C-c RET}
	  ("SmartHelp"   (hui:bind-key #'hkey-help))              ;; {C-h A}
	  ("WinControl"  (hui:bind-key #'hycontrol-windows))      ;; {C-c \}
	  )) 
       '(cust-referents .
         (("Ref display>")
	  ("Any-Frame" (setq hpath:display-where 'other-frame))
	  ("Current-Win" (setq hpath:display-where 'this-window))
	  ("Diff-Frame-One-Win"
	   (setq hpath:display-where 'other-frame-one-window))
	  ("New-Frame" (setq hpath:display-where 'new-frame))
	  ("Other-Win" (setq hpath:display-where 'other-window))
	  ("Single-Win" (setq hpath:display-where 'one-window))))
       '(cust-urls .
         (("URL display>")
	  ("Chrome" (setq browse-url-browser-function #'browse-url-chrome))
	  ("Default" (setq browse-url-browser-function
			   (if (and (boundp 'browse-url-generic-program) (stringp browse-url-generic-program))
			       #'browse-url-generic
			     #'browse-url-default-browser)))
	  ("EWW" (setq browse-url-browser-function #'eww-browse-url))
	  ("Firefox" (setq browse-url-browser-function #'browse-url-firefox))
	  ("KDE" (setq browse-url-browser-function #'browse-url-kde))
	  ("XTerm" (setq browse-url-browser-function #'browse-url-text-xterm))
	  ))
       '(doc .
	 (("Doc>")
	  ("About"        (hypb:display-file-with-logo
			   (expand-file-name "HY-ABOUT" hyperb:dir))
	   "Overview of Hyperbole.")
	  ("Demo"         (hypb:display-file-with-logo
			    (expand-file-name "DEMO" hyperb:dir))
	   "Demonstrates Hyperbole features.")
	  ("Files"        (find-file-read-only
			    (expand-file-name "MANIFEST" hyperb:dir))
	   "Summarizes Hyperbole system files.  Click on an entry to view it.")
	  ("Glossary"
	   (id-info "(hyperbole)Glossary")
	   "Glossary of Hyperbole terms.")
	  ("Info"         (id-info "(hyperbole)Top")
	   "Online Info version of Hyperbole manual.")
	  ("New"          (hypb:display-file-with-logo
			   (expand-file-name "HY-NEWS" hyperb:dir))
	   "Recent changes to Hyperbole.")
	  ("SmartKeys"    (find-file-read-only (hypb:hkey-help-file))
	   "Summarizes Smart Key mouse or keyboard handling.")
	  ("Types/"       (menu . types)
	   "Provides documentation on Hyperbole types.")
	 ))
       '(ebut .
	 (("EButton>")
	  ("Act"    hui:hbut-act
	    "Activates button at point or prompts for explicit button.")
	  ("Create" hui:ebut-create)
	  ("Delete" hui:ebut-delete)
	  ("Edit"   hui:ebut-modify "Modifies any desired button attributes.")
	  ("Help/"  (menu . ebut-help) "Summarizes button attributes.")
	  ("Info"
	   (id-info "(hyperbole)Explicit Buttons")
	   "Displays manual section on explicit buttons.")
	  ("Modify" hui:ebut-modify "Modifies any desired button attributes.")
	  ("Rename" hui:ebut-rename "Relabels an explicit button.")
	  ("Search" hui:ebut-search
	   "Locates and displays personally created buttons in context.")
	  ("Types"  (hui:htype-help-current-window 'actypes)
	   "Displays documentation for one or all action types used by explicit buttons.")
	  ))
       '(ebut-help .
	 (("Help on>")
	  ("BufferButs"   (hui:hbut-report -1)
	   "Summarizes all explicit buttons in buffer.")
	  ("CurrentBut"   (hui:hbut-report)
	   "Summarizes only current button in buffer.")
	  ("OrderedButs"  (hui:hbut-report 1)
	   "Summarizes explicit buttons in lexicographically order.")
	  ))
       '(find .
         (("Find>")
	  ("GrepFiles"           hypb:rgrep  "Show numbered line matches in all specified files.")
	  ("LocateFiles"         hypb:locate "Locate matching file names anywhere across a system.")
	  ("MatchFileBuffers"    moccur      "Show numbered line matches for regexp in all file-based buffers.")
	  ("OccurHere"           occur       "Show numbered line matches for regexp from this buffer.")
	  ("RemoveLines"         hypb:remove-lines "Following point, remove all lines that match regexp.")
	  ("SaveLines"           hypb:save-lines  "Following point, keep only lines that match regexp.")
	  ("Web/" (menu . web) "Searches major web sites.")
	  ))
       '(gbut .
	 (("GButton>")
	  ("Act"    gbut:act        "Activates global button by name.") 
	  ("Create" hui:gbut-create "Adds a global button to gbut:file.")
	  ("Edit"   hui:gbut-modify "Modifies global button attributes.")
	  ("Help"   gbut:help       "Reports on a global button by name.") 
	  ("Info"   (id-info "(hyperbole)Global Buttons")
	   "Displays manual section on global buttons.")
	  ("Modify" hui:gbut-modify "Modifies global button attributes.")
	  ))
       '(ibut .
	 (("IButton>")
	  ("Act"    hui:hbut-current-act  "Activates implicit button at point.") 
	  ("DeleteIButType"   (hui:htype-delete 'ibtypes)
	   "Deletes specified button type.")
	  ("Help"   hui:hbut-help   "Reports on button's attributes.")
	  ("Info"   (id-info "(hyperbole)Implicit Buttons")
	   "Displays manual section on implicit buttons.")
	  ("Types"  (hui:htype-help 'ibtypes 'no-sort)
	   "Displays documentation for one or all implicit button types.")
	  ))
       '(msg .
	 (("Msg>")
	  ("Compose-Hypb-Mail"
	   (hmail:compose "hyperbole-users@gnu.org" '(hact 'hyp-config))
	   "Send a message to the Hyperbole discussion list.")
	  ("Join-Hypb-List"
	   (hmail:compose "hyperbole-users-join@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Subscribe to the Hyperbole discussion list.")
	  ("Leave-Hypb-List"
	   (hmail:compose "hyperbole-users-leave@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Unsubscribe from the Hyperbole discussion list.")
	  ("Report-Hypb-Bug"
	   (hmail:compose "bug-hyperbole@gnu.org" '(hact 'hyp-config))
	   "Send a message to the Hyperbole bug reporting list.")
	  ("Subscribe-Hypb-Bug"
	   (hmail:compose "bug-hyperbole-join@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Subscribe to the Hyperbole bug reporting list.")
	  ("Unsub-Hypb-Bug"
	   (hmail:compose "bug-hyperbole-leave@gnu.org" nil
			  "Just send the message; subject and body are ignored.")
	   "Unsubscribe from the Hyperbole bug reporting list.")
	  ))
       (if hyperb:kotl-p
	   '(otl
	     . (("Kotl>")
		("All"       kotl-mode:show-all "Expand all collapsed cells.") 
		("Blanks"    kvspec:toggle-blank-lines
		 "Toggle blank lines between cells on or off.")
		("Create"    kfile:find   "Create or edit an outline file.")
		("Downto"    kotl-mode:hide-sublevels
		 "Hide all cells in outline deeper than a particular level.")
		("Examp"     kotl-mode:example
		 "Display a self-descriptive example outline file.")
		("Hide"      (progn (kotl-mode:is-p)
				    (kotl-mode:hide-tree (kcell-view:label)))
		 "Collapse tree rooted at point.")
		("Info"
		 (id-info "(hyperbole)Koutliner")
		 "Display manual section on Hyperbole Koutliner.")
		("Kill"      kotl-mode:kill-tree
		 "Kill ARG following trees starting from point.")
		("Link"      klink:create
		 "Create and insert an implicit link at point.")
		("Overvw"  kotl-mode:overview
		 "Show first line of each cell.")
		("Show"      (progn (kotl-mode:is-p)
				    (kotl-mode:show-tree (kcell-view:label)))
		 "Expand tree rooted at point.")
		("Top"       kotl-mode:top-cells
		 "Hide all but top-level cells.") 
		("Vspec"     kvspec:activate
		 "Prompt for and activate a view specifiction.")
		)))
       '(hyrolo .
	 (("Rolo>")
	  ("Add"              hyrolo-add	  "Add a new rolo entry.")
	  ("Display"          hyrolo-display-matches
	   "Display last found rolo matches again.")
	  ("Edit"             hyrolo-edit   "Edit an existing rolo entry.")
	  ("Info"             (id-info "(hyperbole)HyRolo")
	   "Displays manual section on Hyperbole rolo.")
	  ("Kill"             hyrolo-kill   "Kill an existing rolo entry.")
	  ("Mail"             hyrolo-mail-to "Mail to address following point.")
	  ("Order"            hyrolo-sort   "Order rolo entries in a file.")
	  ("RegexFind"        hyrolo-grep   "Find entries containing a regexp.")
	  ("StringFind"       hyrolo-fgrep  "Find entries containing a string.")
	  ("WordFind"         hyrolo-word   "Find entries containing words.")
	  ("Yank"             hyrolo-yank
	   "Find an entry containing a string and insert it at point.")
	  ))
       '(screen .
	 (("Screen>")
	  ("FramesControl"    hycontrol-frames
	   "Interactively delete, jump to, move, replicate, and resize frames.")
	  ("WindowsControl"   hycontrol-windows
	   "Interactively delete, jump to, rebalance, resize, and split windows.")))
       '(types .
	 (("Types>")
	  ("ActionTypes"      (hui:htype-help-current-window 'actypes)
	   "Displays documentation for one or all action types.")
	  ("IButTypes"        (hui:htype-help-current-window 'ibtypes 'no-sort)
	   "Displays documentation for one or all implicit button types.")
	  ))
       '(win .
	 (("WinConfig>")
	  ("AddName"        hywconfig-add-by-name
	   "Name current window configuration.")
	  ("DeleteName"     hywconfig-delete-by-name
	   "Delete named window configuration.")
	  ("RestoreName"    hywconfig-restore-by-name
	   "Restore frame to window configuration given by name.")
	  ("PopRing"        (progn (hywconfig-delete-pop)
				   (hyperbole 'win))
	   "Restores window configuration from ring and removes it from ring.")
	  ("SaveRing"       (hywconfig-ring-save)
	   "Saves current window configuration to ring.")
	  ("YankRing"       (progn (call-interactively 'hywconfig-yank-pop)
				   (hyperbole 'win))
	   "Restores next window configuration from ring.")
	  ))
       (hui:menu-web-search)
       )))

(provide 'hui-mini)

;;; hui-mini.el ends here
