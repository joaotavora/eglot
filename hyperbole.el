;;; hyperbole.el --- GNU Hyperbole: The Everyday Hypertextual Information Manager

;; Copyright (C) 1992-2017  Free Software Foundation, Inc.

;; Author:           Bob Weiner
;; Maintainer:       Bob Weiner <rsw@gnu.org> and Mats Lidell <matsl@gnu.org>
;; Created:          06-Oct-92 at 11:52:51
;; Released:         24-Oct-17
;; Version:          7.0.1
;; Keywords:         comm, convenience, files, frames, hypermedia, languages, mail, matching, mouse, multimedia, outlines, tools, wp
;; Package:          hyperbole
;; Package-Requires: ((emacs "24.4"))
;; URL:              http://www.gnu.org/software/hyperbole

;; See the "HY-COPY" file for license information.

;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;; GNU Hyperbole (pronounced Ga-new Hi-per-bo-lee), or just Hyperbole, is an
;; easy-to-use, yet powerful and programmable hypertextual information
;; management system implemented as a GNU Emacs package.  It offers rapid views
;; and interlinking of all kinds of textual information, utilizing Emacs for
;; editing.  It can dramatically increase your productivity and greatly reduce
;; the number of keyboard/mouse keys you'll need to work efficiently.
;; 
;; Hyperbole lets you:
;; 
;; 1. Quickly create hyperlink buttons either from the keyboard or by dragging
;; between a source and destination window with a mouse button depressed.
;; Later activate buttons by pressing/clicking on them or by giving the name of
;; the button.
;; 
;; 2. Activate many kinds of `implicit buttons' recognized by context within
;; text buffers, e.g. URLs, grep output lines, and git commits.  A single key
;; or mouse button automatically does the right thing in dozens of contexts;
;; just press and go.
;; 
;; 3. Build outlines with multi-level numbered outline nodes, e.g. 1.4.8.6,
;; that all renumber automatically as any node or tree is moved in the
;; outline. Each node also has a permanent hyperlink anchor that you can
;; reference from any other node;
;; 
;; 4. Manage all your contacts quickly with hierarchical categories and embed
;; hyperlinks within each entry. Or create an archive of documents with
;; hierarchical entries and use the same search mechanism to quickly find any
;; matching entry;
;; 
;; 5. Use single keys to easily manage your Emacs windows or frames and quickly
;; retrieve saved window and frame configurations;
;; 
;; 6. Search for things in your current buffers, in a directory tree or across
;; major web search engines with the touch of a few keys.
;; 
;; The common thread in all these features is making retrieval, management and
;; display of information fast and easy. That is Hyperbole's purpose.
;; 
;; ----
;;
;; See the "INSTALL" file for installation instructions and the "README" file
;; for general information.
;;
;; There is no need to manually edit this file unless there are specific
;; customizations you would like to make, such as whether a Hyperbole Action
;; Mouse Key is bound to the middle mouse button.  (See the call of the
;; function, `hmouse-install', below).
;;
;; Other site-specific customizations belong in "hsettings.el".

;;; Code:
;;; ************************************************************************
;;; Start Initializations
;;; ************************************************************************

(defconst hyperbole-loading t
  "Temporary constant available for testing while Hyperbole is loading.") 

;; Ensure defgroup and defcustom are defined for use throughout Hyperbole.
(require 'custom)

(defgroup hyperbole nil
  "Hyperbole customizations category."
  :group 'applications)

(defgroup hyperbole-buttons nil
  "Hyperbole explicit, global and implicit button customizations."
  :group 'hyperbole)

(defgroup hyperbole-commands nil
  "Hyperbole command customizations."
  :group 'hyperbole)

(defgroup hyperbole-keys nil
  "Hyperbole keyboard and mouse key customizations."
  :group 'hyperbole)

;; defgroup hyperbole-rolo is in "hyrolo.el".

(defgroup hyperbole-screen nil
  "Hyperbole screen/display customizations, typically frame or window-related."
  :group 'hyperbole)

;; Reinitialize hyperb:dir on reload if initialization failed for any reason.
(eval-and-compile
  (when (and (boundp 'hyperb:dir) (null hyperb:dir))
    (makunbound 'hyperb:dir)
    (setq features (delq 'hload-path features)
	  features (delq 'hversion features)))

  ;; Defines hyperb:path-being-loaded, hyperb:stack-frame,
  ;; (hyperb:window-system), hyperb:dir and hyperb:kotl-p, which are used later in
  ;; this file.  Also adds Hyperbole to the load-path if need be.
  ;;
  ;; This handles the case when the Hyperbole package directory is not yet in load-path.
  (unless (or (require 'hversion nil t)
	      (and (stringp load-file-name)
		   (require 'hversion (expand-file-name
				       "hversion"
				       (file-name-directory load-file-name))
			    t)))
    (error "(Hyperbole): Startup failure: `hyperb:dir' must be manually added to `load-path' to fix.")))

;; This must be defined before the defcustom `inhbit-hyperbole-messaging'.
;;;###autoload
(defun hyperbole-toggle-messaging (&optional arg)
  "Toggle Hyperbole support for explicit buttons in mail and news buffers.
Toggles the boolean variable `inhibit-hyperbole-messaging’ and either
adds hooks (nil value) or removes them (t value).

With optional prefix ARG > 0, enables support.  If ARG <= 0,
disables/inhibits support."
  (interactive "P")
  (setq inhibit-hyperbole-messaging (if (null arg)
					(not inhibit-hyperbole-messaging)
				      (<= (prefix-numeric-value arg) 0)))
  (if inhibit-hyperbole-messaging
      (var:remove-all)
    (var:append-all)
    ;; Add any hooks that were skipped when inhibit-hyperbole-messaging
    ;; was nil.
    (cond ((boundp 'hyperbole-loading))
	  ((not after-init-time)
	   (add-hook 'after-init-hook (lambda () (load "hyperbole"))))
	  (t (load "hyperbole"))))
  (if (called-interactively-p 'interactive)
      (message "Hyperbole messaging button support is %s"
	       (if inhibit-hyperbole-messaging "disabled" "enabled"))))

;; This must come after "hversion" is loaded for hyperb:kotl-p definition.
(if hyperb:kotl-p
    (defgroup hyperbole-koutliner nil
      "Hyperbole multi-level autonumbered outliner customizations."
      :group 'hyperbole))

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;; Avoid any potential library name conflict by giving the load directory.
(require 'set (expand-file-name "set" hyperb:dir))

(require 'hui-select)  ;; This requires 'hvar which defines the var:append function.

;;; ************************************************************************
;;; Public Variables
;;; ************************************************************************

(defcustom hkey-init t
  "*A non-nil value (default) at system load time binds the Action and Assist Keyboard Keys, as well as other keys.
{\\[hkey-either]} invokes the Action Key and {C-u \\[hkey-either]} invokes the Assist Key.
Additionally, {\\[hkey-help]} shows what the Action Key will do in the current
context (wherever point is).  {C-u \\[hkey-help]} shows what the Assist Key will do."
  :type 'boolean
  :group 'hyperbole-keys)

(defcustom hkey-init-override-local-keys t
  "*If set together with `hkey-init', remove any local key bindings that hide the Hyperbole Smart Keys."
  :type 'boolean
  :group 'hyperbole-keys)

(defcustom inhibit-hyperbole-messaging t
  "*Determines whether Hyperbole supports explicit buttons in mail and news buffers.
The default of t means disable such support (work remains to
modernize these features).  When t, Hyperbole will not alter
messaging mode hooks nor overload functions from these packages,
preventing potential incompatibilities.

If you want to use Hyperbole buttons in mail and news buffers, set
this variable to nil by adding (hyperbole-toggle-messaging t)
to your personal Emacs initialization file, prior to loading
Hyperbole, and then restart Emacs."
  :type 'boolean
  :initialize (lambda (symbol value) (set symbol value))
  :set (lambda (symbol value) 
	 (set symbol (not value))
	 (hyperbole-toggle-messaging nil))
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public key bindings
;;; ************************************************************************

;;
;; Hyperbole key binding for many read-only modes.  Set to nil if unwanted.
;; No longer used; use regular Action Key instead.
;;(defvar action-key-read-only "\C-m"
;;  "Local Action Key binding for special read-only modes.  Set to nil if unwanted.")

(defvar hkey-bindings nil
  "List of global key sequences bound by Hyperbole.
See `hkey-binding-entry' for format.")

(defvar hkey-bindings-flag nil
  "True if Hyperbole key bindings are in use, else nil.")

(defvar hkey-previous-bindings nil
  "List of global key sequences and their pre-Hyperbole bindings that Hyperbole has overridden.
See `hkey-binding-entry' for format.")

(defun hkey-binding-entry (key)
  "Given an Emacs KEY that may be bound, returns an entry to save the associated binding.
Entry format is: (key-description key-sequence key-binding)."
  (list (key-description key) key (key-binding key)))

(defun hkey-bindings-keys (entries)
  (mapcar #'cadr entries))

(defun hkey-get-bindings ()
  "Returns a list of entries for storage of Hyperbole key bindings.
`hkey-initialize' must have already been called or the list will be empty."
  (mapcar (lambda (key) (hkey-binding-entry key))
	  (hkey-bindings-keys hkey-previous-bindings)))

(defun hkey-global-set-key (key command &optional no-add)
  (or no-add (add-to-list 'hkey-previous-bindings (hkey-binding-entry key)))
  (global-set-key key command))

(defun hkey-initialize ()
  "If `hkey-init' is non-nil, initialize Hyperbole key bindings."
  (when hkey-init
    ;;
    ;; Binds the Action Key to {M-RET} and the Assist Key to {C-u M-RET}
    ;; and loads the Hyperbole mouse key bindings.
    (unless (where-is-internal 'hkey-either)
      (hkey-global-set-key "\M-\C-m" 'hkey-either))
    ;;
    ;; Bind a key, {C-h A}, for Action Key help and {C-u C-h A} for Assist key
    ;; help.
    (or (where-is-internal 'hkey-help)
	(hkey-global-set-key "\C-hA" 'hkey-help))
    ;;
    ;; Setup so Hyperbole menus can be accessed from a key.  If not
    ;; already bound to a key, binds the function `hyperbole' to {C-h h}.
    (or (where-is-internal 'hyperbole)
	;; In GNU Emacs, this binding replaces a command that shows
	;; the word hello in foreign languages; this binding makes this
	;; key much more useful.
	(hkey-global-set-key "\C-hh" 'hyperbole))
    ;;
    ;; Provides a site standard way of emulating most Hyperbole mouse drag
    ;; commands from the keyboard.  This is most useful for rapidly creating
    ;; Hyperbole link buttons from the keyboard without invoking the Hyperbole
    ;; menu.  Only works if Hyperbole is run under a window system.
    (when (hyperb:window-system)
      (if (eq (global-key-binding "\M-o") 'facemenu-keymap)
	  ;; Override facemenu package that adds a keymap on M-o,
	  ;; since this binding is more important to Hyperbole
	  ;; users.
	  (hkey-global-set-key "\M-o" 'hkey-operate)
	(hkey-maybe-global-set-key "\M-o" 'hkey-operate)))
    ;;
    ;; Binds {C-c @} to created a user-specified sized grid of windows
    ;; displaying different buffers.
    ;;
    ;; Don't override local bindings of this key.
    (hkey-maybe-global-set-key "\C-c@" 'hycontrol-windows-grid t)
    ;;
    ;; Binds {C-c C-r} as a site standard way of performing explicit
    ;; button renames without invoking the Hyperbole menu.
    ;;
    ;; Don't override local bindings of this key.
    (hkey-maybe-global-set-key "\C-c\C-r" 'hui:ebut-rename t)
    ;;
    ;; Binds {C-c RET} to select larger and larger synctactical units in a
    ;; buffer when invoked repeatedly, showing in the minibuffer the type
    ;; of unit selected each time.
    (hkey-maybe-global-set-key "\C-c\C-m" 'hui-select-thing)
    ;;
    ;; Binds {C-c \} to interactively manage windows and frames.
    (hkey-maybe-global-set-key "\C-c\\" 'hycontrol-enable-windows-mode)
    ;;
    ;; Binds {C-c /} to display the Hyperbole Find/Web search menu.
    (hkey-maybe-global-set-key "\C-c/" 'hui-search-web)
    ;;
    ;; Binds {C-c .} to jump between the start and end of an delimited thing.
    ;; Don't override local bindings of this key.
    (hkey-maybe-global-set-key "\C-c." 'hui-select-goto-matching-delimiter t)
    ;;
    ;; This initializes the Smart Mouse Key bindings.  Shifted mouse buttons
    ;; are always set up.  Under InfoDock or with `hmouse-middle-flag'
    ;; non-nil, this also binds the middle mouse button to the Action Key.
    ;; These bindings are ignored if a particular frame does not have mouse
    ;; support.
    (hmouse-install hmouse-middle-flag)
    ;;
    ;; This makes a double or triple click of the left mouse button do the
    ;; same thing as {C-c RET}.  It also sets up Java, C++ and HTML modes
    ;; for proper entity selection.
    (hui-select-initialize)
    ;;
    ;; Store Hyperbole key bindings so can turn them on and off.
    (setq hkey-bindings (hkey-get-bindings)
	  hkey-bindings-flag t)
    ))

(defun hkey-maybe-global-set-key (key command &optional no-add)
  "Globally set KEY to COMMAND if KEY is unbound and COMMAND is not on any global key.
With third argument NO-ADD non-nil, skip storage of prior KEY binding
which prevents automatic removal of any local bindings to the same key."
  (or (global-key-binding key)
      (where-is-internal command)
      (hkey-global-set-key key command)))

(defun hkey-set-bindings (key-binding-list)
  "Sets keys bound by Hyperbole to those in KEY-BINDING-LIST.
KEY-BINDING-LIST is the value of either `hkey-previous-bindings'
\(key bindings prior to Hyperbole load) or `hkey-bindings' (Hyperbole
bindings after load)."
  (cond
    ;;
    ;; GNU Emacs, XEmacs or InfoDock
    ((or (featurep 'xemacs) hyperb:emacs-p)
     (mapcar
       (lambda (key-and-binding)
	 (global-set-key (cadr key-and-binding) (car (cddr key-and-binding))))
       key-binding-list))
    ;;
    ;; X
    ((equal (hyperb:window-system) "xterm")
     (mapcar
       (lambda (key-and-binding)
	 (define-key mouse-map (cadr key-and-binding) (car (cddr key-and-binding))))
       key-binding-list))
    ;;
    ;; NeXT
    ((equal (hyperb:window-system) "next")
     (mapcar
       (lambda (key-and-binding)
	 (global-set-mouse (cadr key-and-binding) (car (cddr key-and-binding))))
       key-binding-list))))

(defun hyperbole-toggle-bindings ()
  "Toggles between Hyperbole mouse and keyboard keys and their prior bindings."
  (interactive)
  (let ((key-binding-list (if hkey-bindings-flag
			      hkey-previous-bindings
			    hkey-bindings))
	(other-bindings-var (if hkey-bindings-flag
				'hkey-bindings
			      'hkey-previous-bindings)))
    (if key-binding-list
	(progn
	  (set other-bindings-var (hkey-get-bindings))
	  (hkey-set-bindings key-binding-list)
	  (setq hkey-bindings-flag (not hkey-bindings-flag)
		hmouse-bindings-flag (not hmouse-bindings-flag))
	  (if (called-interactively-p 'interactive)
	      (message "%s mouse and keyboard bindings are now in use."
		       (if hkey-bindings-flag "Hyperbole" "Non-Hyperbole"))))
      (error "(hyperbole-toggle-bindings): `%s' is empty."
	     (if hkey-bindings-flag 'hkey-previous-bindings 'hkey-bindings)))))

;;; ************************************************************************
;;; Load Hyperbole mouse bindings
;;; ************************************************************************

(if (boundp 'mouse-position-function)
    (setq mouse-position-function
	  (lambda (frame-x-dot-y)
	    "Make `mouse-position' and `mouse-pixel-position' always return the selected frame.
Under macOS and Windows 7 at least, upon initial selection of a new
frame, those functions by default still return the prior frame."
	    (if (consp frame-x-dot-y) (setcar frame-x-dot-y (selected-frame)))
	    frame-x-dot-y)))

(require 'hmouse-key)

;;; ************************************************************************
;;; You shouldn't need to modify anything below here.
;;; ************************************************************************

;; Add Hyperbole Info directory to Info-directory-list after the Info
;; manual reader package is loaded.
(eval-after-load "info"
  '(when (boundp 'hyperb:dir)
     (info-initialize)
     (let ((info-dir (expand-file-name "man/" hyperb:dir)))
       (if (file-exists-p info-dir)
	   (add-to-list 'Info-directory-list info-dir)))))

;;; ************************************************************************
;;; Prevent local key maps from hiding/overriding the Action and Assist Keys
;;; ************************************************************************

;; (defun hkey-read-only-bindings ()
;;   "Binds Action and Assist Key functions in many read-only modes to the key sequence value of `action-key-read-only'.
;; Does nothing if this variable is nil."
;;   (and action-key-read-only
;;        (mapcar
;; 	(lambda (keymap-sym)
;; 	    (if (and (boundp keymap-sym) (keymapp keymap-sym))
;; 		(define-key (symbol-value keymap-sym) action-key-read-only #'action-key)))
;; 	'(Buffer-menu-mode-map calendar-mode-map dired-mode-map gnus-group-mode-map
;;           gnus-summary-mode-map Info-mode-map oo-browse-mode-map rmail-mode-map
;; 	  rmail-summary-mode-map unix-apropos-map))))

;;
;; Overriding of local key bindings that interfere with global
;; bindings from Hyperbole.  See `hyperb:init' for the hook that calls
;; these functions.
;;
(defun hkey-override-local-bindings ()
  "If `hkey-init-override-local-keys' and `hkey-init' are t, override a local key binding that hides the global Hyperbole Smart Keys, by removing it."
  (interactive)
  (when hkey-init
    ;; Do read-only bindings here, even though this will run
    ;; many times, since each key map exists only after
    ;; its major mode is first used or loaded.
    ;; (hkey-read-only-bindings)
    (if hkey-init-override-local-keys
	(let (hkey
	      binding)
	  (mapc (lambda (descrip-key-cmd)
		  (and (setq hkey (cadr descrip-key-cmd))
		       ;; To see the key name, use: (key-description hkey)
		       (setq binding (local-key-binding hkey))
		       ;; A number indicates an invalid key prefix, so
		       ;; there is not actually a local binding for
		       ;; this key sequence.
		       (not (numberp binding))
		       (local-unset-key hkey)))
		hkey-previous-bindings)))))

(defun hkey-install-override-local-bindings ()
  ;; Run after any major-mode change within any buffer.
  (add-hook 'change-major-mode-after-body-hook #'hkey-override-local-bindings)
  ;; Need to override bindings in any buffers that exist already if
  ;; overriding is enabled.
  (and hkey-init hkey-init-override-local-keys
       (mapc (lambda (buf) (with-current-buffer buf
			     (hkey-override-local-bindings)))
	     (buffer-list))))

(defun hkey-toggle-override-local-bindings (&optional arg)
  "Toggle whether conflicting local key bindings are overridden by Hyperbole.
With optional ARG, override them iff ARG is positive."
  (interactive "P")
  (if (or (and arg (<= (prefix-numeric-value arg) 0))
	  (and (not (and arg (> (prefix-numeric-value arg) 0)))
	       hkey-init-override-local-keys))
      (progn (setq hkey-init-override-local-keys nil)
	     (message "Local key bindings that conflict with Hyperbole will be left in place."))
    (setq hkey-init-override-local-keys t)
    (message "Local key bindings that conflict with Hyperbole will be removed.")))

;;; ************************************************************************
;;; Display Hooks
;;; ************************************************************************

;; Permits restore of the prior window configuration after any help buffer
;; is shown by pressing either the Action or Assist Key at the end of the
;; help buffer.  (Help buffer names end with "Help*".)  Only one of
;; these two settings is used, dependent on emacs version.
;;
(setq temp-buffer-show-hook #'hkey-help-show
      temp-buffer-show-function temp-buffer-show-hook)

;;; ************************************************************************
;;; Autoloads
;;; ************************************************************************

;; Koutliner autoloads in the kotl/ subdirectory are generated by 'make pkg'.
;; This next line ensures they are loaded by hyperbole-autoloads whenever
;; the Hyperbole package is activated in an Emacs session.
;;;###autoload (load "kotl/kotl-autoloads" nil 'nowarn)

;; Before the 6.0.1 release, Hyperbole used to patch the package-generate-autoloads 
;; function to ensure that kotl/ subdirectories were autoloaded.  This
;; is no longer used but is left here temporarily for reference.
;;
;; Ensure Koutliner autoloads in kotl/ subdirectory are generated and loaded.
;; (unless (or (fboundp 'kotl-mode)
;; 	    (and (load "hyperbole-autoloads" t t)
;; 		 (fboundp 'kotl-mode)))
;;   (defun hyperb:package-autoloads-subdirectories-p ()
;;     (require 'package)
;;     (let ((func (symbol-function 'package-generate-autoloads)))
;;       ;; If this function contains a call to apply, then it is patched
;;       ;; with support for finding autoloads in subdirectories and
;;       ;; nothing more need be done.
;;       (if (byte-code-function-p func)
;; 	  (delq nil (mapcar (lambda (item) (eq item 'apply)) (aref func 2)))
;; 	(string-match "(apply " (prin1-to-string func)))))

;;   (unless (hyperb:package-autoloads-subdirectories-p)
;;     ;; Function is not patched, so define it here, call it, and then load
;;     ;; the generated autoloads.  This will happen maximally only once
;;     ;; per installation of a Hyperbole release.
;;     (if (not (file-writable-p (expand-file-name "hyperbole-autoloads.el" hyperb:dir)))
;; 	(error "(Hyperbole): Failure loading, need write permission to \"%s\"" hyperb:dir))
;;     (defun package-generate-autoloads (name pkg-dir)
;;       (let* ((auto-name (format "%s-autoloads.el" name))
;; 	     (generated-autoload-file (expand-file-name auto-name pkg-dir))
;; 	     ;; Silence `autoload-generate-file-autoloads'.
;; 	     (noninteractive t)
;; 	     (backup-inhibited t)
;; 	     (version-control 'never))
;; 	(package-autoload-ensure-default-file generated-autoload-file)
;; 	(apply #'update-directory-autoloads pkg-dir
;; 	       (delq nil (mapcar (lambda (f) (and (file-directory-p f)
;; 						  (not (file-symlink-p f))
;; 						  f))
;; 				 (directory-files pkg-dir t "[a-zA-Z].*" nil))))
;; 	(let ((buf (find-buffer-visiting generated-autoload-file)))
;; 	  (when buf (kill-buffer buf)))
;; 	auto-name))
;;     (package-generate-autoloads "hyperbole" hyperb:dir))
;;   (load "hyperbole-autoloads"))

;; Menu items could call this function before Info is loaded.
(autoload 'Info-goto-node   "info"       "Jump to specific Info node."  t)

;;; Hyperbole user interface entry points that trigger loading of the
;;; full Hyperbole system.  These are left commented here for
;;; reference in case we ever go back to autoloading Hyperbole rather
;;; than initializing it fully in this file.

;; ;; Action type definitions.
;; (autoload 'defact            "hyperbole"
;;   "Creates an action TYPE (an unquoted symbol) with PARAMS, described by DOC."
;;   nil 'macro)
;; ;; Implicit button type definitions.
;; (autoload 'defib             "hyperbole"
;;   "Creates implicit button TYPE (unquoted sym) with PARAMS, described by DOC."
;;   nil 'macro)

;; (autoload 'ebut:map          "hyperbole"      "Map over Hyperbole buffer buttons." nil)
;; (autoload 'hbut:key-src      "hyperbole"      "Called by {e} command in rolo match buffer.")
;; (autoload 'hui:ebut-rename   "hyperbole"      "Rename a Hyperbole button."     t)
;; (autoload 'hyperbole         "hyperbole"      "Hyperbole info manager menus."  t)

;; (autoload 'action-key        "hyperbole"
;;   "Context-sensitive Action Key command."                                  t)
;; (autoload 'action-key-depress "hyperbole"     "Depress context-sensitive Action Key." t)
;; (autoload 'assist-key-depress "hyperbole"     "Depress context-sensitive Assist Key." t)
;; (autoload 'action-key-depress-emacs "hyperbole" "Depress context-sensitive Action Key." t)
;; (autoload 'assist-key-depress-emacs "hyperbole" "Depress context-sensitive Assist Key." t)
;; (autoload 'action-mouse-key-emacs  "hyperbole" "Execute context-sensitive Action Key." t)
;; (autoload 'assist-mouse-key-emacs  "hyperbole" "Execute context-sensitive Assist Key." t)
;; (autoload 'hkey-help         "hyperbole"
;;   "Display help for the Action Key command in current context.
;; With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
;; Returns non-nil iff associated help documentation is found."               t)
;; (autoload 'hkey-assist-help  "hyperbole"
;;   "Display help for the Assist Key command in current context."            t)
;; (autoload 'hkey-help-hide    "hyperbole"
;;   "Restores frame to configuration prior to help buffer display."        nil)
;; (autoload 'hkey-help-show    "hyperbole"
;;   "Saves prior frame configuration if BUFFER displays help."             nil)
;; (autoload 'assist-key        "hyperbole"
;;   "Context-sensitive Assist Key command."                                  t)
;; (autoload 'action-mouse-key  "hyperbole"
;;   "Context-sensitive Action Mouse Key command."                            t)
;; (autoload 'assist-mouse-key  "hyperbole"
;;   "Context-sensitive Assist Mouse Key command."                            t)
;; (autoload 'hkey-operate      "hyperbole"      "Emulate Hyperbole mouse key drags." t)
;; (autoload 'symset:add        "hyperbole"      "Adds ELT to SYMBOL's PROP set." nil)
;; (autoload 'hact              "hyperbole"      "Performs action formed from rest of ARGS." nil)
;; (autoload 'actypes::exec-window-cmd "hyperbole"
;; 	  "Executes an external window-based SHELL-CMD string asynchronously." nil)
;; (autoload 'hpath:absolute-to "hyperbole"
;; 	  "Make PATH absolute from optional DEFAULT-DIRS." nil)
;; (autoload 'hpath:display-buffer "hyperbole"
;; 	  "Displays and selects BUFFER at optional DISPLAY-WHERE location or at `hpath:display-where'." t)
;; (autoload 'hpath:find        "hyperbole"
;; 	  "Edit file FILENAME, possibly using a special command." t)
;; (autoload 'hpath:find-other-frame "hyperbole"
;; 	  "Edit file FILENAME in other frame, possibly using a special command." t)
;; (autoload 'hpath:find-other-window "hyperbole"
;; 	  "Edit file FILENAME in other window, possibly using a special command." t)

;;; Auto-autoload doesn't work for next item because it is defined
;;; within a condition-case, so autoload it here.
(autoload 'Vm-init    "hvm"    "Initializes Hyperbole Vm support." t)

;;; ************************************************************************
;;; Outline Mode Aliases
;;; ************************************************************************

(require 'outline)
(unless (fboundp 'outline-hide-body)
  (defalias 'outline-hide-body 'hide-body))
(unless (fboundp 'outline-hide-entry)
  (defalias 'outline-hide-entry 'hide-entry))
(unless (fboundp 'outline-show-entry)
  (defalias 'outline-show-entry 'show-entry))
(unless (fboundp 'outline-show-all)
  (defalias 'outline-show-all 'show-all))
(unless (fboundp 'outline-hide-subtree)
  (defalias 'outline-hide-subtree 'hide-subtree))
(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))
(unless (fboundp 'outline-flag-region)
  (defun outline-flag-region (from to flag)
    "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
    (if flag
	(subst-char-in-region from to ?\n ?\r t)
      (subst-char-in-region from to ?\r ?\n t))))
(unless (fboundp 'outline-invisible-in-p)
  (defun outline-invisible-in-p (beg end)
    "Return t if there is an invisible character between BEG and END, else nil."
    (catch 'result
      (delq nil (mapcar (lambda (o)
			  (if (eq 'outline (overlay-get o 'invisible))
			      (throw 'result t)))
			(overlays-in beg end))))))

;;; ************************************************************************
;;; Message System Support Configuration
;;; ************************************************************************

;; Even if you don't need some of the following hook settings, you might
;; as well leave them in so that if they ever become useful to you, you
;; need not reconfigure Hyperbole.  These settings do nothing if the
;; corresponding subsystems are never invoked.
;;
;; GNUS USENET news reader/poster support.
;;
(var:append 'gnus-Startup-hook '(Gnus-init))
;;
;; Hyperbole mail reader support configuration.
;;
;; Rmail
(var:append 'rmail-mode-hook    '(Rmail-init))
;; Mh-e
(var:append 'mh-inc-folder-hook '(Mh-init))
;;
;; VM support is based on V5.72 beta of VM.  If you have a version of VM
;; earlier than 5.70 beta, you should either upgrade or comment out the
;; following line so that Hyperbole support for VM is not enabled.
(var:append 'vm-mode-hook       '(Vm-init))
;;
;; Hyperbole mail composer support configuration.
;;
(var:append 'message-mode-hook   (list (lambda () (require 'hsmail))))
(var:append 'mh-letter-mode-hook (list (lambda () (require 'hsmail))))
(var:append 'vm-mail-mode-hook   (list (lambda () (require 'hsmail))))


;;; ************************************************************************
;;; URL Browsing
;;; ************************************************************************

(require 'browse-url)

;; Use any obsolete URL setting from earlier Hyperbole releases to set the
;; new URL browsing variable.
(if (and (boundp 'action-key-url-function) action-key-url-function)
    (cond ((eq action-key-url-function 'w3-fetch)
	   (setq browse-url-browser-function 'browse-url-w3))
	  ((eq action-key-url-function
	       'highlight-headers-follow-url-netscape)
	   (setq browse-url-browser-function 'browse-url-netscape
		 browse-url-new-window-flag nil))
	  ((eq action-key-url-function
	       'highlight-headers-follow-url-netscape-new-window)
	   (setq browse-url-browser-function 'browse-url-netscape
		 browse-url-new-window-flag t))))

;;; ************************************************************************
;;; Load Site-specific Configurations and Initialize Hyperbole Package
;;; ************************************************************************

(require 'hsettings)

(defun hyperb:init ()
  "Standard configuration routine for Hyperbole."
  (interactive)
  (message "Initializing Hyperbole...")
  (run-hooks 'hyperbole-init-hook)
  (hyperb:check-dir-user)
  (or (stringp hyperb:user-email)
      (setq hyperb:user-email
	    (or (and (boundp 'user-mail-address)
		     (stringp user-mail-address)
		     (string-match "@" user-mail-address)
		     user-mail-address)
		(concat (user-login-name) (hypb:domain-name)))))
  ;;
  ;; Conditionally initialize Hyperbole key bindings (when hkey-init is t)
  (hkey-initialize)
  ;;
  ;; Save button attribute file whenever same dir file is saved and
  ;; `ebut:hattr-save' is non-nil.
  (add-hook (if (boundp 'write-file-functions)
		'write-file-functions
	      'write-file-hooks)
	    #'hattr:save t)
  ;;
  (hyperb:init-menubar)
  ;;
  ;; This installs a hook that removes any local key bindings which
  ;; hide the global Action Key, if `hkey-init' and
  ;; `hkey-init-override-local-keys' are t.  Typically, the Action Key
  ;; will be much more useful than the local key anyway.  Setting
  ;; `hkey-init-override-local-keys' to nil at any time, will prevent
  ;; removal of further local bindings.
  (if (featurep 'hyperbole)
      (hkey-install-override-local-bindings)
    (add-hook 'after-load-alist '(hyperbole hkey-install-override-local-bindings)))
  ;;
  ;; Hyperbole initialization is complete. 
  (message "Initializing Hyperbole...done")
  (message "Hyperbole %s is ready for action." hyperb:version))

;; This call loads the rest of the Hyperbole system.
(require 'hinit)

(if after-init-time
    ;; This call initializes Hyperbole key bindings and hooks.
    (hyperb:init)
  ;; Initialize after other key bindings are loaded at startup.
  (add-hook 'after-init-hook #'hyperb:init t))

(makunbound 'hyperbole-loading)

(provide 'hyperbole)

;;; hyperbole.el ends here
