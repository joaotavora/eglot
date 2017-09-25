;;; hargs.el --- GNU Hyperbole user input functions
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    31-Oct-91 at 23:17:35
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This module should be used for any interactive prompting and
;;   argument reading that Hyperbole does through Emacs.
;;
;;   `hargs:iform-read' provides a complete Lisp-based replacement for
;;   interactive argument reading (most of what `call-interactively' does).
;;   It also supports prompting for new argument values with defaults drawn
;;   from current button arguments.  A few extensions to interactive argument
;;   types are also provided, see `hargs:iforms-extensions' for details.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hpath)
(require 'hypb)
(require 'set)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hargs:reading-p nil
  "t only when Hyperbole is prompting user for input, else nil.")

(add-hook 'completion-setup-hook #'hargs:set-string-to-complete)
(add-hook 'minibuffer-exit-hook  #'hargs:unset-string-to-complete)

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

;;; From etags.el, so don't have to load the whole thing.
(unless (fboundp 'find-tag-default)
  (defun find-tag-default ()
    (or (and (boundp 'find-tag-default-hook)
	     (not (memq find-tag-default-hook '(nil find-tag-default)))
	     (condition-case data
		 (funcall find-tag-default-hook)
	       (error
		(message "value of find-tag-default-hook signalled error: %s"
			 data)
		(sit-for 1)
		nil)))
	(save-excursion
	  (if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	      (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
		(forward-char 1)))
	  (while (looking-at "\\sw\\|\\s_")
	    (forward-char 1))
	  (if (re-search-backward "\\sw\\|\\s_" nil t)
	      (regexp-quote
	       (progn (forward-char 1)
		      (buffer-substring (point)
					(progn (forward-sexp -1)
					       (while (looking-at "\\s'")
						 (forward-char 1))
					       (point)))))
	    nil)))))

(unless (fboundp 'find-tag--default)
  (defun find-tag--default ()
    (funcall (or (if (fboundp find-tag-default-function) find-tag-default-function)
		 (get major-mode 'find-tag-default-function)
		 'find-tag-default))))
(defalias 'hargs:find-tag-default 'find-tag--default)


(defun hargs:action-get (action modifying)
  "Interactively gets list of arguments for ACTION's parameters.
Current button is being modified when MODIFYING is non-nil.
Returns nil if ACTION is not a list or byte-code object, has no
interactive form or takes no arguments."
  (and (or (hypb:emacs-byte-code-p action) (listp action))
       (let ((interactive-form (action:commandp action)))
	 (if interactive-form
	     (action:path-args-rel
	      (hargs:iform-read interactive-form modifying))))))

(defun hargs:delimited (start-delim end-delim
			&optional start-regexp-flag end-regexp-flag list-positions-flag)
  "Returns a normalized, single line, delimited string that point is within, or nil.
START-DELIM and END-DELIM are strings that specify the argument
delimiters.  With optional START-REGEXP-FLAG non-nil, START-DELIM is
treated as a regular expression.  END-REGEXP-FLAG is similar.
With optional LIST-POSITIONS-FLAG, return list of (string-matched start-pos end pos)."
  (let* ((opoint (point))
	 (limit (if start-regexp-flag opoint
		  (+ opoint (1- (length start-delim)))))
	 (start-search-func (if start-regexp-flag 're-search-forward
			      'search-forward))
	 (end-search-func (if end-regexp-flag 're-search-forward
			    'search-forward))
	 start end)
    (save-excursion
      (beginning-of-line)
      (while (and (setq start (funcall start-search-func start-delim limit t))
		  (< (point) opoint)
		  ;; This is not to find the real end delimiter but to find
		  ;; end delimiters that precede the current argument and are
		  ;; therefore false matches, hence the search is limited to
		  ;; prior to the original point.
		  (funcall end-search-func end-delim opoint t))
	(setq start nil))
      (when start
	(forward-line 2)
	(setq limit (point))
	(goto-char opoint)
	(and (funcall end-search-func end-delim limit t)
	     (setq end (match-beginning 0))
	     ;; Ignore any preceding backquote, e.g. when a double-quoted
	     ;; string is embedded within a doc string.
	     (if (and (> end (point-min))
		      (= (char-before end) ?\\))
		 (setq end (1- end))
	       t)
	     (< start end)
	     (let ((string (hypb:replace-match-string "[\n\r]\\s-*" (buffer-substring start end) " " t)))
	       (if list-positions-flag
		   (list string start end)
		 string)))))))

(defun hargs:get (interactive-entry &optional default prior-arg)
  "Prompts for an argument, if need be, from INTERACTIVE-ENTRY, a string.
Optional DEFAULT is inserted after prompt.
First character of INTERACTIVE-ENTRY must be a command character from
the list in the documentation for `interactive' or a `+' which
indicates that the following character is a Hyperbole interactive
extension command character.

May return a single value or a list of values, in which case the first
element of the list is always the symbol 'args."
  (let (func cmd prompt)
    (cond ((or (null interactive-entry) (equal interactive-entry ""))
	   (error "(hargs:get): Empty interactive-entry arg."))
	  ;; Hyperbole / user extension command character.  The next
	  ;; character is the actual command character.
	  ((eq (aref interactive-entry 0) ?+)
	   (setq cmd (aref interactive-entry 1)
		 prompt (format (substring interactive-entry 2) prior-arg)
		 func (if (< cmd (length hargs:iform-extensions-vector))
			  (aref hargs:iform-extensions-vector cmd)))
	   (if func
	       (funcall func prompt default)
	     (error
	      "(hargs:get): Bad interactive-entry extension character: `%c'."
	      cmd)))
	  ;; Normal interactive command character
	  (t (setq cmd (aref interactive-entry 0)
		   prompt
		   (format (substring interactive-entry 1) prior-arg)
		   func (if (< cmd (length hargs:iform-vector))
			    (aref hargs:iform-vector cmd)))
	     (if func
		 (funcall func prompt default)
	       (error
		"(hargs:get): Bad interactive-entry command character: `%c'."
		cmd))))))

(defun hargs:make-iform-vector (iform-alist)
  "Returns a vector built from IFORM-ALIST used for looking up interactive command code characters."
  ;; Vector needs to have 1 more elts than the highest char code for
  ;; interactive commands.
  (let* ((size (1+ (car (sort (mapcar 'car iform-alist) '>))))
	 (vec (make-vector size nil)))
    (mapc (lambda (elt)
	    (aset vec (car elt)
		  `(lambda (prompt default)
		     (setq hargs:reading-p ',(cadr elt))
		     ,(cddr elt))))
	  iform-alist)
    vec))

(defun hargs:prompt (prompt default &optional default-prompt)
  "Returns string of PROMPT including DEFAULT.
Optional DEFAULT-PROMPT is used to describe default value."
  (if default
      (format "%s(%s%s%s) " prompt (or default-prompt "default")
	      (if (equal default "") "" " ")
	      default)
    prompt))

(defun hargs:select-event-window ()
  "Select window, if any, that mouse was over during last event."
  (if (featurep 'xemacs)
      (if current-mouse-event
	  (select-window
	   (or (event-window current-mouse-event)
	       (selected-window))))
    (let* ((event last-command-event)
	   (window (posn-window (event-start event))))
      (if (and (eq window (minibuffer-window))
	       (not (minibuffer-window-active-p
		     (minibuffer-window))))
	  (error "Attempt to select inactive minibuffer window")
	(select-window (or window (selected-window)))))))

(defun hargs:set-string-to-complete ()
  "Store the current minibuffer contents into `hargs:string-to-complete'."
  (save-window-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (setq hargs:string-to-complete (minibuffer-contents-no-properties))
    (if (equal hargs:string-to-complete "")
	(setq hargs:string-to-complete nil))))

(defun hargs:unset-string-to-complete ()
  "Remove any value from `hargs:string-to-complete'."
  (setq hargs:string-to-complete nil))

(defun hargs:sexpression-p (&optional no-recurse)
  "Returns an sexpression at point as a string.
If point follows an sexpression end character, the preceding sexpression
is returned.  If point precedes an sexpression start character, the
following sexpression is returned.  Otherwise, the innermost sexpression
that point is within is returned or nil if none."
  (save-excursion
    (condition-case ()
	(let ((not-quoted
	       '(not (and (eq (char-syntax (char-after (- (point) 2))) ?\\)
			  (not (eq (char-syntax (char-after (- (point) 3))) ?\\))))))
	  (cond ((and (eq (char-syntax (preceding-char)) ?\))
		      ;; Ignore quoted end chars.
		      (eval not-quoted))
		 (buffer-substring (point)
				   (progn (forward-sexp -1) (point))))
		((and (eq (char-syntax (following-char)) ?\()
		      ;; Ignore quoted begin chars.
		      (eval not-quoted))
		 (buffer-substring (point)
				   (progn (forward-sexp) (point))))
		(no-recurse nil)
		(t (save-excursion (up-list 1) (hargs:sexpression-p t)))))
      (error nil))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hargs:actype-get (actype &optional modifying)
  "Interactively gets and returns list of arguments for ACTYPE's parameters.
Current button is being modified when MODIFYING is non-nil."
  (hargs:action-get (actype:action actype) modifying))

(defun hargs:at-p (&optional no-default)
  "Returns thing at point, if of hargs:reading-p type, or default.
If optional argument NO-DEFAULT is non-nil, nil is returned instead of any
default values.

Caller should have checked whether an argument is presently being read
and has set `hargs:reading-p' to an appropriate argument type.
Handles all of the interactive argument types that `hargs:iform-read' does."
  (cond ((and (eq hargs:reading-p 'kcell)
	      (eq major-mode 'kotl-mode)
	      (not (looking-at "^$")))
	 (kcell-view:label))
	((and (eq hargs:reading-p 'klink)
	      (not (looking-at "^$")))
	 (if (eq major-mode 'kotl-mode)
	     (kcell-view:reference
	      nil (and (boundp 'default-dir) default-dir))
	   (let ((hargs:reading-p 'file))
	     (list (hargs:at-p)))))
	((eq hargs:reading-p 'kvspec)
	 (read-string "Koutline view spec: "
		      (if (boundp 'kvspec:current) kvspec:current)))
	((eolp) nil)
	((and (eq hargs:reading-p 'hmenu)
	      (eq (selected-window) (minibuffer-window)))
	   (char-to-string
	    (save-excursion
	      (cond ((save-excursion (search-forward ">" nil t))
		     ;; In menu prefix
		     1)
		    ((search-backward " " nil t)
		     ;; On a menu item
		     (skip-chars-forward " ")
		     (following-char))
		    ;; At the end of the menu
		    (t 0)))))
	((hargs:completion t))
	((eq hargs:reading-p 'ebut) (ebut:label-p 'as-label))
	((ebut:label-p) nil)
	((eq hargs:reading-p 'file)
	 (cond ((eq major-mode 'dired-mode)
		(let ((file (dired-get-filename nil t)))
		  (and file (hpath:absolute-to file))))
	       ((eq major-mode 'monkey-mode)
		(let ((file (monkey-filename t)))
		  (and file (hpath:absolute-to file))))
	       ;; Delimited file name.
	       ((hpath:at-p 'file))
	       ;; Unquoted remote file name.
	       ((hpath:is-p (hpath:remote-at-p) 'file))
	       ;; Possibly non-existent file name
	       ((if no-default (hpath:at-p 'file 'non-exist)))
	       (no-default nil)
	       ((buffer-file-name))
	       ))
	((eq hargs:reading-p 'directory)
	 (cond ((eq major-mode 'dired-mode)
		(let ((dir (dired-get-filename nil t)))
		  (and dir (setq dir (hpath:absolute-to dir))
		       (file-directory-p dir) dir)))
	       ((eq major-mode 'monkey-mode)
		(let ((dir (monkey-filename t)))
		  (and dir (setq dir (hpath:absolute-to dir))
		       (file-directory-p dir) dir)))
	       ;; Delimited directory name.
	       ((hpath:at-p 'directory))
	       ;; Unquoted remote directory name.
	       ((hpath:is-p (hpath:remote-at-p) 'directory))
	       ;; Possibly non-existent directory name
	       ((if no-default (hpath:at-p 'directory 'non-exist)))
	       (no-default nil)
	       (default-directory)
	       ))
	((eq hargs:reading-p 'string)
	 (or (hargs:delimited "\"" "\"") (hargs:delimited "'" "'")
	     (hargs:delimited "`" "'")
	     ))
	((or (eq hargs:reading-p 'actype)
	     (eq hargs:reading-p 'actypes))
	 (let ((name (hargs:find-tag-default)))
	   (car (set:member name (htype:names 'actypes)))))
	((or (eq hargs:reading-p 'ibtype)
	     (eq hargs:reading-p 'ibtypes))
	 (let ((name (hargs:find-tag-default)))
	   (car (set:member name (htype:names 'ibtypes)))))
	((eq hargs:reading-p 'sexpression) (hargs:sexpression-p))
	((memq hargs:reading-p '(Info-index-item Info-node))
	 (if (eq major-mode 'Info-mode)
	     (let ((file (Info-current-filename-sans-extension))
		   (node (cond ((Info-note-at-p))
			       ((Info-menu-item-at-p)
				(save-excursion
				  (beginning-of-line)
				  (forward-char 2)
				  (Info-extract-menu-node-name nil (Info-index-node))))
			       (t Info-current-node))))
	       (cond ((and (stringp node) (string-match "\\`\(" node))
		      node)
		     (file 
		      (concat "(" file ")" node))
		     (t node)))))
	((eq hargs:reading-p 'mail)
	 (and (hmail:reader-p) buffer-file-name
	      (prin1-to-string (list (rmail:msg-id-get) buffer-file-name))))
	((eq hargs:reading-p 'symbol)
	 (let ((sym (hargs:find-tag-default)))
	   (if (or (fboundp sym) (boundp sym)) sym)))
	((eq hargs:reading-p 'buffer)
	 (hargs:find-tag-default))
	((eq hargs:reading-p 'character)
	 (following-char))
	((eq hargs:reading-p 'key)
	 (require 'hib-kbd)
	 (let ((key-seq (hbut:label-p 'as-label "{" "}")))
	   (and key-seq (kbd-key:normalize key-seq))))
	((eq hargs:reading-p 'integer)
	 (save-excursion (skip-chars-backward "-0-9")
			 (if (looking-at "-?[0-9]+")
			     (read (current-buffer)))))))

(defun hargs:completion (&optional no-insert)
  "If in the completions buffer, return completion at point.  Also insert unless optional NO-INSERT is non-nil.
Insert in minibuffer if active or in other window if minibuffer is inactive."
  (interactive '(nil))
  (if (or (string-match "[* ]Completions\\*\\'" (buffer-name))
	  (eq major-mode 'completion-mode))
      (let ((opoint (point))
	    (owind (selected-window)))
	(if (re-search-backward "^\\|\t\\| [ \t]" nil t)
	    (let ((insert-window
		   (cond ((> (minibuffer-depth) 0)
			  (minibuffer-window))
			 ((not (eq (selected-window) (next-window nil)))
			  (next-window nil))))
		  (bury-completions)
		  (entry))
	      (skip-chars-forward " \t")
	      (if (and insert-window
		       ;; Allow single spaces in the middle of completions
		       ;; since completions always end with either a tab,
		       ;; newline or two whitespace characters.
		       (looking-at
			"[^ \t\n]+\\( [^ \t\n]+\\)*\\( [ \t\n]\\|[\t\n]\\|\\'\\)"))
		  (progn (setq entry (buffer-substring (match-beginning 0)
						       (match-beginning 2)))
			 (select-window insert-window)
			 (let ((str (or hargs:string-to-complete
					(buffer-substring
					 (point)
					 (save-excursion (beginning-of-line)
							 (point))))))
			   (cond
			    ((and (eq (selected-window) (minibuffer-window)))
			     (cond ((string-match (concat
						   (regexp-quote entry)
						   "\\'")
						  str)
				    ;; If entry matches tail of minibuffer
				    ;; prefix already, then return minibuffer
				    ;; contents as the entry.
				    (setq entry str))
				   ;;
				   ((string-match "[~/][^/]*\\'" str)
				    ;; file or directory entry
				    (setq entry
					  (concat
					   (substring
					    str 0
					    (1+ (match-beginning 0)))
					   entry))))
			     (or no-insert
				 (if entry (progn (erase-buffer)
						  (insert entry)))))
			    ;; In buffer, non-minibuffer completion.
			    ;; Only insert entry if last buffer line does
			    ;; not end in entry.
			    (no-insert)
			    ((or (string-match
				  (concat (regexp-quote entry) "\\'") str)
				 (null entry))
			     (setq bury-completions t))
			    (t (insert entry))))))
	      (select-window owind) (goto-char opoint)
	      (if bury-completions
		  (progn (bury-buffer nil) (delete-window)))
	      entry)))))

(defun hargs:iform-read (iform &optional modifying)
  "Reads action arguments according to IFORM, a list with car = 'interactive.
Optional MODIFYING non-nil indicates current button is being modified, so
button's current values should be presented as defaults.  Otherwise, uses
hargs:defaults as list of defaults, if any.
See also documentation for `interactive'."
  ;; This is mostly a translation of `call-interactively' to Lisp.
  ;;
  ;; Save this now, since use of minibuffer will clobber it.
  (setq prefix-arg current-prefix-arg)
  (if (not (and (listp iform) (eq (car iform) 'interactive)))
      (error
       "(hargs:iform-read): arg must be a list whose car = 'interactive.")
    (setq iform (car (cdr iform)))
    (if (or (null iform) (and (stringp iform) (equal iform "")))
	nil
      (let ((prev-reading-p hargs:reading-p))
	(unwind-protect
	    (progn
	      (setq hargs:reading-p t)
	      (if (not (stringp iform))
		  (let ((defaults (if modifying
				      (hattr:get 'hbut:current 'args)
				    (and (boundp 'hargs:defaults)
					 (listp hargs:defaults)
					 hargs:defaults)
				    )))
		    (eval iform))
		(let ((i 0) (start 0) (end (length iform))
		      (ientry) (results) (val) (default)
		      (defaults (if modifying
				    (hattr:get 'hbut:current 'args)
				  (and (boundp 'hargs:defaults)
				       (listp hargs:defaults)
				       hargs:defaults)
				  )))
		  ;;
		  ;; Handle special initial interactive string chars.
		  ;;
		  ;;   `*' means error if buffer is read-only.
		  ;;   Notion of when action cannot be performed due to
		  ;;   read-only buffer is view-specific, so here, we just
		  ;;   ignore a read-only specification since it is checked for
		  ;;   earlier by any ebut edit code.
		  ;;
		  ;;   `@' means select window of last mouse event.
		  ;;
		  ;;   `^' means activate/deactivate mark depending on invocation thru shift translation
		  ;;   See `this-command-keys-shift-translated' for somewhat of an explanation.
		  ;;
		  ;;   `_' means keep region in same state (active or inactive)
		  ;;   after this command.  (XEmacs only.)
		  ;;
		  (while (cond 
			  ((eq (aref iform i) ?*))
			  ((eq (aref iform i) ?@)
			   (hargs:select-event-window)
			   t)
			  ((eq (aref iform i) ?^)
			   (handle-shift-selection))
			  ((eq (aref iform i) ?_)
			   (setq zmacs-region-stays t)))
		    (setq i (1+ i) start i))
		  ;;
		  (while (and (< start end)
			      (string-match "\n\\|\\'" iform start))
		    (setq start (match-end 0)
			  ientry (substring iform i (match-beginning 0))
			  i start
			  default (car defaults)
			  default (if (or (null default) (stringp default))
				      default
				    (prin1-to-string default))
			  val (hargs:get ientry default (car results))
			  defaults (cdr defaults)
			  results (cond ((or (null val) (not (listp val)))
					 (cons val results))
					;; Is a list of args?
					((eq (car val) 'args)
					 (append (nreverse (cdr val)) results))
					(t ;; regular list value
					 (cons val results)))))
		  (nreverse results))))
	  (setq hargs:reading-p prev-reading-p))))))

(defun hargs:read (prompt &optional predicate default err val-type)
  "PROMPTs without completion for a value matching PREDICATE and returns it.
PREDICATE is an optional boolean function of one argument.  Optional DEFAULT
is a string to insert after PROMPT as the default return value.  Optional
ERR is a string to display temporarily when an invalid value is given.
Optional VAL-TYPE is a symbol indicating the type of value to be read.  If
VAL-TYPE equals `sexpression', then return that type; otherwise return the
string read or nil."
  (let ((bad-val) (val) (stringify)
	(prev-reading-p hargs:reading-p) (read-func)
	(owind (selected-window))
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (cond ((or (null val-type) (eq val-type 'sexpression))
		 (setq read-func 'read-minibuffer
		       hargs:reading-p 'sexpression))
		(t (setq read-func 'read-string hargs:reading-p val-type
			 stringify t)))
	  (while (progn (and default (not (stringp default))
			     (setq default (prin1-to-string default)))
			(condition-case ()
			    (or bad-val (setq val (funcall read-func prompt default)))
			  (error (setq bad-val t)))
			(if bad-val
			    t
			  (and stringify
			       ;; Remove any double quoting of strings.
			       (string-match "\\`\"\\([^\"]*\\)\"\\'" val) 
			       (setq val (match-string 1 val)))
			  (and predicate (not (funcall predicate val)))))
	    (if bad-val (setq bad-val nil) (setq default val))
	    (beep)
	    (if err (progn (message err) (sit-for 3))))
	  val)
      (setq hargs:reading-p prev-reading-p)
      (select-window owind)
      (switch-to-buffer obuf)
      )))

(defun hargs:read-match (prompt collection
			 &optional predicate must-match initial-input val-type)
  "PROMPTs with completion for a value in COLLECTION and returns it.
COLLECTION may be a list of strings, an alist, an obarray (for
symbol-name completion) or a hash collection.  COLLECTION may also be
a function to do the completion itself.  Optional PREDICATE
limits completion to a subset of COLLECTION.  Optional MUST-MATCH
means value returned must be from COLLECTION.  Optional INITIAL-INPUT
is a string inserted after PROMPT as the default value.  Optional
VAL-TYPE is a symbol indicating the type of value to be read."
  (unless (and must-match (null collection))
    (let ((prev-reading-p hargs:reading-p)
	  (completion-ignore-case t)
	  (owind (selected-window))
	  (obuf (current-buffer))
	  result)
      (unwind-protect
	  (progn
	    (setq hargs:reading-p (or val-type t)
		  result (completing-read prompt collection predicate must-match initial-input))
	    (if (and (equal result "") initial-input)
		initial-input
	      result))
	(setq hargs:reading-p prev-reading-p)
	(select-window owind)
	(switch-to-buffer obuf)))))

(defun hargs:select-p (&optional value assist-flag)
  "Returns optional VALUE or value selected at point if any, else nil.
If value is the same as the contents of the minibuffer, it is used as
the current minibuffer argument, otherwise, the minibuffer is erased
and value is inserted there.
Optional ASSIST-FLAG non-nil triggers display of Hyperbole menu item
help when appropriate."
    (if (and (> (minibuffer-depth) 0) (or value (setq value (hargs:at-p))))
	(let ((owind (selected-window)) (back-to)
	      (str-value (and value (format "%s" value)))
	      ;; This command requires recursive minibuffers.
	      (enable-recursive-minibuffers t))
	  (unwind-protect
	      (progn
		(select-window (minibuffer-window))
		(set-buffer (window-buffer (minibuffer-window)))
		(cond
		 ;;
		 ;; Selecting a menu item
		 ((eq hargs:reading-p 'hmenu)
		  (if assist-flag (setq hargs:reading-p 'hmenu-help))
		  (hui:menu-enter str-value))
		 ;;
		 ;; Enter existing value into the minibuffer as the desired parameter.
		 ((string-equal str-value (minibuffer-contents))
		  (exit-minibuffer))
		 ;;
		 ;; Clear minibuffer and insert value.
		 (t (delete-minibuffer-contents)
		    (insert str-value)
		    (setq back-to t)))
		value)
	    (if back-to (select-window owind))))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hargs:iforms nil
  "Alist of (interactive-cmd-chr . (argument-type . get-argument-form)) elts.")
(setq   hargs:iforms
	'(
	  ;; Get function symbol.
	  (?a . (symbol .
		 (intern (completing-read prompt obarray 'fboundp t default))))
	  ;; Get name of existing buffer.
	  (?b . (buffer .
		 (progn
		   (or default (setq default (other-buffer (current-buffer))))
		   (read-buffer prompt default t))))
	  ;; Get name of possibly nonexistent buffer.
	  (?B . (buffer .
		 (progn
		   (or default (setq default (other-buffer (current-buffer))))
		   (read-buffer prompt default nil))))
	  ;; Get character.
	  (?c . (character .
		 (progn (message
			 (if default
			     (hargs:prompt prompt
					   (if (integerp default)
					       (char-to-string default)
					     default)
					   "Curr:")
			   prompt))
			(char-to-string (read-char)))))
	  ;; Get symbol for interactive function, a command.
	  (?C . (symbol .
		 (intern
		  (completing-read prompt obarray 'commandp t default))))
	  ;; Get value of point; does not do I/O.
	  (?d . (integer . (point)))
	  ;; Get directory name.
	  (?D . (directory .
		 (progn
		   (or default (setq default default-directory))
		   (read-file-name prompt default default 'existing))))
	  ;; Get existing file name.
	  (?f . (file .
		 (read-file-name prompt default default
				 (if (eq system-type 'vax-vms)
				     nil 'existing))))
	  ;; Get possibly nonexistent file name.
	  (?F . (file . (read-file-name prompt default default nil)))
	  ;; Get key sequence.
	  (?k . (key .
		 (key-description (read-key-sequence
				   (if default
				       (hargs:prompt prompt default "Curr:")
				     prompt)))))
	  ;; Get key sequence without converting uppercase or shifted
	  ;; function keys to their unshifted equivalents.
	  (?K . (key .
		 (key-description (read-key-sequence
				   (if default
				       (hargs:prompt prompt default "Curr:")
				     prompt)
				   nil t))))
	  ;; Get value of mark.  Does not do I/O.
	  (?m . (integer . (marker-position (hypb:mark-marker t))))
	  ;; Get numeric prefix argument or a number from the minibuffer.
	  (?N . (integer .
		 (if prefix-arg
		     (prefix-numeric-value prefix-arg)
		   (let ((arg))
		     (while (not (integerp 
				  (setq arg (read-minibuffer prompt default))))
		       (beep))
		     arg))))
	  ;; Get number from minibuffer.
	  (?n . (integer .
		 (let ((arg))
		   (while (not (integerp
				(setq arg (read-minibuffer prompt default))))
		     (beep))
		   arg)))
	  ;; Get numeric prefix argument.  No I/O.
	  (?p . (prefix-arg .
		 (prefix-numeric-value prefix-arg)))
	  ;; Get prefix argument in raw form.  No I/O.
	  (?P . (prefix-arg . prefix-arg))
	  ;; Get region, point and mark as 2 args.  No I/O
	  (?r . (region .
		 (if (marker-position (hypb:mark-marker t))
		     (list 'args (min (point) (hypb:mark t))
			   (max (point) (hypb:mark t)))
		   (list 'args nil nil))))
	  ;; Get string.
	  (?s . (string . (read-string prompt default)))
	  ;; Get symbol.
	  (?S . (symbol .
		 (read-from-minibuffer
		  prompt default minibuffer-local-ns-map 'sym)))
	  ;; Get variable name: symbol that is user-variable-p.
	  (?v . (symbol . (read-variable
			   (if default
			       (hargs:prompt prompt default "Curr:")
			     prompt))))
	  ;; Get Lisp expression but don't evaluate.
	  (?x . (sexpression . (read-minibuffer prompt default)))
	  ;; Get Lisp expression and evaluate.
	  (?X . (sexpression . (eval-minibuffer prompt default)))
	  ))

(defvar hargs:iform-vector nil
  "Vector of forms for each interactive command character code.")
(setq   hargs:iform-vector (hargs:make-iform-vector hargs:iforms))

(defvar hargs:iforms-extensions nil
  "Hyperbole extension alist of (interactive-cmd-chr . (argument-type . get-argument-form)) elts.")
(setq   hargs:iforms-extensions
	'(
	  ;; Get existing Info node name, possibly prefixed with its (filename).
	  (?I . (Info-node . 
	         (let ((prev-reading-p hargs:reading-p))
		   (unwind-protect
		       (progn (require 'info)
			      (setq hargs:reading-p 'Info-node)
			      ;; Prevent empty completions list from
			      ;; triggering an error in Info-read-node-name.
			      (unless Info-current-file-completions
				(condition-case nil
				    (Info-build-node-completions)
				  (error (setq Info-current-file-completions '(("None"))))))
			      (Info-read-node-name prompt))
		     (setq hargs:reading-p prev-reading-p)))))
	  ;; Get kcell from koutline.
	  (?K . (kcell . (hargs:read-match
			  prompt
			  ;; Match to visible cell labels only
			  (kview:map-tree (lambda (view) (kcell-view:label)) kview t t)
			  nil t (kcell-view:visible-label) 'kcell)))
	  ;; Get kcell or path reference for use in a link.
	  (?L . (klink . (hargs:read prompt nil default nil 'klink)))
	  ;; Get existing mail msg date and file.
	  (?M . (mail . (progn
			  (while
			      (or (not (listp
					(setq default
					      (read-minibuffer
					       (hargs:prompt
						prompt ""
						"list of (date mail-file)")
					       default))))
				  (/= (length default) 2)
				  (not (and (stringp (car (cdr default)))
					    (file-exists-p
					     (car (cdr default))))))
			    (beep))
			  default)))
	  ;; Get a Koutline viewspec.
	  (?V . (kvspec . (hargs:read prompt nil nil nil 'kvspec)))
	  ;; Get existing Info index item name, possibly prefixed with its (filename).
	  (?X . (Info-index-item . 
	         (let ((prev-reading-p hargs:reading-p))
		   (unwind-protect
		       (let (file item)
			 (require 'info)
			 (setq hargs:reading-p 'Info-index-item
			       item (Info-read-index-item-name prompt))
			 (if (string-match "^(\\([^\)]+\\))\\(.*\\)" item)
			     item
			   (if (setq file (Info-current-filename-sans-extension))
			       (format "(%s)%s" file item)
			     item)))
		     (setq hargs:reading-p prev-reading-p)))))))

(defvar hargs:iform-extensions-vector nil
  "Vector of forms for each interactive command character code.")
(setq   hargs:iform-extensions-vector
	(hargs:make-iform-vector hargs:iforms-extensions))

(defvar hargs:string-to-complete nil
  "The string in the minibuffer the last time a completions buffer was generated, or nil.")

(provide 'hargs)

;;; hargs.el ends here
