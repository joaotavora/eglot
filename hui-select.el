;;; hui-select.el ---  Select larger and larger syntax-driven regions in a buffer.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Oct-96 at 02:25:27
;;
;; Copyright (C) 1996-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This is a radically cool, drop in mouse and keyboard-based library for
;;   selecting successively bigger syntactical regions within a buffer.
;;   Simply load this library and you are ready to try it out by
;;   double-clicking on various kinds of characters in different buffer major
;;   modes.  You'll quickly get the hang of it.  (It also provides a command
;;   to jump between beginning and end tags within HTML and SGML buffers.) 
;;   
;;   A great deal of smarts are built-in so that it does the right thing
;;   almost all of the time; many other attempts at similar behavior such as
;;   thing.el fail to deal with many file format complexities.
;;   
;;   Double clicks of the Selection Key (left mouse key) at the same point
;;   will select bigger and bigger regions with each successive use.  The
;;   first double click selects a region based upon the character at the
;;   point of the click.  For example, with the point over an opening or
;;   closing grouping character, such as { or }, the whole grouping is
;;   selected, e.g. a C function.  When on an _ or - within a programming
;;   language variable name, the whole name is selected.  The type of
;;   selection is displayed in the minibuffer as feedback.  When using a
;;   language based mainly on indenting, like Bourne shell, a double click on
;;   the first alpha character of a line, such as an if statement, selects
;;   the whole statement.
;;
;;   ---------------
;;
;;   This whole package is driven by a single function, available in mouse
;;   and keyboard forms, that first marks a region based on the syntax
;;   category of the character following point.  Successive invocations mark
;;   larger and larger regions until the whole buffer is marked.  See the
;;   documentation for the function, `hui-select-syntactical-region', for the
;;   kinds of syntax categories handled.
;;
;;   Running the command, `hui-select-initialize', will install the
;;   capabilities of this package and make double or triple clicks of
;;   the left mouse key select bigger and bigger syntactic regions.
;;   (See the documentation of the variable, `hui-select-thing-with-mouse',
;;   for how this is done).
;;
;;   The command, `hui-select-thing', may be bound to a key to provide the same
;;   syntax-driven region selection functionality. {C-c RETURN} is a
;;   reasonable site-wide choice since this key is seldom used and it
;;   mnemonically indicates marking something; Hyperbole typically
;;   binds this key for you.  {C-c s} may be preferred as a personal binding.
;;
;;   Use {C-g} to unmark the region when done.  Use,
;;   `hui-select-thing-with-mouse', if you want to bind this to a
;;   different mouse key to use single clicks instead of double clicks.
;;
;;   Four other commands are also provided:
;;    `hui-select-and-copy-thing' - mark and copy the syntactical unit to the
;;      kill ring 
;;    `hui-select-and-kill-thing' - kill the syntactical unit at point
;;    `hui-select-goto-matching-tag' - In HTML and SGML modes (actually any
;;      listed in the variable, `hui-select-markup-modes'), move point to the
;;      start of the tag paired with the closest tag that point is within or
;;      which it precedes, so you can quickly jump back and forth between
;;      open and close tags.  As noted, in html-mode this is bound to {C-c .}
;;    `hui-select-at-p' - predicate to test if a buffer position (or point) starts
;;      or ends a matching syntactical region (excluding a single character).
;;
;;   ---------------
;;   SETUP IF USED SEPARATELY FROM HYPERBOLE (otherwise ignore):
;;
;;   To autoload this package under Emacs, XEmacs or InfoDock via
;;   mouse usage, add the following line to one of your initialization
;;   files.
;;
;;      (hui-select-initialize)
;;
;;   This will autoload this package, bind the selection command to a double
;;   click of the left mouse button and set up Java, C++ and HTML modes
;;   for proper entity selection.
;;
;;   For any version of Emacs you should add the following autoload entries
;;   at your site if the auto-autoloads.el file in this directory will
;;   not be loaded:
;;
;;      (autoload 'hui-select-and-kill-thing
;;         "hui-select" "Kill syntactical region selection" t)
;;      (autoload 'hui-select-and-copy-thing
;;         "hui-select" "Select and copy syntactical region" t)
;;      (autoload 'hui-select-double-click-hook
;;         "hui-select" "Double mouse click syntactical region selection" nil)
;;      (autoload 'hui-select-thing
;;         "hui-select" "Keyboard-driven syntactical region selection" t)
;;      (autoload 'hui-select-thing-with-mouse
;;         "hui-select" "Single mouse click syntactical region selection" t)
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hvar)
(eval-when-compile
  (require 'sgml-mode) ;; for html-mode
  (require 'web-mode nil t))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hui-select-brace-modes
  '(c++-mode c-mode java-mode objc-mode perl-mode tcl-mode)
  "*List of language major modes which define things with brace delimiters."
  :type '(repeat (function :tag "Mode"))
  :group 'hyperbole-commands)

(defcustom hui-select-markup-modes
  '(html-mode sgml-mode web-mode)
  "*List of markup language modes that use SGML-style <tag> </tag> pairs."
  :type '(repeat (function :tag "Mode"))
  :group 'hyperbole-commands)

(defcustom hui-select-text-modes
  '(fundamental-mode kotl-mode indented-text-mode Info-mode outline-mode text-mode)
  "*List of textual modes where paragraphs may be outdented or indented."
  :type '(repeat (function :tag "Mode"))
  :group 'hyperbole-commands)

(defcustom hui-select-indent-modes
  (append '(altmath-mode asm-mode csh-mode eiffel-mode ksh-mode
            math-mode miranda-mode python-mode pascal-mode sather-mode)
	  hui-select-text-modes)
  "*List of language major modes that use indentation mostly to define syntactic structure."
  :type '(repeat (function :tag "Mode"))
  :group 'hyperbole-commands)

(defcustom hui-select-ignore-quoted-sexp-modes
  '(debugger-mode emacs-lisp-mode lisp-mode lisp-interaction-mode slime-mode cider-mode)
  "*List of language major modes in which to ignore quoted sexpressions for syntactic matches."
  :type '(repeat (function :tag "Mode"))
  :group 'hyperbole-commands)

(defvar hui-select-indent-non-end-regexp-alist
  '((altmath-mode "[^ \t\n]\\|[ \t\n]*\n\\s-")
    (csh-mode    "\\(\\|then\\|elsif\\|else\\)[ \t]*$")
    (eiffel-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    (ksh-mode    "\\(\\|then\\|elif\\|else\\)[ \t]*$")
    (math-mode "[^ \t\n]\\|[ \t\n]*\n\\s-")
    (miranda-mode "[ \t>]*$")
    (pascal-mode "\\(\\|then\\|else\\)[ \t]*$")
    (python-mode "[ \t]*$")
    (sather-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    ;;
    (fundamental-mode "[^ \t\n*]")
    (kotl-mode "[^ \t\n*]")
    (indented-text-mode "[^ \t\n*]")
    (Info-mode "[^ \t\n]")
    (outline-mode "[^*]")
    (text-mode  "[^ \t\n*]")
    )
  "List of (major-mode . non-terminator-line-regexp) elements used to avoid early dropoff when marking indented code.")

(defvar hui-select-indent-end-regexp-alist
  '((altmath-mode "[ \t\n]*\n\\S-")
    (csh-mode "end\\|while")
    (eiffel-mode "end")
    (ksh-mode "\\(fi\\|esac\\|until\\|done\\)[ \t\n]")
    (math-mode "[ \t\n]*\n\\S-")
    (pascal-mode "end")
    (sather-mode "end")
    ;;
    (fundamental-mode "[ \t]*$")
    (indented-text-mode "[ \t]*$")
    (Info-mode "[ \t]*$")
    (text-mode  "[ \t]*$")
    )
  "List of (major-mode . terminator-line-regexp) elements used to include a final line when marking indented code.")

(defcustom hui-select-char-p nil
  "*If t, return single character boundaries when all else fails."
  :type 'boolean
  :group 'hyperbole-commands)

(defcustom hui-select-display-type t
  "*If t, display the thing selected with each mouse click."
  :type 'boolean
  :group 'hyperbole-commands)

(defcustom hui-select-whitespace t
  "*If t, groups of whitespace are considered as things."
  :type 'boolean
  :group 'hyperbole-commands)

(defvar hui-select-previous nil)
(defvar hui-select-prior-point nil)
(defvar hui-select-prior-buffer nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;
;; Commands
;;

;;;###autoload
(defun hui-select-at-p (&optional pos)
  "Return non-nil if the character after optional POS (or point) matches a syntax entry in `hui-select-syntax-alist'.
The non-nil value returned is the function to call to select that syntactic unit."
  (interactive "d")
  (unless (and (bobp) (eobp))
    (or (numberp pos) (setq pos (point)))
    (setq hui-select-previous 'char)
    (let* ((syntax (char-syntax (or (char-after pos) (char-before pos))))
	   (pair (assq syntax hui-select-syntax-alist)))
      (and pair (or hui-select-whitespace (not (eq (cdr pair) 'thing-whitespace)))
	   ;; Ignore matches that are preceded by '\' as a quote, e.g. ?\'
	   (or (not (char-after pos))
	       (= pos (point-min))
	       (and (char-before pos) (/= ?\\ (char-before pos))))
	   (cdr pair)))))

;;;###autoload
(defun hui-select-goto-matching-delimiter ()
  "Jump back and forth between the start and end delimiters of a thing."
  (interactive)
  (cond ((memq major-mode hui-select-markup-modes)
	 (hui-select-goto-matching-tag))
	((and (preceding-char) (or (= ?\) (char-syntax (preceding-char)))
				   (= ?\" (preceding-char))))
	 (backward-sexp))
	((and (following-char) (or (= ?\( (char-syntax (following-char)))
				   (= ?\" (following-char))))
	 (forward-sexp))))

;;;###autoload
(defun hui-select-initialize ()
  "Initialize the hui-select mode on a double click of the left mouse key.
Also, add language-specific syntax setups to aid in thing selection."
  (interactive)
  (unless (boundp 'hyperbole-loading)
    (require 'hyperbole))
  (if hkey-init
      (cond (hyperb:emacs-p
	     (transient-mark-mode 1)
	     (hkey-global-set-key [double-down-mouse-1] nil)
	     (hkey-global-set-key [double-mouse-1] 'hui-select-thing-with-mouse)
	     (hkey-global-set-key [triple-down-mouse-1] nil)
	     (hkey-global-set-key [triple-mouse-1] 'hui-select-thing-with-mouse))
	    ((featurep 'xemacs)
	     (add-hook 'mouse-track-click-hook #'hui-select-double-click-hook))))
  ;;
  ;; These hooks let you select C++ and Java methods and classes by
  ;; double-clicking on the first character of a definition or on its
  ;; opening or closing brace.  This is all necessary since some
  ;; programmers don't put their function braces in the first column.
  (var:add-and-run-hook 'java-mode-hook (lambda ()
					  (setq defun-prompt-regexp
						"^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*")))
  (var:add-and-run-hook 'c++-mode-hook (lambda ()
					 (setq defun-prompt-regexp
					       "^[ \t]*\\(template\\s-*<[^>;.{}]+>\\s-*\\)?\\(\\(\\(auto\\|const\\|explicit\\|extern\\s-+\"[^\"]+\"\\|extern\\|friend\\|inline\\|mutable\\|overload\\|register\\|static\\|typedef\\|virtual\\)\\s-+\\)*\\(\\([[<a-zA-Z][]_a-zA-Z0-9]*\\(::[]_a-zA-Z0-9]+\\)?\\s-*<[_<>a-zA-Z0-9 ,]+>\\s-*[*&]*\\|[[<a-zA-Z][]_<>a-zA-Z0-9]*\\(::[[<a-zA-Z][]_<>a-zA-Z0-9]+\\)?\\s-*[*&]*\\)[*& \t\n\r]+\\)\\)?\\(\\(::\\|[[<a-zA-Z][]_a-zA-Z0-9]*\\s-*<[^>;{}]+>\\s-*[*&]*::\\|[[<a-zA-Z][]_~<>a-zA-Z0-9]*\\s-*[*&]*::\\)\\s-*\\)?\\(operator\\s-*[^ \t\n\r:;.,?~{}]+\\(\\s-*\\[\\]\\)?\\|[_~<a-zA-Z][^][ \t:;.,~{}()]*\\|[*&]?\\([_~<a-zA-Z][_a-zA-Z0-9]*\\s-*<[^>;{}]+[ \t\n\r>]*>\\|[_~<a-zA-Z][_~<>a-zA-Z0-9]*\\)\\)\\s-*\\(([^{;]*)\\(\\(\\s-+const\\|\\s-+mutable\\)?\\(\\s-*[=:][^;{]+\\)?\\)?\\)\\s-*")))
  ;;
  ;; Match to Lisp symbols with : in their names, often included in help buffers.
  (var:add-and-run-hook 'help-mode-hook (lambda () (modify-syntax-entry ?:  "_"  help-mode-syntax-table)))
  ;;
  ;; Allow for marking and moving brace delimited groups.
  (var:add-and-run-hook 'lisp-mode-hook
			(lambda ()
			  (modify-syntax-entry ?\{ "\(\}" lisp-mode-syntax-table)
			  (modify-syntax-entry ?\} "\)\{" lisp-mode-syntax-table)))
  ;;
  ;; This hook makes tags, comments, sentences and text blocks
  ;; selectable in HTML mode.  (You would do something similar for
  ;; SGML mode).
  ;;
  ;; Make tag begin and end delimiters act like grouping characters,
  ;; for easy syntactical selection of tags.
  (var:add-and-run-hook 'html-mode-hook
			(lambda ()
			  (modify-syntax-entry ?<  "(>" html-mode-syntax-table)
			  (modify-syntax-entry ?>  ")<" html-mode-syntax-table)
			  (modify-syntax-entry ?\{ "\(\}" html-mode-syntax-table)
			  (modify-syntax-entry ?\} "\)\{" html-mode-syntax-table)
			  (modify-syntax-entry ?\" "\"" html-mode-syntax-table)
			  (modify-syntax-entry ?=  "."  html-mode-syntax-table)
			  (modify-syntax-entry ?.  "_"  html-mode-syntax-table)
			  (make-local-variable 'comment-start)
			  (make-local-variable 'comment-end)
			  (setq comment-start "<!--" comment-end "-->")
			  (make-local-variable 'sentence-end)
			  (setq sentence-end "\\([^ \t\n\r>]<\\|>\\(<[^>]*>\\)*\\|[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)\\)[ \t\n]*")
			  (define-key html-mode-map "\C-c." 'hui-select-goto-matching-tag)))
  (var:add-and-run-hook 'web-mode-hook
			(lambda ()
			  (modify-syntax-entry ?<  "(>" web-mode-syntax-table)
			  (modify-syntax-entry ?>  ")<" web-mode-syntax-table)
			  (modify-syntax-entry ?\{ "\(\}" web-mode-syntax-table)
			  (modify-syntax-entry ?\} "\)\{" web-mode-syntax-table)
			  (modify-syntax-entry ?\" "\"" web-mode-syntax-table)
			  (modify-syntax-entry ?=  "."  web-mode-syntax-table)
			  (modify-syntax-entry ?.  "_"  web-mode-syntax-table)
			  (make-local-variable 'sentence-end)
			  (setq sentence-end "\\([^ \t\n\r>]<\\|>\\(<[^>]*>\\)*\\|[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)\\)[ \t\n]*")
			  (define-key web-mode-map "\C-c." 'hui-select-goto-matching-tag))))

;;;###autoload
(defun hui-select-thing ()
  "Select a region based on the syntax of the thing at point.
If invoked repeatedly, this selects bigger and bigger things.
If `hui-select-display-type' is non-nil and this is called
interactively, the type of selection is displayed in the minibuffer."
  (interactive
   (cond ((and (fboundp 'use-region-p) (use-region-p))
	  nil)
	 ((and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
	  nil)
	 (t
	  ;; Reset selection based on the syntax of character at point.
	  (hui-select-reset)
	  nil)))
  (let ((region (hui-select-boundaries (point))))
    (unless region
      (when (eq hui-select-previous 'punctuation)
	(setq region (hui-select-word (point)))))
    (when region
      (goto-char (car region))
      (set-mark (cdr region))
      (if (fboundp 'activate-region) (activate-region))
      (if (and (boundp 'transient-mark-mode)
	       transient-mark-mode)
	  (setq mark-active t))
      (and (called-interactively-p 'interactive) hui-select-display-type
	   (message "%s" hui-select-previous))
      (run-hooks 'hui-select-thing-hook)
      t)))

;;;###autoload
(defun hui-select-thing-with-mouse (event)
  "Select a region based on the syntax of the character from a mouse click EVENT.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If `hui-select-display-type' is
non-nil and this is called interactively, the type of selection is
displayed in the minibuffer."
  (interactive "@e")
  (mouse-set-point event)
  (cond ((and (eq hui-select-prior-point (point))
	      (eq hui-select-prior-buffer (current-buffer)))
	 ;; Prior click was at the same point as before, so enlarge
	 ;; selection to the next bigger item.
	 (let ((select-active-regions t)) ;; Automatically copy active
	   ;; region to PRIMARY inter-program selection.
	   (when (hui-select-bigger-thing)
	     (and (called-interactively-p 'interactive) hui-select-display-type
		  (message "%s" hui-select-previous))))
	 t)
	(t (setq this-command 'mouse-start-selection)
	   (hui-select-reset)
	   (hui-select-thing-with-mouse event))))

;;;###autoload
(defun hui-select-goto-matching-tag ()
  "If in a major mode listed in `hui-select-markup-modes,' moves point to the start of the tag paired with the closest tag that point is within or precedes.
Returns t if point is moved, else nil.
Signals an error if no tag is found following point or if the closing tag
does not have a `>' terminator character."
  (interactive)
  (when (memq major-mode hui-select-markup-modes)
    (let ((result)
	  ;; Assume case of tag names is irrelevant.
	  (case-fold-search t)
	  (opoint (point))
	  (tag)
	  end-point
	  start-regexp
	  end-regexp)

      ;; Leave point at the start of the tag that point is within or that
      ;; follows point.
      (cond
       ;; Point is at the start of a tag.
       ((looking-at "<[^<> \t\n\r]"))
       ;; Point was within a tag.
       ((and (re-search-backward "[<>]" nil t)
	     (looking-at "<[^<> \t\n\r]")))
       ;; Move to following tag.
       ((and (re-search-forward "<" nil t)
	     (progn (backward-char 1)
		    (looking-at "<[^<> \t\n\r]"))))
       ;; No tag follows point.
       (t (error "(hui-select-goto-matching-tag): No tag found after point.")))

      (if (catch 'done
	    (cond
	     ;; Beginning of a tag pair
	     ((looking-at "<[^/][^<> \t\n\r]*")
	      (setq tag (match-string 0)
		    start-regexp (regexp-quote tag)
		    end-regexp   (concat "</" (substring start-regexp 1)))
	      ;; Skip over nested tags.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp))
		    match-point)
		(while (and (>= count 0)
			    (re-search-forward regexp nil t))
		  (setq match-point (match-beginning 0))
		  (if (/= (char-after (1+ (match-beginning 0))) ?/)
		      ;; Start tag
		      (setq count (1+ count))
		    ;; End tag
		    (setq end-point (point))
		    (if (or (not (re-search-forward "[<>]" nil t))
			    (= (preceding-char) ?<))
			;; No terminator character `>' for end tag
			(progn (setq result end-point)
			       (throw 'done nil)))
		    (setq count (1- count))
		    (when (zerop count)
		      (goto-char match-point)
		      (setq result t)
		      (throw 'done result))))))
	     ;;
	     ;; End of a tag pair
	     ((or (looking-at "</[^\> \t\n\r]+")
		  (and (skip-chars-backward "<")
		       (looking-at "</[^\> \t\n\r]+")))
	      (goto-char (match-end 0))
	      (setq tag (match-string 0)
		    end-regexp (regexp-quote tag)
		    start-regexp   (concat "<" (substring end-regexp 2)))
	      (setq end-point (point))
	      (when (or (not (re-search-forward "[<>]" nil t))
			(= (preceding-char) ?<))
		  ;; No terminator character `>' for end tag
		(setq result end-point)
		(throw 'done nil))
	      ;; Skip over nested tags.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp)))
		(while (and (>= count 0)
			    (re-search-backward regexp nil t))
		  (if (= (char-after (1+ (point))) ?/)
		      ;; End tag
		      (setq count (1+ count))
		    ;; Start tag
		    (setq count (1- count))
		    (when (zerop count)
		      (setq result t)
		      (throw 'done t))))))))
	  nil
	;; Didn't find matching tag.
	(goto-char opoint))

      (cond ((integerp result)
	     (goto-char result)
	     (error "(hui-select-goto-matching-tag): Add a terminator character for this end <tag>"))
	    ((null tag)
	     (error "(hui-select-goto-matching-tag): No <tag> following point"))
	    ((null result)
	     (when (called-interactively-p 'interactive)
	       (beep)
	       (message "(hui-select-goto-matching-tag): No matching tag for %s>" tag)
	       result))
	    (t result)))))

;;;###autoload
(defun hui-select-and-copy-thing ()
  "Copy the region surrounding the syntactical unit at point."
  (interactive)
  (let ((bounds (hui-select-boundaries (point))))
    (if bounds (copy-region-as-kill (car bounds) (cdr bounds)))))

;;;###autoload
(defun hui-select-and-kill-thing ()
  "Kill the region surrounding the syntactical unit at point."
  (interactive "*")
  (let ((bounds (hui-select-boundaries (point))))
    (if bounds (kill-region (car bounds) (cdr bounds)))))


;;
;; Functions
;;

(defun hui-select-boundaries (pos)
  "Return the (start . end) of a syntactically defined region based upon the last region selected or on position POS.
The character at POS is selected if no other thing is matched."
  (interactive)
  (setcar hui-select-old-region (car hui-select-region))
  (setcdr hui-select-old-region (cdr hui-select-region))
  (let ((prior-type hui-select-previous))
    (cond
     ((eq hui-select-previous 'char)
      (hui-select-syntactical-region pos))
     ((and (car hui-select-old-region)
	   (memq hui-select-previous
		 '(sexp sexp-start sexp-end sexp-up))
	   (hui-select-sexp-up pos)
	   (hui-select-region-bigger-p hui-select-old-region hui-select-region))
      hui-select-region)
     ;;
     ;; In the general case, we can't know ahead of time what the next
     ;; biggest type of thing to select is, so we test them all and choose
     ;; the best fit.  This means that dynamically, the order of type
     ;; selection will change based on the buffer context.
     (t (let ((min-region (1+ (- (point-max) (point-min))))
	      (result)
	      region region-size)
	  (mapc
	   (lambda (sym-func)
	     (setq region
		   (if (car (cdr sym-func))
		       (funcall (car (cdr sym-func)) pos)))
	     (if (and region (car region)
		      (hui-select-region-bigger-p
		       hui-select-old-region region)
		      (setq region-size
			    (- (cdr region) (car region)))
		      (< region-size min-region))
		 (setq min-region region-size
		       result 
		       (list
			;; The actual selection type is
			;; sometimes different than the one we
			;; originally tried, so recompute it here.
			(car (assq hui-select-previous
				   hui-select-bigger-alist))
			(car region) (cdr region)))))
	   hui-select-bigger-alist)
	  (if result
	      ;; Returns hui-select-region
	      (progn (setq hui-select-previous (car result))
		     (hui-select-set-region (nth 1 result) (nth 2 result)))
	    ;;
	    ;; Restore prior selection type since we failed to find a
	    ;; new one.
	    (setq hui-select-previous prior-type)
	    (beep)
	    (message
	     "(hui-select-boundaries): `%s' is the largest selectable region"
	     hui-select-previous)
	    nil))))))

;;;###autoload
(defun hui-select-double-click-hook (event click-count)
  "Select a region based on the syntax of the character wherever the mouse is double-clicked.
If the double-click occurs at the same point as the last double-click, select
the next larger syntactic structure.  If `hui-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer."
  (cond ((/= click-count 2)
	 ;; Return nil so any other hooks are performed.
	 nil)
	(t (hui-select-thing-with-mouse event))))

(defun hui-select-syntactical-region (pos)
  "Return the (start . end) of a syntactically defined region based upon the buffer position POS.
Uses `hui-select-syntax-alist' and the current buffer's syntax table to
determine syntax groups.

Typically:
 Open or close grouping character syntax marks an s-expression.
 Double quotes mark strings.
 The end of a line marks the line, including its trailing newline.
 Word syntax marks the current word.
 Symbol syntax (such as _) marks a symbol.
 Whitespace marks a span of whitespace.
 Comment start or end syntax marks the comment.
 Punctuation syntax marks the words on both sides of the punctuation.
 If `hui-select-char-p' is set non-nil, then as a fallback, the
 character at POS will be selected.

If an error occurs during syntax scanning, it returns nil."
  (interactive "d")
  (setq hui-select-previous 'char)
  (if (save-excursion (goto-char pos) (eolp))
      (hui-select-line pos)
    (let* ((syntax (char-syntax (or (char-after pos) (char-before pos))))
	   (pair (assq syntax hui-select-syntax-alist)))
      (cond ((and pair
		  (or hui-select-whitespace
		      (not (eq (cdr pair) 'hui-select-whitespace))))
	     (funcall (cdr pair) pos))
	    (hui-select-char-p
	     (setq hui-select-previous 'char)
	     (hui-select-set-region pos (1+ pos)))))))

(defun hui-select-at-delimited-thing-p ()
  "Returns non-nil if point is at a markup pair, list, array/vector, set, comment or string, else nil.
The non-nil value returned is the function to call to select that syntactic unit.

Ignores any match if on an Emacs button and instead returns nil."
  (unless (button-at (point))
    (setq hkey-value (hui-select-delimited-thing-call #'hui-select-at-p))
    (cond ((eq hkey-value 'hui-select-punctuation)
	   (if (hui-select-comment (point))
	       (setq hkey-value #'hui-select-comment)
	     ;; Else here used to be `hkey-value' but then we are returning a
	     ;; value for any punctuation character without knowing if
	     ;; it is part of a delimited thing.  Nil should be the
	     ;; right thing here.
	     nil))
	  (t hkey-value))))

(defun hui-select-delimited-thing ()
  "Selects a markup pair, list, array/vector, set, comment or string at point and returns t, else nil."
  (interactive)
  (prog1 (and (hui-select-delimited-thing-call #'hui-select-thing) t)
    ;; If selected region is followed by only whitespace and then a
    ;; newline, add the newline to the region.
    (if (use-region-p)
	(if (> (mark) (point))
	    (save-excursion
	      (goto-char (mark))
	      (skip-chars-forward " \t")
	      (and (char-after) (= ?\n (char-after))
		   (set-mark (1+ (point)))))
	  (if (looking-at "[ \t]*\n")
	      (goto-char (match-end 0)))))))

(defun hui-select-at-delimited-sexp-p ()
  (unless (eolp)
    (let ((syn-before (if (char-before) (char-syntax (char-before)) 0))
	  (syn-after  (if (char-after)  (char-syntax (char-after)) 0)))
      (or (and (/= syn-before ?\\) (or (= syn-after ?\() (= syn-after ?\))))
	  (and (= syn-before ?\)) (char-before (1- (point)))
	       (/= ?\\ (char-syntax (char-before (1- (point))))))))))

(defun hui-select-mark-delimited-sexp ()
  "If point is before or after an sexp and not at an end of line, then deactivate the mark and mark the sexp.
Return t if marked, nil otherwise.  If any error occurs such as
  unbalanced start and end sexp delimiters, ignore it, and return
  nil."
  (interactive)
  (let ((mark-sexp-func (lambda ()
			  (if (region-active-p) (deactivate-mark))
			  (mark-sexp) t)))
    (condition-case nil
	(let ((syn-after (char-syntax (char-after)))
	      syn-before)
	  (cond ((eq syn-after ?\()
		 (funcall mark-sexp-func))
		((eq syn-after ?\))
		 (forward-char 1)
		 (backward-sexp)
		 (funcall mark-sexp-func))
		((and (not (eolp))
		      (setq syn-before (char-syntax (char-before)))
		      (eq syn-before ?\)))
		 (backward-sexp)
		 (funcall mark-sexp-func))))
	  (error nil))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hui-select-at-blank-line-or-comment ()
  "Return non-nil if on a blank line or a comment start or end line.
Assumes point is before any non-whitespace character on the line."
  (let ((comment-end-p (and (stringp comment-end)
			    (not (string-equal comment-end "")))))
    (if (looking-at
	 (concat "\\s-*$\\|\\s-*\\(//\\|/\\*\\|.*\\*/"
		 (if comment-start
		     (concat
		      "\\|" (regexp-quote comment-start)))
		 (if comment-end-p
		     (concat
		      "\\|.*" (regexp-quote comment-end)))
		 "\\)"))
	(or (not (and comment-start comment-end-p))
	    ;; Ignore start and end of comments that
	    ;; follow non-commented text.
	    (not (looking-at
		  (format ".*\\S-.*%s.*%s"
			  (regexp-quote comment-start)
			  (regexp-quote comment-end))))))))

(defun hui-select-back-to-indentation ()
  "Move point to the first non-whitespace character on this line and return point.
This respects the current syntax table definition of whitespace, whereas
`back-to-indentation' does not.  This is relevant in literate programming and
mail and news reply modes."
  (goto-char (min (progn (end-of-line) (point))
		  (progn (beginning-of-line)
			 (skip-syntax-forward " ")
			 (point)))))

(defun hui-select-bigger-thing ()
  "Select a bigger object where point is."
  (prog1
      (call-interactively 'hui-select-thing)
    (setq this-command 'select-thing)))

(defun hui-select-delimited-thing-call (func)
  "Selects a markup pair, list, vector/array, set, comment or string at point and returns non-nil, else nil.
The non-nil value returned is the function to call to select that syntactic unit."
  (unless (and (memq major-mode hui-select-ignore-quoted-sexp-modes)
	       ;; Ignore quoted identifier sexpressions, like #'function
	       (char-after) (memq (char-after) '(?# ?\')))
      (let ((hui-select-char-p)
	    (hui-select-whitespace)
	    (hui-select-syntax-alist '((?\" . hui-select-string)
				       (?\( . hui-select-sexp-start)
				       (?\$ . hui-select-sexp-start)
				       (?\' . hui-select-sexp-start)
				       (?\) . hui-select-sexp-end)
				       (?\< . hui-select-comment)
				       ;; Punctuation needed to match
				       ;; multi-char comment delimiters
				       (?\. . hui-select-punctuation))))
	(hui-select-reset)
	(funcall func))))

(defun hui-select-region-bigger-p (old-region new-region)
  "Return t if OLD-REGION is smaller than NEW-REGION and NEW-REGION partially overlaps OLD-REGION, or if OLD-REGION is uninitialized."
  (if (null (car old-region))
      t
    (and (> (abs (- (cdr new-region) (car new-region)))
	    (abs (- (cdr old-region) (car old-region))))
	 ;; Ensure the two regions intersect.
	 (or (and (<= (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (>  (max (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region))))
	     (and (>  (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (<= (min (cdr new-region) (car new-region))
		      (max (cdr old-region) (car old-region))))))))

(defun hui-select-reset ()
  ;; Reset syntactic selection.
  (setq hui-select-prior-point (point)
	hui-select-prior-buffer (current-buffer)
	hui-select-previous 'char)
  (hui-select-set-region nil nil))

(defun hui-select-set-region (beginning end)
  "Set the cons cell held by the variable `hui-select-region' to (BEGINNING . END).
Return the updated cons cell."
  (setcar hui-select-region beginning)
  (setcdr hui-select-region end)
  (when (and (null beginning) (null end))
    (setcar hui-select-old-region nil)
    (setcdr hui-select-old-region nil))
  (if (and (not (eq hui-select-previous 'buffer))
	   (integerp beginning) (integerp end)
	   (= beginning (point-min)) (= end (point-max)))
      ;; If we selected the whole buffer, make sure that 'thing' type is 'buffer'.
      nil
    hui-select-region))

(defun hui-select-string-p (&optional start-delim end-delim)
  "Returns (start . end) of string whose first line point is within or immediately before.
Positions include delimiters.  String is delimited by double quotes unless
optional START-DELIM and END-DELIM (strings) are given.
Returns nil if not within a string."
  (let ((opoint (point))
	(count 0)
	bol start delim-regexp start-regexp end-regexp)
    (or start-delim (setq start-delim "\""))
    (or end-delim (setq end-delim "\""))
    ;; Special case for the empty string.
    (if (looking-at (concat (regexp-quote start-delim)
			    (regexp-quote end-delim)))
	(hui-select-set-region (point) (match-end 0))
      (setq start-regexp (concat "\\(^\\|[^\\]\\)\\("
				 (regexp-quote start-delim) "\\)")
	    end-regexp   (concat "[^\\]\\(" (regexp-quote end-delim) "\\)")
	    delim-regexp (concat start-regexp "\\|" end-regexp))
      (save-excursion
	(beginning-of-line)
	(setq bol (point))
	(while (re-search-forward delim-regexp opoint t)
	  (setq count (1+ count))
	  ;; This is so we don't miss the closing delimiter of an empty
	  ;; string.
	  (if (and (= (point) (1+ bol))
		   (looking-at (regexp-quote end-delim)))
	      (setq count (1+ count))
	    (if (bobp) nil (backward-char 1))))
	(goto-char opoint)
	;; If found an even # of starting and ending delimiters before
	;; opoint, then opoint is at the start of a string, where we want it.
	(if (zerop (mod count 2))
	    (if (bobp) nil (backward-char 1))
	  (re-search-backward start-regexp nil t))
	;; Point is now before the start of the string.
	(if (re-search-forward start-regexp nil t)
	    (progn
	      (setq start (match-beginning 2))
	      (if (re-search-forward end-regexp nil t)
		  (hui-select-set-region start (point)))))))))

;;;
;;; Code selections
;;;

(defun hui-select-brace-def-or-declaration (pos)
  "If POS is at the first character, opening brace or closing brace of a brace delimited language definition, return (start . end) region, else nil.
The major mode for each supported brace language must be included in the
list, hui-select-brace-modes."
  (interactive)
  (when (and (featurep 'cc-mode) (memq major-mode hui-select-brace-modes))
    (save-excursion
      (goto-char pos)
      (let ((at-def-brace
	     (or (looking-at "^\{") (looking-at "^\}")
		 ;; Handle stupid old C-style and new Java
		 ;; style of putting braces at the end of
		 ;; lines.
		 (and (= (following-char) ?\{)
		      (stringp defun-prompt-regexp)
		      (save-excursion
			(beginning-of-line)
			(looking-at defun-prompt-regexp)))
		 (and (= (following-char) ?\})
		      (stringp defun-prompt-regexp)
		      (condition-case ()
			  (progn
			    ;; Leave point at opening brace.
			    (goto-char
			     (scan-sexps (1+ (point)) -1))
			    ;; Test if these are defun braces.
			    (save-excursion
			      (beginning-of-line)
			      (looking-at defun-prompt-regexp)))
			(error nil)))))
	    eod)
	(if (or at-def-brace
		;; At the start of a definition:
		;; Must be at the first non-whitespace character in the line.
		(and (= (point) (save-excursion (hui-select-back-to-indentation)))
		     ;; Must be on an alpha or symbol-constituent character.
		     ;; Also allow ~ for C++ destructors. 
		     (looking-at "[a-zA-z~]\\|\\s_")
		     ;; Previous line, if any,  must be blank or a comment
		     ;; start or end or we must be looking at
		     ;; `defun-prompt-regexp' when at the beginning of the line.
		     (or (and (stringp defun-prompt-regexp)
			      (save-excursion
				(beginning-of-line)
				(looking-at defun-prompt-regexp)))
			 (save-excursion
			   (if (/= (forward-line -1) 0)
			       t
			     (hui-select-at-blank-line-or-comment))))))
	    (progn
	      (setq hui-select-previous 'brace-def-or-declaration)
	      ;; Handle declarations and definitions embedded within classes.
	      (if (and (eq (following-char) ?\{)
		       (/= (point) (save-excursion
				     (hui-select-back-to-indentation))))
		  (setq at-def-brace nil))
	      ;;
	      (if at-def-brace nil (beginning-of-line))
	      (if (and (not at-def-brace)
		       (stringp defun-prompt-regexp)
		       (or (looking-at defun-prompt-regexp)
			   ;; For Java classes mainly
			   (looking-at "[a-zA-Z_$. \t]+\\s-*\{")))
		  ;; Mark the declaration or definition
		  (hui-select-set-region
		   (point)
		   (progn (goto-char (match-end 0))
			  (if (eq (preceding-char) ?\{)
			      (backward-char 1))
			  (if (eq (following-char) ?\{)
			      (forward-list 1)
			    (search-forward ";" nil t))
			  (skip-chars-forward " \t")
			  (skip-chars-forward "\n")
			  (if (looking-at "^\\s-*$")
			      (forward-line 1))
			  (point)))
		;; Mark function definitions only
		(setq eod (save-excursion
			    (condition-case ()
				(progn
				  (if (and (eq major-mode 'java-mode)
					   (fboundp 'id-java-end-of-defun))
				      (id-java-end-of-defun)
				    (end-of-defun))
				  (if (looking-at "^\\s-*$")
				      (forward-line 1))
				  (point))
			      (error (point-max)))))
		(if (= (following-char) ?\})
		    ;; Leave point at opening brace.
		    (goto-char (scan-sexps (1+ (point)) -1)))
		(if (= (following-char) ?\{)
		    (progn
		      (while (and (zerop (forward-line -1))
				  (not (hui-select-at-blank-line-or-comment))))
		      (if (hui-select-at-blank-line-or-comment)
			  (forward-line 1))))
		;; Mark the whole definition
		(setq hui-select-previous 'brace-def-or-declaration)
		(hui-select-set-region (point) eod))))))))

(defun hui-select-indent-def (pos)
  "If POS is at the first alpha character on a line, return (start . end) region.

The major mode for each supported indented language must be included in the
list, hui-select-indent-modes."
  (interactive)
  (when (memq major-mode hui-select-indent-modes)
    (save-excursion
      (goto-char pos)
      (if (and 
	   ;; Use this function only if point is on the first non-blank
	   ;; character of a block, whatever a block is for the current
	   ;; mode.
	   (cond ((eq major-mode 'kotl-mode)
		  (and (looking-at "[1-9*]") (not (kview:valid-position-p))))
		 ((or (eq major-mode 'outline-mode) selective-display)
		  (save-excursion (beginning-of-line)
				  (looking-at outline-regexp)))
		 ;; After indent in any other mode, must be on an alpha 
		 ;; or symbol-constituent character.
		 (t (looking-at "[a-zA-z]\\|\\s_")))
	   ;; Must be at the first non-whitespace character in the line.
	   (= (point) (save-excursion (hui-select-back-to-indentation))))
	  (let* ((start-col (current-column))
		 (opoint (if (eq major-mode 'kotl-mode)
			     (progn (kotl-mode:to-valid-position) (point))
			   (beginning-of-line) (point))))
	    (while
		(and (zerop (forward-line 1))
		     (bolp)
		     (or (progn (hui-select-back-to-indentation)
				(> (current-column) start-col))
			 ;; If in a text mode, allow outdenting, otherwise
			 ;; only include special lines here indented to the
			 ;; same point as the original line.
			 (and (or (memq major-mode hui-select-text-modes)
				  (= (current-column) start-col))
			      (looking-at
			       (or (car (cdr
					 (assq
					  major-mode
					  hui-select-indent-non-end-regexp-alist)))
				   "\\'"))))))
	    (if (and (looking-at
		      (or (car (cdr (assq major-mode
					  hui-select-indent-end-regexp-alist)))
			  "\\'"))
		     (or (memq major-mode hui-select-text-modes)
			 (= (current-column) start-col)))
		(forward-line 1))
	    (beginning-of-line)
	    ;; Mark the whole definition
	    (setq hui-select-previous 'indent-def)
	    (hui-select-set-region opoint (point)))))))

(defun hui-select-symbol (pos)
  "Return (start . end) of a symbol at POS."
  (or (hui-select-markup-pair pos)
   ;; Test for indented def here since might be on an '*' representing
   ;; an outline entry, in which case we mark entries as indented blocks.
   (hui-select-indent-def pos)
   (save-excursion
     (if (memq (char-syntax (if (eobp) (preceding-char) (char-after pos)))
	       '(?w ?_))
	 (progn (setq hui-select-previous 'symbol)
		(condition-case ()
		    (let ((end (scan-sexps pos 1)))
		      (hui-select-set-region
		       (min pos (scan-sexps end -1)) end))
		  (error nil)))))))

(defun hui-select-sexp-start (pos)
  "Return (start . end) of sexp starting at POS."
  (or (hui-select-markup-pair pos)
      (hui-select-brace-def-or-declaration pos)
      (save-excursion
	(setq hui-select-previous 'sexp-start)
	(condition-case ()
	    (hui-select-set-region pos (scan-sexps pos 1))
	  (error nil)))))

(defun hui-select-sexp-end (pos)
  "Return (start . end) of sexp ending at POS."
  (or (hui-select-brace-def-or-declaration pos)
      (save-excursion
	(setq hui-select-previous 'sexp-end)
	(condition-case ()
	    (hui-select-set-region (scan-sexps (1+ pos) -1) (1+ pos))
	  (error nil)))))

(defun hui-select-sexp (pos)
  "Return (start . end) of the sexp that POS is within."
  (setq hui-select-previous 'sexp)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(hui-select-set-region (progn (backward-up-list 1) (point))
			       (progn (forward-list 1) (point)))
      (error nil))))

(defun hui-select-sexp-up (pos)
  "Return (start . end) of the sexp enclosing the selected area or nil."
  (setq hui-select-previous 'sexp-up)
  ;; Keep going up and backward in sexps.  This means that hui-select-sexp-up
  ;; can only be called after hui-select-sexp or after itself.
  (setq pos (or (car hui-select-region) pos))
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(hui-select-set-region (progn (backward-up-list 1) (point))
			      (progn (forward-list 1) (point)))
      (error nil))))

(defun hui-select-preprocessor-def (pos)
  "Return (start . end) of a preprocessor #definition starting at POS, if any.
The major mode for each language that uses # preprocessor notation must be
included in the list, hui-select-brace-modes."
  ;; Only applies in brace modes (strictly, this should apply in a subset
  ;; of brace modes, but doing it this way permits for configurability.  In
  ;; other modes, one doesn't have to use the function on a # symbol.
  (when (memq major-mode hui-select-brace-modes)
    (setq hui-select-previous 'preprocessor-def)
    (save-excursion
      (goto-char pos)
      (if (and (= (following-char) ?#)
	       ;; Must be at the first non-whitespace character in the line.
	       (= (point) (save-excursion (hui-select-back-to-indentation))))
	  (progn
	    ;; Skip past continuation lines that end with a backslash.
	    (while (and (looking-at ".*\\\\\\s-*$")
			(zerop (forward-line 1))))
	    (forward-line 1)
	    ;; Include one trailing blank line, if any.
	    (if (looking-at "^[ \t\n\r]*$") (forward-line 1))
	    (hui-select-set-region pos (point)))))))

;; Allow punctuation marks not followed by white-space to include
;; the previous and subsequent sexpression.  Useful in contexts such as
;; 'foo.bar'.
(defun hui-select-punctuation (pos)
  "Return (start . end) region including sexpressions before and after POS, when at a punctuation character."
  (or (hui-select-comment pos)
      (hui-select-preprocessor-def pos)
      (hui-select-brace-def-or-declaration pos) ;; Might be on a C++ destructor ~.
      (save-excursion
	(setq hui-select-previous 'punctuation)
	(goto-char (min (1+ pos) (point-max)))
	(cond ((and (char-after pos) (= ?\  (char-syntax (char-after pos))))
	       (hui-select-set-region pos (1+ pos)))
	      ((and (char-before pos) (= ?\  (char-syntax (char-before pos))))
	       (hui-select-set-region (1- pos) pos))
	      (t (goto-char pos)
		 (condition-case ()
		     (hui-select-set-region
		      (save-excursion (backward-sexp) (point))
		      (progn (forward-sexp) (point)))
		   (error nil)))))))

(defun hui-select-comment (pos)
  "Return rest of line from POS to newline."
  (setq hui-select-previous 'comment)
  (save-excursion
    (goto-char pos)
    (let ((start-regexp  (if (stringp comment-start)
			     (regexp-quote comment-start)))
	  (end-regexp    (if (stringp comment-end)
			     (regexp-quote comment-end)))
	  bolp)
      (cond
       ;; Beginning of a comment
       ((and (stringp comment-start)
	     (or (looking-at start-regexp)
		 (save-excursion
		   (and (skip-chars-backward comment-start)
			(looking-at start-regexp)))))
	(skip-chars-backward " \t")
	(setq bolp (bolp)
	      pos (point))
	(if (equal comment-end "")
	    (progn (end-of-line)
		   (hui-select-set-region pos (point)))
	  (if (stringp comment-end)
	      ;; Skip over nested comments.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp)))
		(catch 'done
		  (while (re-search-forward regexp nil t)
		    (if (string-equal (match-string 0) comment-start)
			(setq count (1+ count))
		      ;; End comment
		      (setq count (1- count))
		      (if (= count 0)
			  (progn
			    (if (looking-at "[ \t]*[\n\r]")
				;; Don't include final newline unless the
				;; comment is first thing on its line.
				(goto-char (if bolp (match-end 0)
					     (1- (match-end 0)))))
			    (throw 'done (hui-select-set-region
					  pos (point))))))))))))
       ;; End of a comment
       ((and (stringp comment-end)
	     (not (string-equal comment-end ""))
	     (or (looking-at end-regexp)
		 (and (skip-chars-backward comment-end)
		      (looking-at end-regexp))))
	(goto-char (match-end 0))
	(if (looking-at "[ \t]*[\n\r]")
	    (goto-char (match-end 0)))
	(setq pos (point))
	(skip-chars-forward " \t")
	;; Skip over nested comments.
	(let ((count 0)
	      (regexp (concat start-regexp "\\|" end-regexp)))
	  (catch 'done
	    (while (re-search-backward regexp nil t)
	      (if (string-equal (match-string 0) comment-end)
		  (setq count (1+ count))
		;; Begin comment
		(setq count (1- count))
		(if (= count 0)
		    (progn
		      (skip-chars-backward " \t")
		      ;; Don't include final newline unless the comment is
		      ;; first thing on its line.
		      (if (bolp) nil (setq pos (1- pos)))
		      (throw 'done (hui-select-set-region
				    (point) pos)))))))))))))

;;;
;;; Textual selections
;;;

(defun hui-select-word (pos)
  "Return (start . end) of word at POS."
  (or (hui-select-brace-def-or-declaration pos)
      (hui-select-indent-def pos)
      (progn (setq hui-select-previous 'word)
	     (save-excursion
	       (goto-char pos)
	       (forward-word 1)
	       (let ((end (point)))
		 (forward-word -1)
		 (hui-select-set-region (point) end))))))

(defun hui-select-string (pos)
  "Returns (start . end) of string at POS or nil.  Pos include delimiters.
Delimiters may be single, double or open and close quotes."
  (setq hui-select-previous 'string)
  (save-excursion
    (goto-char pos)
    (if (and (memq major-mode hui-select-markup-modes)
	     (/= (following-char) ?\")
	     (save-excursion
	       (and (re-search-backward "[<>]" nil t)
		    (= (following-char) ?>))))
	(progn (setq hui-select-previous 'text)
	       (search-backward ">" nil t)
	       (hui-select-set-region
		(1+ (point))
		(progn (if (search-forward "<" nil 'end)
			   (1- (point))
			 (point)))))
      (or (hui-select-string-p) (hui-select-string-p "'" "'")
	  (hui-select-string-p "`" "'")))))

(defun hui-select-sentence (pos)
  "Return (start . end) of the sentence at POS."
  (setq hui-select-previous 'sentence)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(hui-select-set-region (progn (backward-sentence) (point))
			      (progn (forward-sentence) (point)))
      (error nil))))

(defun hui-select-whitespace (pos)
  "Return (start . end) of all but one char of whitespace POS, unless 
there is only one character of whitespace or this is leading whitespace on
the line.  Then return all of it."
  (setq hui-select-previous 'whitespace)
  (save-excursion
    (goto-char pos)
    (if (= (following-char) ?\^L)
	(hui-select-page pos)
      (let ((end (progn (skip-chars-forward " \t") (point)))
	    (start (progn (skip-chars-backward " \t") (point))))
	(if (looking-at "[ \t]")
	    (if (or (bolp) (= (1+ start) end))
		(hui-select-set-region start end)
	      (hui-select-set-region (1+ start) end)))))))

(defun hui-select-markup-pair (pos)
  "Return (start . end) of region between the opening and closing of an HTML or SGML tag pair, one of which is at POS.
The major mode for each language that uses such tags must be included in the
list, hui-select-markup-modes."
  (when (memq major-mode hui-select-markup-modes)
    (setq hui-select-previous 'markup-pair)
    (let ((pos-with-space)
	  ;; Assume case of tag names is irrelevant.
	  (case-fold-search t)
	  (result)
	  start-regexp
	  end-regexp
	  bolp
	  opoint)
      (save-excursion
	(catch 'done
	  (goto-char pos)
	  (cond
	   ;; Beginning of a tag pair
	   ((looking-at "<[^/][^<> \t\n\r]*")
	    (setq start-regexp (regexp-quote (match-string 0))
		  end-regexp   (concat "</" (substring start-regexp 1)))
	    (setq pos (point))
	    (skip-chars-backward " \t")
	    (setq bolp (bolp)
		  pos-with-space (point))
	    ;; Skip over nested tags.
	    (let ((count 0)
		  (regexp (concat start-regexp "\\|" end-regexp)))
	      (while (and (>= count 0)
			  (re-search-forward regexp nil t))
		(if (/= (char-after (1+ (match-beginning 0))) ?/)
		    ;; Start tag
		    (setq count (1+ count))
		  ;; Move past end tag terminator
		  (setq opoint (point))
		  (if (or (not (re-search-forward "[<>]" nil t))
			  (= (preceding-char) ?<))
		      (progn (setq result opoint)
			     (throw 'done nil)))
		  (setq count (1- count))
		  (if (= count 0)
		      (progn
			(if (looking-at "[ \t]*[\n\r]")
			    ;; Don't include final newline unless the
			    ;; start tag was the first thing on its line.
			    (if bolp
				(progn (goto-char (match-end 0))
				       ;; Include leading space since the
				       ;; start and end tags begin and end
				       ;; lines.
				       (setq pos pos-with-space))
			      (goto-char (1- (match-end 0)))))
			(setq result (hui-select-set-region pos (point)))
			(throw 'done nil)))))))
	   ;;
	   ;; End of a tag pair
	   ((or (looking-at "</[^\> \t\n\r]+")
		(and (skip-chars-backward "\<")
		     (looking-at "</[^\> \t\n\r]+")))
	    (goto-char (match-end 0))
	    (setq end-regexp (regexp-quote (match-string 0))
		  start-regexp   (concat "\<" (substring end-regexp 2)))
	    (setq opoint (point))
	    (if (or (not (re-search-forward "[<>]" nil t))
		    (= (preceding-char) ?<))
		(progn (setq result opoint)
		       (throw 'done nil)))
	    (setq pos (point))
	    (if (looking-at "[ \t]*[\n\r]")
		(setq pos-with-space (match-end 0)))
	    ;; Skip over nested tags.
	    (let ((count 0)
		  (regexp (concat start-regexp "\\|" end-regexp)))
	      (while (and (>= count 0)
			  (re-search-backward regexp nil t))
		(if (= (char-after (1+ (point))) ?/)
		    ;; End tag
		    (setq count (1+ count))
		  ;; Start tag
		  (setq count (1- count))
		  (if (= count 0)
		      (progn
			(if pos-with-space
			    ;; Newline found after original end tag.
			    (progn 
			      (skip-chars-backward " \t")
			      (if (bolp)
				  ;; Don't include final newline unless the
				  ;; start tag is the first thing on its line.
				  (setq pos pos-with-space)
				(setq pos (1- pos-with-space))
				;; Don't include non-leading space.
				(skip-chars-forward " \t"))))
			(setq result (hui-select-set-region (point) pos))
			(throw 'done nil))))))))))
      (if (integerp result)
	  (progn (goto-char result)
		 (error "(hui-select-markup-pair): Add a terminator character for this end tag"))
	result))))

;;;
;;; Document selections
;;;

(defun hui-select-line (pos)
  "Return (start . end) of the whole line POS is in, with newline unless at end of buffer."
  (setq hui-select-previous 'line)
  (save-excursion
    (goto-char pos)
    (let* ((start (progn (beginning-of-line 1) (point)))
	   (end (progn (forward-line 1) (point))))
      (hui-select-set-region start end))))

(defun hui-select-paragraph (pos)
  "Return (start . end) of the paragraph at POS."
  (setq hui-select-previous 'paragraph)
  (save-excursion 
    (goto-char pos)
    (if (looking-at paragraph-start) (forward-paragraph))
    (hui-select-set-region (progn (backward-paragraph) (point))
			  (progn (forward-paragraph) (point)))))

(defun hui-select-page (pos)
  "Return (start . end) of the page preceding POS."
  (setq hui-select-previous 'page)
  (save-excursion 
    (goto-char pos)
    (hui-select-set-region (progn (backward-page) (point))
			  (progn (forward-page) (point)))))

(defun hui-select-buffer (pos)
  "Return (start . end) of the buffer at POS."
  (setq hui-select-previous 'buffer)
  (hui-select-set-region (point-min) (point-max)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hui-select-syntax-table (make-syntax-table emacs-lisp-mode-syntax-table)
  "Syntax table to use when selecting delimited things.")
;; Make braces be thing delimiters, not punctuation.
(modify-syntax-entry ?\{ "\(\}" hui-select-syntax-table)
(modify-syntax-entry ?\} "\)\{" hui-select-syntax-table)

(defvar hui-select-bigger-alist
  '((char nil)
    (whitespace hui-select-whitespace)
    (word hui-select-word)
    (symbol hui-select-symbol)
    (punctuation nil)
    (string hui-select-string)
    (text nil)
    (comment hui-select-comment)
    (markup-pair nil)
    (preprocessor-def nil)
    (sexp hui-select-sexp)
    (sexp-start nil)
    (sexp-end nil)
    (sexp-up hui-select-sexp-up)
    (line hui-select-line)
    (sentence hui-select-sentence)
    (brace-def-or-declaration hui-select-brace-def-or-declaration)
    (indent-def hui-select-indent-def)
    (paragraph hui-select-paragraph)
    (page hui-select-page)
    (buffer hui-select-buffer)
    )
  "Unordered list of (<region-type-symbol> <region-selection-function>) pairs.
Used to go from one thing to a bigger thing.  See `hui-select-bigger-thing'.
Nil value for <region-selection-function> means that region type is skipped
over when trying to grow the region and is only used when a selection is made
with point on a character that triggers that type of selection.")

(defvar hui-select-prior-buffer nil)
(defvar hui-select-prior-point nil)

(defvar hui-select-previous 'char
  "Most recent type of selection.  Must be set by all hui-select functions.")

(defvar hui-select-region (cons nil nil)
  "Cons cell that contains a region (<beginning> . <end>).
The function `hui-select-set-region' updates and returns it.")

(defvar hui-select-old-region (cons nil nil)
  "Cons cell that contains a region (<beginning> . <end>).")

(defcustom hui-select-syntax-alist
  '((?w  . hui-select-word)
    (?_  . hui-select-symbol)
    (?\" . hui-select-string)
    (?\( . hui-select-sexp-start)
    (?\$ . hui-select-sexp-start)
    (?\' . hui-select-sexp-start)
    (?\) . hui-select-sexp-end)
    (?   . hui-select-whitespace)
    (?\< . hui-select-comment)
    (?\. . hui-select-punctuation))
  "*Unordered list of pairs of the form (<syntax-char> <function>) used by the function `hui-select-syntactical-region'.
Each <function> takes a single position argument and returns a
region (start . end) defining the boundaries of the thing at that position."
  :type '(repeat (cons (character :tag "Syntax-Char") function))
  :group 'hyperbole-commands)


(provide 'hui-select)

;;; hui-select.el ends here
