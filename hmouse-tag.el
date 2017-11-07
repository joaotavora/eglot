;;; hmouse-tag.el --- Smart Key support of programming language constructs
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    24-Aug-91
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

(eval-and-compile
  (mapc #'require '(find-func hpath hui-select))
  (cond ((or (featurep 'etags) (featurep 'tags))
	 nil)
	((or (featurep 'xemacs) hyperb:emacs-p)
	 ;; Force use of .elc file here since otherwise the bin/etags
	 ;; executable might be found in a user's load-path by the load
	 ;; command.
	 (or (load "etags.elc" t nil t)
	     (load "tags-fix" t)))
	((load "tags" t))))

;; If etags utilizes the new xref.el library, define some helper
;; functions to simplify programming.
(when (and (featurep 'xref) (not (fboundp 'xref-definition)))
  (defun xref-definition (identifier)
    "Return the first definition of string IDENTIFIER."
    (car (xref-backend-definitions (xref-find-backend) identifier)))
  (defun xref-definitions (identifier)
    "Return a list of all definitions of string IDENTIFIER."
    (xref-backend-definitions (xref-find-backend) identifier))
  (defun xref-item-buffer (item)
    "Return the buffer in which xref ITEM is defined."
    (marker-buffer (save-excursion (xref-location-marker (xref-item-location item)))))
  (defun xref-item-position (item)
    "Return the buffer position where xref ITEM is defined."
    (marker-position (save-excursion (xref-location-marker (xref-item-location item))))))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom smart-asm-include-path nil
  "*Ordered list of directories to search for assembly language include files.
Each directory must end with a directory separator."
  :type '(repeat directory)
  :group 'hyperbole-commands)
(define-obsolete-variable-alias 'smart-asm-include-dirs
  'smart-asm-include-path "06.00")

(defconst smart-asm-include-regexp
  "[ \t*#|;]*\\(include\\|lib\\)[ \t]+\\([^ \t\n\r]+\\)"
  "Regexp to match to assembly language include file lines.
Include keyword matched is grouping 1.  File name is grouping 2 but may be
missing its suffix, so add \".ins\" or \".inc\" if need be.
Examples include:
       INCLUDE GLOBALS
         should jump to file \"globals.ins\"
       lib conditionals_equ.inc
         should include \"conditionals_equ.inc\"")

(defcustom smart-c-cpp-include-path '("/usr/include/")
  "*Ordered list of include directories by default searched by C/C++ preprocessor.
Each directory must end with a directory separator.  See also
'smart-c-include-path'."
  :type '(repeat directory)
  :group 'hyperbole-commands)
(define-obsolete-variable-alias 'smart-c-cpp-include-dirs
  'smart-c-cpp-include-path "06.00")

(defcustom smart-c-include-path nil
  "*Ordered list of directories to search for C/C++ include files.
Each directory must end with a directory separator.  Directories normally
searched by the C/C++ pre-processor should be set instead in
`smart-c-cpp-include-path'."
  :type '(repeat directory)
  :group 'hyperbole-commands)
(define-obsolete-variable-alias 'smart-c-include-dirs
  'smart-c-include-path "06.00")

(defcustom smart-c-use-lib-man nil
  "When non-nil makes `smart-c' and `smart-c++' display man pages for recognized library symbols.
When nil, the default, `smart-c' and `smart-c++' look up only symbols defined in
an etags TAGS file.

Create the file ~/.CLIBS-LIST and populate it with the full pathnames (one per
line) of all of the C/C++ libraries whose symbols you want to match against.
Your MANPATH environment variable must include paths for the man pages of
these libraries also.

Your smart-clib-sym executable script included with Hyperbole must output a 1 if a
symbol is from a C/C++ library listed in ~/.CLIBS-LIST or 0 if not!  Otherwise,
don't set this variable to t."
  :type 'boolean
  :group 'hyperbole-commands)

(defconst smart-c-include-regexp
  "[ \t/*]*#[ \t]*\\(include\\|import\\)[ \t]+\\([\"<]\\)\\([^\">]+\\)[\">]"
  "Regexp to match to C, C++, or Objective-C include file lines.
Include keyword matched is grouping 1.  Type of include, user-specified via
double quote, or system-related starting with `<' is given by grouping 2.
File name is grouping 3.")

(defcustom smart-java-package-path
  (and (fboundp 'getenv) (getenv "JAVA_HOME")
       (list (expand-file-name "src/" (file-name-as-directory (getenv "JAVA_HOME")))))
  "*Ordered list of directories to search for imported Java packages.
Each directory must end with a directory separator."
  :type '(repeat directory)
  :group 'hyperbole-commands)
(define-obsolete-variable-alias 'smart-java-include-dirs
  'smart-java-include-path "06.00")

(defconst smart-java-package-regexp
  "[ \t/*]*\\(package\\|import\\)[ \t]+\\([^; \t\n\r\f]+\\)"
  "Regexp to match to Java `package' and `import' lines.
Keyword matched is grouping 1.  Referent is grouping 2.")

(defcustom smart-emacs-tags-file nil
  "*Full path name of etags file for InfoDock, XEmacs or GNU Emacs source."
  :type '(file :must-match t)
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun smart-asm (&optional identifier next)
  "Jumps to the definition of optional assembly IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching assembly tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on an include statement, the include file is displayed;
     Look for include file in directory list `smart-asm-include-path';
 (2) on an identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories."

  (interactive)
  (or (if identifier nil (smart-asm-include-file))
      (let ((tag (or identifier (smart-asm-at-tag-p t))))
	(message "Looking for `%s'..." tag)
	(condition-case ()
	    (progn
	      (smart-tags-display tag next)
	      (message "Found definition for `%s'" tag))
	  (error (message "`%s' not found in tag tables" tag)
		 (beep))))))

;;;###autoload
(defun smart-asm-at-tag-p (&optional no-flash)
  "Return assembly tag name that point is within, else nil."
  (let* ((identifier-chars "_.$a-zA-Z0-9")
	 (identifier (concat "[_.$a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (if no-flash
	      (buffer-substring-no-properties (point) (match-end 0))
	    (smart-flash-tag
	     (buffer-substring-no-properties (point) (match-end 0))
	     (point) (match-end 0)))))))

;;;###autoload
(defun smart-c++ (&optional identifier next)
  "Jumps to the definition of optional C++ IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C++ tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `c++-to-definition' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-path'
     and `smart-c-include-path';
 (2) on a C++ identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories;
 (3) if `smart-c-use-lib-man' is non-nil, the C++ identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (if (fboundp 'c++-to-definition)
      ;; Only fboundp if the OO-Browser has been loaded.
      (c++-to-definition t)
    (or (if identifier nil (smart-c-include-file))
	(smart-c++-tag identifier next))))

;;;###autoload
(defun smart-c++-tag (&optional identifier next)
  (let ((tag (or identifier (smart-c++-at-tag-p))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag)
	  t)
      (error
       (if (or (not smart-c-use-lib-man)
	       (not (file-readable-p "~/.CLIBS-LIST")))
	   (progn (message "`%s' not found in tag tables" tag)
		  (beep)
		  nil)
	 (message "Checking if `%s' is a C++ library function..." tag)
	 (if (smart-library-symbol tag)
	     (progn (message "Displaying C++ library man page for `%s'" tag)
		    (manual-entry tag)
		    t)
	   (message "`%s' not found in tag tables or C++ libraries" tag)
	   (beep)
	   nil))))))

(defconst smart-c++-keywords
  '("_pragma" "alignas" "alignof" "and" "and_eq" "asm" "atomic_cancel"
    "atomic_commit " "atomic_noexcept" "auto" "bitand" "bitor" "bool"
    "break" "case" "catch" "char" "char16_t" "char32_t" "class" "compl"
    "concept" "const" "const_cast" "constexpr" "continue" "decltype"
    "default" "define" "defined" "delete" "do" "double" "dynamic_cast"
    "elif" "else" "else" "endif" "enum" "error" "explicit" "export"
    "extern" "false" "final" "float" "for" "friend" "goto" "if" "if"
    "ifdef" "ifndef" "import" "include" "inline" "int" "line" "long"
    "module" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "override" "posix" "pragma"
    "private" "protected" "public" "register" "reinterpret_cast"
    "requires" "return" "short" "signed" "sizeof" "static" "static_assert"
    "static_cast" "std" "struct" "switch" "synchronized" "template" "this"
    "thread_local" "throw" "transaction_safe" "transaction_safe_dynamic"
    "true" "try" "typedef" "typeid" "typename" "undef" "union" "unsigned"
    "using" "virtual" "void" "volatile" "wchar_t" "while" "xor" "xor_eq"))

(defun smart-c++-at-tag-p ()
  "Return C++ tag name that point is within, else nil."
  (let* ((identifier-chars "_:~<>a-zA-Z0-9")
	 (identifier-regexp
	  (concat "\\([_~:<a-zA-Z][" identifier-chars "]*\\)"
		  "\\([ \t]*[^]\) \t:;.,?~{}][^[\( \t:;.,~^!|?{}]?[=*]?\\)?"))
	 op-identifier identifier)
    (save-excursion
      (skip-chars-backward identifier-chars)
      (when (and (looking-at identifier-regexp)
		 (not (member (downcase (match-string 0)) smart-c++-keywords)))
	  ;; Don't flash button here since it was already flashed before
	  ;; this was called.
	(setq op-identifier (buffer-substring-no-properties (match-beginning 0) (match-end 0))
	      identifier (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	;; Have to allow for identifiers such as, `operator () (int, int)'
	;; yet not include the opening parenthesis in `min ()'.
	(if (string-match "\\<operator\\>" op-identifier)
	    op-identifier
	  identifier)))))

(defun smart-c (&optional identifier next list-of-tags-tables)
  "Jumps to the definition of optional C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-path'
     and `smart-c-include-path';
 (2) on a C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories;
 (3) if `smart-c-use-lib-man' is non-nil, the C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  (or (if identifier nil (smart-c-include-file))
      (let ((tag (or identifier (smart-c-at-tag-p t))))
	(message "Looking for `%s'..." tag)
	(condition-case ()
	    (progn
	      (smart-tags-display tag next list-of-tags-tables)
	      (message "Found definition for `%s'" tag))
	  (error
	   (if (or (not smart-c-use-lib-man)
		   (not (file-readable-p "~/.CLIBS-LIST")))
	       (progn (message "`%s' not found in tag tables" tag)
		      (beep))
	     (message "Checking if `%s' is a C library function..." tag)
	     (if (smart-library-symbol tag)
		 (progn (message "Displaying C library man page for `%s'" tag)
			(manual-entry tag))
	       (message "`%s' not found in tag tables or C libraries" tag)
	       (beep))))))))

(defconst smart-c-keywords
  '("_generic" "_pragma" "alignas" "alignof" "asm" "atomic_bool"
    "atomic_char" "atomic_char16_t" "atomic_char32_t" "atomic_int"
    "atomic_int_fast16_t" "atomic_int_fast32_t" "atomic_int_fast64_t"
    "atomic_int_fast8_t" "atomic_int_least16_t" "atomic_int_least32_t"
    "atomic_int_least64_t" "atomic_int_least8_t" "atomic_intmax_t"
    "atomic_intptr_t" "atomic_llong" "atomic_long" "atomic_ptrdiff_t"
    "atomic_schar" "atomic_short" "atomic_size_t" "atomic_uchar"
    "atomic_uint" "atomic_uint_fast16_t" "atomic_uint_fast32_t"
    "atomic_uint_fast64_t" "atomic_uint_fast8_t" "atomic_uint_least16_t"
    "atomic_uint_least32_t" "atomic_uint_least64_t" "atomic_uint_least8_t"
    "atomic_uintmax_t" "atomic_uintptr_t" "atomic_ullong" "atomic_ulong"
    "atomic_ushort" "atomic_wchar_t" "auto" "bool" "break" "case" "char"
    "complex" "const" "continue" "default" "define" "defined" "do"
    "double" "elif" "else" "endif" "enum" "error" "extern" "float"
    "for" "fortran" "goto" "if" "ifdef" "ifndef" "imaginary"
    "include" "inline" "int" "line" "long" "noreturn" "pragma" "register"
    "restrict" "return" "short" "signed" "sizeof" "static" "static_assert"
    "struct" "switch" "thread_local" "typedef" "undef" "union" "unsigned"
    "void" "volatile" "while")
  "Sorted list of C keywords, all in lowercase.")

;;;###autoload
(defun smart-c-at-tag-p (&optional no-flash)
  "Return C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (and (looking-at identifier)
	       (not (member (downcase (match-string 0)) smart-c++-keywords)))
	  (if no-flash
	      (buffer-substring-no-properties (point) (match-end 0))
	    (smart-flash-tag
	     (buffer-substring-no-properties (point) (match-end 0))
	     (point) (match-end 0)))))))

;;;###autoload
(defun smart-cc-mode-initialize ()
  "Load and initialize cc-mode if possible and always return nil."
  (condition-case ()
      (progn (require 'cc-mode)
	     (c-initialize-cc-mode))
    (error nil))
  nil)

(defun smart-emacs-lisp-mode-p ()
  "Return t if in a mode which uses Emacs Lisp symbols."
  ;; Beyond Lisp files, Emacs Lisp symbols appear frequently in Byte-Compiled
  ;; buffers, debugger buffers, and Help buffers.
  (or (memq major-mode #'(emacs-lisp-mode lisp-interaction-mode debugger-mode))
      (string-match "\\`\\*Compile-Log\\(-Show\\)?\\*" (buffer-name))
      (and (or (eq major-mode #'help-mode)
	       (string-match "\\`\\*Help\\|Help\\*\\'" (buffer-name)))
	   (smart-lisp-at-known-identifier-p))))

(defun smart-fortran (&optional identifier next)
  "Jumps to the definition of optional Fortran IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Fortran tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If on a Fortran identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories."
  (interactive)
  (let ((tag (or identifier (smart-fortran-at-tag-p t))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag))
      (error
       (message "`%s' not found in tag tables" tag)
       (beep)))))

(defconst smart-fortran-keywords
  '("abstract" "all" "allocatable" "allocate" "assign" "associate"
    "asynchronous" "backspace" "bind" "block" "block" "call" "case"
    "class" "close" "codimension" "common" "concurrent" "contains"
    "contiguous" "continue" "critical" "cycle" "data" "data" "deallocate"
    "deferred" "dimension" "do" "elemental" "else" "else" "elsewhere"
    "end" "endfile" "endif" "entry" "enum" "enumerator" "equivalence"
    "error" "exit" "extends" "external" "final" "flush" "forall" "format"
    "function" "generic" "goto" "if" "if" "images" "implicit" "import"
    "include" "inquire" "intent" "interface" "intrinsic" "lock" "memory"
    "module" "namelist" "non_overridable" "nopass" "nullify" "only" "open"
    "operator" "optional" "parameter" "pass" "pause" "pointer" "print"
    "private" "procedure" "program" "protected" "public" "pure" "read"
    "recursive" "result" "return" "rewind" "rewrite" "save" "select"
    "sequence" "stop" "stop" "submodule" "subroutine" "sync" "target"
    "then" "unlock" "use" "value" "volatile" "wait" "where" "while"
    "write")
  "Sorted list of Fortran keywords, all in lowercase.")

;;;###autoload
(defun smart-fortran-at-tag-p (&optional no-flash)
  "Return Fortran tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier (concat "[_a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (looking-at identifier)
	  (if no-flash
	      (buffer-substring-no-properties (point) (match-end 0))
	    (smart-flash-tag
	     (buffer-substring-no-properties (point) (match-end 0))
	     (point) (match-end 0)))))))

;;;###autoload
(defun smart-java (&optional identifier next)
  "Jumps to the definition of optional Java IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Java tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-java-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-path';
 (3) on a Java identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories."

  (interactive)
  (if (fboundp 'java-to-definition)
      ;; Only fboundp if the OO-Browser has been loaded.
      (smart-java-oo-browser)
    (or (if identifier nil (or (smart-java-cross-reference) (smart-java-packages)))
	(smart-java-tag identifier next))))

;;;###autoload
(defun smart-java-tag (&optional identifier next)
  (let ((tag (or identifier (smart-java-at-tag-p t))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag))
      (error (progn (message "`%s' not found in tag tables" tag)
		    (beep))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-java-oo-browser (&optional junk)
  "Jumps to the definition of selected Java construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) within a commented @see cross-reference, the referent is displayed;
 (2) on a `package' or `import' statement, the referent is displayed;
     Look for referent files in the directory list `smart-java-package-path';
 (3) within a method declaration, its definition is displayed;
 (4) on a class name, the class definition is shown;
 (5) on a unique identifier reference, its definition is shown (when possible)."

  (interactive)
  (or (smart-java-cross-reference)
      (smart-java-packages)
      (java-to-definition t)))

(defconst smart-java-keywords
  '("abstract" "assert" "boolean" "break" "byte" "case" "catch" "char"
    "class" "const" "continue" "default" "do" "double" "else" "enum"
    "extends" "final" "finally" "float" "for" "goto" "if" "implements"
    "import" "instanceof" "int" "interface" "long" "native" "new"
    "package" "private" "protected" "public" "return" "short" "static"
    "strictfp" "super" "switch" "synchronized" "this" "throw" "throws"
    "transient" "try" "void" "volatile" "while")
  "Sorted list of Java keywords, all in lowercase.")

;;;###autoload
(defun smart-java-at-tag-p (&optional no-flash)
  "Return Java tag name that point is within, else nil."
  (let* ((identifier-chars "_$.a-zA-Z0-9")
	 (identifier
	  (concat "[_$a-zA-Z][" identifier-chars "]*")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (and (/= (preceding-char) ?@) (looking-at identifier)
	       (not (member (downcase (match-string 0)) smart-java-keywords)))
	  (if no-flash
	      (buffer-substring-no-properties (point) (match-end 0))
	    (smart-flash-tag
	     (buffer-substring-no-properties (point) (match-end 0))
	     (point) (match-end 0)))))))

;;;###autoload
(defun smart-javascript (&optional identifier next)
  "Jumps to the definition of optional JavaScript IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching JavaScript tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If on a JavaScript identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories."
  (interactive)
  (let ((tag (or identifier (smart-javascript-at-tag-p t))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag))
      (error
       (message "`%s' not found in tag tables" tag)
       (beep)))))

(defconst smart-javascript-keywords
  '("break" "case" "catch" "class" "const" "continue" "debugger"
    "default" "delete" "do" "else" "extends" "export" "false" "finally"
    "for" "function" "if" "in" "instanceof" "import" "let" "new" "null"
    "return" "super" "switch" "this" "throw" "true" "try" "typeof" "var"
    "void" "while" "with" "yield")
  "Sorted list of JavaScript keywords, all in lowercase.")

;;;###autoload
(defun smart-javascript-at-tag-p (&optional no-flash)
  "Return JavaScript tag name that point is within, else nil."
  (if (if (memq major-mode '(html-mode web-mode))
	  ;; Must be within a <script> tag or this predicate function
	  ;; fails (returns nil).
	  (save-excursion (if (re-search-backward "<\\(/?script\\)[\> \t\n]" nil t)
			      (string-equal (match-string 1) "script")))
	t)
      (let* ((identifier-chars "_$a-zA-Z0-9")
	     (identifier (concat "[_$a-zA-Z][" identifier-chars "]*")))
	(save-excursion
	  (skip-chars-backward identifier-chars)
	  (and (looking-at identifier)
	       (not (member (downcase (match-string 0)) smart-javascript-keywords))
	  (if no-flash
	      (buffer-substring-no-properties (point) (match-end 0))
	    (smart-flash-tag
	     (buffer-substring-no-properties (point) (match-end 0))
	     (point) (match-end 0))))))))

(defun smart-lisp (&optional show-doc)
  "Jumps to the definition of any selected Lisp identifier or optionally SHOW-DOC.
If on an Emacs Lisp require, load, or autoload clause and the
`find-library' function from the \"load-library\" package has
been loaded, this jumps to the library source whenever possible.

Otherwise, if a definition for the identifier is found within a TAGS
file in the current directory or any of its ancestor directories, this
jumps to the definition.

Optional SHOW-DOC flag means show documentation for the tag at point
rather than displaying its source code definition.

This command assumes that its caller has already checked that the key was
pressed in an appropriate buffer and has moved the cursor to the selected
buffer."

  (interactive)
  (unless (and (smart-emacs-lisp-mode-p) (fboundp 'find-library)
	       ;; Handle Emacs Lisp `require', `load', and `autoload' clauses.
	       (let ((opoint (point))
		     type
		     name
		     lib)
		 (setq lib (and (search-backward "\(" nil t)
				(or
				 ;; load with a filename
				 (looking-at "(\\(load\\)[ \t]+\\(\\)\"\\([^][() \t\n\r`'\"]+\\)")
				 ;; autoload or require with a name and filename
				 (looking-at "(\\(autoload\\|require\\)[ \t]+'\\([^][() \t\n\r`'\"]+\\)[ \t\n\r]+\"\\([^][() \t\n\r`'\"]+\\)")
				 ;; require without a separate filename
				 (looking-at "(\\(require\\)\\(\\)[ \t]+'\\([^][() \t\n\r`'\"]+\\)"))))
		 (goto-char opoint)
		 (when lib
		   (setq type (match-string-no-properties 1)
			 name (match-string-no-properties 2)
			 lib (match-string-no-properties 3))
		   (hpath:display-buffer (current-buffer))
		   (find-library lib)
		   (goto-char (point-min))
		   (when (equal type "autoload")
		     ;; Ignore defgroup matches
		     (if (re-search-forward
			  (format "^[; \t]*(def.[^r][^ \t\n\r]*[ \t]+%s[ \t\n\r]" (regexp-quote name))
			  nil t)
			 (goto-char (match-beginning 0))
		       (error "(smart-lisp): Found autoload library but no definition for `%s'" name)))
		   t)))
    (let* ((elisp-p (smart-emacs-lisp-mode-p))
	   (tag (smart-lisp-at-tag-p t))
	   (tag-sym (intern-soft tag)))
      (cond ((and show-doc elisp-p)
	     ;; Emacs Lisp function, variable and face documentation display.
	     (cond ((fboundp tag-sym) (describe-function tag-sym))
		   ((and tag-sym (boundp tag-sym)) (describe-variable tag-sym))
		   ((facep tag-sym) (describe-face tag-sym))
		   (t (error "(smart-lisp): `%s' unbound symbol definition not found" tag))))
	    ((and elisp-p (fboundp 'find-function-noselect)
		  (let ((result (smart-lisp-bound-symbol-def tag-sym)))
		    (when (cdr result)
		      (hpath:display-buffer (car result))
		      (goto-char (cdr result))
		      t))))
	    (t (condition-case ()
		   ;; Tag of any language
		   (and (featurep 'etags) (smart-tags-display tag show-doc))
		 (error (unless (and elisp-p (stringp smart-emacs-tags-file)
				     (condition-case ()
					 (smart-tags-display
					  tag show-doc (list smart-emacs-tags-file))
				       (error nil)))
			  (error "(smart-lisp): `%s' definition not found in any tag table" tag)))))))))

(defun smart-lisp-at-definition-p ()
    "Returns t when point is in a non-help buffer on the first line of a non-alias Lisp definition, else nil."
    (unless (derived-mode-p 'help-mode)
      (save-excursion
	(beginning-of-line)
	;; Exclude any define- lines.
	(and (looking-at "\\(;*[ \t]*\\)?(def[[:alnum:]]*[[:space:]]")
	     ;; Ignore alias definitions since those typically have symbol tags to lookup.
	     (not (looking-at "\\(;*[ \t]*\\)?(def[^ \t\n\r]*alias"))
	     ;; Ignore lines that start with (default
	     (not (looking-at "\\(;*[ \t]*\\)?(default"))))))

(defun smart-lisp-at-load-expression-p ()
    "Returns t when point is on the first line of a Lisp library load expression, else nil."
    (save-excursion
      (beginning-of-line)
      (looking-at "\\(;*[ \t]*\\)?(\\(autoload\\|load\\|require\\)")))

(defun smart-lisp-at-tag-p (&optional no-flash)
  "Returns Lisp tag name that point is within, else nil.
Returns nil when point is on the first line of a non-alias Lisp definition."
  (unless (smart-lisp-at-definition-p)
    (let* ((identifier-chars "-_:/*+=%$&?!<>a-zA-Z0-9~^")
	   (identifier (concat "[-<>*a-zA-Z][" identifier-chars "]*")))
      (save-excursion
	(skip-chars-backward identifier-chars)
	(if (and (looking-at identifier)
		 ;; Ignore any all punctuation matches.
		 (not (string-match "\\`[-<>*]+\\'" (match-string 0)))
		 ;; Needed to set match string.
		 (looking-at identifier))
	    (if no-flash
		(if (eq (char-after (1- (match-end 0))) ?:)
		    (buffer-substring-no-properties (point) (1- (match-end 0)))
		  (buffer-substring-no-properties (point) (match-end 0)))
	      (if (eq (char-after (1- (match-end 0))) ?:)
		  (smart-flash-tag
		   (buffer-substring-no-properties (point) (1- (match-end 0)))
		   (point) (1- (match-end 0)))
		(smart-flash-tag
		 (buffer-substring-no-properties (point) (match-end 0))
		 (point) (match-end 0)))))))))

;;;###autoload
(defun smart-lisp-mode-p ()
  "Return t if in a mode which uses Lisp symbols."
  (or (smart-emacs-lisp-mode-p)
      (memq major-mode '(lisp-mode scheme-mode))))

;;;###autoload
(defun smart-objc (&optional identifier next)
  "Jumps to the definition of optional Objective-C IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Objective-C tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-objc-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.
Otherwise:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `objc-cpp-include-path' and
     `objc-include-path';
 (2) on an Objective-C identifier, the identifier definition is displayed,
     assuming the identifier is found within an `etags' generated tag file
     in the current directory or any of its ancestor directories;
 (3) if `smart-c-use-lib-man' is non-nil, the Objective-C identifier is
     recognized as a library symbol, and a man page is found for the
     identifier, then the man page is displayed."

  (interactive)
  
  (if (fboundp 'objc-to-definition)
      ;; Only fboundp if the OO-Browser has been loaded.
      (smart-objc-oo-browser)
    (or (if identifier nil (smart-c-include-file))
	(smart-objc-tag identifier next))))

;;;###autoload
(defun smart-objc-tag (&optional identifier next)
  (let ((tag (or identifier (smart-objc-at-tag-p t))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag))
      (error
       (if (or (not smart-c-use-lib-man)
	       (not (file-readable-p "~/.CLIBS-LIST")))
	   (progn (message "`%s' not found in tag tables" tag)
		  (beep))
	 (message
	  "Checking if `%s' is an Objective-C library function..." tag)
	 (if (smart-library-symbol tag)
	     (progn
	       (message
		"Displaying Objective-C library man page for `%s'" tag)
	       (manual-entry tag))
	   (message "`%s' not found in tag tables or Objective-C libraries"
		    tag)
	   (beep)))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-objc-oo-browser (&optional junk)
  "Jumps to the definition of selected Objective-C construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a `#include' statement, the include file is displayed;
     Look for include file in directory lists `smart-c-cpp-include-path'
     and `smart-c-include-path';
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown;
 (4) on a global variable or function identifier, its definition is shown.

 (2) and (3) require that an OO-Browser Environment has been loaded with
     the {M-x br-env-load RET} command."

  (interactive)
  (objc-to-definition t))

(defconst smart-objc-keywords
  (append '("bycopy" "byref" "id" "in" "inout" "oneway" "out" "self" "super")
	  smart-c-keywords)
  "Sorted list of Objective-C keywords, all in lowercase.")

(defun smart-objc-at-tag-p (&optional no-flash)
  "Return Objective-C tag name that point is within, else nil."
  (let* ((identifier-chars "_a-zA-Z0-9")
	 (identifier
	  (concat "\\([-+][ \t]*\\)?\\([_a-zA-Z][" identifier-chars "]*\\)")))
    (save-excursion
      (skip-chars-backward identifier-chars)
      (if (and (/= (preceding-char) ?@) (looking-at identifier)
	       (not (member (downcase (match-string 0)) smart-objc-keywords)))
	  (if no-flash
	      (buffer-substring-no-properties (match-beginning 2) (match-end 2))
	    (smart-flash-tag
	     (buffer-substring-no-properties (match-beginning 2) (match-end 2))
	     (match-beginning 2) (match-end 2)))))))

;;;###autoload
(defun smart-python (&optional identifier next)
  "Jumps to the definition of optional Python IDENTIFIER or the one at point.
Optional second arg NEXT means jump to next matching Python tag.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

See the documentation for `smart-python-oo-browser' for the behavior of this
function when the OO-Browser has been loaded.

Otherwise, on a Python identifier, the identifier definition is displayed,
assuming the identifier is found within an `etags' generated tag file
in the current directory or any of its ancestor directories."
  (interactive)
  (cond ((fboundp 'python-to-definition)
	 ;; Only fboundp if the OO-Browser has been loaded.
	 (smart-python-oo-browser))
	(identifier
	 (smart-python-tag identifier next))))

;;;###autoload
(defun smart-python-tag (&optional identifier next)
  (let ((tag (or identifier (smart-python-at-tag-p t))))
    (message "Looking for `%s'..." tag)
    (condition-case ()
	(progn
	  (smart-tags-display tag next)
	  (message "Found definition for `%s'" tag))
      (error (progn (message "`%s' not found in tag tables" tag)
		    (beep))))))

;;; The following should be called only if the OO-Browser is available.
(defun smart-python-oo-browser (&optional junk)
  "Jumps to the definition of selected Python construct via OO-Browser support.
Optional JUNK is ignored.  Does nothing if the OO-Browser is not available.

It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on an `import' line, the referent is displayed;
 (2) within a method declaration, its definition is displayed;
 (3) on a class name, the class definition is shown;
 (4) on a unique identifier reference, its definition is shown (when possible)."
  (interactive)
  (or (python-import-file t)
      (python-to-definition t)))

(defconst smart-python-keywords
  '("and" "as" "assert" "break" "class" "continue" "def" "del" "elif"
    "else" "except" "false" "finally" "for" "from" "global" "if"
    "import" "in" "is" "lambda" "none" "nonlocal" "not" "or" "pass"
    "raise" "return" "true" "try" "while" "with" "yield")
  "Sorted list of Python keywords, all in lowercase.")

;;;###autoload
(defun smart-python-at-tag-p (&optional no-flash)
  "Return Python tag name that point is within, else nil."
  (let* ((identifier-chars "a-zA-Z0-9_")
	 (identifier-fragment (concat "[a-zA-Z_][" identifier-chars "]*"))
	 (identifier (concat identifier-fragment "\\(\\."
			     identifier-fragment "\\)*"))
	 start end tag)
    (save-excursion
      ;; Allow for name1.name2.module tags.
      (while (and (/= (skip-chars-backward identifier-chars) 0)
		  (/= (skip-chars-backward "\.") 0)))
      (when (= (following-char) ?.)
	(forward-char 1))
      (setq start (point))
      (while (and (/= (skip-chars-forward identifier-chars) 0)
		  (/= (skip-chars-forward "\.") 0)))
      (when (= (preceding-char) ?.)
	(backward-char 1))
      (setq end (point))
      (goto-char start)
      (setq tag (buffer-substring-no-properties start end))
      (if (and (looking-at identifier)
	       (not (member (downcase tag) smart-python-keywords)))
	  (if no-flash
	      tag
	    (smart-flash-tag tag start end))))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun smart-ancestor-tag-files (&optional path name-of-tags-file)
  "Walk up path tree looking for tags files and return list from furthest to deepest (nearest)."
  (or path (setq path default-directory))
  (let ((tags-table-list)
	tags-file)
    (while (and
	    (stringp path)
	    (setq path (file-name-directory path))
	    (setq path (directory-file-name path))
	    ;; Not at root directory
	    (not (string-match
		  (concat (file-name-as-directory ":?") "\\'")
		  path)))
      (setq tags-file (expand-file-name (or name-of-tags-file "TAGS") path))
      (if (file-readable-p tags-file)
	  (setq tags-table-list (cons tags-file tags-table-list))))
    tags-table-list))

(defun smart-asm-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in `smart-asm-include-path' and add suffix \".ins\" or
\".inc\" to filename if it lacks a suffix." 
  (let ((opoint (point)))
    ;; Some assemblers utilize the C preprocessor, so try that first.
    (cond ((smart-c-include-file))
	  ((progn (beginning-of-line)
		  (looking-at smart-asm-include-regexp))
	   (let ((file (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		 (path)
		 (dir-list smart-asm-include-path))
	     (goto-char opoint)
	     (setq dir-list (cons (file-name-directory buffer-file-name)
				  dir-list))
	     (if (string-match "\\." file)
		 (setq file (regexp-quote file))
	       (setq file (concat (regexp-quote file) "\\.in[sc]$")))
	     (while dir-list
	       (setq dir-list
		     (if (setq path (car (directory-files
					  (car dir-list) t file)))
			 nil
		       (cdr dir-list))))
	     ;;
	     ;; If path exists, display file
	     ;;
	     (if path
		 (unless (and (file-readable-p path)
			      (progn
				(hpath:find path)
				(or (require 'asm-mode nil t)
				    (progn (message "(smart-asm-include-file):  asm-mode undefined")
					   nil))))
		   (beep)
		   (message "(smart-asm-include-file):  `%s' unreadable" path))
	       (beep)
	       (message "(smart-asm-include-file):  `%s' not found" file))
	     path))
	  ;; not on an include file line
	  (t (goto-char opoint)
	     nil))))

(defun smart-c-include-file ()
  "If point is on an include file line, tries to display file.
Returns non-nil iff on an include file line, even if file is not found.
Look for include file in `smart-c-cpp-include-path' and in directory list
`smart-c-include-path'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at smart-c-include-regexp)
	(let ((incl-type (string-to-char
			  (buffer-substring-no-properties (match-beginning 2)
							  (1+ (match-beginning 2)))))
	      (file (buffer-substring-no-properties (match-beginning 3) (match-end 3)))
	      (path)
	      (dir-list smart-c-include-path)
	      (found))
	  (goto-char opoint)
	  (setq dir-list (if (eq incl-type ?<)
			     (append dir-list smart-c-cpp-include-path)
			   (cons (file-name-directory buffer-file-name)
				 dir-list)))
	  (while dir-list
	    (setq path (expand-file-name file (car dir-list))
		  dir-list (if (setq found (file-exists-p path))
			       nil
			     (cdr dir-list))))
	  ;;
	  ;; If found, display file
	  ;;
	  (if found
	      (if (and (file-readable-p path)
		       (progn
			 (hpath:find path)
			 (or (require 'cc-mode nil t)
			     (progn (beep)
				    (message "(smart-c-include-file):  c-mode undefined")
				    nil))))
		  (smart-cc-mode-initialize)
		(beep)
		(message "(smart-c-include-file):  `%s' unreadable" path))
	    (beep)
	    (message "(smart-c-include-file):  `%s' not found" file))
	  path)
      (goto-char opoint)
      nil)))

(defun smart-flash-tag (tag start end)
  "Tries to flash TAG at START to END in buffer, to indicate that it is serving as a hyperlink button.
Returns TAG."
  ;; Button flashing code might not yet have been loaded if the whole
  ;; Hyperbole system has not been started.
  (when (and (fboundp 'hui:but-flash)
	     (fboundp 'ibut:label-set))
    (ibut:label-set tag start end)
    (hui:but-flash))
  tag)

(defun smart-lisp-at-known-identifier-p ()
  "Returns non-nil if point is within a Lisp identifier listed in a tags table or is a known Emacs Lisp identifier, else nil."
  (interactive)
  (unless (and (fboundp 'find-library)
	       ;; Handle Emacs Lisp `require', `load', and `autoload' clauses.
	       (let ((lib)
		     (opoint (point)))
		 (setq lib (and (re-search-backward "[()]" nil t)
				(looking-at (concat
					     "(\\(require\\|load\\|autoload\\)"
					     "[ \t]+.*['\"]"
					     "\\([^][() \t\n\r`'\"]+\\)"))))
		 (goto-char opoint)
		 (if lib
		     (condition-case ()
			 (and (find-library-name lib) t)
		       (error nil)))))
    (let* ((tag (smart-lisp-at-tag-p t))
	   (tag-sym (intern-soft tag)))
      (cond ((if (fboundp 'find-function-noselect)
		 (let ((result (smart-lisp-bound-symbol-def tag-sym)))
		   (if (cdr result) t))))
	    ;; This part only works properly for Emacs Lisp, so is conditionalized.
	    (tag (smart-tags-find-p tag))))))

(defun smart-lisp-bound-symbol-def (tag-sym)
  (save-excursion
    ;; Bound Emacs Lisp function, variable and face definition display.
    (or (condition-case () (find-function-noselect tag-sym) (error nil))
	(condition-case () (find-variable-noselect tag-sym) (error nil))
	(condition-case () (find-definition-noselect tag-sym 'defface) (error nil)))))

(defun smart-tags-find-p (tag)
  "Returns non-nil if TAG is found within a tags table, else nil."
  (let* ((tags-table-list (or (and (boundp 'tags-table-list) tags-table-list)
			      (smart-tags-file-list)))
	 (func (smart-tags-noselect-function))
	 (tags-file-name (if tags-table-list
			     nil
			   (and (boundp 'tags-file-name) tags-file-name)))
	 find-tag-result
	 ;; For InfoDock and XEmacs
	 (tags-always-exact t)
	 (tag-table-alist
	  (mapcar (lambda (tags-file) (cons "." tags-file))
		  tags-table-list))
	 ;; For GNU Emacs
	 (tags-add-tables nil))
    ;; For InfoDock (XEmacs may also take this branch), force exact match
    ;; (otherwise tag might = nil and the following stringp test could fail).
    (if (or (featurep 'infodock) (featurep 'xemacs))
	(if (stringp tag) (setq tag (list tag))))
    (condition-case ()
	(and func (funcall func tag) t)
      (error nil))))

(defun smart-java-cross-reference ()
  "If within a Java @see comment, displays the associated definition for editing and returns non-nil, else nil.
Non-nil is returned even if the @see referent cannot be found.

Does nothing if the `oo-browser' command is undefined, since it requires that
package for class and feature lookups."
  ;;
  ;; Valid forms of @see cross-references are:
  ;;    * @see #getComponent                        - current class attribute
  ;;    * @see #waitForAll()                        - current class method, no arguments
  ;;    * @see #checkID(int, boolean)               - current class method, with arguments
  ;;    * @see java.awt.ColorModel#getRGBdefault    - library class method
  ;;    * @see Component#paintAll                   - class method
  ;;    * @see java.awt.GridBagLayout               - library class
  ;;    * @see Container                            - class
  ;;
  ;; For simplicity sake, this code ignores the library path given with any
  ;; class in favor of the OO-Browser's lookup tables.  It also ignores any
  ;; parameters associated with a method, and thus cannot distinguish between
  ;; methods with the same name within a single class, which we believe to be
  ;; fairly bad form anyway.
  ;;
  (let ((opoint (point)))
    (if (and (eq major-mode 'java-mode) buffer-file-name
	     (fboundp 'br-env-load)
	     (or (looking-at "@see[ \t]+")
		 (and (re-search-backward "[@\n\r\f]" nil t)
		      (looking-at "@see[ \t]+"))))
	(let ((class) (feature))
	  ;; Ignore any library path preceding a classname (grouping 1)
	  (cond
	   ((looking-at
	     "@see[ \t]+\\(#\\)?\\([^][(){} \t\n\r\f#]+[.]\\)?\\([^][(){} \t\n\r\f#.]+\\)[][(){} \t\n\r\f]")
	    (if (match-beginning 1)
		(setq class nil
		      feature (buffer-substring-no-properties (match-beginning 3)
							      (match-end 3)))
	      (setq class (buffer-substring-no-properties (match-beginning 3) (match-end 3))
		    feature nil)))
	   ((looking-at
	     "@see[ \t]+\\([^][(){} \t\n\r\f#]+[.]\\)?\\([^][(){} \t\n\r\f#.]+\\)#\\([^][(){} \t\n\r\f#.]+\\)")
	    (setq class (buffer-substring-no-properties (match-beginning 2)
							(match-end 2))
		  feature (buffer-substring-no-properties (match-beginning 3)
							  (match-end 3)))))
	  ;; Return to original point.
	  (goto-char opoint)
	  ;; Lookup class / feature.
	  (cond
	   ((and (null class) (null feature))
	    ;; Invalid or unrecognized @see format, so ignore.
	    (message "(smart-java-cross-reference): Invalid @see cross-reference format")
	    (beep)
	    t)
	   ;; Ensure that a Java OO-Browser environment has been loaded.
	   (t (if (or (and (boundp 'br-lang-prefix)
			   (equal br-lang-prefix "java-")
			   (boundp 'br-env-file) (stringp br-env-file)
			   (null br-env-spec))
		      ;; Load an existing Environment based on current
		      ;; buffer or prompt to build one.  This also
		      ;; loads the "br-java.el" library in which the
		      ;; `java-class-def-regexp' variable used below
		      ;; is defined.
		      (and (br-env-load
			    (car (nreverse
				  (smart-tags-file-list
				   nil
				   (if (boundp 'br-env-default-file)
				       br-env-default-file "OOBR")))))
			   (equal br-lang-prefix "java-")))
		  (cond ((null feature)
			 (br-edit nil class))
			(t
			 (if (null class)
			     (if (save-excursion
				   (or (re-search-backward java-class-def-regexp nil t)
				       (re-search-forward java-class-def-regexp nil t)))
				 (setq class (buffer-substring-no-properties
					      (match-beginning java-class-def-name-grpn)
					      (match-end java-class-def-name-grpn)))
			       (error "(smart-java-cross-reference): This @see must be in a class definition.")))
			 (br-edit-feature class feature t)))
		(error "(smart-java-cross-reference): The OO-Browser failed to load a Java environment.")))))
      ;; Return to original point.
      (goto-char opoint)
      nil)))

(defun smart-java-library-path (library-name)
  "Search up directory tree from current directory for a match to LIBRARY-NAME."
  (let ((path default-directory)
	(library-path)
	(library-regexp (if (string-match "\\.\\|'//" library-name)
			    (regexp-quote
			     (concat (file-name-as-directory "")
				     (substring library-name 0 (match-beginning 0))
				     (file-name-as-directory "")))))
	(start 0))
    ;; Return rightmost match to first part of library-name.
    (if library-regexp
	(while (string-match library-regexp path start)
	  (setq start (1+ (match-beginning 0))
		library-path (substring path 0 (match-beginning 0)))))
    library-path))

(defun smart-java-packages ()
  "If point is on a `package' or `import' line, this tries to display the associated referent.
Returns non-nil iff on such a line, even if the referent is not found.
Look for packages in `smart-java-package-path'."
  (let ((opoint (point)))
    (beginning-of-line)
    (if (looking-at smart-java-package-regexp)
	(let ((keyword-type (buffer-substring-no-properties
			     (match-beginning 1) (match-end 1)))
	      (referent (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
	      (found)
	      (subpath)
	      dir-list path subfile)
	  (goto-char opoint)
	  (if (string-equal keyword-type "package")
	      (let ((library-path (smart-java-library-path referent)))
		(if library-path
		    (hpath:find (expand-file-name 
				 (hypb:replace-match-string
				  "\\." referent (file-name-as-directory "") t)
				 library-path))
		  ;; Show the current directory, which should contain this package.
		  (hpath:find default-directory)))
	    ;; This is an `import' statement.  If it includes a *, show the
	    ;; associated library directory, otherwise, show the specific
	    ;; package.
	    (if (string-match "\\.\\*" referent)
		(setq subfile (substring referent 0 (match-beginning 0))
		      subfile (hypb:replace-match-string
			       "\\." subfile (file-name-as-directory "") t))
	      (setq subpath (hypb:replace-match-string
			     "\\." referent (file-name-as-directory "") t)
		    subfile (concat subpath ".java")))
	    ;;
	    ;; Try to find the path containing referent.
	    ;;
	    ;; Search up the current directory tree for a possible matching
	    ;; directory below which the referent library might live and add
	    ;; this to smart-java-package-path for searching.
	    (let ((library-path (smart-java-library-path referent)))
	      (if library-path
		  (setq dir-list (cons library-path smart-java-package-path))))

	    (while dir-list
	      (setq path (expand-file-name subfile (car dir-list))
		    dir-list (if (setq found (file-exists-p path))
				 nil
			       (cdr dir-list))))
	    (when (and (not found) subpath hyperb:microcruft-os-p)
		;; Try .jav suffix.
	      (setq subfile (concat subpath ".jav")
		    dir-list smart-java-package-path)
	      (while dir-list
		(setq path (expand-file-name subfile (car dir-list))
		      dir-list (if (setq found (file-exists-p path))
				   nil
				 (cdr dir-list)))))
	    ;;
	    ;; If found, display file
	    ;;
	    (if found
		(if (file-readable-p path)
		    (hpath:find path)
		  (beep)
		  (message "(smart-java-packages):  `%s' unreadable" path))
	      (beep)
	      (message "(smart-java-packages):  `%s' not found" referent))
	    path))
      (goto-char opoint)
      nil)))

(defun smart-library-symbol (tag)
  "Return non-nil if TAG is a library symbol listed in cache of such symbols.
See the \"${hyperb:dir}/smart-clib-sym\" script for more information."
  (let ((buf (get-buffer-create "*junk*"))
	(found))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (call-process (expand-file-name "smart-clib-sym" hyperb:dir)
		    nil buf nil tag)
      (setq found (string-equal (buffer-substring-no-properties 1 2) "1"))
      (set-buffer-modified-p nil)
      (kill-buffer buf)
      found)))

(defun smart-tags-display (tag next &optional list-of-tags-tables)
  (if next (setq tag nil))
  (let* ((tags-table-list (or list-of-tags-tables
			      (and (boundp 'tags-table-list)
				   (nconc (smart-tags-file-list) tags-table-list))
			      (smart-tags-file-list)))
	 (func (smart-tags-noselect-function))
	 (tags-file-name (if tags-table-list
			     nil
			   (and (boundp 'tags-file-name) tags-file-name)))
	 find-tag-result
	 ;; For InfoDock and XEmacs
	 (tags-always-exact t)
	 (tag-table-alist
	  (mapcar (lambda (tags-file) (cons "." tags-file))
		  tags-table-list))
	 ;; For GNU Emacs
	 (tags-add-tables nil))
    ;; For InfoDock (XEmacs may also take this branch), force exact match
    ;; when `next' is false (otherwise tag would = nil and the following
    ;; stringp test would fail).
    (if (or (featurep 'infodock) (featurep 'xemacs))
	(if (stringp tag) 
	    (setq tag (list tag))))
    (if (and func (setq find-tag-result (funcall func tag)))
	(cond ((eq func 'find-tag-internal)
	       ;; InfoDock and XEmacs
	       (hpath:display-buffer (car find-tag-result))
	       (goto-char (cdr find-tag-result)))
	      ((vectorp find-tag-result)
	       ;; Newer GNU Emacs with xref.el
	       (hpath:display-buffer (xref-item-buffer find-tag-result))
	       (goto-char (xref-item-position find-tag-result)))
	      ((bufferp find-tag-result)
	       ;; Older GNU Emacs
	       (hpath:display-buffer find-tag-result))
	      (t
	       ;; Emacs with some unknown version of tags.
	       ;; Signals an error if tag is not found which is caught by
	       ;; many callers of this function.
	       (with-no-warnings (find-tag tag))))
      ;; Signals an error if tag is not found which is caught by
      ;; many callers of this function.
      (with-no-warnings	(find-tag tag)))))

;;;###autoload
(defun smart-tags-file-path (file)
  "Expand relative FILE name by looking it up within appropriate tags files.
Return FILE unchanged if it exists relative to the current directory or
cannot be expanded via a tags file."
  (if (or (file-exists-p file) (file-name-absolute-p file))
      file
    (let ((tags-table-list (smart-tags-file-list))
	  (file-regexp
	   (concat "\^L\r?\n\\(.*/\\)?" (regexp-quote file) ",")))
      (save-excursion
	(while tags-table-list
	  (set-buffer (find-file-noselect (car tags-table-list)))
	  (goto-char (point-min))
	  (if (re-search-forward file-regexp nil t)
	      (setq file
		    (expand-file-name
		     (buffer-substring-no-properties (1- (match-end 0))
						     (progn (beginning-of-line)
							    (point))))
		    tags-table-list nil)
	    (setq tags-table-list (cdr tags-table-list)))))
      file)))

;;;###autoload
(defun smart-tags-file-list (&optional curr-dir-or-filename name-of-tags-file)
  "Return appropriate tag files list for optional CURR-DIR-OR-FILENAME or for `default-directory'.
Optional NAME-OF-TAGS-FILE is the literal filename (no directory) for which
to look.  If no tags file is found, an error is signaled."
  (let* ((path (or curr-dir-or-filename default-directory))
	 (tags-table-list (smart-ancestor-tag-files path name-of-tags-file)))
    ;; If no tags files were found and the current buffer may contain Emacs Lisp identifiers and
    ;; is in a 'load-path' directory, then use the default Emacs Lisp tag table.
    (if (and (not tags-table-list)
	     (stringp curr-dir-or-filename)
	     smart-emacs-tags-file
	     (smart-emacs-lisp-mode-p)
	     (let ((path (file-name-directory curr-dir-or-filename)))
	       (and path (delq nil (mapcar
				    (lambda (p)
				      (and p (equal (file-name-as-directory p)
						    path)))
				    load-path)))))
	(setq tags-table-list (list smart-emacs-tags-file)))
    ;; Return the appropriate tags file list.
    (cond (tags-table-list
	   ;; GNU Emacs when tags tables are found or provided by the user
	   (nreverse tags-table-list))
	  ((fboundp 'tags-table-check-computed-list)
	   ;; GNU Emacs in other cases
	   (tags-table-check-computed-list)
	   tags-table-computed-list)
	  ((fboundp 'buffer-tag-table-list)
	   ;; InfoDock and XEmacs
	   (buffer-tag-table-list))
	  ((and (boundp 'buffer-tag-table) buffer-tag-table)
	   ;; InfoDock and XEmacs
	   (list buffer-tag-table))
	  ((and (boundp 'tags-file-name) tags-file-name)
	   (list tags-file-name))
	  (t (error "Needed tags file not found; see `man etags' for how to build one.")))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun smart-tags-noselect-function ()
  "Return the best available function for finding a tag definition without selecting it."
  (car (delq nil (mapcar (lambda (func) (if (fboundp func) func))
			 #'(xref-definition find-tag-noselect find-tag-internal)))))

(provide 'hmouse-tag)

;;; hmouse-tag.el ends here
