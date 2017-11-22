;;; hypb.el --- Miscellaneous GNU Hyperbole support features
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     6-Oct-91 at 03:42:38
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

(eval-and-compile (mapc #'require '(hversion hact locate)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hypb:help-buf-prefix "*Help: Hyperbole "
  "Prefix attached to all native Hyperbole help buffer names.
This should end with a space.")

(defcustom hypb:rgrep-command
  (format "%sgrep -insIHr " (if (executable-find "zgrep") "z" ""))
  "*Grep command string and initial arguments to send to `hypb:rgrep' command.
It must end with a space."
  :type 'string
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hypb:call-process-p (program &optional infile predicate &rest args)
  "Calls an external PROGRAM with INFILE for input.
If PREDICATE is given, it is evaluated in a buffer with the PROGRAM's
output and the result returned.  If PREDICATE is nil, returns t iff
program has no output or just a 0-valued output.
Rest of ARGS are passed as arguments to PROGRAM."
  (let ((buf (get-buffer-create "*test-output*"))
	(found))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq found 
	    (if predicate
		(eval predicate)
	      (or (= (point-max) 1) ;; No output, consider cmd a success.
		  (and (< (point-max) 4)
		       (string= (buffer-substring 1 2) "0")))))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    found))


(defun hypb:char-count (char array)
  "Return count of occurrences of CHAR in ARRAY."
  (let ((i 0) (c 0) (l (length array)))
    (while (< i l)
      (if (= char (aref array i)) (setq c (1+ c)))
      (setq i (1+ i)))
    c))

(defun hypb:chmod (op octal-permissions file)
  "Uses OP and OCTAL-PERMISSIONS integer to set FILE permissions.
OP may be +, -, xor, or default =."
  (let ((func (cond ((eq op '+)   #'logior)
		    ((eq op '-)   (lambda (p1 p2) (logand (lognot p1) p2)))
		    ((eq op 'xor) #'logxor)
		    (t            (lambda (p1 p2) p1)))))
    (set-file-modes file (funcall func (hypb:oct-to-int octal-permissions)
				  (file-modes file)))))

(defun hypb:cmd-key-string (cmd-sym &optional keymap)
  "Returns a single pretty printed key sequence string bound to CMD-SYM.
Global keymap is used unless optional KEYMAP is given."
  (if (and cmd-sym (symbolp cmd-sym) (fboundp cmd-sym))
  (let* ((get-keys (lambda (cmd-sym keymap)
		     (key-description (where-is-internal
				       cmd-sym keymap 'first))))
	 (keys (funcall get-keys cmd-sym keymap)))
    (concat "{"
	    (if (string= keys "")
		(concat (funcall get-keys 'execute-extended-command nil)
			" " (symbol-name cmd-sym) " RET")
	      keys)
	    "}"))
  (error "(hypb:cmd-key-string): Invalid cmd-sym arg: %s." cmd-sym)))

;;;###autoload
(defun hypb:configuration (&optional out-buf)
  "Insert Emacs configuration information at the end of optional OUT-BUF or the current buffer."
  (save-excursion
    (and out-buf (set-buffer out-buf))
    (goto-char (point-min))
    (if (re-search-forward mail-header-separator nil t)
	(forward-line 1)
      (goto-char (point-max)))
    (delete-blank-lines) (delete-blank-lines)
    (let ((start (point)))
      (insert (format "I use:\tEditor:      %s\n\tHyperbole:   %s\n"
		      (cond ((boundp 'infodock-version)
			     infodock-version)
			    (t (hypb:replace-match-string
				" of .+" (emacs-version) "" t)))
                      hyperb:version))
      (if (and (boundp 'br-version) (stringp br-version))
	  (insert (format "\tOO-Browser:  %s\n" br-version)))
      (if (and (boundp 'system-configuration) (stringp system-configuration))
	  (insert (format "\tSys Type:    %s\n" system-configuration)))
      (insert (format "\tOS Type:     %s\n\tWindow Sys:  %s\n"
                      system-type (or window-system (hyperb:window-system)
				      "None")))
      (if (and (boundp 'hmail:reader) hmail:reader)
          (insert (format "\tMail Reader: %s\n"
                          (cond ((eq hmail:reader 'rmail-mode) "RMAIL")
                                ((eq hmail:reader 'vm-mode)
                                 (concat "VM " vm-version))
                                ((and (eq hmail:reader 'mh-show-mode)
                                      (string-match "v ?\\([0-9]+.[0-9]+\\)"
                                          mh-e-RCS-id))
                                 (concat "MH-e "
                                         (substring mh-e-RCS-id
                                                    (match-beginning 1)
                                                    (match-end 1))))
                                ((eq hmail:reader 'pm-fdr-mode)
                                 (concat "PIEmail " pm-version))
                                ))))
      (if (and (boundp 'hnews:reader) (boundp 'gnus-version) hnews:reader)
          (insert (format "\tNews Reader: %s\n" gnus-version)))
      (insert "\n")
      ;; Insert recent Hyperbole debugging messages if any.
      (if (get-buffer "*Messages*")
	  (let ((opoint (point)))
	    (insert-buffer-substring "*Messages*")
	    (keep-lines "^(HyDebug)" opoint (point))))
      (untabify start (point)))))

(defun hypb:debug ()
  "Loads Hyperbole hbut.el source file and sets debugging traceback flag."
  (interactive)
  (or (featurep 'hinit) (load "hyperbole"))
  (or (and (featurep 'hbut)
	   (let ((func (hypb:indirect-function 'ebut:create)))
	     (not (or (hypb:emacs-byte-code-p func)
		      (eq 'byte-code
			  (car (car (nthcdr 3 (hypb:indirect-function
					       'ebut:create)))))))))
      (load "hbut.el"))
  (setq debug-on-error t))

;; Copied from eww.el so as to not require that package.
(defun hypb:decode-url (string)
  (let* ((binary (url-unhex-string string))
         (decoded
          (decode-coding-string
           binary
           ;; Possibly set by `universal-coding-system-argument'.
           (or coding-system-for-read
               ;; RFC 3986 says that %AB stuff is utf-8.
               (if (equal (decode-coding-string binary 'utf-8)
                          '(unicode))
                   'utf-8
                 ;; But perhaps not.
                 (car (detect-coding-string binary))))))
         (encodes (find-coding-systems-string decoded)))
    (if (or (equal encodes '(undecided))
            (memq (coding-system-base (or file-name-coding-system
                                          default-file-name-coding-system))
                  encodes))
        decoded
      ;; If we can't encode the decoded file name (due to language
      ;; environment settings), then we return the original, hexified
      ;; string.
      string)))

(defun hypb:domain-name ()
  "Returns current Internet domain name with '@' prepended or nil if none."
  (let* ((dname-cmd (or (file-exists-p "/usr/bin/domainname")
			(file-exists-p "/bin/domainname")))
	 (dname (or (and (boundp 'message-user-fqdn) (stringp message-user-fqdn)
			 (string-match "\\." message-user-fqdn)
			 message-user-fqdn)
		    (getenv "DOMAINNAME")
		    (if dname-cmd
			(hypb:call-process-p
			 "domainname" nil 
			 '(substring (buffer-string) 0 -1)))))
	 host-and-domain)
    (if (or (and dname (string-match "\\." dname))
	    (and (setq host-and-domain (hypb:call-process-p
					"hostname" nil '(substring (buffer-string) 0 -1) "-f"))
		 (setq dname (if (string-match "\\`[^.]+\\." host-and-domain)
				 (substring host-and-domain (match-end 0)))))
	    (let* ((src "/etc/resolv.conf")
		   (src-buf-exists-p (get-file-buffer src)))
	      (and (file-exists-p src) (file-readable-p src)
		   (with-temp-buffer
		     (insert-file-contents-literally src)
		     (goto-char (point-min))
		     (if (re-search-forward  "^domain[ \t]+\\([^ \t\n\r]+\\)"
					     nil t)
			 (setq dname (buffer-substring (match-beginning 1)
						       (match-end 1))))
		     (or src-buf-exists-p (kill-buffer nil))
		     dname))))
	(concat "@" dname))))

(defun hypb:emacs-byte-code-p (obj)
  "Return non-nil iff OBJ is an Emacs byte compiled object."
  (or (and (fboundp 'compiled-function-p) (compiled-function-p obj))
      (and (fboundp 'byte-code-function-p) (byte-code-function-p obj))))

(defun hypb:error (&rest args)
  "Signals an error typically to be caught by `hyperbole'."
  (let ((msg (if (< (length args) 2) (car args) (apply 'format args))))
    (put 'error 'error-message msg)
    (error msg)))

(defun hypb:file-major-mode (file)
  "Return the major mode used by FILE.
FILE is temporarily read into a buffer to determine the major mode if necessary."
  (let ((existing-flag (get-file-buffer file))
	(buf (find-file-noselect file)))
    (prog1 (when buf (save-excursion (set-buffer buf) major-mode))
      (unless (or existing-flag (null buf))
	(kill-buffer buf)))))

(defun hypb:format-quote (string)
  "Replace all single % with %% in STRING so a call to `format' or `message' ignores them."
  (if (stringp string)
      (replace-regexp-in-string
       "@@@" "%%" (replace-regexp-in-string
		   "%" "%%" (replace-regexp-in-string "%%" "@@@" string nil t)
		   nil t)
       nil t)))

;;;###autoload
(defun hypb:functionp (obj)
"Returns t if OBJ is a function, nil otherwise."
  (cond
    ((symbolp obj) (fboundp obj))
    ((subrp obj))
    ((hypb:emacs-byte-code-p obj))
    ((consp obj)
     (if (eq (car obj) 'lambda) (listp (car (cdr obj)))))
    (t nil)))

(defun hypb:function-copy (func-symbol)
  "Copies FUNC-SYMBOL's body for overloading.  Returns copy of body."
  (if (fboundp func-symbol)
      (let ((func (hypb:indirect-function func-symbol)))
	(cond ((listp func) (copy-sequence func))
	      ((subrp func) (error "(hypb:function-copy): `%s' is a primitive; can't copy body."
				   func-symbol))
	      ((and (hypb:emacs-byte-code-p func) (fboundp 'make-byte-code))
	       (if (not (fboundp 'compiled-function-arglist))
		   (let ((new-code (append func nil))) ; turn it into a list
		     (apply 'make-byte-code new-code))
		 ;; Can't reference bytecode objects as vectors in modern
		 ;; XEmacs.
		 (let ((new-code (nconc
				  (list (compiled-function-arglist func)
					(compiled-function-instructions func)
					(compiled-function-constants func)
					(compiled-function-stack-depth func)
					(compiled-function-doc-string func))))
		       spec)
		   (if (setq spec (compiled-function-interactive func))
		       (setq new-code (nconc new-code (list (nth 1 spec)))))
		   (apply 'make-byte-code new-code))))
	      (t (error "(hypb:function-copy): Can't copy function body: %s" func))))
    (error "(hypb:function-copy): `%s' symbol is not bound to a function."
	   func-symbol)))

(defun hypb:function-overload (func-sym prepend &rest new-forms)
  "Redefine function named FUNC-SYM by either PREPENDing (or appending if nil) rest of quoted NEW-FORMS."
  (let ((old-func-sym (intern
			(concat "hypb--old-"
				(symbol-name func-sym)))))
    (or (fboundp old-func-sym)
	(defalias old-func-sym (hypb:function-copy func-sym)))
    (let* ((old-func (hypb:indirect-function old-func-sym))
	   (old-param-list (action:params old-func))
	   (param-list (action:param-list old-func))
	   (old-func-call
	     (list (if (memq '&rest old-param-list)
		       ;; Have to account for extra list wrapper from &rest.
		       (cons 'apply
			     (cons (list 'quote old-func-sym) param-list))
		     (cons old-func-sym param-list)))))
      (eval (append
	      (list 'defun func-sym old-param-list)
	      (delq nil
		    (list
		      (documentation old-func-sym)
		      (action:commandp old-func-sym)))
	      (if prepend
		  (append new-forms old-func-call)
		(append old-func-call new-forms)))))))

(defun hypb:function-symbol-replace (func-sym sym-to-replace replace-with-sym)
  "Replaces in body of FUNC-SYM SYM-TO-REPLACE with REPLACE-WITH-SYM.
FUNC-SYM may be a function symbol or its body.  All occurrences within lists
are replaced.  Returns body of modified FUNC-SYM."
  (let ((body (hypb:indirect-function func-sym))
	(constant-vector) (constant))
    (if (listp body)
	;; assume V18 byte compiler
	(setq constant-vector
	      (car (delq nil (mapcar
			      (lambda (elt)
				(and (listp elt)
				     (vectorp (setq constant-vector (nth 2 elt)))
				     constant-vector))
			      body))))
      ;; assume EMACS byte compiler   (eq (compiled-function-p body) t)
      (setq constant (if (fboundp 'compiled-function-constants)
			 (compiled-function-constants body)
		       (aref body 2))
	    constant-vector (if (vectorp constant) constant)))
    (if constant-vector
	;; Code is byte-compiled.
	(hypb:constant-vector-symbol-replace
	 constant-vector sym-to-replace replace-with-sym)
      ;;
      ;; Code is not byte-compiled.
      ;; Replaces occurrence of symbol within lists only.
      (hypb:map-sublists
       (lambda (atom list)
	 ;; The ' in the next line *is* required for proper substitution.
	 (if (eq atom 'sym-to-replace)
	     (let ((again t))
	       (while (and again list)
		 (if (eq (car list) atom)
		     (progn (setcar list replace-with-sym)
			    (setq again nil))
		   (setq list (cdr list)))))))
       body))
    body))

;; Derived from pop-global-mark of "simple.el" in GNU Emacs.
(defun hypb:goto-marker (marker)
  "Make MARKER's buffer and position current.
If MARKER is invalid signal an error."
  (cond ((not (markerp marker))
	 (error "Invalid marker: %s" marker))
	((not (marker-buffer marker))
	 (error "Invalid marker buffer: %s" marker))
	(t (let* ((buffer (marker-buffer marker))
		  (position (marker-position marker)))
	     (set-buffer buffer)
	     (unless (and (>= position (point-min))
			  (<= position (point-max)))
	       (if widen-automatically
		   (widen)
		 (error "Marker position is outside accessible part of buffer: %s" marker)))
	     (goto-char position)
	     (switch-to-buffer buffer)))))

(defun hypb:help-buf-name (&optional suffix)
  "Returns a Hyperbole help buffer name for current buffer.
With optional SUFFIX string, uses it rather than buffer name."
  (let ((bn (or suffix (buffer-name))))
    (if (string-match (regexp-quote hypb:help-buf-prefix) bn)
	(buffer-name (generate-new-buffer bn))
      (concat hypb:help-buf-prefix bn "*"))))

(defun hypb:hkey-help-file ()
  "Return the full path to the Hyperbole mouse key help file."
  (cond ((and (fboundp 'locate-data-file)
	      (locate-data-file "hkey-help.txt")))
	(t (let* ((hypb-man (expand-file-name "man/" hyperb:dir))
		  (help-file (expand-file-name "hkey-help.txt" hypb-man)))
	     (if (or (file-exists-p help-file)
		     (file-exists-p
		      (setq help-file (expand-file-name
				       "hkey-help.txt" data-directory))))
		 help-file
	       (error "(hypb:hkey-help-file): Non-existent file: \"%s\""
		      help-file))))))

(defun hypb:indirect-function (obj)
  "Return the function at the end of OBJ's function chain.
Resolves autoloadable function symbols properly."
  (let ((func
	 (if (fboundp 'indirect-function)
	     (indirect-function obj)
	   (while (symbolp obj)
	     (setq obj (symbol-function obj)))
	   obj)))
    ;; Handle functions with autoload bodies.
    (if (and (symbolp obj) (listp func) (eq (car func) 'autoload))
	(let ((load-file (car (cdr func))))
	  (load load-file)
	  ;; Prevent infinite recursion
	  (if (equal func (symbol-function obj))
	      (error "(hypb:indirect-function): Autoload of '%s' failed" obj)
	    (hypb:indirect-function obj)))
      func)))

(defun hypb:insert-region (buffer start end invisible-flag)
  "Insert into BUFFER the contents of the region from START to END within the current buffer.
INVISIBLE-FLAG, if non-nil, means invisible text in an outline region is
copied, otherwise, it is omitted."
  (if invisible-flag
      ;; Skip hidden blank lines between cells but include hidden outline text.
      (while (< start end)
	(if (not (get-text-property start 'invisible))
	    (append-to-buffer buffer start (1+ start)))
	(setq start (1+ start)))
    ;; Skip both hidden blank lines between cells and hidden outline text.
    (while (< start end)
      (or (kview:char-invisible-p start) (append-to-buffer buffer start (1+ start)))
      (setq start (1+ start)))))
	
;;;###autoload
(defun hypb:locate (search-string &optional filter arg)
  "Find file name matches anywhere, calling the value of `locate-command', and putting results in the `*Locate*' buffer.
Pass it SEARCH-STRING as argument.  Interactively, prompt for SEARCH-STRING.
With prefix arg ARG, prompt for the exact shell command to run instead.

This program searches for those file names in a database that match
SEARCH-STRING and normally outputs all matching absolute file names,
one per line.  The database normally consists of all files on your
system, or of all files that you have access to.  Consult the
documentation of the program for the details about how it determines
which file names match SEARCH-STRING.  (Those details vary highly with
the version.)

You can specify another program for this command to run by customizing
the variables `locate-command' or `locate-make-command-line'.

The main use of FILTER is to implement `locate-with-filter'.  See
the docstring of that function for its meaning.

After preparing the results buffer, this runs `dired-mode-hook' and
then `locate-post-command-hook'."
  (interactive (list (let ((default (symbol-at-point)))
		       (read-string (format "Locate files anywhere with names that match%s: "
					    (if default
						(format " (default %s)" default)
					      ""))
				    nil nil default))
		     nil
		     current-prefix-arg))
  (locate search-string filter arg))

(if (or (featurep 'xemacs) hyperb:emacs-p)
    (defalias 'hypb:mark 'mark)
  (defun hypb:mark (inactive-p)
    "Return this buffer's mark value as integer, or nil if no mark.
INACTIVE-P non-nil means return value of mark even if region is not active
under Emacs version 19.
If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
    (mark)))

(if (featurep 'xemacs)
    (defalias 'hypb:mark-marker 'mark-marker)
  (defun hypb:mark-marker (inactive-p)
    "Return this buffer's mark as a marker object, or nil if no mark.
INACTIVE-P is unused, it is for compatibility with XEmacs' version of
mark-marker."
    (mark-marker)))

(defun hypb:map-sublists (func list)
  "Applies FUNC to every atom found at any level of LIST.
FUNC must take two arguments, an atom and a list in which the atom is found.
Returns values from applications of FUNC as a list with the same
structure as LIST.  FUNC is therefore normally used just for its side-effects."
  (mapcar (lambda (elt)
	    (if (atom elt)
		(funcall func elt list)
	      (hypb:map-sublists func elt)))
	  list))

(defun hypb:map-vector (func object)
  "Returns list of results of application of FUNC to each element of OBJECT.
OBJECT should be a vector or byte-code object."
  (if (not (or (vectorp object) (hypb:emacs-byte-code-p object)))
      (error "(hypb:map-vector): Second argument must be a vector or byte-code object."))
  (let ((end (length object))
	(i 0)
	(result))
    (while (< i end)
      (setq result (cons (funcall func (aref object i)) result)
	    i (1+ i)))
    (nreverse result)))

;; Derived from "window.el".
(defun hypb:maximize-window-height (&optional window)
  "Maximize WINDOW.
Make WINDOW as large as possible without deleting any windows.
WINDOW must be a valid window and defaults to the selected one.

If the option `window-resize-pixelwise' is non-nil maximize
WINDOW pixelwise."
  (interactive)
  (setq window (window-normalize-window window))
  (window-resize
   window (window-max-delta window nil nil nil nil nil window-resize-pixelwise)
   nil nil window-resize-pixelwise))

(defun hypb:replace-match-string (regexp str newtext &optional literal)
  "Replaces all matches for REGEXP in STR with NEWTEXT string and returns the result.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\.
NEWTEXT may instead be a function of one argument (the string to replace in)
that returns a replacement string."
  (unless (stringp str)
    (error "(hypb:replace-match-string): 2nd arg must be a string: %s" str))
  (unless (or (stringp newtext) (hypb:functionp newtext))
    (error "(hypb:replace-match-string): 3rd arg must be a string or function: %s"
	   newtext))
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	      rtn-str
	      (substring str prev-start match)
	      (cond ((hypb:functionp newtext)
		     (hypb:replace-match-string
		      regexp (substring str match start)
		      (funcall newtext str) literal))
		    (literal newtext)
		    (t (mapconcat
			 (lambda (c)
			   (if special
			       (progn
				 (setq special nil)
				 (cond ((eq c ?\\) "\\")
				       ((eq c ?&)
					(substring str
						   (match-beginning 0)
						   (match-end 0)))
				       ((and (>= c ?0) (<= c ?9))
					(if (> c (+ ?0 (length
							(match-data))))
					    ;; Invalid match num
					    (error "(hypb:replace-match-string) Invalid match num: %c" c)
					  (setq c (- c ?0))
					  (substring str
						     (match-beginning c)
						     (match-end c))))
				       (t (char-to-string c))))
			     (if (eq c ?\\) (progn (setq special t) nil)
			       (char-to-string c))))
			 newtext ""))))))
    (concat rtn-str (substring str start))))

(defun hypb:return-process-output (program &optional infile &rest args)
  "Returns as a string the output from external PROGRAM with INFILE for input.
Rest of ARGS are passed as arguments to PROGRAM.
Removes any trailing newline at the end of the output."
  (let ((buf (get-buffer-create "*test-output*"))
	(output))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (apply 'call-process program infile buf nil args)
      (setq output (buffer-string))
      ;; Remove trailing newline from output.
      (if (> (length output) 0) (setq output (substring output 0 -1)))
      (set-buffer-modified-p nil)
      (kill-buffer buf))
    output))

(defun hypb:remove-lines (regexp)
 "Remove lines containing matches for REGEXP within the active region or to the end of buffer."
    (interactive "sRemove lines with match for regexp: ")
    (flush-lines regexp nil nil t))

;;;###autoload
(defun hypb:rgrep (pattern &optional prefix-arg)
  "Recursively grep with symbol at point or PATTERN over all non-backup and non-autosave files in the current directory tree.
If in an Emacs Lisp mode buffer and no PREFIX-ARG is given, limit search to only .el and .el.gz files."
  (interactive (list (if (and (not current-prefix-arg) (equal (buffer-name) "*Locate*"))
			 (read-string "Grep files listed here for: ")
		       (let ((default (symbol-at-point)))
			 (if default (setq default (symbol-name default)))
			 (read-string (format "Rgrep below current dir for%s: "
					      (if default
						  (format " (default %s)" default)
						""))
				      nil nil default)))
		     current-prefix-arg))
  (let* ((delim (cond ((not (string-match "\'" pattern)) ?\')
			      ((not (string-match "\"" pattern)) ?\")
			      ((not (string-match "=" pattern)) ?=)
			      (t ?@)))
	 (grep-cmd
	  (if (and (not current-prefix-arg) (equal (buffer-name) "*Locate*"))
	      (format "%s -e \%c%s\%c %s" hypb:rgrep-command delim pattern delim (hypb:locate-pathnames))
	    (format "%s%s -e \%c%s\%c ."
		    hypb:rgrep-command
		    (if (and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
			     (not prefix-arg))
			"--include=\"*.el\" --include=\"*.el.gz\""
		      "--exclude=\"*~\" --exclude=\"#*\" --exclude=\"TAGS\"")
		    delim pattern delim))))
    (setq this-command `(grep ,grep-cmd))
    (push this-command command-history)
    (grep grep-cmd)))

(defun hypb:save-lines (regexp)
 "Save only lines containing matches for REGEXP within the active region or to the end of buffer."
    (interactive "sSave lines with match for regexp: ")
    (keep-lines regexp nil nil t))

(defun hypb:supercite-p ()
  "Returns non-nil iff the Emacs add-on supercite package is in use."
  (let (hook-val)
    (if (memq t (mapcar
		 (lambda (hook-var)
		   (and (boundp hook-var)
			(progn (setq hook-val (symbol-value hook-var))
			       (cond ((listp hook-val)
				      (if (memq 'sc-cite-original hook-val)
					  t))
				     ((eq hook-val 'sc-cite-original))))))
		 '(mail-citation-hook mail-yank-hooks)))
	t)))

(defun hypb:toggle-isearch-invisible (&optional arg)
  "Toggle interactive invisible searching on or off.
This determines whether to search inside invisible text or not.
Toggles the variable ‘isearch-invisible’ between values
nil and a non-nil value of the option ‘search-invisible’
(or ‘open’ if ‘search-invisible’ is nil).

With optional prefix ARG > 0, turn on searching invisible text.
If ARG <= 0, turn search only visible text."
  (interactive "P")
  (if (not (boundp 'isearch-invisible))
      (error "(hypb:toggle-isearch-invisible): Feature not supported by the version of Emacs")
    (setq isearch-invisible (if (if (null arg)
				    (not isearch-invisible)
				  (> (prefix-numeric-value arg) 0))
				(or search-invisible 'open)))
    (message "I-search will %ssearch invisible text"
	     (if isearch-invisible "" "not "))))

(defun hypb:user-name ()
  "Return the current user's email or login name (sans any domain name)."
  (if (string-match "@" hyperb:user-email)
      (substring hyperb:user-email 0 (match-beginning 0))
    (user-login-name)))

(defun hypb:window-list (&optional minibuffer-flag)
  "Returns a list of Lisp window objects for all Emacs windows in selected frame.
Optional first arg MINIBUFFER-FLAG t means include the minibuffer window
in the list, even if it is not active.  If MINIBUFFER-FLAG is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (window-list nil minibuffer-flag))

;;; ************************************************************************
;;; About Hyperbole Setup
;;; ************************************************************************

(defvar hypb:home-page "https://www.gnu.org/software/hyperbole/"
  "The web home page for Hyperbole")

(defvar hypb:hyperbole-banner-keymap
  (let ((map (make-sparse-keymap)))
    (cond (hyperb:emacs-p
	   (define-key map [mouse-1]  'hypb:browse-home-page)
	   (define-key map [mouse-2]  'hypb:browse-home-page)
	   (define-key map "\C-m"     'hypb:browse-home-page))
	  ((featurep 'xemacs)
	   (define-key map 'button1  'hypb:browse-home-page)
	   (define-key map 'button2  'hypb:browse-home-page)
	   (define-key map '(return) 'hypb:browse-home-page)))
    map)
  "Keymap used when on the Hyperbole banner glyph.")

;;;###autoload
(defun hypb:display-file-with-logo (&optional file)
  "Display an optional text FILE in help mode with the Hyperbole banner prepended.
Without file, the banner is prepended to the current buffer."
  (if file
      ;; A stub for this function is defined in hversion.el when not running in InfoDock.
      (id-browse-file file))
  (if hyperb:emacs-p
      (hypb:display-file-with-logo-emacs file)
    (hypb:display-file-with-logo-xemacs file))
  (goto-char (point-min))
  (skip-syntax-forward "-")
  (set-window-start (selected-window) 1)
  (set-buffer-modified-p nil)
  (help-mode))

(defun hypb:browse-home-page ()
  "Visit the web home page for Hyperbole."
  (interactive)
  (require 'hsys-www)
  (hact 'www-url hypb:home-page))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hypb:constant-vector-symbol-replace
  (constant-vector sym-to-replace replace-with-sym)
  ;; Replace symbols within a byte-compiled constant vector.
  (let ((i (length constant-vector))
	constant)
    (while (>= (setq i (1- i)) 0)
      (setq constant (aref constant-vector i))
      (cond ((eq constant sym-to-replace)
	     (aset constant-vector i replace-with-sym))
	    ((and (fboundp 'compiled-function-p)
		  (compiled-function-p constant))
	     (hypb:function-symbol-replace
	      constant sym-to-replace replace-with-sym))))))

(defun hypb:display-file-with-logo-emacs (&optional file)
  "Display an optional text FILE with the Hyperbole banner prepended.
Without file, the banner is prepended to the current buffer."
  (let ((hyperbole-banner-path (expand-file-name "hyperbole-banner.png" hyperb:dir)))
    (if (not (file-readable-p hyperbole-banner-path))
	(setq hyperbole-banner-path (if (fboundp 'locate-data-file)
					(locate-data-file "hyperbole-banner.png")
				      (expand-file-name "hyperbole-banner.png"
							data-directory))))
    (if (or (not (fboundp 'create-image))
	    (not (display-graphic-p))
	    (let ((button (next-button (point-min))))
	      (and button (button-has-type-p button 'hyperbole-banner)))
	    (not hyperbole-banner-path)
	    (not (file-readable-p hyperbole-banner-path)))
	;; Either image support is unavailable, the file cannot be read
	;; or the image has already been inserted, so don't reinsert it.
	nil
      (let ((hyperbole-banner (create-image hyperbole-banner-path))
	     (buffer-read-only)
	     button)
	(goto-char (point-min))
	(insert "\n")
	(insert-image hyperbole-banner)
	(insert "\n\n")
	(setq button (make-button (- (point) 3) (- (point) 2) :type 'hyperbole-banner))
	(button-put button 'help-echo (concat "Click to visit " hypb:home-page))
	(button-put button 'action #'hypb:browse-home-page)
	(button-put button 'keymap hypb:hyperbole-banner-keymap)))))

(defun hypb:display-file-with-logo-xemacs (&optional file)
  "Display an optional text FILE with the Hyperbole banner prepended.
Without file, the banner is prepended to the current buffer."
  (let ((hyperbole-banner-path (expand-file-name "hyperbole-banner.png" hyperb:dir)))
    (if (not (file-readable-p hyperbole-banner-path))
	(setq hyperbole-banner-path (if (fboundp 'locate-data-file)
					(locate-data-file "hyperbole-banner.png")
				      (expand-file-name "hyperbole-banner.png"
							data-directory))))
    (if (or (not (fboundp 'make-glyph))
	    (let ((extent (next-extent (current-buffer))))
	      (and extent (extent-property extent 'hyperbole-banner)))
	    (not hyperbole-banner-path)
	    (not (file-readable-p hyperbole-banner-path)))
	;; Either image support is unavailable, the file cannot be read
	;; or the image has already been inserted, so don't reinsert it.
	nil
      (let ((hyperbole-banner (make-glyph hyperbole-banner-path))
	     (buffer-read-only)
	     extent)
	(goto-char (point-min))
	(insert "\n")
	(indent-to (startup-center-spaces hyperbole-banner))
	(insert "\n\n")
	(setq extent (make-extent (- (point) 3) (- (point) 2)))
	(set-extent-end-glyph extent hyperbole-banner)
	(set-extent-property extent 'hyperbole-banner t)
	(set-extent-property extent 'help-echo
			     (concat "Click to visit " hypb:home-page))
	(set-extent-property extent 'keymap hypb:hyperbole-banner-keymap)))))

(defun hypb:locate-pathnames ()
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n" nil t 3)
    (replace-regexp-in-string " *\\([^\n]+\\)\n" "\\1 "
			      (buffer-substring-no-properties (point) (point-max)))))

(defun hypb:oct-to-int (oct-num)
  "Returns octal integer OCTAL-NUM converted to a decimal integer."
  (let ((oct-str (int-to-string oct-num))
	(dec-num 0))
    (and (string-match "[^0-7]" oct-str)
	 (error "(hypb:oct-to-int): Bad octal number: %s" oct-str))
    (mapconcat (lambda (o)
		 (setq dec-num (+ (* dec-num 8)
				  (if (and (>= o ?0) (<= o ?7))
				      (- o ?0)))))
	       oct-str "")
    dec-num))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(if hyperb:emacs-p (define-button-type 'hyperbole-banner))

(provide 'hypb)

;;; hypb.el ends here
