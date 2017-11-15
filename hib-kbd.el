;;; hib-kbd.el --- Implicit button type for key sequences delimited with {}.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    22-Nov-91 at 01:37:57
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.
;;
;;; Commentary:
;;
;;   A press of the Action Key on a key sequence executes its
;;   command binding or Hyperbole minibuffer menu binding.
;;
;;   A press of the Assist Key on a key sequence displays the
;;   documentation for it.
;;
;;   Key sequences should be in human readable string form with spaces
;;   between each key and the whole sequence delimited by braces, 
;;   e.g. {C-x o}.  Forms such as {\C-b}, {\^b}, and {^b} will not be
;;   recognized. 

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hactypes)

;;; ************************************************************************
;;; Public implicit button types
;;; ************************************************************************
  
(defact kbd-key (key-sequence)
  "Executes a normalized key sequence without curly braces, {}.
KEY-SEQUENCE must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments.

Returns t if the sequence appears to be valid, else nil."
  (interactive "kKey sequence to execute (no {}): ")
  (kbd-key:act key-sequence))

(defib kbd-key ()
  "Executes a key sequence found around point, delimited by curly braces, {}, if any.
Key sequences should be in human readable form, e.g. {C-x C-b}, or what `key-description' returns.
Forms such as {\C-b}, {\^b}, and {^b} will not be recognized.

Any key sequence must be a string of one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (unless (or (br-in-browser)
	      (and (looking-at "[{}]") (/= ?\\ (preceding-char))))
    (let* ((seq-and-pos (or (hbut:label-p t "{`" "'}" t)
			    (hbut:label-p t "{" "}" t)
			    ;; Regular dual single quotes (Texinfo smart quotes)
			    (hbut:label-p t "``" "''" t)
			    ;; Typical GNU manual key sequences; note
			    ;; these are special quote marks, not the
			    ;; standard ASCII characters.
			    (hbut:label-p t "‘" "’" t)))
	   (key-sequence (car seq-and-pos))
	   (start (cadr seq-and-pos))
	   binding)
      ;; Match only when start delimiter is preceded by whitespace or
      ;; is the 1st buffer character, so do not match to things like ${variable}.
      (when (= (char-syntax (or (char-before start) ?\t)) ?\ )
	(when (and (stringp key-sequence)
		   (not (eq key-sequence "")))
	  (setq key-sequence (kbd-key:normalize key-sequence)
		binding (key-binding key-sequence)))
	(and (stringp key-sequence)
	     (or (and binding (not (integerp binding)))
		 (kbd-key:special-sequence-p key-sequence))
	     (ibut:label-set seq-and-pos)
	     (hact 'kbd-key key-sequence))))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kbd-key:act (key-sequence)
  "Executes the command binding for normalized KEY-SEQUENCE.
Returns t if KEY-SEQUENCE has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (setq current-prefix-arg nil) ;; Execution of the key-sequence may set it.
  (let ((binding (key-binding key-sequence)))
    (cond ((null binding)
	   ;; If this is a special key seqence, execute it by adding
	   ;; its keys to the stream of unread command events.
	   (when (kbd-key:special-sequence-p key-sequence)
	     (setq unread-command-events (nconc unread-command-events (mapcar 'identity key-sequence)))
	     t))
	  ((memq binding '(action-key action-mouse-key hkey-either))
	   (beep)
	   (message "(kbd-key:act): This key does what the Action Key does.")
	   t)
	  (t (call-interactively binding) t))))

(defun kbd-key:doc (key-sequence &optional full)
  "Shows first line of doc for binding of keyboard KEY-SEQUENCE in minibuffer.
With optional prefix arg FULL, displays full documentation for command."
  (interactive "kKey sequence: \nP")
  (let* ((keys (kbd-key:normalize key-sequence))
	 (cmd  (let ((cmd (key-binding keys)))
		 (if (not (integerp cmd)) cmd)))
	 (doc (and cmd (documentation cmd)))
	 (end-line))
    (cond (cmd
	   (if doc
	       (or full
		   (setq end-line (string-match "[\n]" doc)
			 doc (substitute-command-keys (substring doc 0 end-line))))
	     (setq doc (format "No documentation for {%s} %s" key-sequence (or cmd ""))))
	   (if (and cmd doc)
	       (if full
		   (describe-function cmd)
		 (message doc))))
	  ((setq doc (hui:menu-doc keys (not full)))
	   (if full
	       (hui:menu-help doc)
	     (message doc)))
	  (t (error "(kbd-key:doc): No binding found for keys {%s}" key-sequence)))))

(defun kbd-key:help (but)
  "Display documentation for binding of keyboard key given by BUT's label."
  (let ((kbd-key (hbut:key-to-label (hattr:get but 'lbl-key))))
    (if kbd-key (kbd-key:doc kbd-key t))))

(defun kbd-key:normalize (key-sequence)
  "Returns KEY-SEQUENCE string (without surrounding {}) normalized into a form that can be parsed by commands."
  (interactive "kKeyboard key sequence to normalize (no {}): ")
  (if (stringp key-sequence)
      (let ((norm-key-seq (copy-sequence key-sequence))
	    (case-fold-search nil)
	    (case-replace t)
	    (substring)
	    (arg))
	(setq norm-key-seq (hypb:replace-match-string
			    "@key{DEL}\\|<DEL>\\|\\<DEL\\>" norm-key-seq "\177" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{RET}\\|<RET>\\|@key{RTN}\\|\\<RETURN\\>\\|\\<RET\\>\\|\\<RTN\\>"
			    norm-key-seq "$#@!" t)
	      norm-key-seq (hypb:replace-match-string
			    "\\<ESC\s-*ESC\\>" norm-key-seq "\233" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{ESC}\\|<ESC>\\|\\<ESC\\(APE\\)?\\>" norm-key-seq "M-" t)
	      norm-key-seq (hypb:replace-match-string
			    "C-M-" norm-key-seq "M-C-" t)
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "(" ")")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "\\[" "\\]")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "<" ">")
	      norm-key-seq (kbd-key:mark-spaces-to-keep norm-key-seq "\"" "\"")
	      norm-key-seq (hypb:replace-match-string "\\\\ " norm-key-seq "\0\0\0" t)
	      norm-key-seq (hypb:replace-match-string
			    "[ \t\n\r]+" norm-key-seq "" t)
	      norm-key-seq (hypb:replace-match-string
			    "\0\0\0\\|@key{SPC}\\|<SPC>\\|\\<SPC\\>" norm-key-seq "\040" t)
	      norm-key-seq (hypb:replace-match-string "$#@!" norm-key-seq "\015" t)
	      ;; Unqote special {} chars.
	      norm-key-seq (hypb:replace-match-string "\\\\\\([{}]\\)"
						      norm-key-seq "\\1"))
	(while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-seq)
	  (setq arg
		(string-to-number (substring norm-key-seq (match-beginning 2)
					     (match-end 2)))
		norm-key-seq (substring norm-key-seq (match-end 0))))
	(let (arg-val)
	  (while (string-match "\\`C-u" norm-key-seq)
	    (if (or (not (listp arg))
		    (not (integerp (setq arg-val (car arg)))))
		(setq arg '(1)
		      arg-val 1))
	    (setq arg-val (* arg-val 4)
		  arg (cons arg-val nil)
		  norm-key-seq (substring norm-key-seq (match-end 0)))))
	(if arg (setq norm-key-seq (concat (format "\025%s" arg) norm-key-seq)))
	;;
	;; Quote Control and Meta key names
	(setq norm-key-seq (hypb:replace-match-string
			    "C-\\(.\\)" norm-key-seq
			    (lambda (str)
			      (char-to-string
			       (1+ (- (downcase
				       (string-to-char
					(substring str (match-beginning 1)
						   (1+ (match-beginning 1)))))
				      ?a)))))
	      norm-key-seq (hypb:replace-match-string
			    "M-\\(.\\)" norm-key-seq
			    (lambda (str)
			      (concat "" (substring str (match-beginning 1)
						      (1+ (match-beginning 1))))))))
    (error "(kbd-key:normalize): requires a string argument, not `%s'" key-sequence)))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun kbd-key:extended-command-p (key-sequence)
  "Returns non-nil if the string KEY-SEQUENCE is a normalized extended command invocation, i.e. M-x command."
  (and (stringp key-sequence) (string-match kbd-key:extended-command-prefix key-sequence)))
  
(defun kbd-key:hyperbole-hycontrol-key-p (key-sequence)
  "Returns t if normalized KEY-SEQUENCE is given when in a HyControl mode, else nil.
Allows for multiple key sequences strung together."
  (and key-sequence
       (featurep 'hycontrol)
       (or hycontrol-windows-mode hycontrol-frames-mode)
       ;; If wanted to limit to single key bindings and provide tighter checking:
       ;;   (string-match "[-.0-9]*\\(.*\\)" key-sequence)
       ;;   (key-binding (match-string 1 key-sequence))
       t))

(defun kbd-key:hyperbole-mini-menu-key-p (key-sequence)
  "Returns t if normalized KEY-SEQUENCE appears to invoke a Hyperbole menu item or sequence of keys, else nil."
  (when key-sequence
    (let ((mini-menu-key (kbd-key:normalize (key-description (car (where-is-internal 'hyperbole))))))
      (if (string-match (regexp-quote mini-menu-key) key-sequence) t))))

(defun kbd-key:key-and-arguments (key-sequence)
  "Returns t if normalized KEY-SEQUENCE appears to be a bound key sequence possibly with following interactive arguments, else nil."
  (let ((prefix-binding (and (stringp key-sequence) (key-binding (substring key-sequence 0 1)))))
       ;; Just ensure that 1st character is bound to something that is
       ;; not a self-insert-command or a number.
    (and prefix-binding
	 (not (or (integerp prefix-binding)
		  (eq prefix-binding 'self-insert-command)))
	 t)))

(defun kbd-key:mark-spaces-to-keep (string start-delim end-delim)
  "Return STRING with all spaces between any START-DELIM string and END-DELIM string marked for non-replacement."
  (let ((regexp (format "\\(%s\\S-*\\)\\s-\\(.*%s\\)"
			start-delim end-delim))
	(start 0)
	(end)
	(substring))
    (while (string-match regexp string start)
      (setq start (match-beginning 0)
	    end (match-end 0)
	    substring (match-string 0 string)
	    string (concat (substring string 0 start)
			   (hypb:replace-match-string "\\s-" substring "\0\0\0" t)
			   (if (< end (length string))
			       (substring string end)
			     ""))
	    start end))
    string))

(defun kbd-key:special-sequence-p (key-sequence)
  "Returns non-nil if normalized KEY-SEQUENCE string is one of the following:
  a Hyperbole minibuffer menu item key sequence,
  a HyControl key sequence,
  a M-x extended command,
  or a valid key sequence together with its interactive arguments."
  (or (kbd-key:hyperbole-mini-menu-key-p key-sequence)
      (kbd-key:hyperbole-hycontrol-key-p key-sequence)
      (kbd-key:extended-command-p key-sequence)
      (kbd-key:key-and-arguments key-sequence)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst kbd-key:extended-command-prefix
  (kbd-key:normalize (key-description (where-is-internal 'execute-extended-command (current-global-map) t)))
  "Normalized prefix string that invokes an extended command; typically ESC x.")

(provide 'hib-kbd)

;;; hib-kbd.el ends here
