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
  "Executes the command binding or the Hyperbole minibuffer menu action for KEY-SEQUENCE, delimited by {}.
Returns t if this is a valid KEY-SEQUENCE, else nil."
  (interactive "kKey sequence to execute (no {}): ")
  (kbd-key:act key-sequence))

(defib kbd-key ()
  "Executes a key sequence delimited by curly braces.
Key sequences should be in human readable form, e.g. {C-x C-b}, or what `key-description' returns.
Forms such as {\C-b}, {\^b}, and {^b} will not be recognized."
  (unless (or (br-in-browser)
	      (and (looking-at "[{}]") (/= ?\\ (preceding-char))))
    (let* ((seq-and-pos (or (hbut:label-p t "{`" "'}" t)
			    (hbut:label-p t "{" "}" t)
			    ;; Typical GNU manual key sequences; note
			    ;; these are special quote marks, not the
			    ;; standard ASCII characters.
			    (hbut:label-p t "‘" "’" t)))
	   (key-sequence (car seq-and-pos))
	   binding)
      (when (and (stringp key-sequence)
		 (not (eq key-sequence "")))
	(setq key-sequence (kbd-key:normalize key-sequence)
	      binding (key-binding key-sequence)))
      (and (if binding
	       (not (integerp binding))
	     (kbd-key:hyperbole-mini-menu-key-p key-sequence))
	   (ibut:label-set seq-and-pos)
	   (hact 'kbd-key key-sequence)))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun kbd-key:act (key-sequence)
  "Executes the command binding for normalized KEY-SEQUENCE.
Returns t if KEY-SEQUENCE has a binding, else nil."
  (interactive "kKeyboard key to execute (no {}): ")
  (setq current-prefix-arg nil) ;; kbd-key:normalize below sets it.
  (let ((binding (key-binding key-sequence)))
    (cond ((null binding)
	   ;; If this is a Hyperbole minibuffer menu item key sequence, execute it
	   ;; by adding its keys to the stream of unread command events.
	   (if (kbd-key:hyperbole-mini-menu-key-p key-sequence)
	       (setq unread-command-events (nconc unread-command-events (mapcar 'identity key-sequence)))))
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

(defun kbd-key:hyperbole-mini-menu-key-p (key-sequence)
  "Returns t if normalized KEY-SEQUENCE appears to invoke a Hyperbole menu item, else nil."
  (when key-sequence
    (let ((mini-menu-key (kbd-key:normalize (key-description (car (where-is-internal 'hyperbole))))))
      (if (string-match (regexp-quote mini-menu-key) key-sequence) t))))

(defun kbd-key:normalize (key-sequence)
  "Returns KEY-SEQUENCE string normalized into a form that can be parsed by commands."
  (interactive "kKeyboard key sequence to normalize (no {}): ")
  (if (stringp key-sequence)
      (let ((norm-key-seq (copy-sequence key-sequence))
	    (case-fold-search nil) (case-replace t))
	;; Quote Control and Meta key names
	(setq norm-key-seq (hypb:replace-match-string
			    "[ \t\n\r]+" norm-key-seq "" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{SPC}\\|<SPC>\\|SPC" norm-key-seq "\040" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{DEL}\\|<DEL>\\|DEL" norm-key-seq "\177" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{RET}\\|<RET>\\|@key{RTN}\\|RETURN\\|RET\\|RTN"
			    norm-key-seq "\015" t)
	      norm-key-seq (hypb:replace-match-string
			    "ESCESC" norm-key-seq "\233" t)
	      norm-key-seq (hypb:replace-match-string
			    "@key{ESC}\\|<ESC>\\|ESC" norm-key-seq "M-" t)
	      ;; Unqote special {} chars.
	      norm-key-seq (hypb:replace-match-string "\\\\\\([{}]\\)"
						      norm-key-seq "\\1"))
	(while (string-match "\\`\\(C-u\\|M-\\)\\(-?[0-9]+\\)" norm-key-seq)
	  (setq current-prefix-arg
		(string-to-number (substring norm-key-seq (match-beginning 2)
					     (match-end 2)))
		norm-key-seq (substring norm-key-seq (match-end 0))))
	(let (arg-val)
	  (while (string-match "\\`C-u" norm-key-seq)
	    (if (or (not (listp current-prefix-arg))
		    (not (integerp (setq arg-val (car current-prefix-arg)))))
		(setq current-prefix-arg '(1)
		      arg-val 1))
	    (setq arg-val (* arg-val 4)
		  current-prefix-arg (cons arg-val nil)
		  norm-key-seq (substring norm-key-seq (match-end 0)))))
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
			      (char-to-string (+ (downcase (string-to-char
							    (substring str (match-beginning 1)
								       (1+ (match-beginning 1)))))
						 128))))))
    (error "(kbd-key:normalize): requires a string argument, not `%s'" key-sequence)))

(provide 'hib-kbd)

;;; hib-kbd.el ends here
