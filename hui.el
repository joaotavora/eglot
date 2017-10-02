;;; hui.el ---  GNU Hyperbole button and hyperlink user interface
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Sep-91 at 21:42:03
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

(require 'hargs)
(require 'set)
(require 'hmail)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hui:ebut-delete-confirm-p t
  "*Non-nil means prompt before interactively deleting explicit buttons."
  :type 'boolean
  :group 'hyperbole-buttons)

(defcustom hui:ebut-prompt-for-action nil
  "*Non-nil means prompt for a button-specific action when creating buttons."
  :type 'boolean
  :group 'hyperbole-buttons)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hui:bind-key (cmd &optional new-key)
  "Remove existing global key binding for CMD, rebind it to optional NEW-KEY (prompted for) and confirm the change."
  (interactive "CCommand to change key binding of: \nKNew key to bind: ")
  (if (not (functionp cmd))
      (error "(hui:bind-key): Invalid command, `%s'" cmd))
  (let* ((old-key (where-is-internal cmd global-map t))
	 ;; Force multi-character key sequences to echo in the minibuffer
	 (echo-keystrokes 1)
	 old-key-text
	 new-key-text)
    (if old-key (setq old-key-text (key-description old-key)))
    (if (null new-key)
	(setq new-key
	      (with-selected-window (minibuffer-window)
		(read-key-sequence
		 (if old-key
		     (format "{%s} runs `%s'; change it to key: " old-key-text cmd)
		   (format "New key to run `%s': " cmd))))))
    (cond ((equal new-key (kbd "\C-g"))
	   (keyboard-quit))
	  (new-key (global-set-key new-key cmd)
		   (setq new-key-text (key-description new-key))))
    (if old-key
	(progn (global-unset-key old-key)
	       (message "{%s} now runs `%s'; prior {%s} binding removed" new-key-text cmd old-key-text))
      (message "{%s} now runs `%s'" new-key-text cmd))))

(defun hui:ebut-create (&optional start end)
  "Creates an explicit but starting from label between optional START and END.
Indicates by delimiting and adding any necessary instance number of the button
label."
  (interactive (list (and (marker-position (hypb:mark-marker t))
			  (region-beginning))
		     (and (marker-position (hypb:mark-marker t))
			  (region-end))))
  (let ((default-lbl) lbl but-buf actype)
    (save-excursion
      (setq default-lbl
	    (hui:hbut-label-default start end (not (called-interactively-p 'interactive)))
	    lbl (hui:hbut-label default-lbl "ebut-create"))
      (if (not (equal lbl default-lbl)) (setq default-lbl nil))
      
      (setq but-buf (if default-lbl (current-buffer) (hui:ebut-buf)))
      (hui:buf-writable-err but-buf "ebut-create")
      
      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
      (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
      (setq actype (hui:actype))
      (hattr:set 'hbut:current 'actype actype)
      (hattr:set 'hbut:current 'args (hargs:actype-get actype))
      (hattr:set 'hbut:current 'action
		 (and hui:ebut-prompt-for-action (hui:action actype))))
    (ebut:operate lbl nil)
    (when (called-interactively-p)
      (hui:ebut-message nil))))

(defun hui:ebut-delete (but-key &optional key-src)
  "Deletes explicit Hyperbole button given by BUT-KEY in optional KEY-SRC.
KEY-SRC may be a buffer or a pathname, when nil the current buffer is used.
Returns t if button is deleted, nil if user chooses not to delete or signals
an error otherwise.  If called interactively, prompts user whether to delete
and derives BUT-KEY from the button that point is within.
Signals an error if point is not within a button."
  (interactive (list (if (ebut:at-p)
			 (hattr:get 'hbut:current 'lbl-key)
		       nil)))
  (cond ((null but-key)
	 (hypb:error
	  "(ebut-delete): Point is not over the label of an existing button."))
	((not (stringp but-key))
	 (hypb:error
	  "(ebut-delete): Invalid label key argument: '%s'." but-key)))
  (let ((interactive (called-interactively-p 'interactive)))
    (if (and hui:ebut-delete-confirm-p interactive)
	(if (y-or-n-p (format "Delete button %s%s%s? "
			      ebut:start
			      (hbut:key-to-label but-key) ebut:end))
	    (hui:ebut-delete-op interactive but-key key-src)
	  (message ""))
      (hui:ebut-delete-op interactive but-key key-src))))
      
(defun hui:ebut-edit ()
  "Creates or modifies an explicit Hyperbole button when conditions are met.
A region must have been delimited with the action-key and point must now be
within it before this function is called or it will do nothing.  The region
must be no larger than the size given by 'ebut:max-len'.  It must be entirely
within or entirely outside of an existing explicit button.  When region is
within the button, the button is interactively modified.  Otherwise, a new
button is created interactively with the region as the default label."
  (interactive)
  (let ((m (marker-position (hypb:mark-marker t)))
	(op action-key-depress-prev-point) (p (point)) (lbl-key))
    (if (and m (eq (marker-buffer m) (marker-buffer op))
	     (< op m) (<= (- m op) ebut:max-len)
	     (<= p m) (<= op p))
	(progn
	  (if (setq lbl-key (ebut:label-p))
	      (hui:ebut-modify lbl-key)
	    (hui:ebut-create op m))
	  t))))

(defun hui:ebut-modify (lbl-key)
  "Modifies an explicit Hyperbole button given by LBL-KEY.
Signals an error when no such button is found in the current buffer."
  (interactive (list (save-excursion
		       (hui:buf-writable-err (current-buffer) "ebut-modify")
		       (or (ebut:label-p)
			   (ebut:label-to-key
			    (hargs:read-match "Button to modify: "
					      (ebut:alist) nil t
					      nil 'ebut))))))
  (let ((lbl (ebut:key-to-label lbl-key))
	(but-buf (current-buffer))
	actype but new-lbl)
    (save-excursion
      (or (called-interactively-p 'interactive)
	  (hui:buf-writable-err but-buf "ebut-modify"))
      
      (or (setq but (ebut:get lbl-key but-buf))
	  (progn (pop-to-buffer but-buf)
		 (hypb:error "(ebut-modify): Invalid button, no data for '%s'." lbl)))
      
      (setq new-lbl
	    (hargs:read
	     "Change button label to: "
	     (lambda (lbl)
	       (and (not (string= lbl "")) (<= (length lbl) ebut:max-len)))
	     lbl
	     (format "(ebut-modify): Enter a string of at most %s chars."
		     ebut:max-len)
	     'string))
      
      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
      (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
      (setq actype (hui:actype (hattr:get but 'actype)))
      (hattr:set 'hbut:current 'actype actype)
      (hattr:set 'hbut:current 'args (hargs:actype-get actype 'modifying))
      (hattr:set 'hbut:current 'action
		 (and hui:ebut-prompt-for-action (hui:action actype))))
    (ebut:operate lbl new-lbl)
    (if (called-interactively-p)
	(hui:ebut-message t))))

(defun hui:ebut-rename (curr-label new-label)
  "Renames explicit Hyperbole button given by CURR-LABEL to NEW-LABEL.
If called interactively when point is not within an explicit button:
   prompts for old and new button label values and performs rename.
If called interactively when point is within an explicit button:
   saves button label and tells user to edit label, then call again.
   second call changes the button's name from the stored value to the
   edited value.
Signals an error if any problem occurs."
  (interactive
   (save-excursion
     (let (curr-label new-label)
       (hui:buf-writable-err (current-buffer) "ebut-rename")
       (if hui:ebut-label-prev
	   (setq curr-label hui:ebut-label-prev
		 new-label (ebut:label-p 'as-label))
	 (setq new-label nil
	       curr-label 
	       (or (ebut:label-p 'as-label)
		   (let ((buts (ebut:alist)))
		     (if (null buts)
			 (hypb:error "(ebut-rename): No explicit buttons in buffer.")
		       (prog1 (hargs:read-match
			       "Button label to rename: "
			       buts nil t nil 'ebut)
			 (setq new-label
			       (hargs:read
				"Rename button label to: "
			        (lambda (lbl)
				  (and (not (string= lbl ""))
				       (<= (length lbl) ebut:max-len)))
				curr-label
				(format
				 "(ebut-rename): Use a quoted string of at most %s chars."
				 ebut:max-len)
				'string))))))))
       (list curr-label new-label))))

  (save-excursion
    (if (called-interactively-p 'interactive)
	nil
      (hui:buf-writable-err (current-buffer) "ebut-rename")
      (if (or (not (stringp curr-label)) (string= curr-label ""))
	  (hypb:error "(ebut-rename): 'curr-label' must be a non-empty string: %s"
		 curr-label))
      (and (stringp new-label) (string= new-label "")
	   (hypb:error "(ebut-rename): 'new-label' must be a non-empty string: %s"
		  new-label)))
    (or (ebut:get (ebut:label-to-key curr-label))
	(hypb:error "(ebut-rename): Can't rename %s since no button data."
	       curr-label))
    )
  (cond (new-label
	 (ebut:operate curr-label new-label)
	 (setq hui:ebut-label-prev nil)
	 (message "Renamed from '%s' to '%s'." curr-label new-label))
	(curr-label
	 (setq hui:ebut-label-prev curr-label)
	 (message "Edit button label and use same command to finish rename."))
	(t (hypb:error "(ebut-rename): Move point to within a button label."))))

(defun hui:ebut-search (string &optional match-part)
  "Shows lines of files/buffers containing an explicit but match for STRING.
Returns number of buttons matched and displayed.
By default, only matches for whole button labels are found, optional MATCH-PART
enables partial matches.  The match lines are shown in a buffer which serves as
a menu to find any of the occurrences."
  (interactive (list (read-string "Search for button string: ")
		     (y-or-n-p "Enable partial matches? ")))
  (if (not (stringp string))
      (hypb:error "(ebut-search): String to search for is required."))
  (let*  ((prefix (if (> (length string) 14)
		      (substring string 0 13) string))
	  (out-buf (get-buffer-create (concat "*" prefix " Hypb Search*")))
	  (total (ebut:search string out-buf match-part)))
    (if (> total 0)
	(progn
	  (set-buffer out-buf)
	  (moccur-mode)
	  (if (fboundp 'outline-minor-mode)
	      (and (progn (goto-char 1)
			  (search-forward "\C-m" nil t))
		   (outline-minor-mode 1)))
	  (if (fboundp 'hproperty:but-create)
	      (hproperty:but-create nil nil (regexp-quote
				      (if match-part string
					(concat ebut:start string ebut:end)))))
	  (goto-char (point-min))
	  (pop-to-buffer out-buf)
	  (if (called-interactively-p 'interactive) (message "%d match%s." total
				       (if (> total 1) "es" ""))
	    total))
      (if (called-interactively-p 'interactive) (message "No matches.")
	total))))

(defun hui:error (&rest args)
  (hypb:error "(hui:error): Obsolete, use hypb:error instead."))

(defun hui:gbut-create (lbl)
  "Creates Hyperbole global button with LBL."
  (interactive "sCreate global button labeled: ")
  (let (but-buf actype)
    (save-excursion
      (setq actype (hui:actype))
      (setq but-buf (set-buffer (find-file-noselect gbut:file)))
      (hui:buf-writable-err but-buf "ebut-create")
      ;; This prevents movement of point which might be useful to user.
      (save-excursion
	(goto-char (point-max))
	(hattr:set 'hbut:current 'loc (hui:key-src but-buf))
	(hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
	(hattr:set 'hbut:current 'actype actype)
	(hattr:set 'hbut:current 'args (hargs:actype-get actype))
	(hattr:set 'hbut:current 'action
		   (and hui:ebut-prompt-for-action (hui:action actype)))
	(setq lbl (concat lbl (ebut:operate lbl nil)))
	(goto-char (point-max))
	(insert "\n")
	(save-buffer)
	)
      (message "%s created." lbl)
      )))

(defun hui:gbut-modify (lbl-key)
  "Modifies a global Hyperbole button given by LBL-KEY.
Signals an error when no such button is found."
  (interactive (list (save-excursion
		       (hui:buf-writable-err
			(find-file-noselect gbut:file) "gbut-modify")
		       (hbut:label-to-key
			(hargs:read-match "Global button to modify: "
					  (mapcar 'list (gbut:label-list))
					  nil t nil 'ebut)))))
  (let ((lbl (hbut:key-to-label lbl-key))
	(but-buf (find-file-noselect gbut:file))
	actype but new-lbl)
    (save-excursion
      (or (called-interactively-p 'interactive)
	  (hui:buf-writable-err but-buf "gbut-modify"))
      
      (or (setq but (ebut:get lbl-key but-buf))
	  (progn (pop-to-buffer but-buf)
		 (hypb:error
		  "(gbut-modify): Invalid button, no data for '%s'." lbl)))
      
      (setq new-lbl
	    (hargs:read
	     "Change global button label to: "
	     (lambda (lbl)
	       (and (not (string= lbl "")) (<= (length lbl) ebut:max-len)))
	     lbl
	     (format "(gbut-modify): Enter a string of at most %s chars."
		     ebut:max-len)
	     'string))
      
      (hattr:set 'hbut:current 'loc (hui:key-src but-buf))
      (hattr:set 'hbut:current 'dir (hui:key-dir but-buf))
      (setq actype (hui:actype (hattr:get but 'actype)))
      (hattr:set 'hbut:current 'actype actype)
      (hattr:set 'hbut:current 'args (hargs:actype-get actype 'modifying))
      (hattr:set 'hbut:current 'action
		 (and hui:ebut-prompt-for-action (hui:action actype)))
      (set-buffer but-buf)
      (ebut:operate lbl new-lbl))))

(defun hui:hbut-act (&optional but)
  "Executes action for optional Hyperbole button symbol BUT in current buffer.
Default is the current button."
  (interactive
   (let ((but (hbut:at-p)) (lst))
     (list
      (cond (but)
	    ((setq lst (ebut:alist))
	     (ebut:get (ebut:label-to-key
			(hargs:read-match "Button to execute: " lst nil t
					  (ebut:label-p 'as-label) 'ebut))))
	    (t (hypb:error "(hbut-act): No explicit buttons in buffer."))))))
  (cond ((and (called-interactively-p 'interactive) (null but))
	 (hypb:error "(hbut-act): No current button to activate."))
	((not (hbut:is-p but))
	 (hypb:error "(hbut-act): Button is invalid; it has no attributes."))
	(t (or but (setq but 'hbut:current))
	   (hui:but-flash) (hbut:act but))))

(defun hui:hbut-current-act ()
  "Activate Hyperbole button at point or signal an error if there is no such button."
  (interactive)
  (let ((but (hbut:at-p)))
    (cond ((null but)
	   (hypb:error "(hbut-act): No current button to activate."))
	  ((not (hbut:is-p but))
	   (hypb:error "(hbut-act): Button is invalid; it has no attributes."))
	  (t (hui:but-flash) (hbut:act but)))))

(defun hui:hbut-help (&optional but)
  "Checks for and explains an optional button given by symbol, BUT.
BUT defaults to the button whose label point is within."
  (interactive)
  (setq but (or but (hbut:at-p)
		(ebut:get (ebut:label-to-key
			   (hargs:read-match "Help for button: "
					     (ebut:alist) nil t nil 'ebut)))))
  (or but
      (hypb:error "(hbut-help):  Move point to a valid Hyperbole button."))
  (if (not (hbut:is-p but))
      (cond (but (hypb:error "(hbut-help): Invalid button."))
	    (t   (hypb:error
		  "(hbut-help): Not on an implicit button and no buffer explicit buttons."))))
  (let ((type-help-func (intern-soft
			 (concat 
			  (htype:names 'ibtypes (hattr:get but 'categ))
			  ":help"))))
    (or (equal (hypb:indirect-function 'hui:but-flash)
	       (lambda nil))
	;; Only flash button if point is on it.
	(let ((lbl-key (hattr:get but 'lbl-key)))
	  (and lbl-key
	       (or (equal lbl-key (ebut:label-p))
		   (equal lbl-key (ibut:label-p)))
	       (hui:but-flash))))
    (if (functionp type-help-func)
	(funcall type-help-func but)
      (let ((total (hbut:report but)))
	(if total (hui:help-ebut-highlight))))))

(defun hui:hbut-label (default-label func-name)
  "Reads button label from user using DEFAULT-LABEL and caller's FUNC-NAME."
  (hargs:read "Button label: "
	      (lambda (lbl)
		(and (not (string= lbl "")) (<= (length lbl) ebut:max-len)))
	      default-label
	      (format "(%s): Enter a string of at most %s chars."
		      func-name ebut:max-len)
	      'string))

(defun hui:hbut-label-default (start end &optional skip-len-test)
  "Returns default label based on START and END region markers or points.
Optional SKIP-LEN-TEST means don't limit label to `ebut:max-len' length.
Returns nil if START or END are invalid or if region fails length test. 

Also has side effect of moving point to start of default label, if any."
  (if (markerp start) (setq start (marker-position start)))
  (if (markerp end) (setq end (marker-position end)))
  ;; Test whether to use region as default button label.
  (if (and (integerp start) (integerp end) 
	   (or skip-len-test
	       (<= (max (- end start) (- start end)) ebut:max-len)))
      (progn (goto-char start)
	     (buffer-substring start end))))

(defun hui:hbut-report (&optional arg)
  "Pretty prints attributes of current button, using optional prefix ARG.
See 'hbut:report'."
  (interactive "P")
  (if (and arg (symbolp arg))
      (hui:hbut-help arg)
    (let ((total (hbut:report arg)))
      (if total
	  (progn (hui:help-ebut-highlight)
		 (message "%d button%s." total (if (/= total 1) "s" "")))))))

(defalias 'hui:hbut-summarize 'hui:hbut-report)

(defun hui:link-directly ()
  "Creates a Hyperbole link button at depress point, linked to release point.
See also documentation for `hui:link-possible-types'."
  (let* ((link-types (hui:link-possible-types))
	 (but-window action-key-depress-window)
	 (num-types (length link-types))
	 (release-window (selected-window))
	 (but-modify nil)
	 type-and-args lbl-key but-loc but-dir)
    (select-window action-key-depress-window)
    (hui:buf-writable-err (current-buffer) "link-directly")
    (if (ebut:at-p)
	(setq but-modify t
	      but-loc (hattr:get 'hbut:current 'loc)
	      but-dir (hattr:get 'hbut:current 'dir)
	      lbl-key (hattr:get 'hbut:current 'lbl-key))
      (setq but-loc (hui:key-src (current-buffer))
	    but-dir (hui:key-dir (current-buffer))
	    lbl-key (hbut:label-to-key
		     (hui:hbut-label
		      (cond ((hmouse-prior-active-region)
			     hkey-region)
			    ((marker-position (hypb:mark-marker t))
			     (hui:hbut-label-default
			      (region-beginning) (region-end))))
		      "link-directly"))))
    (select-window release-window)

    (cond ((= num-types 0)
	   (error "(link-directly): No possible link type to create."))
	  ((= num-types 1)
	   (setq type-and-args (hui:list-remove-text-properties (car link-types)))
	   (hui:link-create but-modify but-window lbl-key but-loc but-dir type-and-args))
	  (t ;; more than 1
	    (let ((item)
		  type)
	      (setq type-and-args
		    (hui:menu-select
		     (cons '("Link to>")
			   (mapcar
			    (lambda (type-and-args)
			      (setq type (car type-and-args))
			      (list 
			       (capitalize
				(if (string-match
				     "^\\(link-to\\|eval\\)-"
				     (setq item (symbol-name type)))
				    (setq item (substring
						item (match-end 0)))
				  item))
			       type-and-args
			       (documentation
				(intern (concat "actypes::"
						(symbol-name type))))))
			    link-types)))
		    type-and-args (hui:list-remove-text-properties type-and-args))
	      (hui:link-create
		but-modify but-window
		lbl-key but-loc but-dir type-and-args))))
    (hui:ebut-message but-modify)))

;;; ************************************************************************
;;; Private functions - used only within Hyperbole
;;; ************************************************************************

(defun hui:action (actype &optional prompt)
  "Prompts for and returns an action to override action from ACTYPE."
  (and actype
       (let* ((act) (act-str)
	      (params (actype:params actype))
	      (params-no-keywords (actype:param-list actype))
	      (params-str (and params (concat " " (prin1-to-string params))))
	      )
	 (while (progn
		 (while (and (setq act-str
				   (hargs:read
				    (or prompt (concat "Action" params-str
						       ": ")) nil nil
						       nil 'string))
			     (not (string= act-str ""))
			     (condition-case ()
				 (progn (setq act (read act-str)) nil)
			       (error
				(beep) (message "Invalid action syntax.")
				(sit-for 3) t))))
		 (and (not (symbolp act))
		      params-no-keywords
		      ;; Use the weak condition that action must
		      ;; involve at least one of actype's parameters
		      ;; or else we assume the action is invalid, tell
		      ;; the user and provide another chance for entry.
		      (not (memq t
				 (mapcar
				  (lambda (param)
				    (setq param (symbol-name param))
				    (and (string-match
					  (concat "[\( \t\n\r,']"
						  (regexp-quote param)
						  "[\(\) \t\n\r\"]")
					  act-str)
					 t))
				  params-no-keywords)))
		      ))
	   (beep) (message "Action must use at least one parameter.")
	   (sit-for 3))
	 (let (head)
	   (while (cond ((listp act)
			 (and act (setq head (car act))
			      (not (memq head '(lambda defun defmacro defsubst defin)))
			      (setq act (list 'lambda params act))
			      nil  ;; terminate loop
			      ))
			((symbolp act)
			 (setq act (cons act params-no-keywords)))
			((stringp act)
			 (setq act (action:kbd-macro act 1)))
			;; Unrecognized form
			(t (setq act nil))
			)))
	 act)))

(defun hui:actype (&optional default-actype prompt)
  "Using optional DEFAULT-ACTYPE, PROMPTs for a button action type.
DEFAULT-ACTYPE may be a valid symbol or symbol-name."
  (and default-actype (symbolp default-actype)
       (progn
	 (setq default-actype (symbol-name default-actype))
	 (if (string-match "actypes::" default-actype)
	     (setq default-actype (substring default-actype (match-end 0))))))
  (if (or (null default-actype) (stringp default-actype))
      (intern-soft
       (concat "actypes::"
	       (hargs:read-match (or prompt "Button's action type: ")
				(mapcar 'list (htype:names 'actypes))
				nil t default-actype 'actype)))
    (hypb:error "(actype): Invalid default action type received.")))

(defun hui:buf-writable-err (but-buf func-name)
  "If BUT-BUF is read-only, signal an error from FUNC-NAME."
  (let ((obuf (prog1 (current-buffer) (set-buffer but-buf)))
	;; (unwritable (and buffer-file-name
	;;		 (not (file-writable-p buffer-file-name))))
	(err))
    ;; (if unwritable
    ;;     Commented error out since some people want to be able to create
    ;;     buttons within files which they have purposely marked read-only.
    ;;     (setq err 
    ;;	     (format "(ebut-modify): You are not allowed to modify '%s'."
    ;;		     (file-name-nondirectory buffer-file-name))))
    (if buffer-read-only
	(setq err
	      (format "Button buffer '%s' is read-only.  Use {%s} to change it."
		      (buffer-name but-buf) (hmouse-read-only-toggle-key))))
    (set-buffer obuf)
    (if err (progn (pop-to-buffer but-buf) (hypb:error err)))))

(defun hui:ebut-buf (&optional prompt)
  "Prompt for and return a buffer in which to place a button."
  (let ((buf-name))
    (while
	(progn
	  (setq buf-name
		(hargs:read-match
		 (or prompt "Button's buffer: ")
		 (delq nil
		       (mapcar
		        (lambda (buf)
			  (let ((b (buffer-name buf)))
			    (if (and (not (string-match "mail\\*" b))
				     (not (string-match "\\*post-news\\*" b))
				     (string-match "\\`[* ]" b))
				nil 
			      (cons b nil))))
			(buffer-list)))
		 nil t (buffer-name) 'buffer))
	  (or (null buf-name) (equal buf-name "")))
      (beep))
  (get-buffer buf-name)))

(defun hui:ebut-delete-op (interactive but-key key-src)
  "INTERACTIVEly or not deletes explicit Hyperbole button given by BUT-KEY in KEY-SRC.
KEY-SRC may be a buffer or a pathname, when nil the current buffer is used.
Returns t if button is deleted, signals error otherwise.  If called
with INTERACTIVE non-nil, derives BUT-KEY from the button that point is
within."
  (let ((buf (current-buffer)) (ebut))
    (if (if interactive
	    (ebut:delete)
	  (cond ((or (null key-src) (and (bufferp key-src) (setq buf key-src)))
		 (setq ebut (ebut:get but-key key-src)))
		((and (stringp key-src)
		      (setq buf (find-file-noselect key-src)))
		 (setq ebut (ebut:get but-key buf)))
		(t (hypb:error "(ebut-delete): Invalid key-src: '%s'." key-src)))
	  (if ebut
	      (ebut:delete ebut)
	    (hypb:error "(ebut-delete): No valid %s button in %s."
		   (ebut:key-to-label but-key) buf))
	  )
	(progn (set-buffer buf)
	       (if interactive
		   (progn
		     (call-interactively 'hui:ebut-unmark)
		     (message "Button deleted."))
		 (hui:ebut-unmark but-key key-src))
	       (if (hmail:reader-p) (hmail:msg-narrow))
	       )
      (hypb:error "(ebut-delete): You may not delete buttons from this buffer."))))

(defun hui:ebut-delimit (start end instance-str)
  (hypb:error "(hui:ebut-delimit): Obsolete, use ebut:delimit instead."))

(defun hui:ebut-message (but-modify-flag)
  (let ((actype (symbol-name (hattr:get 'hbut:current 'actype)))
	(args (hattr:get 'hbut:current 'args)))
    (if (string-match "\\`actypes::" actype)
	(setq actype (intern (substring actype (match-end 0)))))
    (message "%s%s%s %s %S"
	     ebut:start
	     (hbut:key-to-label (hattr:get 'hbut:current 'lbl-key))
	     ebut:end
	     (if but-modify-flag "now executes" "executes")
	     (cons actype args))))

(defun hui:ebut-unmark (&optional but-key key-src directory)
  "Remove delimiters from button given by BUT-KEY in KEY-SRC of DIRECTORY.
All args are optional, the current button and buffer file are the defaults."
  (interactive)
  (let ((form (lambda ()
		(let ((buffer-read-only) start end)
		  (setq start (match-beginning 0)
			end (match-end 0))
		  (and (fboundp 'hproperty:but-delete)
		       (hproperty:but-delete start))
		  (skip-chars-backward " \t\n\r")
		  (skip-chars-backward "0-9")
		  (if (eq (preceding-char) (string-to-char ebut:instance-sep))
		      (setq start (1- (point))))
		  (if (search-backward ebut:start (- (point) ebut:max-len) t)
		      (if current-prefix-arg
			  ;; Remove button label, delimiters and preceding
			  ;; space, if any.
			  (delete-region (max (point-min)
					      (1- (match-beginning 0)))
					 end)
			;;
			;; Remove button delimiters only.
			;;
			;; Remove button ending delimiter
			(delete-region start end)
			;; Remove button starting delimiter
			(delete-region (match-beginning 0)
				       (match-end 0))))))))
    (if (called-interactively-p 'interactive)
	(save-excursion
	  (if (search-forward ebut:end nil t) (funcall form)))
      ;; Non-interactive invocation.
      (let ((cur-p))
	(if (and (or (null key-src) (eq key-src buffer-file-name))
		 (or (null directory) (eq directory default-directory)))
	    (setq cur-p t)
	  (set-buffer (find-file-noselect
			(expand-file-name key-src directory))))
	(save-excursion
	  (goto-char (point-min))
	  (if (re-search-forward (ebut:label-regexp but-key) nil t)
	      (progn (funcall form)
		     ;; If modified a buffer other than the current one,
		     ;; save it.
		     (or cur-p (save-buffer)))))))))

(defun hui:file-find (file-name)
  "If FILE-NAME is readable, finds it, else signals an error."
  (if (and (stringp file-name) (file-readable-p file-name))
      (find-file file-name)
    (hypb:error "(file-find): \"%s\" does not exist or is not readable."
	   file-name)))

(defun hui:hbut-term-highlight (start end)
  "For terminals only: Emphasize a button spanning from START to END."
  (save-restriction
    (save-excursion
      (goto-char start)
      (narrow-to-region (point-min) start)
      (sit-for 0)
      (setq inverse-video t)
      (goto-char (point-min))
      (widen)
      (narrow-to-region (point) end)
      (sit-for 0)
      (setq inverse-video nil)
      )))

(defun hui:hbut-term-unhighlight (start end)
  "For terminals only: Remove any emphasis from hyper-button at START to END."
  (save-restriction
    (save-excursion
      (goto-char start)
      (narrow-to-region (point-min) start)
      (sit-for 0)
      (setq inverse-video nil))))

(defun hui:help-ebut-highlight ()
  "Highlight any explicit buttons in help buffer associated with current buffer."
  (if (fboundp 'hproperty:but-create)
      (with-current-buffer (get-buffer (hypb:help-buf-name))
	(hproperty:but-create))))

(defun hui:htype-delete (htype-sym)
  "Deletes HTYPE-SYM from use in current Hyperbole session.
HTYPE-SYM must be redefined for use again."
  (and htype-sym (symbolp htype-sym)
       (let ((type
	      (intern (hargs:read-match
		       (concat "Delete from " (symbol-name htype-sym) ": ")
		       (mapcar 'list (htype:names htype-sym))
		       nil t nil htype-sym))))
	 (htype:delete type htype-sym))))

(defun hui:htype-help (htype-sym &optional no-sort)
  "Displays documentation for types from HTYPE-SYM which match to a regexp.
Optional NO-SORT means display in decreasing priority order (natural order)."
  (and htype-sym (symbolp htype-sym)
       (let* ((tstr (symbol-name htype-sym))
	      (tprefix (concat tstr "::"))
	      (buf-name (hypb:help-buf-name (capitalize tstr)))
	      (temp-buffer-show-hook
	       (lambda (buf)
		 (set-buffer buf) (goto-char (point-min))
		 (while (re-search-forward "^" nil t)
		   (replace-match "  " t nil))
		 (goto-char (point-min))
		 (while (search-forward (concat "  " tprefix) nil t)
		   (replace-match "" t nil))
		 (goto-char (point-min))
		 (set-buffer-modified-p nil)
		 (display-buffer buf nil)))
	      (temp-buffer-show-function temp-buffer-show-hook)
	      (names (htype:names htype-sym))
	      (term (hargs:read-match
		     (concat (capitalize tstr)
			     " to describe (RET for all): ")
		     (mapcar 'list (cons "" names))
		     nil t nil htype-sym))
	      nm-list
	      doc-list)
	 (setq nm-list
	       (if (string= term "")
		   (let ((type-names
			   (mapcar (lambda (nm) (concat tprefix nm))
				   names)))
		     (if no-sort type-names
		       (sort type-names 'string<)))
		 (cons (concat tprefix term) nil))
	       doc-list (delq nil (mapcar
				    (lambda (name)
				      (let ((doc (documentation
						  (intern-soft name))))
					(if doc (cons name doc))))
				    nm-list)))
	 (with-output-to-temp-buffer buf-name
	   (mapcar (lambda (nm-doc-cons)
		     (princ (car nm-doc-cons)) (terpri)
		     (princ (cdr nm-doc-cons)) (terpri))
		   doc-list)))))

(defun hui:htype-help-current-window (htype-sym &optional no-sort)
  "Displays in the current window, documentation for types from HTYPE-SYM which match to a regexp.
Optional NO-SORT means display in decreasing priority order (natural order)."
  (let ((display-buffer-alist
	 '(("\\`*Help" . ((lambda (buf _alist) (switch-to-buffer buf)))))))
    (hui:htype-help htype-sym no-sort)))

(defun hui:key-dir (but-buf)
  "Returns button key src directory based on BUT-BUF, a buffer."
  (if (bufferp but-buf)
      (let ((file (buffer-file-name but-buf)))
	(if file
	    (file-name-directory (hpath:symlink-referent file))
	  (cdr (assq 'default-directory (buffer-local-variables but-buf)))))
    (hypb:error "(hui:key-dir): '%s' is not a valid buffer.")))

(defun hui:key-src (but-buf)
  "Returns button key src location based on BUT-BUF, a buffer.
This is BUT-BUF when button data is stored in the buffer and the
button's source file name when the button data is stored externally."
  (with-current-buffer but-buf
    (cond ((hmail:mode-is-p) but-buf)
	  ((hpath:symlink-referent (buffer-file-name but-buf)))
	  (t but-buf))))

(defun hui:link-create (modify but-window lbl-key but-loc but-dir type-and-args)
  "Creates or modifies a new Hyperbole link button.
If MODIFY is non-nil, modifies button at point in BUT-WINDOW,
otherwise, prompts for button label and creates a button.
LBL-KEY is internal form of button label.  BUT-LOC is file or buffer
in which to create button.  BUT-DIR is directory of BUT-LOC.
TYPE-AND-ARGS is the action type for the button followed by any
arguments it requires.  Any text properties are removed from string arguments." 
  (hattr:set 'hbut:current 'loc but-loc)
  (hattr:set 'hbut:current 'dir but-dir)
  (hattr:set 'hbut:current 'actype (intern-soft
				     (concat "actypes::"
					     (symbol-name
					       (car type-and-args)))))
  (hattr:set 'hbut:current 'args (cdr type-and-args))
  (select-window but-window)
  (let ((label (ebut:key-to-label lbl-key)))
    (ebut:operate label (if modify label))))

(defun hui:link-possible-types ()
  "Returns list of possible link types for a Hyperbole button link to point.
Each list element is a list of the link type and any arguments it requires.

The link types considered are fixed; this function must be changed to alter
the contexts recognized.  Defining new link types will not alter the
possible types.

Referent Context         Possible Link Type Returned
----------------------------------------------------
Explicit Button          link-to-ebut
Info Index Item          link-to-Info-index-item
Info Node                link-to-Info-node
Mail Reader Message      link-to-mail
Directory Name           link-to-directory
File Name                link-to-file
Koutline Cell            link-to-kcell
Outline Heading          link-to-string-match
Buffer attached to File  link-to-file
Buffer without File      link-to-buffer-tmp"
;; Elisp Buffer at Start
;; or End of Sexpression    eval-elisp

  (let (val)
    (delq nil
	  (list (if (ebut:at-p)
		    (list 'link-to-ebut buffer-file-name (ebut:label-p)))
		(cond ((eq major-mode 'Info-mode)
		       (if (and Info-current-node
				(member Info-current-node
				       (Info-index-nodes Info-current-file))
				(Info-menu-item-at-p))
			   (let ((hargs:reading-p 'Info-index-item))
			     (list 'link-to-Info-index-item (hargs:at-p)))
			 (let ((hargs:reading-p 'Info-node))
			   (list 'link-to-Info-node (hargs:at-p)))))
		      ((hmail:reader-p)
		       (list 'link-to-mail
			     (list (rmail:msg-id-get) buffer-file-name))))
		(cond
		 ((let ((hargs:reading-p 'directory))
		    (setq val (hargs:at-p t)))
		  (list 'link-to-directory val))
		 ((let ((hargs:reading-p 'file))
		    (setq val (hargs:at-p t)))
		  (list 'link-to-file val (point)))
		 ((eq major-mode 'kotl-mode)
		  (list 'link-to-kcell buffer-file-name (kcell-view:idstamp)))
		 ;; If link is within an outline-regexp prefix, use
		 ;; a link-to-string-match.
		 ((and (boundp 'outline-regexp)
		       (stringp outline-regexp)
		       (save-excursion
			 (<= (point)
			     (progn
			       (beginning-of-line)
			       (if (looking-at outline-regexp)
				   (match-end 0)
				 0)))))
		  (save-excursion
		    (end-of-line)
		    (let ((heading (buffer-substring-no-properties
				    (point)
				    (progn (beginning-of-line) (point))))
			  (occur 1))
		      (while (search-backward heading nil t)
			(setq occur (1+ occur)))
		      (list 'link-to-string-match
			    heading occur buffer-file-name))))
		 (buffer-file-name
		  (list 'link-to-file buffer-file-name (point)))
		 (t (list 'link-to-buffer-tmp (buffer-name))))
		;;
		;; Deleted link to elisp possibility as it can embed
		;; long elisp functions in the button data file and
		;; possibly not parse them correctly.
		;;
		;; (and (fboundp 'smart-emacs-lisp-mode-p)
		;;      (smart-emacs-lisp-mode-p)
		;;      (or (eq (char-syntax (following-char)) ?\()
		;; 	 (eq (char-syntax (preceding-char)) ?\)))
		;;      (setq val (hargs:sexpression-p))
		;;      (list 'eval-elisp val))
		))))

(defun hui:list-remove-text-properties (lst)
  "Returns LST, a list, with text properties removed from any string elements."
  (mapcar (lambda (elt) (if (stringp elt) (substring-no-properties elt) elt))
	  lst))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(defvar hui:ebut-label-prev nil
  "String value of previous button name during an explicit button rename.
At other times, values must be nil.")

(provide 'hui)

;;; hui.el ends here
