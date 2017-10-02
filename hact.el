;;; hact.el --- GNU Hyperbole button action handling
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    18-Sep-91 at 02:57:09
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

(eval-and-compile (mapc #'require '(hhist hpath set)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar hrule:action 'actype:act
  "Value is a function of any number of arguments that executes actions.
Variable is used to vary actual effect of evaluating a Hyperbole action,
e.g. to inhibit actions.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; ========================================================================
;;; symset class - Hyperbole internal symbol set maintenance
;;; ========================================================================

(defun    symset:add (elt symbol prop)
  "Adds ELT to SYMBOL's PROP set.
Returns nil iff ELT is already in SET.  Uses `eq' for comparison."
  (let* ((set (get symbol prop))
	 (set:equal-op 'eq)
	 (new-set (set:add elt set)))
    (and new-set (put symbol prop new-set))))

(defalias    'symset:delete 'symset:remove)

(defun    symset:get (symbol prop)
  "Returns SYMBOL's PROP set."
  (get symbol prop))

(defun    symset:remove (elt symbol prop)
  "Removes ELT from SYMBOL's PROP set and returns the new set.
Assumes PROP is a valid set.  Uses `eq' for comparison."
  (let ((set (get symbol prop))
	(set:equal-op 'eq))
    (put symbol prop (set:remove elt set))))

;;; ========================================================================
;;; htype class - Hyperbole Types, e.g. action and implicit button types
;;; ========================================================================

(defun    htype:body (htype-sym)
  "Returns body for HTYPE-SYM.  If HTYPE-SYM is nil, returns nil."
  (and htype-sym (hypb:indirect-function htype-sym)))

(defun    htype:category (type-category)
  "Returns list of symbols in Hyperbole TYPE-CATEGORY in priority order.
Symbols contain category component.
TYPE-CATEGORY should be 'actypes, 'ibtypes or nil for all."
  (let ((types (symset:get type-category 'symbols))
	(categ-name (symbol-name type-category)))
    (mapcar (lambda (type)
	      (intern (concat categ-name "::" (symbol-name type))))
	    types)))

;; Thanks to JWZ for help on this.
(defmacro htype:create (type type-category doc params body property-list)
  "Creates a new Hyperbole TYPE within TYPE-CATEGORY (both unquoted symbols).
Third arg DOC is a string describing the type.
Fourth arg PARAMS is a list of parameters to send to the fifth arg BODY,
which is a list of forms executed when the type is evaluated.
Sixth arg PROPERTY-LIST is attached to the new type's symbol.

Returns the new function symbol derived from TYPE."
  (let* ((sym (htype:symbol type type-category))
	(action (nconc (list 'defun sym params doc) body)))
    `(progn
       ,action
       (setplist ',sym ,property-list)
       (symset:add ',type ',type-category 'symbols)
       (run-hooks 'htype-create-hook)
       ',sym)))

(defun    htype:delete (type type-category)
  "Deletes a Hyperbole TYPE derived from TYPE-CATEGORY (both symbols).
Returns the Hyperbole symbol for the TYPE if it existed, else nil."
  (let* ((sym (htype:symbol type type-category))
	 (exists (fboundp 'sym)))
    (setplist sym nil)
    (symset:delete type type-category 'symbols)
    (fmakunbound sym)
    (run-hooks 'htype-delete-hook)
    (and exists sym)))

(defun    htype:doc (type)
  "Returns documentation for Hyperbole TYPE, a symbol."
  (documentation type))

(defun    htype:names (type-category &optional sym)
  "Returns a list of the current names for Hyperbole TYPE-CATEGORY in priority order.
Names do not contain the category component.
TYPE-CATEGORY should be 'actypes, 'ibtypes or nil for all.
When optional SYM is given, returns the name for that symbol only, if any."
  (let ((types (symset:get type-category 'symbols))
	(sym-name (and sym (symbol-name sym))))
    (if sym-name
	;; Strip category from sym-name before looking for a match.
	(progn (if (string-match "::" sym-name)
		   (setq sym (intern (substring sym-name (match-end 0)))))
	       (if (memq sym types) (symbol-name sym)))
      (mapcar 'symbol-name types))))

;;; ------------------------------------------------------------------------

(defun   htype:symbol (type type-category)
  "Returns Hyperbole type symbol composed from TYPE and TYPE-CATEGORY (both symbols)."
  (intern (concat (symbol-name type-category) "::"
		  (symbol-name type))))

;;; ========================================================================
;;; action class
;;; ========================================================================

(defun action:commandp (function)
  "Returns interactive calling form if FUNCTION has one, else nil."
  (let ((action
	 (cond ((null function) nil)
	       ((symbolp function)
		(and (fboundp function)
		     (hypb:indirect-function function)))
	       ((and (listp function)
		     (eq (car function) 'autoload))
		(error "(action:commandp): Autoload not supported: %s" function))
	       (t function))))
    (cond ((and action (fboundp 'interactive-form))
	   (interactive-form action))
	  ((hypb:emacs-byte-code-p action)
	   (cond ((fboundp 'compiled-function-interactive)
		  (compiled-function-interactive action))
		 ((commandp action)
		  (list 'interactive (aref action 5)))))
	  (t (commandp action)))))

(defun action:create (param-list body)
  "Creates Hyperbole action defined by PARAM-LIST and BODY, a list of Lisp forms."
  (if (symbolp body)
      body
    (list 'function (cons 'lambda (cons param-list body)))))

(defun action:kbd-macro (macro &optional repeat-count)
  "Returns Hyperbole action that executes a keyboard MACRO REPEAT-COUNT times."
  (list 'execute-kbd-macro macro repeat-count))

;; This function is based on Emacs `help-function-arglist'.
(defun action:params-emacs (def)
  "Return the argument list for the function DEF which may be a symbol or a function body."
  ;; Handle symbols aliased to other symbols.
  (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
  ;; If definition is a macro, find the function inside it.
  (if (eq (car-safe def) 'macro) (setq def (cdr def)))
  (cond
   ((and (byte-code-function-p def) (listp (aref def 0))) (aref def 0))
   ((eq (car-safe def) 'lambda) (nth 1 def))
   ((eq (car-safe def) 'closure) (nth 2 def))
   ((or (and (byte-code-function-p def) (integerp (aref def 0)))
	(subrp def))
    (or (let* ((doc (condition-case nil (documentation def) (error nil)))
	       (docargs (if doc (car (help-split-fundoc doc nil))))
	       (arglist (if docargs
			    (cdar (read-from-string (downcase docargs)))))
	       (valid t))
	  ;; Check validity.
	  (dolist (arg arglist)
	    (unless (and (symbolp arg)
			 (let ((name (symbol-name arg)))
			   (if (eq (aref name 0) ?&)
			       (memq arg '(&rest &optional))
			     (not (string-match "\\." name)))))
	      (setq valid nil)))
	  (when valid arglist))
	(let* ((args-desc (if (not (subrp def))
			      (aref def 0)
			    (let ((a (subr-arity def)))
			      (logior (car a)
				      (if (numberp (cdr a))
					  (lsh (cdr a) 8)
					(lsh 1 7))))))
	       (max (lsh args-desc -8))
	       (min (logand args-desc 127))
	       (rest (logand args-desc 128))
	       (arglist ()))
	  (dotimes (i min)
	    (push (intern (concat "arg" (number-to-string (1+ i)))) arglist))
	  (when (> max min)
	    (push '&optional arglist)
	    (dotimes (i (- max min))
	      (push (intern (concat "arg" (number-to-string (+ 1 i min))))
		    arglist)))
	  (unless (zerop rest) (push '&rest arglist) (push 'rest arglist))
	  (nreverse arglist))))
   ((and (autoloadp def) (not (eq (nth 4 def) 'keymap)))
    ;; Force autoload to get function signature.
    (setq def (autoload-do-load def))
    (if (not autoloadp def)
	(action:params-emacs def)))))

(defun action:params (action)
  "Returns unmodified ACTION parameter list.
Autoloads action function if need be to get the parameter list."
  (when (and (symbolp action) (fboundp action))
    (setq action (hypb:indirect-function action)))
  (cond ((null action) nil)
	((listp action)
	 (if (eq (car action) 'autoload)
	     (error "(action:params): Autoload not supported: %s" action)
	   (car (cdr action))))
	((hypb:emacs-byte-code-p action)
	 (if (fboundp 'compiled-function-arglist)
	     (compiled-function-arglist action)
	   (action:params-emacs action)))
	((symbolp action)
	 (car (cdr (and (fboundp action) (hypb:indirect-function action)))))))

(defun action:param-list (action)
  "Returns list of actual ACTION parameters (removes `&' special forms)."
  (delq nil (mapcar (lambda (param)
		      (if (eq (aref (symbol-name param) 0) ?&)
			  nil param))
	      (action:params action))))

(defun action:path-args-abs (args-list &optional default-dirs)
  "Returns any paths in ARGS-LIST made absolute.
Uses optional DEFAULT-DIRS or `default-directory'.
Other arguments are returned unchanged."
  (mapcar (lambda (arg) (hpath:absolute-to arg default-dirs))
	  args-list))

(defun action:path-args-rel (args-list)
  "Returns any paths in ARGS-LIST below current directory made relative.
Other paths are simply expanded.  Non-path arguments are returned unchanged."
  (let ((dir (hattr:get 'hbut:current 'dir)))
    (mapcar (lambda (arg) (hpath:relative-to arg dir))
	    args-list)))


;;; ========================================================================
;;; action type class, actype
;;; ========================================================================

(defmacro hact (&rest args)
  "Performs action formed from rest of ARGS.
First arg may be a symbol or symbol name for either an action type or a
function.  Runs `action-act-hook' before performing action."
  (eval `(cons 'funcall (cons 'hrule:action ',args))))

(defun    actype:act (actype &rest args)
  "Performs action formed from ACTYPE and rest of ARGS and returns value.
If value is nil, however, t is returned instead, to ensure that implicit button
types register the performance of the action.  ACTYPE may be a symbol or symbol
name for either an action type or a function.  Runs `action-act-hook' before
performing ACTION."
  (let ((prefix-arg current-prefix-arg)
	(action (actype:action actype))
	(act '(apply action args)))
    (if (null action)
	(error "(actype:act): Null action for: `%s'" actype)
      ;; Next 2 lines are needed so that relative paths are expanded
      ;; properly.  But in rare cases, this can improperly expand simple
      ;; string arguments like "tags" as a pathname, when it is not
      ;; being used as a path.  So do this only if actype is a defact
      ;; and not a defun to limit any potential impact. RSW - 9/22/2017
      (if (symbolp action) (string-match "\\`actypes::" (symbol-name action))
	(setq args (action:path-args-abs args)))
      (let ((hist-elt (hhist:element)))
	(run-hooks 'action-act-hook)
	(prog1 (or (cond ((or (symbolp action) (listp action)
			      (hypb:emacs-byte-code-p action))
			  (eval act))
			 ((and (stringp action)
			       (let ((func (key-binding action)))
				 (if (not (integerp action))
				     (setq action func))))
			  (eval act))
			 (t (eval action)))
		   t)
	  (hhist:add hist-elt))
	))))

(defun    actype:action (actype)
  "Returns action part of ACTYPE (a symbol or symbol name).
ACTYPE may be a Hyperbole actype or Emacs Lisp function."
  (let (actname)
    (if (stringp actype)
	(setq actname actype
	      actype (intern actype))
      (setq actname (symbol-name actype)))
    (cond ((htype:body (if (string-match "\\`actypes::" actname)
			   actype
			 (intern-soft (concat "actypes::" actname)))))
	  ((fboundp actype) actype)
	  )))

(defmacro actype:create (type params doc &rest default-action)
  "Creates an action TYPE (an unquoted symbol) with PARAMS, described by DOC.
The type uses PARAMS to perform DEFAULT-ACTION (list of the rest of the
arguments).  A call to this function is syntactically the same as for
`defun',  but a doc string is required.
Returns symbol created when successful, else nil."
 (list 'htype:create type 'actypes doc params default-action nil))

(defalias 'defact 'actype:create)
(put      'actype:create 'lisp-indent-function 'defun)

(defun    actype:delete (type)
  "Deletes an action TYPE (a symbol).  Returns TYPE's symbol if it existed."
  (htype:delete type 'actypes))

(defun    actype:doc (hbut &optional full)
  "Returns first line of act doc for HBUT (a Hyperbole button symbol).
With optional FULL, returns full documentation string.
Returns nil when no documentation."
  (let* ((act (and (hbut:is-p hbut) (or (hattr:get hbut 'action)
					(hattr:get hbut 'actype))))
	 (but-type (hattr:get hbut 'categ))
	 (sym-p (and act (symbolp act)))
	 (end-line) (doc))
    (cond ((and but-type (fboundp but-type)
		(setq doc (htype:doc but-type)))
	   ;; Is an implicit button, so use its doc string if any.
	   )
	  (sym-p
	   (setq doc (htype:doc act))))
    (if (null doc)
	nil
      (setq doc (substitute-command-keys doc))
      (or full (setq end-line (string-match "[\n]" doc)
		     doc (substring doc 0 end-line))))
    doc))

(defun    actype:identity (&rest args)
  "Returns list of ARGS unchanged or if no ARGS, returns t.
Used as the setting of `hrule:action' to inhibit action evaluation."
  (or args t))

(defun    actype:interact (actype)
  "Interactively calls default action for ACTYPE.
ACTYPE is a symbol that was previously defined with `defact'.
Returns nil only when no action is found or the action has no interactive
calling form." 
  (let ((action (htype:body
		 (intern-soft (concat "actypes::" (symbol-name actype))))))
    (and action (action:commandp action) (or (call-interactively action) t))))

(defun    actype:params (actype)
  "Returns list of ACTYPE's parameters, including keywords."
  (action:params (actype:action actype)))

(defun    actype:param-list (actype)
  "Returns list of ACTYPE's parameters without keywords."
  (action:param-list (actype:action actype)))

(provide 'hact)

;;; hact.el ends here
