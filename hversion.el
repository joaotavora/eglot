;;; hversion.el --- GNU Hyperbole version and system information setup
;;
;; Author:       Bob Weiner
;; Maintainer:   Bob Weiner and Mats Lidell
;;
;; Orig-Date:     1-Jan-94
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hload-path)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defconst hyperb:version "6.0.2d" "GNU Hyperbole revision number.")

;;;###autoload
(defvar hyperb:microcruft-os-p
  (memq system-type '(ms-windows windows-nt ms-dos win32))
  "T iff Hyperbole is running under a Microcruft OS.")

;;;###autoload
(defvar hyperb:mouse-buttons
  (if (or (and hyperb:microcruft-os-p (not (memq window-system '(w32 w64 x))))
	  (and hyperb:emacs-p (memq window-system '(ns dps))))
      2 3)
  "Number of live buttons available on the mouse.
Override this if the system-computed default is incorrect for your specific mouse.")

(defvar hyperb:automount-prefixes
  (if (and (boundp 'automount-dir-prefix) (stringp automount-dir-prefix))
      automount-dir-prefix
    "^/tmp_mnt/"
    "Regexp to match any automounter prefix in a pathname."))

;;; ************************************************************************
;;; Support functions
;;; ************************************************************************

;; Called in hyperbole.el.
(defun hyperb:stack-frame (function-list &optional debug-flag)
  "Return the nearest Emacs Lisp stack frame which called any function symbol from FUNCTION-LIST or nil if no match.
If FUNCTION-LIST contains 'load, 'autoload or 'require, detect
autoloads not visible within the Lisp level stack frames.

With optional DEBUG-FLAG non-nil, if no matching frame is found, return list
of stack frames (from innermost to outermost)."
  (let ((count 0)
	(frame-list)
	(load-flag (or (memq 'load function-list)
		       (memq 'autoload function-list)
		       (memq 'require function-list)))
	fsymbol
	fbody
	frame)
    (or (catch 'hyperb:stack-frame
	  (while (setq frame (backtrace-frame count))
	    (if debug-flag (setq frame-list (cons frame frame-list)))
	    (setq count (1+ count)
		  fsymbol (nth 1 frame))
	    (and (eq fsymbol 'command-execute)
		 (not (memq 'command-execute function-list))
		 ;; Use command being executed instead because it might not
		 ;; show up in the stack anywhere else, e.g. if it is an
		 ;; autoload under Emacs.
		 (setq fsymbol (nth 2 frame)))
	    (cond ((and load-flag (symbolp fsymbol)
			(fboundp fsymbol)
			(listp (setq fbody (symbol-function fsymbol)))
			(eq (car fbody) 'autoload))
		   (setq frame (list (car frame) 'load
				     (car (cdr fbody))
				     nil noninteractive nil))
		   (throw 'hyperb:stack-frame frame))
		  ((memq fsymbol function-list)
		   (throw 'hyperb:stack-frame frame))))
	  nil)
	(if debug-flag (nreverse frame-list)))))

(defun hyperb:path-being-loaded ()
  "Return the full pathname used by the innermost `load' or 'require' call.
Removes any matches for `hyperb:automount-prefixes' before returning
the pathname."
  (let* ((frame (hyperb:stack-frame '(load require)))
	 (function (nth 1 frame))
	 file nosuffix)
    (cond ((eq function 'load)
	   (setq file (nth 2 frame)
		 nosuffix (nth 5 frame)))
	  ((eq function 'require)
	   (setq file (or (nth 3 frame) (symbol-name (nth 2 frame))))))
    (if (stringp file)
	(setq nosuffix (or nosuffix
			   (string-match
			    "\\.\\(elc?\\|elc?\\.gz\\|elc?\\.Z\\)$"
			    file))
	      file (substitute-in-file-name file)
	      file (locate-file file load-path
				(if (null nosuffix) '(".elc" ".el" ".el.gz" ".el.Z"))
				;; accept any existing file
				nil)
	      file (if (and (stringp file)
			    (string-match hyperb:automount-prefixes file))
		       (substring file (1- (match-end 0)))
		     file)))))

(defun hyperb:window-sys-term (&optional frame)
  "Returns the first part of the term-type if running under a window system, else nil.
Where a part in the term-type is delimited by a `-' or  an `_'."
  (unless frame (setq frame (selected-frame)))
  (let* ((display-type (if (fboundp 'device-type) (device-type) window-system))
	 (term (cond ((or (memq display-type '(x gtk mswindows win32 w32 ns dps pm))
			  ;; May be a graphical client spawned from a
			  ;; dumb terminal Emacs, e.g. under X, so if
			  ;; the selected frame has mouse support,
			  ;; then there is a window system to support.
			  (display-mouse-p))
		      ;; X11, macOS, NEXTSTEP (DPS), or OS/2 Presentation Manager (PM)
		      (cond (hyperb:emacs-p "emacs")
			    ((featurep 'xemacs)  "xemacs")
			    (t                "xterm")))
		     ((or (featurep 'eterm-fns)
			  (equal (getenv "TERM") "NeXT")
			  (equal (getenv "TERM") "eterm"))
		      ;; NEXTSTEP add-on support to Emacs
		      "next")
		     )))
    (set-frame-parameter frame 'hyperb:window-system
			 (and term (setq term (substring term 0 (string-match "[-_]" term)))))
    term))

(defun hyperb:window-system (&optional frame)
  "Returns the string name for window system or term type under which the selected frame is running.
If nil after system initialization, no window system or mouse support is available."
  (unless frame (setq frame (selected-frame)))
  (frame-parameter frame 'hyperb:window-system))

;; Each frame could be on a different window system when under a
;; client-server window system, so set `hyperb:window-system'  for
;; each frame.
(mapc #'hyperb:window-sys-term (frame-list))
;; Ensure this next hook is appended so that if follows the hook that
;; selects the new frame. 
(add-hook 'after-make-frame-functions #'hyperb:window-sys-term t)

;;; ************************************************************************
;;; Public functions used by pulldown and popup menus
;;; ************************************************************************

(if (not (fboundp 'id-browse-file))
(defalias 'id-browse-file 'find-file-read-only))

(unless (fboundp 'id-info)
(defun id-info (string)
  (if (stringp string)
      (progn (let ((wind (get-buffer-window "*info*")))
	       (cond (wind (select-window wind))
		     ((br-in-browser) (br-to-view-window))
		     (t (hpath:display-buffer (other-buffer)))))
	     ;; Force execution of Info-mode-hook which adds the
	     ;; Hyperbole man directory to Info-directory-list.
	     (info)
	     (condition-case ()
		 (Info-goto-node string)
	       ;; If not found as a node, try as an index item.
	       (error (id-info-item string))))
    (error "(id-info): Invalid Info argument, `%s'" string))))

(unless (fboundp 'id-info-item)
(defun id-info-item (index-item)
  (if (stringp index-item)
      (progn (let ((wind (get-buffer-window "*info*")))
	       (cond (wind (select-window wind))
		     ((br-in-browser) (br-to-view-window))
		     (t (hpath:display-buffer (other-buffer)))))
	     ;; Force execution of Info-mode-hook which adds the
	     ;; Hyperbole man directory to Info-directory-list.
	     (info)
	     (if (string-match "^(\\([^\)]+\\))\\(.*\\)" index-item)
		 (let ((file (match-string-no-properties 1 index-item))
		       (item-name (match-string-no-properties 2 index-item)))
		   (if (and file (setq file (hpath:substitute-value file)))
		       (progn (Info-goto-node (concat "(" file ")"))
			      (Info-index item-name))
		     (Info-goto-node "(hyperbole)")
		     (Info-index index-item))
		   (recenter 0))
	       (error "(id-info-item): Invalid Info index item: `%s'" index-item)))
    (error "(id-info-item): Info index item must be a string: `%s'" index-item))))

(if (not (fboundp 'id-tool-quit))
(defalias 'id-tool-quit 'eval))

(if (not (fboundp 'id-tool-invoke))
(defun id-tool-invoke (sexp)
  (if (commandp sexp)
      (call-interactively sexp)
    (funcall sexp))))

(provide 'hversion)

;;; hversion.el ends here
