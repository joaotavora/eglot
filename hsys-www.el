;;; hsys-www.el ---  GNU Hyperbole support for Emacs World-Wide Web (WWW) browsing
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     7-Apr-94 at 17:17:39 by Bob Weiner
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This module defines an implicit button type and associated action and
;;   help types.  A press of the Action Key on a unified resource locator
;;   (URL) displays the referent for the URL.  A press of the Help Key on a
;;   URL displays what action the Action Key will take when pressed.
;;
;;   Customize the web browser used by setting, `browse-url-browser-function'
;;   to a function that invokes the desired browser on the URL.  It
;;   may be set from the Hyperbole Customization menu.  This menu also
;;   includes a setting for whether the browser reuses windows or
;;   generates new ones.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

;;; This does not require any particular web browser.
(require 'browse-url)
(require 'hbut)

;;; ************************************************************************
;;; Public functions and types
;;; ************************************************************************

;; eww-mode should define these next functions but presently does not,
;; so define them here when needed.
(unless (fboundp 'eww-link-at-point)
  (defun shr-link-at-point ()
    "Return any shr hyperlink url at point or nil if none."
    (get-text-property (point) 'shr-url))
  (defun eww-link-at-point ()
    "Return any eww web page hyperlink url at point or nil if none."
    (shr-link-at-point))
  (defun eww-bookmark-property (property)
    "Return value of PROPERTY, a symbol, for current eww bookmark line or nil."
    (if (eq major-mode 'eww-bookmark-mode)
	(plist-get (get-text-property (line-beginning-position) 'eww-bookmark)
		   property)))
  (defun eww-history-property (property)
    "Return value of PROPERTY, a symbol, for current eww history line or nil."
    (if (eq major-mode 'eww-history-mode)
	(plist-get (get-text-property (line-beginning-position) 'eww-history)
		   property))))

(defib www-url ()
  "Follow any non-ftp url (link) at point.
The variable, `browse-url-browser-function', customizes the url browser that
is used.
Valid values of this variable include `browse-url-default-browser' and
`browse-url-generic'."
  (cond ((looking-at "\\s-*\\'")
	 ;; Don't match if at the end of the buffer; end of line is
	 ;; handled elsewhere.
	 nil)
	((and (eq major-mode 'eww-mode) (eww-link-at-point))
	 (ibut:label-set (eww-link-at-point))
	 (hact 'eww-follow-link))
	((eq major-mode 'eww-bookmark-mode)
	 (ibut:label-set (concat (eww-bookmark-property :title)
				 (if (eww-bookmark-property :url)
				     (concat " <" (eww-bookmark-property :url) ">"))))
	 (hact 'eww-bookmark-browse))
	((eq major-mode 'eww-history-mode)
	 (ibut:label-set (concat (eww-history-property :title)
				 (if (eww-history-property :url)
				     (concat " <" (eww-history-property :url) ">"))))
	 (hact 'eww-history-browse))
	(t (let ((link-and-pos (hpath:www-at-p t)))
	     ;; Skip ftp URLs which are handled elsewhere.
	     (if (and link-and-pos (not (hpath:remote-at-p)))
		 (progn (ibut:label-set link-and-pos)
			(hact 'www-url (car link-and-pos))))))))

(defact www-url (url)
  "Follow a link given by URL.
The variable, `browse-url-browser-function', customizes the url browser that
is used.  Valid values of this variable include `browse-url-default-browser' and
`browse-url-generic'."
  (interactive "sURL to follow: ")
  (or (stringp url)
      (error "(www-url): URL = `%s' but must be a string" url))
  (if (or (functionp browse-url-browser-function)
	  ;; May be a predicate alist of functions from which to select
	  (consp browse-url-browser-function))
      (let (browse-function-name
	    browser)
	(if (symbolp browse-url-browser-function)
	    (setq browse-function-name (symbol-name browse-url-browser-function)
		  browser (and (string-match
				"-\\([^-]+\\)\\'"
				browse-function-name)
			       (capitalize (substring browse-function-name
						      (match-beginning 1)
						      (match-end 1)))))
	  (setq browser "default browser"))
	(message "Sending %s to %s..." url browser)
	(browse-url url)
	(message "Sending %s to %s...done" url browser))
    (error "(www-url): `browse-url-browser-function' must be set to a web browser invoking function")))

;;;###autoload
(defun www-url-expand-file-name (path &optional dir)
  "Expand PATH in DIR.  Return http urls unchanged."
  (if (listp path)
      (setq dir  (car (cdr path))
	    path (car path)))
  (cond ((string-match "\\`www\\.\\|\\`https?:" path)
	 path)
	(t (require 'hpath)
	   (or (hpath:remote-p path) path))))

;;;###autoload
(defun www-url-find-file-noselect (path &rest args)
  "Find PATH without selecting its buffer.  Handle http urls."
  (if (listp path)
      (setq args (cdr path)
	    path (car path)))
  (let* ((remote-sym (hpath:remote-available-p))
	 (inhibit-file-name-handlers
	  (if remote-sym
	      (append (list 'dired-handler-fn 
		       (intern-soft (concat (symbol-name remote-sym)
					    "-file-handler-function")))
		      (and (eq inhibit-file-name-operation 'find-file-noselect)
			   inhibit-file-name-handlers))
	    inhibit-file-name-handlers))
	 (inhibit-file-name-operation 'find-file-noselect))
    (if (string-match "\\`www\\.\\|\\`https?:" path)
	(progn (require 'hyperbole)
	       ;; Display url.
	       (hact 'www-url path)
	       ;; return same buffer
	       (current-buffer))
      (apply 'find-file-noselect path args))))

(provide 'hsys-www)

;;; hsys-www.el ends here
