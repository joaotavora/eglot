;;; hsys-org.el --- GNU Hyperbole support for Emacs Org mode links
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     2-Jul-16 at 14:54:14
;;
;; Copyright (C) 2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This defines a context-sensitive implicit button type, org-mode, triggered
;;   when the major mode is org-mode and point is anywhere other than
;;   the end of a line.
;;   When:
;;     on an Org mode link - displays the link referent
;;     on an Org mode heading - cycles through the available display
;;       views for that heading
;;     anywhere else - executes `org-meta-return'.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hbut)
(require 'org)

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib org-mode ()
  "Follows any Org mode link at point or cycles through views of the outline subtree at point.
The variable, `browse-url-browser-function', customizes the url browser that
is used for urls.  Valid values of this variable include `browse-url-default-browser'
and `browse-url-generic'."
  (when (eq major-mode 'org-mode)
    (cond ((org-link-at-p)
	   (hact 'org-link nil))
	  ((org-at-heading-p)
	   (hact 'org-cycle nil))
	  (t (hact 'org-meta-return)))))

(defun org-mode:help (&optional _but)
  "If on an Org mode heading, cycles through views of the whole buffer outline.
If on an Org mode link, displays standard Hyperbole help."
  (when (eq major-mode 'org-mode)
    (cond ((org-link-at-p)
	   (hkey-help current-prefix-arg)
	   t)
	  ((org-at-heading-p)
	   (org-global-cycle nil)
	   t))))

(defact org-link (link)
  "Follows an Org mode LINK.  If LINK is nil, follows the link at point."
  (if (stringp link)
      (org-open-link-from-string link) ;; autoloaded
    (org-open-at-point-global))) ;; autoloaded

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;; Assumes caller has already checked that the current buffer is in org-mode.
(defun org-link-at-p ()
  (let ((face-prop (get-text-property (point) 'face)))
    (or (eq face-prop 'org-link)
	(and (listp face-prop) (memq 'org-link face-prop)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(provide 'hsys-org)

;;; hsys-org.el ends here
