;;; hload-path.el --- GNU Hyperbole load-path setup
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    29-Jun-16 at 14:39:33
;;
;; Copyright (C) 1992-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

;;; Support button highlighting and flashing under XEmacs.
;;;
;;;###autoload
(defvar hyperb:xemacs-p
  (let ((case-fold-search t))
    (if (string-match "XEmacs" emacs-version)
	emacs-version))
  "Version string under XEmacs or nil")

;;; Support mouse handling under GNU Emacs V19 or higher.
;;;
;;;###autoload
(defvar hyperb:emacs-p
  (and (not hyperb:xemacs-p)
       ;; Version 19 and above, the only ones supported.
       (string-lessp "19" emacs-version)
       emacs-version)
  "Version string under GNU Emacs 19 or higher, or nil")

;;; Koutlines work only with specific versions of Emacs and XEmacs.
;;;###autoload
(defconst hyperb:kotl-p
  (if hyperb:xemacs-p
      ;; Only works for XEmacs 19.9 and above.
      (or (string-match "^19\\.9 \\|^19\\.[1-9][0-9]" emacs-version)
	  ;; Version 20 and above.
	  (string-lessp "20" emacs-version))
    hyperb:emacs-p)
  "Non-nil iff this Emacs version supports the Hyperbole Koutliner.")

;;; ************************************************************************
;;; Hyperbole Directory Setting (dynamically computed)
;;; ************************************************************************

(defconst hyperb:dir (file-name-directory
		      (or (and (stringp load-file-name) load-file-name)
			  (hyperb:path-being-loaded)
			  (locate-file "hmouse-tag.el" load-path)
			  ""))
  "Directory where the Hyperbole executable code is kept.
It must end with a directory separator character.")
(if (stringp hyperb:dir)
    (setq hyperb:dir (file-name-directory hyperb:dir))
  (error
   "(Hyperbole): Failed to set hyperb:dir.  Try setting it manually."))

;; Add hyperb:dir to load-path so other Hyperbole libraries can be found.
(add-to-list 'load-path hyperb:dir)

;;; ************************************************************************
;;; Koutliner mode and file suffix importation settings
;;; ************************************************************************

;; If the Koutliner is supported by this emacs version, perform
;; initializations.

(if hyperb:kotl-p
    (progn (add-to-list 'load-path (expand-file-name "kotl/" hyperb:dir))
	   ;; Invoke kotl-mode for files ending in ".kotl".  Also
	   ;; allow ".kot" for DOS and Windows users.
	   (setq auto-mode-alist (cons '("\\.kotl$\\|\\.kot$" . kotl-mode)
				       auto-mode-alist))))

(provide 'hload-path)

;;; hload-path.el ends here
