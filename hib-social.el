;;; hib-social.el --- Implicit button type for social media hashtag and username references
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    20-Jul-16 at 22:41:34
;;
;; Copyright (C) 2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   This defines an implicit button type, social-reference, that displays 
;;   the web page associated with the given hashtag or username.
;;
;;   A hashtag reference is either: [facebook|instagram|twitter]?#<hashtag>
;;   or [fb|in|tw]?#<hashtag>.
;;
;;   A username reference is either: [facebook|instagram|twitter]?@<username>
;;   or [fb|in|tw]?@<username>.
;;
;;   If the social media service is not given, it defaults to \"twitter\".

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (require 'browse-url))
(require 'hbut)
(require 'hargs)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hibtypes-social-default-service "twitter"
  "Lowercase string matching the name of the default social media service to use when none is specified."
  :type '(radio (const "facebook")
		(const "github")
		(const "instagram")
		(const "twitter"))
  :group 'hyperbole-button)

(defcustom hibtypes-social-display-function #'browse-url
  "Function of one argument, a url, to display when a social media reference is activated."
  :type 'function
  :group 'hyperbole-button)

(defcustom hibtypes-social-github-default-project nil
  "Default project name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-social-github-default-user nil
  "Default user name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hibtypes-social-hashtag-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/hashtag/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/%s/commit/%s")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/explore/tags/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=%%23%s&src=hashtag")
)
  "Alist of (social-media-service-regexp  . url-with-%s-for-hashtag) elements.")

(defconst hibtypes-social-username-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=@%s")
    )
  "Alist of (social-media-service-regexp  . url-with-%s-for-username) elements.")

(defconst hibtypes-social-regexp "\\([[:alpha:]]*\\)\\([#@]\\)\\([[:alnum:]]*[._/[:alnum:]]*[_[:alnum:]]\\)"
  "Regular expression that matches a social media hashtag or username reference.
See `ibtypes::social-reference' for format details.")

(defvar hibtypes-social-inhibit-modes '(texinfo-mode para-mode)
  "*List of major modes in which to inhibit any possible social media tag matches.")

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib social-reference ()
  "Displays the web page associated with a social media hashtag or username reference at point.
Reference format is:
  [facebook|instagram|twitter]?[#@]<hashtag-or-username> or
  [fb|in|tw]?[#@]<hashtag-or-username>.

The first part of the label for a button of this type is the social
media service name.  The service name defaults to the value of
`hibtypes-social-default-service' (default value of \"twitter\")
when not given, so #hashtag would be the same as twitter#hashtag.

This will not match within any single line, single or
double-quoted strings or within any buffer whose major mode is
listed in `hibtypes-social-inhibit-modes'."
  (when (and (not (or (memq major-mode hibtypes-social-inhibit-modes)
		      (hargs:delimited "\"" "\"")
		      (hargs:delimited "[\`\']" "\'" t)
		      ;; Avoid Markdown parenthesized hash links
		      (and (eq major-mode 'markdown-mode)
			   (hargs:delimited "(" ")"))))
	     (save-excursion
	       (if (looking-at "[#@/._[:alnum:]]")
		   (skip-chars-backward "#@/._[:alnum:]"))
	       (and (looking-at hibtypes-social-regexp)
		    (save-match-data
		      ;; Heuristic to ensure this is not an email address
		      (not (and (looking-at mail-address-regexp)
				(let ((case-fold-search t))
				  (string-match mail-address-tld-regexp
						(match-string-no-properties 1)))))))))
    (save-match-data
      (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0)))
    (if (save-match-data (and (equal (match-string-no-properties 2) "#")
			      (string-match "\\`\\(gh\\|github\\)\\'" (match-string-no-properties 1))))
	(hact 'github-commit-reference (match-string-no-properties 3))
      (hact 'social-reference (match-string-no-properties 1)
	    (match-string-no-properties 2) (match-string-no-properties 3)))))

(defact github-commit-reference (commit-hashtag &optional user project)
  "Display the Github web page showing a commit given by COMMIT-HASHTAG and optional USER and PROJECT.
USER defaults to the value of `hibtypes-social-github-default-user'.
PROJECT defaults to the value of `hibtypes-social-github-default-project'."
  (if (or (null commit-hashtag) (string-empty-p commit-hashtag))
      (error "(github-commit-reference): Github commit hashtag must not be empty")
    (let ((case-fold-search t)
	  (url-to-format (assoc-default "github" hibtypes-social-hashtag-alist #'string-match)))
      (when url-to-format
	(when (string-match "\\(\\([^/#@]+\\)/\\)?\\([^/#@]+\\)/" commit-hashtag)
	  (setq user (or (match-string-no-properties 2 commit-hashtag) user)
		project (or (match-string-no-properties 3 commit-hashtag) project)
		commit-hashtag (substring commit-hashtag (match-end 0))))
	(unless (stringp user) (setq user hibtypes-social-github-default-user))
	(unless (stringp project) (setq project hibtypes-social-github-default-project))
	(when (and  (stringp user) (stringp project))
	  (funcall hibtypes-social-display-function
		   (format url-to-format user project commit-hashtag)))))))

(defact social-reference (service ref-type-str hashtag-or-username)
  "Display the web page at social media SERVICE for REF-TYPE-STR and HASHTAG-OR-USERNAME.
REF-TYPE-STR is either \"#\" for a hashtag reference or \"@\" for a username reference."
  (if (or (null service) (equal service "")) (setq service hibtypes-social-default-service))
  (let ((case-fold-search t)
	url-to-format)
    (when (or (and (equal ref-type-str "#")
		   (setq url-to-format
			 (assoc-default service hibtypes-social-hashtag-alist #'string-match)))
	      (and (equal ref-type-str "@")
		   (setq url-to-format
			 (assoc-default service hibtypes-social-username-alist #'string-match))))
      (funcall hibtypes-social-display-function (format url-to-format hashtag-or-username)))))

(provide 'hib-social)
