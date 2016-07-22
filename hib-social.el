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

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom hibtypes-social-default-service "twitter"
  "Lowercase string matching the name of the default social media service to use when none is specified."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-social-display-function #'browse-url
  "Function of one argument, a url, to display when a social media reference is activated."
  :type 'function
  :group 'hyperbole-button)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hibtypes-social-hashtag-alist
  '(("\\`\\(fb\\|facebook\\)\\'" . "https://www.facebook.com/hashtag/%s")
    ("\\`\\(tw\\|twitter\\)\\'" . "https://twitter.com/search?q=%%23%s&src=hashtag")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/explore/tags/%s/"))
  "Alist of (social-media-service-regexp  . url-with-%s-for-hashtag) elements.")

(defconst hibtypes-social-username-alist
  '(("\\`\\(fb\\|facebook\\)\\'" . "https://www.facebook.com/%s")
    ("\\`\\(tw\\|twitter\\)\\'" . "https://twitter.com/search?q=@%s")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/%s/"))
  "Alist of (social-media-service-regexp  . url-with-%s-for-username) elements.")

(defconst hibtypes-social-regexp "\\([[:alpha:]]*\\)\\([#@]\\)\\([._[:alnum:]]*[_[:alnum:]]\\)"
  "Regular expression that matches a social media hashtag or username reference.
See `ibtypes::social-reference' for format details.")

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
when not given, so #hashtag would be the same as twitter#hashtag."
  (when (and (not (memq major-mode '(texinfo-mode para-mode)))
	     (save-excursion
	       (if (looking-at "[#@._[:alnum:]]")
		   (skip-chars-backward "#@._[:alnum:]"))
	       (and (looking-at hibtypes-social-regexp)
		    (save-match-data
		      ;; Heuristic to ensure this is not an email address
		      (not (and (looking-at mail-address-regexp)
				(let ((case-fold-search t))
				  (string-match mail-address-tld-regexp
						(match-string-no-properties 1)))))))))
    (save-match-data
      (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0)))
    (hact 'social-reference (match-string-no-properties 1)
	  (match-string-no-properties 2) (match-string-no-properties 3))))

(defact social-reference (service ref-type-char hashtag-or-username)
  "Display the web page at social media SERVICE for REF-TYPE-CHAR and HASHTAG-OR-USERNAME.
REF-TYPE-CHAR is either \"#\" for a hashtag reference or \"@\" for a username reference."
  (if (or (null service) (equal service "")) (setq service hibtypes-social-default-service))
  (let ((case-fold-search t)
	url-to-format)
    (when (or (and (equal ref-type-char "#")
		   (setq url-to-format
			 (assoc-default service hibtypes-social-hashtag-alist #'string-match)))
	      (and (equal ref-type-char "@")
		   (setq url-to-format
			 (assoc-default service hibtypes-social-username-alist #'string-match))))
      (funcall hibtypes-social-display-function (format url-to-format hashtag-or-username)))))

(provide 'hib-social)
