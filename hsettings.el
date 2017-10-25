;;; hsettings.el --- Hyperbole settings which may require customization
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    15-Apr-91 at 00:48:49
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   See the "INSTALL" file for installation instructions and the
;;   "README" file for general information.
;;
;;   Be sure to have users load any personal mail/news personalizations
;;   before they load Hyperbole so that Hyperbole's mail or news
;;   support features work as desired.

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion)

;;; Read the comments and modify as desired.

;;; ************************************************************************
;;; TIMEZONE SETTING
;;; ************************************************************************

;; The following section applies only to MS-DOS and MS-Windows OSs.
;; Users of other OSs may simply ignore this section.

;; Some versions of Microcruft OSs don't automatically set the
;; timezone so that Hyperbole can read it.  Nor do they include a
;; UNIX-style date program.  So follow the commented instructions in
;; the code below here.

;; If Hyperbole loads without error, then your system sets the
;; timezone properly and you need not do anything.  If you receive a
;; timezone error, simply follow the instructions below to set the
;; timezone manually and then reload Hyperbole.
(if (and hyperb:microcruft-os-p
	 (require 'htz)
	 (not (stringp htz:local)))
    (progn
      ;; Comment out the following `error' line...
      (error "(hsettings.el): Configure the TIMEZONE SETTING section in this file.")
      ;; ... and uncomment the following line, substituting an appropriate
      ;;     timezone from the list in the variable, `htz:world-timezones'
      ;;     in the file, "htz.el".
      ;;   (setenv "TZ" "your-3char-timezone")
      ))

;;; ************************************************************************
;;; SMART SETTINGS FOR THE ACTION AND ASSIST KEYS AND SMART MENUS
;;; ************************************************************************

;; The following setting allows direct selection of Helm completion
;; entries with the Smart Mouse Keys.  Otherwise, by default, helm
;; disables all mouse keys while completing.  If you change this
;; setting (simply comment it out), you must restart Emacs for it to
;; take full effect.  Setting this to 't' or 'nil' will not provide
;; Hyperbole support.
(eval-after-load "helm" '(setq helm-allow-mouse 'no-mode-bindings))

;; The Smart Menu system is an attractive in-buffer menu system that
;; predates Emacs menu systems; it is included in InfoDock.
(defvar hkey-always-display-menu nil
  "*Non-nil means always display the Smart Menu window when the Action or Assist Key is pressed and the Smart Menu system has been loaded.
If a Smart Menu is already displayed, perform another Action or Assist Key
function.")

(defcustom hmouse-middle-flag (and (boundp 'infodock-version) infodock-version t)
  "*Under InfoDock or when t, additionally bind the middle mouse button as an
Action Key."
  :type 'boolean
  :group 'hyperbole-keys)

(defcustom smart-scroll-proportional t
  "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful."
  :type 'boolean
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; INTERNET SETTINGS
;;; ************************************************************************

;; String to be used in the call: (hpath:rfc rfc-num) to create a remote
;; path to the RFC document for `rfc-num'.  Uncomment and alter this setting
;; if another site is closer for you.
;; (setq hpath:rfc "/anonymous@ftp.ietf.org:rfc/rfc%s.txt")

;; When a user creates an explicit button or edits a Koutline, Hyperbole
;; tries to store her Internet e-mail address from the variable,
;; `user-mail-address'.  This should be set in your personal Emacs
;; initialization file, "~/.emacs" with a line like so:
;;
;;   (setq user-mail-address "your-email@address.com")
;;
;; and Hyperbole should be loaded after this setting is made.

;; Web search setttings for Hyperbole Find/Web menu.
(require 'browse-url)

(defun hyperbole-update-menus ()
  "Rebuild all Hyperbole menus with any updated settings."
  (hyperbole-menubar-menu)
  (hyperbole-minibuffer-menu))

(defun hyperbole-read-web-search-arguments (&optional service-name search-term)
  "Read from the keyboard a list of (web-search-service-string search-term-string) if not given as arguments."
  (let ((completion-ignore-case t))
    (while (or (not (stringp service-name)) (equal service-name ""))
      (setq service-name (completing-read "Search service: " hyperbole-web-search-alist
					  nil t)))
    (while (or (not (stringp search-term)) (equal search-term ""))
     (setq search-term (read-string (format "Search %s for: " service-name))))
    (list service-name search-term)))

(defun hyperbole-web-search (&optional service-name search-term)
  "Search web SERVICE-NAME for SEARCH-TERM.
Both arguments are optional and are prompted for when not given or when null.
Uses `hyperbole-web-search-alist' to match each service to its search url.
Uses `hyperbole-web-search-browser-function' and the `browse-url'
package to display search results."
  (interactive)
  (cl-multiple-value-bind (service-name search-term)
      (hyperbole-read-web-search-arguments service-name search-term)
    (if (assoc service-name hyperbole-web-search-alist)
	(let ((browse-url-browser-function
	       hyperbole-web-search-browser-function))
	  (browse-url (format (cdr (assoc service-name hyperbole-web-search-alist))
			      search-term)))
      (user-error "(Hyperbole): Invalid web search service `%s'" service-name))))

(defcustom hyperbole-web-search-browser-function browse-url-browser-function
  "*Function of one url argument called by any Hyperbole Find/Web search."
  :type 'boolean
  :group 'hyperbole-commands)

(defcustom hyperbole-web-search-alist
  '(("Amazon" . "http://www.amazon.com/s/field-keywords=%s")
    ("Bing" . "http://www.bing.com/search?q=%s")
    ;; Wikipedia Dictionary
    ("Dictionary" . "https://en.wiktionary.org/wiki/%s")
    ("Elisp" . "http://www.google.com/search?q=%s+filetype:el")
    ;; Facebook Hashtags
    ("Facebook" . "https://www.facebook.com/hashtag/%s")
    ;; To search for a Facebook user, use "https://www.facebook.com/%s".
    ("Google" . "http://www.google.com/search?q=%s")
    ("Hub(git)" . "https://github.com/search?ref=simplesearch&q=%s")
    ("Images" . "http://www.google.com/images?hl=en&q=%s")
    ("Maps" . "http://maps.google.com/maps?q=%s")
    ("RFCs" . "https://tools.ietf.org/html/rfc%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s")
    ("Twitter" . "https://twitter.com/search?q=%s")
    ("Wikipedia" . "https://en.wikipedia.org/wiki/%s")
    ("Youtube" . "https://www.youtube.com/results?search_query=%s"))
  "*Alist of (web-service-name . url-with-%s-parameter) elements.
The first character of each web-service-name must be unique.
This custom option is used in the Hyperbole Find/Web menu where
the %s in the url-with-%s-parameter is replaced with an interactively
obtained search string."
  :initialize 'custom-initialize-default
  :set (lambda (_option value)
	 (setq hyperbole-web-search-alist value)
	 (hyperbole-update-menus))
  :type '(alist :key-type string :value-type string)
  :group 'hyperbole-commands)

;;; ************************************************************************
;;; GNU EMACS AND XEMACS CONFIGURATION
;;; ************************************************************************

;; No-op unless set by one of the conditionals below.
(defun hui:but-flash ())

(cond ((and hyperb:emacs-p (not noninteractive))
       (require 'hui-em-but)
       ;; Highlight explicit buttons whenever a file is read in.
       (add-hook 'find-file-hook #'hproperty:but-create t)
       (defalias 'hui:but-flash 'hproperty:but-flash)
       ;;
       ;; Substitute for the nil argument below a valid X color name with
       ;; which to highlight buttons if the default highlighting does not
       ;; appeal to you. See "hui-em-but.el" for how this works.
       (hproperty:cycle-but-color nil)
       ;;
       ;; Non-nil means visually emphasize that button under mouse cursor is
       ;; selectable.
       (setq hproperty:but-emphasize-p nil)
       ;;
       ;; If you find that the Hyperbole button flash time is too slow
       ;; or too fast, adjust it here.
       (setq hproperty:but-flash-time 1000))

      ((and (featurep 'xemacs) (not noninteractive))
       (require 'hui-xe-but)
       ;;
       ;; If running XEmacs 19.8 or below, don't highlight explicit buttons
       ;; whenever a file is read in since this can cause a sporadic crash
       ;; when find-files are done.
       (if hyperb:kotl-p (add-hook 'find-file-hook #'hproperty:but-create t))
       (defalias 'hui:but-flash 'hproperty:but-flash)
       ;;
       ;; Substitute for the nil argument below a valid X color name with
       ;; which to highlight buttons if the default highlighting does not
       ;; appeal to you. See "hui-xe-but.el" for how this works.
       (hproperty:cycle-but-color nil)
       ;;
       ;; Non-nil means visually emphasize that button under mouse cursor is
       ;; selectable.
       (setq hproperty:but-emphasize-p nil)
       ;;
       ;; If you find that the Hyperbole button flash time is too slow
       ;; or too fast, adjust it here.
       (setq hproperty:but-flash-time 1000)))

;;; ************************************************************************
;;; ONLINE LIBRARY CONFIGURATION
;;; ************************************************************************

;;; Support for online library document id references is loaded here but
;;; requires some additional configuration before use.  See the DESCRIPTION
;;; section in "hib-doc-id.el" for complete installation and use information.
;;;
(add-hook 'hibtypes-end-load-hook (lambda () (require 'hib-doc-id)))

;;; ************************************************************************
;;; HYPERBOLE LOCAL VARIABLE SUPPORT
;;; ************************************************************************

;;; Uncomment this if you really need to be able to use Hyperbole variables
;;; (and others with colons in their names) within file local variable lists.
;;; See the source file for more details.
;;;
;;  (require 'hlvar)

;;; ************************************************************************
;;; SITE-SPECIFIC ADDITIONS - Add your Hyperbole configuration additions here.
;;; ************************************************************************

;;; ************************************************************************
;;; END OF HYPERBOLE SITE-SPECIFIC CUSTOMIZATIONS
;;; ************************************************************************

(provide 'hsettings)

;;; hsettings.el ends here
