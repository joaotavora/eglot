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
;;   information (often a web page) associated with the given hashtag or username.
;;   When the referent is a web page, this calls the function given by
;;   `hibtypes-social-display-function' to display it, initially set to `browse-url'.
;;
;;   A hashtag reference is either: [facebook|github|git|instagram|twitter]?#<hashtag>
;;   or using 2-letter service abbreviations: [fb|gh|gt|in|tw]?#<hashtag>.
;;
;;   A username reference is either: [facebook|github|instagram|twitter]?@<username>
;;   or [fb|gh|in|tw]?@<username>.
;;
;;   If the social media service is not given, it defaults to the value of
;;   `hibtypes-social-default-service', initially set to \"twitter\".
;;
;;   Below are a list of examples; simply press the Action Key on each one
;;   to test it; use the Assist Key to see what it will do.  The git
;;   examples require that you have a local git clone of the Hyperbole
;;   repository.

;;     facebook@zuck                             Display user's home page
;;     github@rswgnu
;;     instagram@lostart
;;     twitter@nytimestravel

;;     fb#technology                             Display page of hashtag matches
;;     in#art
;;     tw#travel

;;   Git (local) reference links
;;
;;     git#branches                              List branches in current repo/project
;;     git#commits                               List commits in current project
;;     git#tags                                  List tags in current project
;;
;;     git#/hyperbole                            From any buffer, dired on the top
;;                                               directory of the local hyperbole
;;                                               project
;; 
;;     git#/hyperbole/55a1f0 or                  From any buffer, display hyperbole
;;     git#hyperbole/55a1f0                      local git commit diff
;;                                               
;;     git#55a1f0                                Based on current default-directory,
;;                                               display current repo's local git
;;                                               commit diff; works when default-directory
;;                                               is inside a git project with commit
;;                                               hashtag 55a1f0
;;
;;     (setq hibtypes-git-default-project "hyperbole")
;;     git#55a1f0                                From any buffer, once the above default
;;                                               is set, display current project's local
;;                                               git commit diff
;;     git#master                                Shows latest commit diff for branch
;;     git#hyperbole-6.0.2                       From any buffer, show the commit diff
;;                                               for tag `hyperbole-6.0.2'
;;
;;     When you want to be more explicit, use:
;;       
;;       git#commit/55a1f0
;;       git#branch/master
;;       git#tag/hyperbole-6.0.2

;;   Github (remote) reference links
;;
;;     gh@rswgnu                                 Display user's home page & projects
;;
;;     github#rswgnu/hyperbole                   Display user's project
;;     gh#rswgnu/helm/global_mouse               Display user project's branch
;;     gh#rswgnu/hyperbole/55a1f0                Display user project's commit diff
;;
;;     (setq hibtypes-github-default-user "rswgnu")
;;     github#/hyperbole                         Display default user's project
;;
;;
;;     Once you set the default user and project variables, you can leave
;;     them off any reference links:
;;
;;       (setq hibtypes-github-default-user "emacs-helm")
;;       (setq hibtypes-github-default-project "helm")
;;
;;     like so:
;;
;;       gh#issues                               List emacs-helm/helm's open issues
;;       gh#1878                                 Display a specific project issue
;;
;;       gh#pulls                                List project's open pull requests
;;       gh#pull/1871                            Display a specific project pull request
;;
;;       gh#branches                             List project's branches
;;       gh#branch/global_mouse                  List files in a specific branch
;;       gh#global_mouse                         You can even leave off the `branch' keyword
;;
;;       gh#tags                                 List project's tagged commits, typically releases
;;       gh#tag/v2.8.4 or gh#v2.8.4              List files in a specific tagged commit
;;
;;       gh#commits                              List project's commits
;;       gh#898e55c                              Display default user and default
;;                                               project commit diff

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
		(const "git")
		(const "github")
		(const "instagram")
		(const "twitter"))
  :group 'hyperbole-button)

(defcustom hibtypes-social-display-function #'browse-url
  "Function of one argument, a url, to display when a social media reference is activated."
  :type 'function
  :group 'hyperbole-button)

(defcustom hibtypes-git-default-project nil
  "Default project name to associate with any local git commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-github-default-project nil
  "Default project name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

(defcustom hibtypes-github-default-user nil
  "Default user name to associate with any Github commit link."
  :type 'string
  :group 'hyperbole-button)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defconst hibtypes-social-hashtag-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/hashtag/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/%s/%s%s")
    ("\\`\\(gt\\|git\\)\\'"       . "(cd %s; git %s %s)")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/explore/tags/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=%%23%s&src=hashtag")
)
  "Alist of (social-media-service-regexp  . expression-to-display-hashtag-reference) elements.")

(defconst hibtypes-social-username-alist
  '(("\\`\\(fb\\|facebook\\)\\'"  . "https://www.facebook.com/%s")
    ("\\`\\(gh\\|github\\)\\'"    . "https://github.com/%s/")
    ("\\`\\(in\\|instagram\\)\\'" . "https://www.instagram.com/%s/")
    ("\\`\\(tw\\|twitter\\)\\'"   . "https://twitter.com/search?q=@%s")
    )
  "Alist of (social-media-service-regexp  . url-with-%s-for-username) elements.")

(defconst hibtypes-social-regexp "\\([[:alpha:]]*\\)\\([#@]\\)\\(/?[[:alnum:]]*[-._/[:alnum:]]*[-_[:alnum:]]\\)"
  "Regular expression that matches a social media hashtag or username reference.
See `ibtypes::social-reference' for format details.")

(defvar hibtypes-social-inhibit-modes '(texinfo-mode para-mode)
  "*List of major modes in which to inhibit any possible social media tag matches.")

;;; ************************************************************************
;;; Public Button Types
;;; ************************************************************************

(defib social-reference ()
  "Displays the web page associated with a social hashtag or username reference at point.
Reference format is:
  [facebook|git|github|instagram|twitter]?[#@]<reference> or
  [fb|gt|gh|in|tw]?[#@]<reference>.

The first part of the label for a button of this type is the social
service name.  The service name defaults to the value of
`hibtypes-social-default-service' (default value of \"twitter\")
when not given, so #hashtag would be the same as twitter#hashtag.

Local git references allow hashtags only, not username references.

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
	       (if (looking-at "[-#@/._[:alnum:]]")
		   (skip-chars-backward "-#@/._[:alnum:]"))
	       (and (looking-at hibtypes-social-regexp)
		    ;; Ensure prefix matches to a social web service
		    (save-match-data
		      (let ((ref (match-string-no-properties 1)))
			(delq nil (mapcar (lambda (regexp) (string-match regexp ref))
					  (mapcar #'car hibtypes-social-hashtag-alist)))))
		    ;; Heuristic to ensure this is not an email address
		    (save-match-data
		      (not (and (looking-at mail-address-regexp)
				(let ((case-fold-search t))
				  (string-match mail-address-tld-regexp
						(match-string-no-properties 1)))))))))

    (save-match-data
      (ibut:label-set (match-string-no-properties 0) (match-beginning 0) (match-end 0)))
    (let ((ref (match-string-no-properties 0))
	  (service (match-string-no-properties 1))
	  (ref-kind-str (match-string-no-properties 2))
	  (after-hash-str (match-string-no-properties 3)))
      (cond ((string-match "\\`\\(gt\\|git\\)#" ref)
	     (hact 'git-reference after-hash-str))
	    ((string-match "\\`\\(gh\\|github\\)#" ref)
	     (hact 'github-reference after-hash-str))
	    (t (hact 'social-reference service ref-kind-str after-hash-str))))))

;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun social-reference (service ref-kind-str hashtag-or-username)
  "Display the web page at social media SERVICE for REF-KIND-STR and HASHTAG-OR-USERNAME.
REF-KIND-STR is either \"#\" for a hashtag reference or \"@\" for a username reference."
  (if (or (null service) (equal service "")) (setq service hibtypes-social-default-service))
  (let ((case-fold-search t)
	expr-to-format)
    (when (or (and (equal ref-kind-str "#")
		   (setq expr-to-format
			 (assoc-default service hibtypes-social-hashtag-alist #'string-match)))
	      (and (equal ref-kind-str "@")
		   (setq expr-to-format
			 (assoc-default service hibtypes-social-username-alist #'string-match))))
      (if expr-to-format
	  (funcall hibtypes-social-display-function (format expr-to-format hashtag-or-username))
	(error "(social-reference): Service `%s' does not support reference format, `%s%s'"
	       service ref-kind-str hashtag-or-username)))))

;;; Remote Github commit references

;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun github-reference (reference &optional user project)
  "Display the Github entity associated with REFERENCE and optional USER and PROJECT.
REFERENCE is a string of the form:
    <ref-item>
    <user>/<project>/<ref-item>
    <project>/<ref-item>
or  /<project>.

<ref-item> is one of these:
  one of the words: branches, commits, issues, pulls, or tags; the associated items are listed;

  one of the words: branch, commit, issue, pull or tag followed by a '/' and
  item id; the item is shown;

  an issue reference given by a positive integer, e.g. 92 or prefaced with GH-, e.g. GH-92;
  the issue is displayed;

  a commit reference given by a hex number, 55a1f0; the commit diff is displayed;

  a branch or tag reference given by an alphanumeric name, e.g. hyper20; the
  files in the branch are listed.

USER defaults to the value of `hibtypes-github-default-user'.
If given, PROJECT overrides any project value in REFERENCE.  If no
PROJECT value is provided, it defaults to the value of
`hibtypes-github-default-project'."
  (if (or (null reference) (equal reference ""))
      (error "(github-reference): Github reference must not be empty")
    (let ((case-fold-search t)
	  (url-to-format (assoc-default "github" hibtypes-social-hashtag-alist #'string-match))
	  (ref-type))
      (when url-to-format
	(cond ((string-match "\\`\\(branch\\|commit\\|issue\\|pull\\|tag\\)/" reference)
	       ;; [branch | commit | issue | pull | tag]/ref-item
	       nil)
	      ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)\\([^/#@]+\\)\\'" reference)
	       ;; /?user/project
	       (setq user (or user (match-string-no-properties 2 reference))
		     project (or project (match-string-no-properties 3 reference))
		     reference nil))
	      ((string-match "\\`/?\\(\\([^/#@]+\\)/\\)?\\([^/#@]+\\)/\\([^#@]+\\)\\'" reference)
	       ;; /?[user/]project/ref-item
	       (setq user (or user (match-string-no-properties 2 reference))
		     project (or project (match-string-no-properties 3 reference))
		     reference (match-string-no-properties 4 reference)))
	      ((string-match "\\`/\\([^/#@]+\\)\\'" reference)
	       ;; /project
	       (setq project (or project (match-string-no-properties 1 reference))
		     reference nil)))
	(unless (stringp user) (setq user hibtypes-github-default-user))
	(unless (stringp project) (setq project hibtypes-github-default-project))
	(when reference
	  (cond ((member reference '("branches" "commits" "issues" "pulls" "tags"))
		 ;; All branches, commits, open issues, pull requests or commit tags reference
		 (setq ref-type reference
		       reference ""))
		((and (< (length reference) 7) (string-match "\\`\\([gG][hH]-\\)?[0-9]+\\'" reference))
		 ;; Specific issue reference
		 (setq ref-type "issues/"))
		((string-match "\\`\\(commit\\|issue\\|pull\\)/" reference)
		 ;; Specific reference preceded by keyword branch, commit,
		 ;; issue, or pull
		 (setq ref-type (substring reference 0 (match-end 0))
		       reference (substring reference (match-end 0))))
		((string-match "\\`[0-9a-f]+\\'" reference)
		 ;; Commit reference
		 (setq ref-type "commit/"))
		(t
		 ;; Specific branch or commit tag reference
		 (setq ref-type "tree/")
		 (when (string-match "\\`\\(branch\\|tag\\)/" reference)
		   ;; If preceded by optional keyword, remove that from the reference.
		   (setq reference (substring reference (match-end 0)))))))
	(if (and (stringp user) (stringp project))
	    (funcall hibtypes-social-display-function
		     (if reference
			 (format url-to-format user project ref-type reference)
		       (format url-to-format user project "" "")))
	  (cond ((and (null user) (null project))
		 (error "(github-reference): Set `hibtypes-github-default-user' and `hibtypes-github-default-project'"))
		((null user)
		 (error "(github-reference): Set `hibtypes-github-default-user'"))
		(t
		 (error "(github-reference): Set `hibtypes-github-default-project'"))))))))

;;; Local git repository commit references

(defvar hibtypes-git-repos-cache 
  (expand-file-name "Local-Git-Repos" hbmap:dir-user)
  "Filename of cache of local git repository directories found by `locate-command'.")

(defun hibtypes-git-build-repos-cache (&optional prompt-flag)
  "Store cache of local git repo directories in `hibtypes-git-repos-cache'.
With optional prompt-flag non-nil, prompt user whether to build the cache before building.
Return t if built, nil otherwise."
  (when (or (not prompt-flag)
	    (y-or-n-p "Find all local git repositories (will take some time)?"))
    (message "Please wait while all local git repositories are found...")
    (unless (zerop (shell-command (format "%s -r \.git$ | sed -e 's+/.git$++' > %s"
					  (if (and (boundp 'locate-command) (string-match "locate" locate-command))
					      locate-command
					    "locate")
					  hibtypes-git-repos-cache)))
      (error "(hibtypes-git-build-repos-cache): Cache build failed; `locate-command' must accept `-r' argument for regexp matching"))
    (message "Please wait while all local git repositories are found...Done")
    t))

(defun hibtypes-git-add-project-to-repos-cache (project)
  "Locate PROJECT directory and add to the cache of local git repo directories in `hibtypes-git-repos-cache'.
Return the project directory found or nil if none."
  (message "Please wait while %s's local git repository is found..." project)
  (let ((project-dir (shell-command-to-string
		      (format "%s -l1 /%s/.git | sed -e 's+/.git++' | tr -d '\n'"
			      (if (and (boundp 'locate-command) (string-match "locate" locate-command))
				  locate-command
				"locate")
			      project
			      hibtypes-git-repos-cache))))
    (message "")
    (when (and (> (length project-dir) 0) (= ?/ (aref project-dir 0)))
      ;; project-dir a directory, prepend it to the cache file...
      (shell-command-to-string (format "echo -e \"%s\n$(cat %s)\" > %s"
				       project-dir hibtypes-git-repos-cache
				       hibtypes-git-repos-cache))
      ;; ...and return it.
      project-dir)))

(defun hibtypes-git-build-or-add-to-repos-cache (project &optional prompt-flag)
  "Store cache of local git repo directories in `hibtypes-git-repos-cache'.
With optional prompt-flag non-nil, prompt user whether to build the cache before building.
Return t if built, nil otherwise."
  (if (and (file-readable-p hibtypes-git-repos-cache)
	   ;; Non-zero file size
	   (not (zerop (nth 7 (file-attributes hibtypes-git-repos-cache)))))
      (hibtypes-git-add-project-to-repos-cache project)
    (hibtypes-git-build-repos-cache t)))

(defun hibtypes-git-project-directory (project)
  "Given git PROJECT name, return local git repository directory or nil if none found."
  (if (or (and (file-readable-p hibtypes-git-repos-cache)
	       ;; Non-zero file size
	       (not (zerop (nth 7 (file-attributes hibtypes-git-repos-cache)))))
	  (hibtypes-git-build-repos-cache t))
      ;; echo -n deletes trailing newline
      (shell-command-to-string (format "grep -m1 '/%s$' %s | tr -d '\n'" project hibtypes-git-repos-cache))
    (message "")
    nil))

;; Pseudo-code for next action definition:
;;  1. If within a git repo directory, use that repo unless specified in path
;;  2. If project name is given or is default, see if assocated repo dir is in cache and use it.
;;  3. Prompt to rebuild locate db and then goto 2 if yes else quit
;;  4. Run: (cd <dir-found>; git <cmd> <item>)
;;  5. Otherwise, do nothing.
;;
;; Don't make this a defact or its arguments may be improperly expanded as pathnames.
(defun git-reference (reference &optional project)
  "Display the git entity associated with REFERENCE and optional PROJECT.
REFERENCE is of the form:
    <ref-item>
    /?<project>/<ref-item>
or  /<project>.

<ref-item> is one of these:
  one of the words: branches, commits, or tags; the associated items are listed;

  one of the words: branch, commit, or tag followed by a '/' and item id; the item is shown;

  a commit reference given by a hex number, 55a1f0; the commit diff is displayed;

  a branch or tag reference given by an alphanumeric name, e.g. hyper20; the
  files in the branch are listed.

If given, PROJECT overrides any project value in REFERENCE.  If no
PROJECT value is provided, it defaults to the value of
`hibtypes-git-default-project'."
  (if (or (null reference) (equal reference ""))
      (error "(git-reference): Git commit hashtag must not be empty")
    (let ((case-fold-search t)
	  (shell-cmd-to-format (assoc-default "git" hibtypes-social-hashtag-alist #'string-match)))
      (when shell-cmd-to-format
	(cond ((string-match "\\`\\(branch\\|commit\\|tag\\)/" reference)
	       ;; [branch | commit | tag]/ref-item
	       nil)
	      ((string-match "\\`/?\\([^/#@]+\\)/\\([0-9a-f]+\\)\\'" reference)
	       ;; /?project/hashtag
	       (setq project (or project (match-string-no-properties 1 reference))
		     reference (match-string-no-properties 2 reference)))
	      ((string-match "\\`/\\([^/#@]+\\)\\'" reference)
	       ;; /project
	       (setq project (or project (match-string-no-properties 1 reference))
		     reference nil))
	      ((string-match "/.*/" reference)
	       ;; Invalid user/project/hashtag
	       (error "(git-reference): Username or path not allowed, only <project>/<commit hashtag>")))
	(let ((cmd)
	      (ref-type)
	      ;; `project' now may be a project directory or a project name.
	      ;; If a project name:
	      ;;   If reference is within a git project, use its project directory.
	      ;;   Otherwise, look up the project in Hyperbole's local git repo directory cache;
	      ;;   the user is prompted to have it built when necessary.
	      (project-dir (or (and project (file-readable-p project) (file-directory-p project) project)
			       (locate-dominating-file default-directory ".git"))))
	  (unless (stringp project)
	    (unless (setq project (cond (project-dir (file-name-nondirectory (directory-file-name project-dir)))
					((stringp hibtypes-git-default-project)
					 hibtypes-git-default-project)))
	      (error "(git-reference): Set `hibtypes-git-default-project' to a default project name")))
	  (unless project-dir
	    (setq project-dir (and project (hibtypes-git-project-directory project))))
	(when reference
	  (cond ((member reference '("branches" "commits" "tags"))
		 ;; All branches, commits or commit tags reference
		 (setq ref-type reference
		       reference ""))
		((string-match "\\`\\(commit\\)/" reference)
		 ;; Specific reference preceded by keyword commit.
		 (setq ref-type (substring reference 1 (match-end 1))
		       reference (substring reference (match-end 0))))
		((string-match "\\`[0-9a-f]+\\'" reference)
		 ;; Commit reference
		 (setq ref-type "commit"))
		(t
		 ;; Specific branch or commit tag reference
		 (setq ref-type "tree/")
		 (when (string-match "\\`\\(branch\\|tag\\)/" reference)
		   ;; If preceded by optional keyword, remove that from the reference.
		   (setq reference (substring reference (match-end 0)))))))
	  (if (or (null project-dir) (equal project-dir ""))
	      (if (and project
		       ;; Maybe the Hyperbole git project cache is
		       ;; out-of-date and needs to be rebuilt or added
		       ;; to.  Prompt user and if rebuilt or added to,
		       ;; continue.
		       (hibtypes-git-build-or-add-to-repos-cache project t))
		  (setq project-dir (and project (hibtypes-git-project-directory project)))
		(error "(git-reference): No git directory found for project `%s'" project)))
	  (if (equal project-dir "") (setq project-dir nil))
	  (cond ((and project-dir (file-readable-p project-dir) (file-directory-p project-dir))
		 (if reference
		     ;; Display commit diffs in a help buffer
		     ;; Ensure these do not invoke with-output-to-temp-buffer a second time.
		     (let ((temp-buffer-show-hook)
			   (temp-buffer-show-function))
		       (setq cmd
			     (pcase ref-type
			       ("branches" (format shell-cmd-to-format project-dir "branch -la" ""))
			       ("commits"  (format shell-cmd-to-format project-dir "log --abbrev-commit --pretty=oneline" ""))
			       ("tags"     (format shell-cmd-to-format project-dir "tag -l" ""))
			       (t          (format shell-cmd-to-format project-dir "show" reference))))
		       (with-help-window (format "*git %s %s%s%s*" project ref-type
						 (if (not (equal reference "")) " " "")
						 reference)
			 (princ (format "Command: %s\n\n" cmd))
			 (princ (shell-command-to-string cmd))))
		   ;; Project-only reference, run dired on the project home directory
		   (hpath:display-buffer (dired-noselect
					  (file-name-as-directory project-dir)))))
		(t (if project-dir
		       (error "(git-reference): git project `%s' directory is unreadable or invalid: \"%s\""
			      project project-dir)
		     (error "(git-reference): No git project found for `%s'" project)))))))))

(provide 'hib-social)
