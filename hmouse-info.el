;;; hmouse-info.el --- Walks through Info networks using one key.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    04-Apr-89
;;
;; Copyright (C) 1989-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;  This code is machine independent.
;;
;;  To install see "hui-mouse.el".

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'info)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun Info-read-index-item-name (prompt)
  "Read an Info index item name with completion, prompting with PROMPT.
An index item name can have the form \"itemname\", referring to an index
item in the current Info file, or \"(filename)itemname\", referring to
an item in filename.  \"(filename)\" is a short format to go to
the Top node in filename.  Signal an error if a filename without an
index is given."
  (let ((completion-ignore-case t)
	(Info-history-list nil)
	Info-complete-menu-buffer
	item-name)
    (save-window-excursion
      (info)
      (setq Info-complete-menu-buffer (clone-buffer)))
    (unwind-protect
	(progn (while (equal "" (setq item-name
				      (completing-read prompt #'Info-read-index-item-name-1 nil t))))
	       item-name)
      (kill-buffer Info-complete-menu-buffer))))

;;;###autoload
(defun smart-info ()
  "Walks through Info documentation networks using one key or mouse key.

If key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the desired node is found;
 (3) the File entry of a Node Header (first line),       
       the `Top' node within that file is found;
 (4) at the end of the current node, the Next node is found (this will
       descend subtrees if the function `Info-global-next' is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled up one windowful.

Returns t if key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil."

  (interactive)
  (cond 
    ;;
    ;; If at end of node, go to next node
    ;;
    ((last-line-p)
     (if (fboundp 'Info-global-next) (Info-global-next)
       (Info-next)))
    ((and (fboundp 'Info-mouse-follow-link)
	  (mouse-event-p action-key-release-args)
	  (let ((opoint (point)))
	    (Info-mouse-follow-link action-key-release-args)
	    (/= opoint (point)))))
    ((and (fboundp 'mouse-event-p)
	  (not (mouse-event-p action-key-release-args))
	  (Info-handle-in-node-hdr)))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ((pos-visible-in-window-p (point-max))
     (if (fboundp 'Info-global-next)
	 (Info-global-next)
       (Info-next)))
    ;;
    ;; If nothing else scroll forward a windowful.
    ;;
    ((smart-scroll-up))))

;;;###autoload
(defun smart-info-assist ()
  "Walks through Info documentation networks using one assist-key or mouse assist-key.

If assist-key is pressed within:
 (1) the first line of an Info Menu Entry or Cross Reference, the desired node
       is found;
 (2) the Up, Next, or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (3) the File entry of a Node Header (first line),       
       the `DIR' root-level node is found;
 (4) at the end of the current node, the Previous node is found (this will
       return from subtrees if the function 'Info-global-prev is bound);
 (5) anywhere else (e.g. at the end of a line), the current node entry is
       scrolled down one windowful.

Returns t if assist-key is pressed within an Info Node Header, Cross Reference,
or a Menu; otherwise returns nil."

  (interactive)
  (cond
    ;;
    ;; If at end or beginning of node, go to previous node
    ;;
    ((last-line-p)
     (if (fboundp 'Info-global-prev) (Info-global-prev)
       (Info-prev)))
    ((and (fboundp 'Info-mouse-follow-link)
	  (mouse-event-p assist-key-release-args)
	  (let ((opoint (point)))
	    (Info-mouse-follow-link assist-key-release-args)
	    (/= opoint (point)))))
    ((and (fboundp 'mouse-event-p)
	  (not (mouse-event-p assist-key-release-args))
	  (Info-handle-in-node-hdr-assist)))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ((pos-visible-in-window-p (point-min))
     (if (fboundp 'Info-global-prev)
	 (Info-global-prev)
       (Info-prev)))
    ;;
    ;; If anywhere else, scroll backward a windowful.
    ;;
    ((smart-scroll-down))))

;;; ************************************************************************
;;; Private functions - used only within Hyperbole
;;; ************************************************************************

(defun Info-handle-in-node-hdr ()
  "If within an Info node header, move to <FILE>Top, <Up>, <Previous>, or
<Next> node, depending on which label point is on, and return t.
Otherwise, return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (when (first-line-p)
    (let ((nodename "Top") (filep nil))
      (save-excursion
	(if (and
	     (re-search-forward "[:, \t\n\r]" nil t)
	     (re-search-backward
	      "\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\):[ \t]" nil t))
	    (progn (setq filep (string-equal
				"file"
				(downcase (match-string-no-properties 1))))
		   (if (re-search-forward (concat ":[ \n\r]\\([^,\t\n\r"
						  (if filep " ")
						  "]*\\)") nil t)
		       (setq nodename (match-string-no-properties 1))))
	  (error "Node header not found.")))
      (setq nodename
	    (cond ((eq (aref nodename 0) ?\() nodename)
		  (filep (concat "(" nodename ")" "Top"))
		  (buffer-file-name (concat "(" buffer-file-name ")" nodename))
		  (t nodename)))
      (if (featurep 'xemacs)
	  (Info-goto-node nodename nil t)
	(Info-goto-node nodename))
      t)))

(defun Info-handle-in-node-hdr-assist ()
  "If within an Info node header when the `smart-info-assist' command is
executed, when within the <FILE> header go to the DIR top-level node.  When
within any other header (<Up>, <Previous>, or <Next>) go to last node from
history list.  Return t if in Info node header.  Otherwise return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (when (first-line-p)
    (save-excursion
      (if (and 
	   (re-search-forward "[:, \t\n\r]" nil t)
	   (re-search-backward
	    "\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\):[ \t]" nil t) )
	  ;; If in <FILE> hdr
	  (progn (if (string-equal
		      "file"
		      (downcase (match-string-no-properties 1)))
		     (Info-directory)
		   (Info-last))
		 t)
	(error "Node header not found.")
	nil))))

;;;###autoload
(defun Info-handle-in-note ()
  "Follows an Info cross-reference.
If point is within the first line of an Info note (cross-reference), follows
cross-reference and returns t; otherwise returns nil."
  (let ((note-name (Info-note-at-p)))
    (when note-name
      (Info-follow-reference note-name) t)))

(defun Info-handle-in-menu ()
  "Displays node referred to by an Info Menu Entry.
If point is within an Info menu entry, goes to node referenced by
entry and returns t; otherwise returns nil."
  ;;
  ;; Test if there is a menu in this node
  ;;
  (when (Info-menu-item-at-p)
    (let ((node))
      (save-excursion
	(forward-char)	       ; Pass `*' char if point is in front of
	(when (search-backward "\n*" nil t)
	  (forward-char 2)
	  (setq node (Info-extract-menu-node-name nil (Info-index-node)))))
      (when node
	(if (featurep 'xemacs)
	    (Info-goto-node node nil t)
	  (Info-goto-node node))
	t))))

;;; ************************************************************************
;;; Private functions - used by the link-to-Info-index-item action type
;;; ************************************************************************

;;; Much of this is derived in part from "info.el".

(defun Info-build-menu-item-completions (string predicate action)
  ;; See comments in `Info-complete-menu-item' for free variables used.
  (with-current-buffer Info-complete-menu-buffer
    (save-excursion
      (let ((completion-ignore-case t)
            (case-fold-search t)
            (orignode Info-current-node)
            nextnode)
        (goto-char (point-min))
        (search-forward "\n* Menu:")
        (cond
         ((eq (car-safe action) 'boundaries) nil)
         (t
          (let ((pattern (concat "\n\\* +\\("
                                 (regexp-quote string)
                                 Info-menu-entry-name-re "\\):"
                                 Info-node-spec-re))
                completions
                (complete-nodes Info-complete-nodes))
            ;; Check the cache.
            (if (and (equal (nth 0 Info-complete-cache) Info-current-file)
                     (equal (nth 1 Info-complete-cache) Info-current-node)
                     (equal (nth 2 Info-complete-cache) Info-complete-next-re)
                     (equal (nth 5 Info-complete-cache) Info-complete-nodes)
                     (string-prefix-p (nth 3 Info-complete-cache) string) t)
                ;; We can reuse the previous list.
                (setq completions (nth 4 Info-complete-cache))
              ;; The cache can't be used.
              (while
                  (progn
                    (while (re-search-forward pattern nil t)
                      (push (match-string-no-properties 1)
                            completions))
		    (setq completions (delete-dups completions))
                    ;; Check subsequent nodes if applicable.
                    (or (and Info-complete-next-re
                             (setq nextnode (Info-extract-pointer "next" t))
                             (string-match Info-complete-next-re nextnode))
                        (and complete-nodes
                             (setq complete-nodes (cdr complete-nodes)
                                   nextnode (car complete-nodes)))))
                (Info-goto-node nextnode))
              ;; Go back to the start node (for the next completion).
              (unless (equal Info-current-node orignode)
                (Info-goto-node orignode))
              ;; Update the cache.
              (setq Info-complete-cache
		   (list Info-current-file Info-current-node
			 Info-complete-next-re string completions
			 Info-complete-nodes)))
	    completions)))))))

(defun Info-complete-menu-item (string predicate action)
  ;; This uses three dynamically bound variables:
  ;; - `Info-complete-menu-buffer' which contains the buffer in which
  ;; is the menu of items we're trying to complete.
  ;; - `Info-complete-next-re' which, if non-nil, indicates that we should
  ;; also look for menu items in subsequent nodes as long as those
  ;; nodes' names match `Info-complete-next-re'.  This feature is currently
  ;; not used.
  ;; - `Info-complete-nodes' which, if non-nil, indicates that we should
  ;; also look for menu items in these nodes.  This feature is currently
  ;; only used for completion in Info-index.

  ;; Note that `Info-complete-menu-buffer' could be current already,
  ;; so we want to save point.
  (complete-with-action action (Info-build-menu-item-completions
				string predicate action) string predicate))

;;;###autoload
(defun Info-current-filename-sans-extension ()
  "Return the filename for the current Info node, if any, without directory or file extension.
This works regardless of the current buffer."
  (save-window-excursion
    (info)
    (if (stringp Info-current-file)
	(file-name-sans-extension (file-name-nondirectory Info-current-file))
      ;; Some legacy code can still use a symbol.
      Info-current-file)))

;;;###autoload
(defun Info-menu-item-at-p ()
  "Return the name of the Info menu item at point, or nil if none."
  (let ( ;; If point is within a menu item, use that item as the default
	(p (point))
	beg
	(case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n* menu:" nil t)
	(setq beg (point))
	(and (< (point) p)
	     (save-excursion
	       (goto-char p)
	       (end-of-line)
	       (if (re-search-backward (concat "\n\\* +\\("
					       Info-menu-entry-name-re
					       "\\):")
				       beg t)
		   (match-string-no-properties 1))))))))

(defun Info-node-has-menu-p ()
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n* menu:" nil t))))

;;;###autoload
(defun Info-note-at-p ()
  "Return the name of the Info cross-reference note at point, or nil if none."
  (let ((note-name) (opoint (point)))
    (save-excursion
      (skip-chars-forward "^:")
      (if (and (re-search-backward
		"\*\\(Ref\\|Note\\|See\\)\\([ \t\n\r]+\\|$\\)" nil t)
	       (looking-at "\*\\(Ref\\|Note\\|See\\)[ \t\n\r]+\\([^:]*\\):")
	       (<= (match-beginning 0) opoint)
	       (> (match-end 0) opoint))
	  ;; Remove newline and extra spaces from `note-name'
	  (hypb:replace-match-string "[ \t\n\r]+" (match-string-no-properties 2)
				     " " t)))))

(defun Info-read-index-item-name-1 (string predicate code)
  "Internal function used by `Info-read-index-item-name'to generate completions.
See `completing-read' for a description of arguments and usage."
  (cond
   ;; First complete embedded file names.
   ((string-match "\\`([^)]*\\'" string)
    (completion-table-with-context
     "("
     (apply-partially #'completion-table-with-terminator ")"
                      (apply-partially #'Info-read-node-name-2
                                       Info-directory-list
                                       (mapcar #'car Info-suffix-list)))
     (substring string 1)
     predicate
     code))
   ;; If a file name was given, complete index-items in the file.
   ((string-match "\\`(\\([^)]+\\))" string)
    (let ((file0 (match-string 0 string))
	  (file1 (match-string 1 string))
	  (index-item (substring string (match-end 0)))
	  Info-complete-nodes
	  completions)
      (save-window-excursion
	(with-current-buffer Info-complete-menu-buffer
	  (Info-goto-node file0)
	  (Info-goto-index)
	  (setq Info-complete-nodes (Info-index-nodes)
		completions (Info-build-menu-item-completions index-item predicate code))))
      (completion-table-with-context file0 completions index-item predicate code)))
   ;; Otherwise, this must be a link within the current Info file, so generate
   ;; its index item completion table; if outside of the Info buffer, return nil.
   (t (when (and (minibuffer-selected-window)
		 (string-match "\\`*info*"
			       (buffer-name (window-buffer (minibuffer-selected-window)))))
	(let (Info-complete-nodes)
	  (save-window-excursion
	    (with-current-buffer Info-complete-menu-buffer
	      (Info-goto-index)
	      (setq Info-complete-nodes (Info-index-nodes))
	      (Info-build-menu-item-completions string predicate code))))))))

(provide 'hmouse-info)

;;; hmouse-info.el ends here
