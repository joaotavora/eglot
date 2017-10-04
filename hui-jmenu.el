;;; hui-jmenu.el ---  Popup menus for jumping to and managing buffers, frames, and windows
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:     9-Mar-94 at 23:37:28
;;
;; Copyright (C) 1994-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   The following commands may be bound to keys or used within menus.
;;
;;     (hui-menu-screen-commands) - Modeline popup menu of jump to
;;                                   commands plus window and frame
;;                                   management commands.
;;
;;     (hui-menu-jump-to)          - Popup a single menu which selects a
;;                                   buffer, frame or window.
;;
;;     (hui-menu-jump-to-buffer)   - Popup buffer selection menu.
;;                                   Skips internal buffers whose names begin
;;                                   with a space.
;;
;;     (hui-menu-jump-to-frame)    - Popup frame selection menu.
;;                                   Includes invisible and iconified frames.
;;
;;     (hui-menu-jump-to-window)   - Popup window selection menu.
;;                                   Includes windows in invisible and iconified
;;                                   frames.
;;
;;   By default, Hyperbole sets `assist-key-modeline-function' to
;;   `hui-menu-screen-commands'.

;;; Code:
;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;###autoload
(defun hui-menu-of-buffers ()
  (let* ((buf-name)
	 (buffer-and-mode-name-list
	  ;; Remove internal buffers whose names begin with a space
	  ;; and killed buffers which have no name.
	  (delq nil
		(mapcar (lambda (buffer)
			  (setq buf-name (buffer-name buffer))
			  (and (stringp buf-name)
			       (/= (length buf-name) 0)
			       (not (eq (aref buf-name 0) ?\ ))
			       (cons buf-name
				     (hui-menu-buffer-mode-name buffer))))
			(buffer-list)))))
    (setq buffer-and-mode-name-list
	  (hui-menu-sort-buffers buffer-and-mode-name-list))
    (if (and (equal hui-menu-buffer-and-mode-list-cache buffer-and-mode-name-list)
	     hui-menu-of-buffers-cache)
	hui-menu-of-buffers-cache
      ;; Create sub-menus grouping buffers by mode-name.
      (let ((menu) (mode-menu) (prev-mode-name) mode-name buf-name)
	(mapc (lambda (name-and-mode)
		(setq buf-name (car name-and-mode)
		      mode-name (cdr name-and-mode))
		(if (not (equal mode-name prev-mode-name))
		    ;; Save previous mode-menu and begin a new one.
		    (progn (if mode-menu
			       (setq menu (cons (cons prev-mode-name
						      mode-menu) menu)
				     mode-menu nil))
			   (setq prev-mode-name mode-name)))
		;; Add current buffer to mode-menu.
		(setq mode-menu (cons
				 (vector buf-name
					 (list 'switch-to-buffer buf-name)
					 t)
				 mode-menu)))
	      buffer-and-mode-name-list)
	;; Save previous mode-menu.
	(if mode-menu
	    (setq menu (cons (cons prev-mode-name mode-menu) menu)))
	;; Uncomment if you want to limit category menu length to `hui-menu-max-list-length'.
	;;   (and (integerp hui-menu-max-list-length)
	;;        (> hui-menu-max-list-length 0)
	;;        (hui-menu-cutoff-list menu))
	;; Cache menus for next display.
	(setq hui-menu-buffer-and-mode-list-cache buffer-and-mode-name-list
	      hui-menu-of-buffers-cache (cons "Buffers" menu))))))

;;;###autoload
(defun hui-menu-screen-commands ()
  "Popup a menu of buffers, frames, and windows, allowing user to jump to one."
  (interactive)
  (popup-menu '("Hyperbole Screen Commands" :filter hui-menu-modeline)))

(defun hui-menu-jump-to ()
  "Popup a menu of buffers, frames, and windows, allowing user to jump to one."
  (interactive)
  (popup-menu (list "Jump to"
		    (hui-menu-of-buffers)
		    (hui-menu-of-frames)
		    (hui-menu-of-windows))))

;;;###autoload
(defun hui-menu-jump-to-buffer ()
  "Popup a menu of existing buffers categorized by mode name.  Jump to chosen buffer."
  (interactive)
  (popup-menu (cons "Jump to Buffer" (cdr (hui-menu-of-buffers)))))

;;;###autoload
(defun hui-menu-jump-to-frame ()
  "Popup a menu of existing frames.  Jump to chosen frame."
  (interactive)
  (popup-menu (cons "Jump to Frame" (cdr (hui-menu-of-frames)))))

;;;###autoload
(defun hui-menu-jump-to-window ()
  "Popup a menu of existing frames.  Jump to chosen frame."
  (interactive)
  (popup-menu (cons "Jump to Window" (cdr (hui-menu-of-windows)))))

(defconst hui-menu-hywconfig
  '("Window-Configuration"
    ["Manual" (id-info "(hyperbole)Window Configurations") t]
    "----"
    ["Name-Configuration" hywconfig-add-by-name     t]
    ["Delete-Name"        hywconfig-delete-by-name
     (if (boundp 'hywconfig-names) hywconfig-names)]
    ["Restore-Name"       hywconfig-restore-by-name
     (if (boundp 'hywconfig-names) hywconfig-names)]
    "----"
    ["Pop-from-Ring"      hywconfig-delete-pop      (not (hywconfig-ring-empty-p))]
    ["Save-to-Ring"       hywconfig-ring-save       t]
    ["Yank-from-Ring"     hywconfig-yank-pop        (not (hywconfig-ring-empty-p))]
    ))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hui-menu-buffer-mode-name (buffer)
  (let ((mname (cdr (assq 'mode-name (buffer-local-variables buffer)))))
    (if mname
	;; Next line needed to ensure mode name is always formatted as
	;; a string.
	(format-mode-line mname)
      (capitalize (symbol-name
		   (cdr (assq 'major-mode (buffer-local-variables buffer))))))))

(defun hui-menu-frame-name (frame)
  "Return the name of FRAME."
  (frame-parameter frame 'name))

(defun hui-menu-modeline (_ignore)
  (list
   ["Control-Frames"  hycontrol-frames t]
   ["Control-Windows" hycontrol-windows t]
   "----"
   (hui-menu-of-buffers)
   (hui-menu-of-frames)
   (hui-menu-of-windows)
   hui-menu-hywconfig))

   ;; "----"
   ;; ["Close-Buffer"                hui-menu-delete-buffer               t]
   ;; ["Close-Buffer-and-Window"     hui-menu-delete-buffer-and-window    t]
   ;; ["Move-Window-to-New-Frame"    hui-menu-move-window-to-new-frame    t]
   ;; "----"
   ;; '("Manage-Windows"
   ;;  ["Balance-Windows"           balance-windows                t]
   ;;  ["Delete-Window"             delete-window        (not (one-window-p t))]
   ;;  ["Delete-Other-Windows"      delete-other-windows (not (one-window-p t))]
   ;;  ["Split-Window-Stacked"      split-window-vertically        t]
   ;;  ["Split-Window-Side-by-Side" split-window-horizontally      t])
   ;; '("Manage-Frames"
   ;;  ["Create-Frame"              (select-frame (make-frame))    t]
   ;;  ["Delete-Frame"              hui-menu-delete-frame          t]
   ;;  ["Delete-All-Other-Frames"   delete-other-frames    (/= (length (frame-list)) 1)]
   ;;  ["Iconify-Frame"             iconify-frame                  t]
   ;;  ["Iconify-Emacs"             (mapc (lambda (frame) (iconify-frame frame)) (frame-list)) t]
   ;;  ["Lower-Frame"               lower-frame                    t]
   ;;  ["Other-Frame"               other-frame   (/= (length (frame-list)) 1)]
   ;;  ["Raise-Frame"               raise-frame                    t])

(defun hui-menu-to-frame (frame)
  (make-frame-visible frame)
  (raise-frame (select-frame frame)))
  
(defun hui-menu-to-window (window)
  (if (window-live-p window)
      (let ((frame (window-frame window)))
	(make-frame-visible frame)
	(raise-frame (select-frame frame))
	(select-window window))
    (error "(Hyperbole): `%s' window no longer exists" (buffer-name (window-buffer window)))))

(defun hui-menu-sort-buffers (buffer-and-mode-name-list)
  "Reverse sort and return list of (buffer-name . mode-name) elements by mode-name and then by buffer-name."
  (let ((buf (get-buffer-create " tmp-sort")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (let ((standard-output (current-buffer)))
	(mapc #'print buffer-and-mode-name-list))
     (while (search-forward "\n\n" nil t)
	(replace-match "\n"))
      (if (hui-menu-program-path "sort")
	  (call-process-region (point-min) (point-max)
			       "sort" t t nil "-r" "-k3,3" "-k1,1")
	;; This fallback of sort-fields can only sort on one field, so
	;; sort by major-mode and leave buffers within each mode
	;; unsorted when no UNIX sort program is available.
	(sort-fields 3 (point-min) (point-max))
	(reverse-region (point-min) (point-max)))
      (insert "\)\n")
      (goto-char (point-min))
      (insert "\(")
      (goto-char (point-min))
      (read (current-buffer)))))

(defun hui-menu-of-frames ()
  (let ((frames (copy-sequence (frame-list))))
    (hui-menu-cutoff-list frames)
    (cons "Frames"
	  (mapcar (lambda (frame)
		    (vector (hui-menu-frame-name frame)
			    (list 'hui-menu-to-frame frame)
			    t))
		  (sort frames
			(lambda (fm1 fm2)
			  (string-lessp (hui-menu-frame-name fm1)
					(hui-menu-frame-name fm2))))))))

(defun hui-menu-of-windows ()
  (let ((windows (hui-menu-window-list-all-frames 'nomini)))
    (hui-menu-cutoff-list windows)
    (cons "Windows"
	  (mapcar (lambda (window)
		    (vector (buffer-name (window-buffer window))
			    (list 'hui-menu-to-window window)
			    t))
		  (sort windows
			(lambda (wind1 wind2)
			  (string-lessp
			   (buffer-name (window-buffer wind1))
			   (buffer-name (window-buffer wind2)))))))))

(defun hui-menu-program-path (exe &optional insert-flag)
  "Return the full path name of the executable named by EXE.
This command searches the directories in `exec-path'.
With optional prefix arg INSERT-FLAG, inserts the pathname at point."
  (interactive "sGet pathname of executable: \nP")
  (catch 'answer
    (mapc
     (lambda (dir)
       (let ((path (expand-file-name exe dir)))
	 (and (file-executable-p path)
	      (null (file-directory-p path))
	      (progn
		(if insert-flag (insert path))
		(throw 'answer path)))))
     exec-path)
    nil))

(defun hui-menu-window-list-all-frames (&optional mini)
  "Returns a list of Lisp window objects for all Emacs windows in all frames.
Optional first arg MINI t means include the minibuffer window in the list,
even if it is not active.  If MINI is neither t nor nil it means to not count
the minibuffer window even if it is active."
  (let* ((first-window (next-window
			(previous-window (selected-window) nil t)
			mini t))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window mini t)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w mini t)))
    windows))

(defun    hui-menu-delete-buffer ()
  "Delete the current buffer, handling Emacs edit server frames properly."
  (interactive)
  (or (hui-menu-edit-server-finish) (kill-buffer)))

(defun    hui-menu-delete-buffer-and-window ()
  "Delete the current buffer and window, handling Emacs edit server frames properly."
  (interactive)
  (or (hui-menu-edit-server-finish)
      (progn (kill-buffer) (delete-window))))

(defun    hui-menu-delete-frame ()
  "Delete the selected frame, handling Emacs edit server frames properly."
  (interactive)
  (or (hui-menu-edit-server-finish) (delete-frame)))

(defun    hui-menu-move-window-to-new-frame ()
  "Delete the selected window if possible and display its buffer in a newly selected frame.
The window is deleted only if there are two or more windows in the selected
frame.  The current buffer is buried in the old frame's buffer list."
  (interactive)
  (let ((buffer (current-buffer)))
    (bury-buffer)
    (unless (one-window-p t)
      (delete-window))
    (select-frame (make-frame))
    (switch-to-buffer buffer)))

(defun hui-menu-server-buffer-p ()
  "Return t if the current buffer is attached to an edit server process, else nil."
  (and (boundp 'server-clients) server-clients
       (memq (current-buffer) (mapcar #'process-buffer server-clients))
       t))

(defun hui-menu-edit-server-finish ()
  (if (hui-menu-server-buffer-p)
      ;; If this buffer is the result of an edit request from an external
      ;; application, signal that edit is done and delete frame.
      (let ((buf (current-buffer)))
	(server-save-buffers-kill-terminal nil)
	(if (buffer-live-p buf) (kill-buffer buf))
	t)))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar hui-menu-buffer-and-mode-list-cache nil
  "Last set of buffer and mode names used in hui-menu-of-buffers or nil.")

(defvar hui-menu-of-buffers-cache nil
  "Last menu of mode-name ordered buffers from hui-menu-of-buffers or nil.")

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(provide 'hui-jmenu)

;;; hui-jmenu.el ends here
