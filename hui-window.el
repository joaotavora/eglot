;;; hui-window.el --- Smart Mouse Key window and modeline depress/release actions.
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    21-Sep-92
;;
;; Copyright (C) 1991-2016  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Must be loaded AFTER hmouse-alist has been defined in "hui-mouse.el".
;;
;;   Handles drags in same window or across windows and modeline depresses.
;;
;; What drags and modeline presses do.  (Note that a `thing' is a
;; delimited expression, such as a string, list or markup language tag
;; pair).
;; ==============================================================================
;;                                              Smart Keys
;; Context                         Action Key                 Assist Key
;; ==============================================================================
;; Drag from thing start or end    Yank thing at release      Kill thing and yank at release
;; Drag from shared window side
;;   or from left of scroll bar    Resize window width        <- same
;; Modeline depress & wind release Resize window height       <- same
;; Click in modeline
;;     Left window edge            Bury buffer                Unbury bottom buffer
;;     Right window edge           Info                       Smart Key Summary
;;     Otherwise                   Action Key Modeline Hook   Assist Key Modeline Hook
;; Drag between windows            Create/modify a link but   Swap window buffers
;; Drag in a window, region active Error, not allowed         Error, not allowed
;; Drag horizontally in a window   Split window below         Delete window
;; Drag vertically in a window     Split window side-by-side  Delete window
;; Drag diagonally in a window     Save window-config         Restore window-config from ring
;; Active region exists            Yank region at release     Kill region and yank at release

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-when-compile (defvar assist-flag nil)) ;; Silences free variable compiler warnings

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defcustom action-key-minibuffer-function #'hyperbole
  "*Function run by the Action Key after a click in an inactive minibuffer.
Its default value is #'hyperbole, which displays the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-minibuffer-function #'hui-menu-screen-commands
  "*Function run by the Assist Key after a click in an inactive minibuffer.
Its default value is #'hui-menu-screen-commands, which displays a popup menu
of screen control commands."
  :type 'function
  :group 'hyperbole-keys)

(defcustom action-key-modeline-function #'hmouse-context-menu
  "Function to call when the Action Mouse Key is clicked in the center portion of a modeline."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-modeline-function #'hui-menu-screen-commands
  "Function to call when the Assist Mouse Key is clicked in the center portion of a modeline."
  :type 'function
  :group 'hyperbole-keys)

 ;; Mats Lidell says this should be 10 characters for GNU Emacs.
(defvar hmouse-edge-sensitivity (if hyperb:emacs-p 10 3)
  "*Number of characters from window edges within which a click is considered at an edge.")

(defvar hmouse-side-sensitivity (if hyperb:emacs-p 5 1)
  "*Characters in either direction from window side within which a click is considered on the side.")

(defvar hmouse-x-drag-sensitivity 5
  "*Number of characters mouse must move horizontally between depress/release to register a horizontal drag.")

(defvar hmouse-y-drag-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a vertical drag.")

(defvar hmouse-x-diagonal-sensitivity 4
  "*Number of characters mouse must move horizontally between depress/release to register a diagonal drag.")
(defvar hmouse-y-diagonal-sensitivity 3
  "*Number of lines mouse must move vertically between depress/release to register a diagonal drag.")

;; Ensure any helm item at Action Mouse Key depress point is selected
;; before a drag that ends in another window.
(add-hook 'action-key-depress-hook
	  (lambda () (if (eq major-mode 'helm-major-mode)
			 ;; Select any line with an action.
			 (smart-helm-line-has-action))))

;;;
;;; Add window handling to hmouse-alist dispatch table.
;;;
(if (not (boundp 'hmouse-alist))
    (error
      "\"hui-window.el\": `hmouse-alist' must be defined before loading this.")
  (or (assoc #'(hmouse-drag-thing) hmouse-alist)
      (setq hmouse-alist
	    (append
	      '(
		;; If click in the minibuffer when it is not active (blank),
		;; display the Hyperbole minibuffer menu or popup the jump menu.
		((hmouse-inactive-minibuffer-p) .
		 ((funcall action-key-minibuffer-function) . 
		  (funcall assist-key-minibuffer-function)))
		((hmouse-drag-thing) .
		 ((hmouse-yank-region) . (hmouse-kill-and-yank-region)))
		((hmouse-drag-window-side) .
		 ((hmouse-resize-window-side) . (hmouse-resize-window-side 'assist)))
		((hmouse-modeline-depress) .
		 ((action-key-modeline) . (assist-key-modeline)))
		((hmouse-drag-between-windows) .
		 ;; Note that `hui:link-directly' uses any active
		 ;; region as the label of the button to create.
		 ((or (hmouse-drag-item-to-window) (hui:link-directly)) . (hmouse-swap-buffers 'assist)))
		((hmouse-drag-region-active) .
		 ((hmouse-drag-not-allowed) . (hmouse-drag-not-allowed)))
		((setq hkey-value (and (not (hmouse-drag-between-windows))
				       (hmouse-drag-horizontally))) .
		 ((hmouse-horizontal-action-drag) . (hmouse-horizontal-assist-drag)))
		((hmouse-drag-vertically) .
		 ((hmouse-vertical-action-drag) . (hmouse-vertical-assist-drag)))
		((setq hkey-value (hmouse-drag-diagonally)) .
		 ((hywconfig-ring-save) . (hywconfig-yank-pop
					   (prefix-numeric-value current-prefix-arg))))
		;;
		;; Now since this is not a drag and if there was an active
		;; region prior to when the Action or Assist Key was
		;; pressed, then store point at one end of the region into
		;; `hkey-value' and the string value of the region
		;; into `hkey-region' which is either yanked, or
		;; killed and yanked at the current point.
		((hmouse-prior-active-region) .
		 ((hmouse-yank-region) . (hmouse-kill-and-yank-region)))
		;;
		)
	      hmouse-alist))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun hmouse-context-menu ()
  "If running under a window system, display or hide the buffer menu.
If not running under a window system and Smart Menus are loaded, display the
appropriate Smart Menu for the context at point.  (Smart Menus are a
part of InfoDock and not a part of Hyperbole)."
  (interactive)
  (if (and (fboundp 'smart-menu)
	   (or (null window-system)
	       (not (or hyperb:emacs-p (featurep 'xemacs)))))
      (smart-menu)
    (let ((wind (get-buffer-window "*Buffer List*"))
	  owind)
      (if wind
	  (unwind-protect
	      (progn (setq owind (selected-window))
		     (select-window wind)
		     (bury-buffer nil))
	    (select-window owind))
	(buffer-menu)))))

(defun hmouse-context-ibuffer-menu ()
  "If running under a window system, display or hide the IBuffer menu.
If not running under a window system and Smart Menus are loaded, display the
appropriate Smart Menu for the context at point.  (Smart Menus are a
part of InfoDock and not a part of Hyperbole)."
  (interactive)
  (if (and (fboundp 'smart-menu)
	   (or (null window-system)
	       (not (or hyperb:emacs-p (featurep 'xemacs)))))
      (smart-menu)
    (let ((wind (get-buffer-window "*Ibuffer*"))
	  owind)
      (if wind
	  (unwind-protect
	      (progn (setq owind (selected-window))
		     (select-window wind)
		     (bury-buffer nil))
	    (select-window owind))
	(ibuffer)))))

(defun hmouse-prior-active-region ()
  (and (region-active-p)
       ;; Store and goto any prior value of point from the region
       ;; prior to the Smart Key depress, so we can return to it later.
       (setq hkey-value (marker-position
			 (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point)))
       (goto-char hkey-value)
       (hmouse-save-region)))

(defun hmouse-drag-region-active ()
  "Return non-nil if an active region existed in the depress buffer prior to the depress and a drag motion has occurred."
  (save-excursion
    (hmouse-goto-depress-prev-point)
    (and (region-active-p)
	 (or (hmouse-drag-vertically) (hmouse-drag-horizontally) (hmouse-drag-diagonally))
	 (setq hkey-value (point)))))

(defun hmouse-drag-thing ()
  "Return t if no region is active and a Smart Key drag began at the start/end of a delimited construct and ended at some other point in the same buffer, else nil.
Delimited constructs include lists, comments, strings,
 arrays/vectors, sets, and markup pair tags, such as <div>
 </div>.  Point must be on the start or end delimiter or in the
 case of markup pair tags, on the first character of either tag.
 For strings and comments, point must be on the first line."
    ;; Move point back to Smart Key depress location for testing whether at a thing.
    (let ((depress-args (if assist-flag assist-key-depress-args action-key-depress-args))
	  (release-args (if assist-flag assist-key-release-args action-key-release-args))
	  (marked-thing))
      (save-excursion
	(hmouse-goto-depress-point)
	(if (and (not (region-active-p)) (hui-select-at-delimited-thing-p)
		 (or (markerp depress-args) (markerp release-args)
		     (and (not (or (hmouse-drag-window-side) (hmouse-modeline-depress)))
			  (or (hmouse-drag-between-windows) (hmouse-drag-vertically) 
			      (hmouse-drag-horizontally) (hmouse-drag-diagonally))))
		 (let ((start-buf (window-buffer (smart-window-of-coords depress-args)))
		       (end-buf (window-buffer (smart-window-of-coords release-args)))
		       (start-point (smart-point-of-coords depress-args))
		       (end-point (smart-point-of-coords release-args)))
		   ;; Ignore this, if it is a click or if drag end point is within the thing to operate upon
		   (not (and (eq start-buf end-buf)
			     (/= start-point end-point)
			     (setq marked-thing (hui-select-delimited-thing))
			     (>= end-point (min (point) (mark)))
			     (<= end-point (max (point) (mark)))))))
	    (progn (if (not (region-active-p)) (hui-select-delimited-thing))
		   ;; Store any new value of point as a result of marking the region,
		   ;; so we can return to it later.
		   (setq hkey-value (point))
		   (hmouse-save-region)
		   t)
	  (if marked-thing (deactivate-mark))
	  nil))))

(defun hmouse-kill-region ()
  "Kill the marked region near where the Smart Key was depressed.
Signals an error if the depress buffer is read-only."
  ;; Move point back to Smart Key depress buffer but not necessarily
  ;; the same point at the depress since region selection may have
  ;; moved it.
  (hmouse-goto-region-point)
  (if buffer-read-only
      ;; In this case, we want an error that will terminate execution so that
      ;; hkey-region is not reset to nil.  This allows the user to fix the
      ;; problem and then to try killing again.
      (error "(hmouse-kill-region): Use {%s} to enable killing from this buffer."
	     (hmouse-read-only-toggle-key))
    (kill-region (point) (mark))))

(defun hmouse-kill-and-yank-region ()
  "Kill the marked region near where the Smart Key was depressed and yank it at the point of release.
Signals an error if either depress or release buffer is read-only."
  ;; Move point back to Smart Key depress buffer but not necessarily
  ;; the same point, for testing if real-only.
  (hmouse-goto-region-point)
  (if buffer-read-only
      ;; In this case, we want an error that will terminate execution so that
      ;; hkey-region is not reset to nil.  This allows the user to fix the
      ;; problem and then to try killing again.
      (error "(hmouse-kill-and-yank-region): Use {%s} to enable killing from this buffer."
	     (hmouse-read-only-toggle-key))
    ;; Depress and release may be in the same buffer, in which case, 
    ;; save the release point that may change as a result of
    ;; the kill; also, before the kill, restore the point to where it
    ;; was when the region was set.
    (hmouse-goto-release-point)
    (let ((release-point (point-marker)))
      (if buffer-read-only
	  ;; In this case, we want an error that will terminate execution so that
	  ;; hkey-region is not reset to nil.  This allows the user to fix the
	  ;; problem and then to try yanking again.
	  (error "(hmouse-kill-and-yank-region): Use {%s} to enable yanking into this buffer."
		 (hmouse-read-only-toggle-key))
	;; Now kill and yank the region into the Smart Key release buffer.
	(hmouse-goto-region-point)
	(kill-region (point) (mark))
	;; Permanently return to release point
	(select-window (if assist-flag assist-key-release-window action-key-release-window))
	(goto-char release-point)
	(hmouse-insert-region)))))

(defun hmouse-yank-region ()
  "Yank the region of text saved in `hkey-region' into the current buffer.
Signals an error if the buffer is read-only."
  ;; If a region was just selected for yanking, deactivate it as we
  ;; have already copied the region into `hkey-region'.
  (hmouse-goto-region-point)
  (if (region-active-p) (deactivate-mark))
  (hmouse-goto-release-point)
  (if buffer-read-only
      ;; In this case, we want an error that will terminate execution so that
      ;; hkey-region is not reset to nil.  This allows the user to fix the
      ;; problem and then to try yanking again.
      (error "(hmouse-yank-region): Use {%s} to enable yanking into this buffer."
	     (hmouse-read-only-toggle-key))
    ;; Permanently return to release point
    (select-window (if assist-flag assist-key-release-window action-key-release-window))
    (hmouse-insert-region)))

(defun hmouse-drag-between-windows ()
  "Returns non-nil if last Action Key depress and release were in different windows.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (if assist-flag
      (and assist-key-depress-window assist-key-release-window
	   (not (eq assist-key-depress-window
		    assist-key-release-window)))
    (and action-key-depress-window action-key-release-window
	 (not (eq action-key-depress-window action-key-release-window)))))

(defun hmouse-drag-item-to-window ()
  "Depress on a buffer name in Buffer-menu or ibuffer mode and release in another window in which to display the buffer.
Return t unless source is not a buffer menu mode, then nil."
  (let ((mode (cdr (assq 'major-mode (buffer-local-variables (window-buffer action-key-depress-window))))))
    (when (memq mode '(Buffer-menu-mode ibuffer-mode helm-major-mode))
      (hmouse-item-to-window)
      t)))

(defun hmouse-drag-diagonally ()
  "Returns non-nil iff last Action Key use was a diagonal drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a diagonal drag, or one of the following symbols
depending on the direction of the drag: southeast, southwest, northwest, northeast."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-diagonal-sensitivity)
	 (>= (- (max last-depress-y last-release-y)
		(min last-depress-y last-release-y))
	     hmouse-y-diagonal-sensitivity)
	 (cond
	   ((< last-depress-x last-release-x)
	    (if (< last-depress-y last-release-y)
		'southeast 'northeast))
	   (t (if (< last-depress-y last-release-y)
		  'southwest 'northwest))))))

(defun hmouse-drag-horizontally ()
  "Returns non-nil iff last Action Key use was a horizontal drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a horizontal drag, 'left if drag moved left or
'right otherwise."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-drag-sensitivity)
	 ;; Don't want to register vertical drags here, so ensure any
	 ;; vertical movement was less than the vertical drag sensitivity.
	 (< (- (max last-depress-y last-release-y)
	       (min last-depress-y last-release-y))
	    hmouse-y-drag-sensitivity)
	 (if (< last-depress-x last-release-x) 'right 'left))))

(defun hmouse-drag-vertically ()
  "Returns non-nil iff last Action Key use was a vertical drag within a single window.
If free variable `assist-flag' is non-nil, uses Assist Key.
Value returned is nil if not a vertical line drag, 'up if drag moved up or
'down otherwise."
  (let ((last-depress-x) (last-release-x)
	(last-depress-y) (last-release-y))
    (if assist-flag
	(setq last-depress-x (hmouse-x-coord assist-key-depress-args)
	      last-release-x (hmouse-x-coord assist-key-release-args)
	      last-depress-y (hmouse-y-coord assist-key-depress-args)
	      last-release-y (hmouse-y-coord assist-key-release-args))
      (setq last-depress-x (hmouse-x-coord action-key-depress-args)
	    last-release-x (hmouse-x-coord action-key-release-args)
	    last-depress-y (hmouse-y-coord action-key-depress-args)
	    last-release-y (hmouse-y-coord action-key-release-args)))
    (and last-depress-x last-release-x last-depress-y last-release-y
	 (>= (- (max last-depress-y last-release-y)
		(min last-depress-y last-release-y))
	     hmouse-y-drag-sensitivity)
	 ;; Don't want to register horizontal drags here, so ensure any
	 ;; horizontal movement was less than or equal to the horizontal drag
	 ;; sensitivity.
	 (<= (- (max last-depress-x last-release-x)
		(min last-depress-x last-release-x))
	     hmouse-x-drag-sensitivity)
	 (if (< last-depress-y last-release-y) 'down 'up))))

(defun hmouse-drag-window-side ()
  "Returns non-nil if Action Key was dragged from a window side divider.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (cond ((featurep 'xemacs)
	 ;; Depress events in scrollbars or in non-text area of buffer are
	 ;; not visible or identifiable at the Lisp-level, so always return
	 ;; nil.
	 nil)
	((hyperb:window-system)
	 (let* ((depress-args (if assist-flag assist-key-depress-args
				action-key-depress-args))
		(release-args (if assist-flag assist-key-release-args
				action-key-release-args))
		(w (smart-window-of-coords depress-args))
		(right-side-ln (and w (1- (nth 2 (window-edges w)))))
		(last-press-x   (and depress-args (hmouse-x-coord depress-args)))
		(last-release-x (and release-args (hmouse-x-coord release-args))))
	   (and last-press-x last-release-x right-side-ln
		(/= last-press-x last-release-x)
		(not (<= (abs (- right-side-ln (frame-width))) 5))
		(<= (abs (- last-press-x right-side-ln))
		    hmouse-side-sensitivity))))))

(defun hmouse-read-only-toggle-key ()
  "Return the first key binding that toggles read-only mode, or nil if none."
  (key-description (or (where-is-internal #'read-only-mode nil t)
		       (where-is-internal #'vc-toggle-read-only nil t)
		       (where-is-internal #'toggle-read-only nil t))))

(defun hmouse-vertical-action-drag ()
  "Handles an Action Key vertical drag within a window: adds a window to the right of this one.
Beeps and prints message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (split-window-horizontally nil)
    (error (beep)
	   (message "(hmouse-vertical-action-drag): Can't split the window further."))))

(defun hmouse-vertical-assist-drag ()
  "Handles an Assist Key vertical drag within a window: deletes the current window.
Beeps and prints message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-vertical-assist-drag): A single window cannot be deleted."))))

(defun hmouse-horizontal-action-drag ()
  "Handles an Action Key vertical drag within a window: adds a window below this one.
Beeps and prints message if the window cannot be split further."
  (interactive)
  (condition-case ()
      (if (fboundp 'split-window-quietly)
	  (split-window-quietly nil)
	(split-window-vertically nil))
    (error (beep)
	   (message "(hmouse-horizontal-action-drag): Can't split the window further."))))

(defun hmouse-horizontal-assist-drag ()
  "Handles an Assist Key horizontal drag within a window: deletes the current window.
Beeps and prints message if the window cannot be split further."
  (condition-case ()
      (delete-window)
    (error (beep)
	   (message "(hmouse-horizontal-assist-drag): A single window cannot be deleted."))))

(defun smart-coords-in-window-p (coords window)
  "Tests if COORDS are in WINDOW.  Returns WINDOW if they are, nil otherwise."
  (cond ((null coords) nil)
	((and hyperb:emacs-p (eventp coords))
	 (let ((w-or-f (posn-window (event-start coords))))
	   (if (framep w-or-f) (setq w-or-f (frame-selected-window w-or-f)))
	   (eq w-or-f window)))
	((if (featurep 'xemacs)
	     (if (eventp coords)
		 (eq (event-window coords) window)
	       (eq (car coords) window))))
	((fboundp 'window-edges)
	 (let* ((edges (window-edges window))
		  (w-xmin (nth 0 edges))
		  (w-ymin (nth 1 edges))
		  (w-xmax (nth 2 edges))
		  (w-ymax (nth 3 edges))
		  (x  (hmouse-x-coord coords))
		  (y  (hmouse-y-coord coords)))
	     (and (<= w-xmin x) (<= x w-xmax)
		  (<= w-ymin y) (<= y w-ymax)
		  window)))))

(defun smart-point-of-coords (coords)
  "Returns point within window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((markerp coords)
	 (marker-position coords))
	((and hyperb:emacs-p (eventp coords))
	 (posn-point (event-start coords)))
	((and (featurep 'xemacs) (eventp coords))
	 (event-point coords))))

(defun smart-window-of-coords (coords)
  "Returns window in which COORDS fall or nil if none.
Ignores minibuffer window."
  (cond ((markerp coords)
	 (get-buffer-window (marker-buffer coords)))
	((and hyperb:emacs-p (eventp coords))
	 (let ((w-or-f (posn-window (event-start coords))))
	   (if (framep w-or-f) (setq w-or-f (frame-selected-window w-or-f)))
	   w-or-f))
	((if (featurep 'xemacs)
	     (if (eventp coords)
		 (event-window coords)
	       (car coords))))
	(t (let ((window-list (hypb:window-list 'no-minibuf))
		 (window)
		 (w))
	     (while (and (not window) window-list)
	       (setq w (car window-list)
		     window-list (cdr window-list)
		     window (smart-coords-in-window-p coords w)))
	     window))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun hmouse-drag-not-allowed ()
  "Display an error when a region is active and in-window drags are not allowed."
  ;; Return point to where it was prior to depress so the region does not permanently change.
  (goto-char hkey-value)
  (error "(hmouse-drag-region-active): Region is active; Smart Key drags are not allowed.  Use a press/click instead."))

(defun hmouse-set-buffer-and-point (buffer point)
  (when buffer
    (set-buffer buffer)
    (if point (goto-char point))))

(defun hmouse-goto-depress-prev-point ()
  "Temporarily set point to where the last Smart Key was depressed."
  (let ((buf (marker-buffer (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point)))
	(loc (marker-position (if assist-flag assist-key-depress-prev-point action-key-depress-prev-point))))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-goto-depress-point ()
  "Temporarily set point to where the last Smart Key was depressed."
  (let ((buf (window-buffer (if assist-flag assist-key-depress-window action-key-depress-window)))
	(loc (smart-point-of-coords (if assist-flag assist-key-depress-args action-key-depress-args))))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-goto-region-point ()
  "Temporarily set point back to where it was when the region was activated."
  (let ((buf (window-buffer (if assist-flag assist-key-depress-window action-key-depress-window)))
	(loc hkey-value))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-goto-release-point ()
  "Temporarily set point to where the last Smart Key was depressed."
  (let ((buf (window-buffer (if assist-flag assist-key-release-window action-key-release-window)))
	(loc (smart-point-of-coords (if assist-flag assist-key-release-args action-key-release-args))))
    (hmouse-set-buffer-and-point buf loc)))

(defun hmouse-inactive-minibuffer-p ()
  "Return t if the last command event was a mouse press or release within an inactive minibuffer, else nil."
  (let ((window (posn-window (event-start last-command-event))))
    (if (framep window) (setq window (frame-selected-window window)))
    (and (window-minibuffer-p window)
	 (not (minibuffer-window-active-p window)))))

(defun hmouse-insert-region ()
  "Save a mark, then insert at point the text from `hkey-region' and indent it."
  (indent-for-tab-command)
  (push-mark nil t)
  (if (eobp) (insert "\n"))
  (insert hkey-region)
  (indent-region (point) (mark))
  (message "") ;; Clear any indenting message.
  ;; From par-align.el library; need to think about all the
  ;; possibilities before enabling filling of this region.
  ;; (if (fboundp 'fill-region-and-align) (fill-region-and-align (mark) (point)))
  )

(defun hmouse-item-to-window ()
  "Displays buffer or file menu item at Action Key depress in window of Action Key release."
  (let* ((w1 action-key-depress-window)
	 (w2 action-key-release-window)
	 (buf-name)
	 (w1-ref (when (and w1 w2)
		   (unwind-protect
		       (progn (select-window w1)
			      (cond ((eq major-mode 'Buffer-menu-mode)
				     (Buffer-menu-buffer t))
				    ((eq major-mode 'ibuffer-mode)
				     (ibuffer-current-buffer t))
				    ((eq major-mode 'helm-major-mode)
				     ;; Returns item string
				     (helm-get-selection (current-buffer)))
				    (t nil)))
		     (select-window w2)))))
    (unwind-protect
	(cond ((not w1-ref)
	       (error "(hmouse-item-to-window): Last depress was not within a window."))
	      ((buffer-live-p w1-ref)
	       (set-window-buffer w2 w1-ref))
	      ((and (stringp w1-ref) (file-readable-p w1-ref))
	       (set-window-buffer w2 (find-file-noselect w1-ref)))
	      (t (error "(hmouse-item-to-window): Cannot find or read `%s'." w1-ref)))
      ;; If helm is active, end in the minibuffer window.
      (if (smart-helm-alive-p)
	  (smart-helm-to-minibuffer)))))

(defun action-key-modeline ()
  "Handles Action Key depresses on a window mode line.
If the Action Key is:
 (1) clicked on left edge of a window's modeline,
     window's buffer is buried (placed at bottom of buffer list);
 (2) clicked on right edge of a window's modeline,
     the Info buffer is displayed, or if already displayed and the
     modeline clicked belongs to a window displaying Info, the Info
     buffer is hidden;
 (3) clicked anywhere in the middle of a window's modeline,
     the function given by `action-key-modeline-function' is called;
 (4) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((w (smart-window-of-coords action-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click)
	   (cond ((hmouse-release-left-edge)  (bury-buffer))
		 ((hmouse-release-right-edge)
		  (if (eq major-mode 'Info-mode)
		      (Info-exit)
		    (info)))
		 (t (funcall action-key-modeline-function))))
	  (t (hmouse-modeline-resize-window)))))

(defun assist-key-modeline ()
  "Handles Assist Key depresses on a window mode line.
If the Assist Key is:
 (1) clicked on left edge of a window's modeline,
     bottom buffer in buffer list is unburied and placed in window;
 (2) clicked on right edge of a window's modeline,
     the summary of Smart Key behavior is displayed, or if already
     displayed and the modeline clicked belongs to a window displaying
     the summary, the summary buffer is hidden;
 (3) clicked anywhere in the middle of a window's modeline,
     the function given by `assist-key-modeline-function' is called;
 (4) dragged vertically from modeline to within a window,
     the modeline is moved to point of key release, thereby resizing
     its window and potentially its vertical neighbors."
  (let ((buffers)
	(w (smart-window-of-coords assist-key-depress-args)))
    (if w (select-window w))
    (cond ((hmouse-modeline-click 'assist)
	   (cond ((hmouse-release-left-edge 'assist)
		  (if (fboundp 'last)
		      (switch-to-buffer (car (last (buffer-list))))
		    (setq buffers (buffer-list))
		    (switch-to-buffer (nth (1- (length buffers)) buffers))))
		 ((hmouse-release-right-edge 'assist)
		  (if (string-match "Hyperbole Smart Keys" (buffer-name))
		      (hkey-help-hide)
		    (hkey-summarize 'current-window)))
		 (t (funcall assist-key-modeline-function))))
	  (t (hmouse-modeline-resize-window 'assist)))))

(defun hmouse-modeline-click (&optional assist-flag)
  "Returns non-nil if last Action Key depress and release was at same point in a modeline.
Optional ASSIST-FLAG non-nil means test for Assist Key click instead."
  ;; Assume depress was in modeline and that any drag has already been handled.
  ;; So just check that release was in modeline.
  (hmouse-modeline-release assist-flag))

(defun hmouse-modeline-depress ()
  "Returns non-nil if Action Key was depressed on a window mode line.
If free variable `assist-flag' is non-nil, uses Assist Key."
  (let ((args (if assist-flag assist-key-depress-args
		action-key-depress-args)))
    (if (and (hyperb:window-system) args)
	(if (fboundp 'event-over-modeline-p)
	    (event-over-modeline-p args)
	  (let* ((w (smart-window-of-coords args))
		 (mode-ln (if w (nth 3 (window-edges w))))
		 (last-press-y (hmouse-y-coord args)))
	    ;; Mode-line is always 1 less than the bottom of the window, unless it
	    ;; is a minibuffer window which does not have a modeline.
	    (if (not (eq w (minibuffer-window))) (setq mode-ln (1- mode-ln)))
	    (and last-press-y mode-ln (= last-press-y mode-ln)))))))

(defun hmouse-modeline-release (&optional assist-flag)
  "Returns non-nil if Action Key was released on a window mode line.
Optional non-nil ASSIST-FLAG means test release of Assist Key instead."
  (let ((args (if assist-flag assist-key-release-args
		action-key-release-args)))
    (if (and (hyperb:window-system) args)
	(if (fboundp 'event-over-modeline-p)
	    (event-over-modeline-p args)
	  (let* ((w (smart-window-of-coords args))
		 (mode-ln (and w (1- (nth 3 (window-edges w)))))
		 (last-press-y (hmouse-y-coord args)))
	    (and last-press-y mode-ln (= last-press-y mode-ln)))))))

(defun hmouse-modeline-resize-window (&optional assist-flag)
  "Resizes window whose mode line was depressed upon by the Action Key.
Resize amount depends upon the vertical difference between press and release
of the Action Key.  Optional arg ASSIST-FLAG non-nil means use values from
Assist Key instead."
  (cond ((not (hyperb:window-system)) nil)
	((and (featurep 'xemacs) (not (fboundp 'window-edges)))
	 (error "Drag from a mode-line with button1 to resize windows."))
	(t (let* ((owind (selected-window))
		  (window (smart-window-of-coords
			   (if assist-flag assist-key-depress-args
			     action-key-depress-args)))
		  (mode-ln (and window (1- (nth 3 (window-edges window)))))
		  (last-release-y
		   (hmouse-y-coord
		    (if assist-flag assist-key-release-args
		      action-key-release-args)))
		  (shrink-amount (- mode-ln last-release-y)))
	     ;; Restore position of point prior to Action Key release.
	     (if action-key-release-prev-point
		 (let ((obuf (current-buffer)))
		   (unwind-protect
		       (progn
			 (set-buffer
			  (marker-buffer action-key-release-prev-point))
			 (goto-char
			  (marker-position action-key-release-prev-point)))
		     (set-buffer obuf))))
	     (cond
	      ((>= (+ mode-ln 2) (frame-height))
	       (error
		"(hmouse-modeline-resize-window): Can't move bottom window in frame."))
	      ((< (length (hypb:window-list 'no-minibuf)) 2)
	       (error
		"(hmouse-modeline-resize-window): Can't resize sole window in frame."))
	      (t (unwind-protect
		     (progn
		       (select-window window)
		       (shrink-window shrink-amount)
		       ;; Keep redisplay from scrolling other window.
		       (select-window (next-window nil 'no-mini))
		       (condition-case ()
			   (scroll-down shrink-amount)
			 (error nil)))
		   (select-window owind))))))))

(defun hmouse-release-left-edge (&optional assist-flag)
  "Returns non-nil if last Action Key release was at left window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-left last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-left 0)
      (setq window-left (car (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
    (and last-release-x (< (- last-release-x window-left)
			   hmouse-edge-sensitivity)
	 (>= (- last-release-x window-left) 0))))

(defun hmouse-release-right-edge (&optional assist-flag)
  "Returns non-nil if last Action Key release was at right window edge.
`hmouse-edge-sensitivity' value determines how near to actual edge the
release must be."
  (let ((args (if assist-flag assist-key-release-args
		 action-key-release-args))
	window-right last-release-x)
    (if (fboundp 'window-lowest-p) ;; XEmacs >= 19.12 
	(setq last-release-x (and args (eq (event-window args)
					   (selected-window))
				  (hmouse-x-coord args))
	      window-right (window-width))
      (setq window-right (nth 2 (window-edges))
	    last-release-x (and args (hmouse-x-coord args))))
    (and last-release-x (>= (+ last-release-x hmouse-edge-sensitivity)
			    window-right)
	 (>= (- window-right last-release-x) 0))))

(defun hmouse-resize-window-side (&optional assist-flag)
  "Resizes window whose side was depressed upon by the Action Key.
Resize amount depends upon the horizontal difference between press and release
of the Action Key.  Optional arg ASSIST-FLAG non-nil means use values from
Assist Key instead."
  (cond ((featurep 'xemacs)
	 ;; Depress events in scrollbars or in non-text area of buffer are
	 ;; not visible or identifiable at the Lisp-level, so always return
	 ;; nil.
	 nil)
	((hyperb:window-system)
	 (let* ((owind (selected-window))
		(window (smart-window-of-coords
			 (if assist-flag assist-key-depress-args
			   action-key-depress-args)))
		(right-side-ln (and window (1- (nth 2 (window-edges window)))))
		(last-release-x
		 (hmouse-x-coord
		  (if assist-flag assist-key-release-args
		    action-key-release-args)))
		(shrink-amount (- right-side-ln last-release-x))
		)
	   ;; Restore position of point prior to Action Key release.
	   (if action-key-release-prev-point
	       (let ((obuf (current-buffer)))
		 (unwind-protect
		     (progn
		       (set-buffer (marker-buffer action-key-release-prev-point))
		       (goto-char (marker-position action-key-release-prev-point)))
		   (set-buffer obuf))))
	   (cond
	    ((>= (+ right-side-ln 2) (frame-width))
	     (error
	      "(hmouse-resize-window-side): Can't change width of full frame width window."))
	    ((< (length (hypb:window-list 'no-minibuf)) 2)
	     (error
	      "(hmouse-resize-window-side): Can't resize sole window in frame."))
	    (t (unwind-protect
		   (progn
		     (select-window window)
		     (shrink-window-horizontally shrink-amount))
		 (select-window owind))))))))

(defun hmouse-swap-buffers (&optional assist-flag)
  "Swaps buffers in windows selected with last Action Key depress and release.
If optional arg ASSIST-FLAG is non-nil, uses Assist Key."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-buf (and w1 (window-buffer w1)))
	 (w2-buf (and w2 (window-buffer w2)))
	 )
    (or (and w1 w2)
	(error "(hmouse-swap-buffers): Last depress or release was not within a window."))
    ;; Swap window buffers.
    (set-window-buffer w1 w2-buf)
    (set-window-buffer w2 w1-buf)))

(defun hmouse-swap-windows (&optional assist-flag)
  "Swaps windows selected with last Action Key depress and release.
If optional arg ASSIST-FLAG is non-nil, uses Assist Key."
  (let* ((w1 (if assist-flag assist-key-depress-window
	       action-key-depress-window))
	 (w2 (if assist-flag assist-key-release-window
	       action-key-release-window))
	 (w1-width  (and w1 (window-width w1)))
	 (w1-height (and w1 (window-height w1)))
	 (w2-width  (and w2 (window-width w2)))
	 (w2-height (and w2 (window-height w2)))
	 )
    (or (and w1 w2)
	(error "(hmouse-swap-windows): Last depress or release was not within a window."))
    (unwind-protect
	(progn
	  (select-window w1)
	  (if (not (= w1-height (frame-height)))
	      (shrink-window (- w1-height w2-height)))
	  (if (not (= w1-width (frame-width)))
	      (shrink-window-horizontally (- w1-width w2-width)))
	  (select-window w2)
	  (setq w2-width (window-width w2)
		w2-height (window-height w2))
	  (if (not (= w2-height (frame-height)))
	      (shrink-window (- w2-height w1-height)))
	  (if (not (= w2-width (frame-width)))
	      (shrink-window-horizontally (- w2-width w1-width)))
	  )
      (select-window w2)
      )))

(defun hmouse-x-coord (args)
  "Returns x coordinate in characters from window system dependent ARGS."
  (let ((x (if (markerp args)
	       (save-excursion
		 (hypb:goto-marker args)
		 (current-column))
	     (eval (cdr (assoc (hyperb:window-system)
			       '(("emacs" . (if (eventp args)
						(let ((w-or-f (posn-window (event-start args))))
						  (if (framep w-or-f)
						      (setq w-or-f (frame-selected-window w-or-f)))
						  (+ (car (posn-col-row
							   (event-start args)))
						     (nth 0 (window-edges w-or-f))))
					      (car args)))
				 ("xemacs" .  (if (eventp args)
						  (event-x args)
						(car args)))
				 ("xterm"  .  (car args))
				 ("next"   .  (nth 1 args))
				 )))))))
    (if (integerp x) x (error "(hmouse-x-coord): invalid X coord: %s" x))))

(defun hmouse-y-coord (args)
  "Returns y coordinate in frame lines from window system dependent ARGS."
  (let ((y (eval (cdr (assoc (hyperb:window-system)
			     '(("emacs" . (if (eventp args)
					      (let ((w-or-f (posn-window (event-start args))))
						(if (framep w-or-f)
						    (setq w-or-f (frame-selected-window w-or-f)))
						(+ (cdr (posn-col-row
							 (event-start args)))
						   (nth 1 (window-edges w-or-f))))
					    (cdr args)))
			       ("xemacs" .  (if (eventp args)
						(event-y args)
					      (cdr args)))
			       ("xterm"  .  (nth 1 args))
			       ("next"   .  (nth 2 args))
			       ))))))
    (if (integerp y) y (error "(hmouse-y-coord): invalid Y coord: %s" y))))


;;; ************************************************************************
;;; Private variables
;;; ************************************************************************


(provide 'hui-window)

;;; hui-window.el ends here
