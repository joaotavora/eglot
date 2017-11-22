;;; hui-treemacs.el --- Hyperbole Smart Key support for the Treemacs file manager package
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    19-Nov-17
;;
;; Copyright (C) 2017  Free Software Foundation, Inc.
;; See the "HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (require 'treemacs nil t))

;;; ************************************************************************
;;; smart-treemacs functions
;;; ************************************************************************

;;;###autoload
(defun smart-treemacs ()
  "Uses a single key or mouse key to manipulate directory entries.

Invoked via a key press when in treemacs-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on an entry icon, the treemacs TAB command is run to expand and
     collapse the entry;
 (2) elsewhere within an entry line, the item is displayed for editing,
     normally in another window;
 (3) on the first line of the buffer (other than the end of line),
     dired is run on the current directory of this Treemacs;
 (4) at the end of the first or last line of the buffer,
     this Treemacs invocation is quit."

  (interactive)
  (cond ((first-line-p)
	 (if (eolp)
	     (treemacs-toggle)
	   (hact 'link-to-directory default-directory)))
	((and (last-line-p) (eolp))
	 (treemacs-toggle))
	(t (let ((over-icon (and (treemacs--current-button)
				 (= (point) (- (button-start (treemacs--current-button)) 2))))
		 (result (treemacs-node-buffer-and-position)))
	     (if (and (not over-icon) result (or (bufferp result) (listp result)))
		 (if (listp result)
		     (hact 'link-to-buffer-tmp (seq-elt result 0) (seq-elt result 1))
		   ;; (bufferp result)
		   (hact 'link-to-buffer-tmp result))
	       (treemacs-push-button current-prefix-arg))))))

;;;###autoload
(defun smart-treemacs-modeline ()
  "Toggle display of Treemacs file viewer based on Smart Action Key click on a modeline.

When pressed on the Treemacs buffer modeline or Treemacs is displaying
the default directory of the buffer modeline clicked upon, then
quit/hide the Treemacs window.  Otherwise, display the Treemacs window
with the default directory of the buffer modeline clicked upon.

Suitable for use as a value of `action-key-modeline-buffer-id-function'."
  (if (fboundp 'treemacs)
      (progn
	(require 'treemacs)
	(cond
	 ;; Clicked on Treemacs buffer id
	 ((if action-key-depress-window
	      (treemacs--is-treemacs-window? action-key-depress-window)
	    (string-match " Treemacs " (format-mode-line mode-line-format)))
	  ;; Quit/hide treemacs.
	  (treemacs-toggle))
	 ;;
	 ;; Treemacs is visible and displaying the same dir as
	 ;; the default dir of the clicked on modeline.
	 ((and (treemacs--buffer-exists?)
	       (string-equal (expand-file-name default-directory)
			     (with-current-buffer (treemacs--buffer-exists?)
			       default-directory)))
	  ;; Quit/hide treemacs.
	  (treemacs-toggle))
	 ;;
	 ;; Otherwise, invoke treemacs on the default dir of the clicked on modeline.
	 (t (treemacs))))
    (error "(smart-treemacs-modeline): Treemacs package is not installed")))

;;; ************************************************************************
;;; treemacs function updates
;;; ************************************************************************

;; Add this in treemacs-tags.el
(defun treemacs--imenu-tag-noselect (file tag-path)
  "Return a list of the source buffer for FILE and the position of the tag from TAG-PATH."
  (let ((tag (car tag-path))
        (path (cdr tag-path)))
    (condition-case e
        (progn
          (find-file-noselect file)
          (let ((index (treemacs--get-imenu-index file)))
            (dolist (path-item path)
              (setq index (cdr (assoc path-item index))))
            (-let [(buf pos) (treemacs--pos-from-marker
                              (cdr (--first
                                    (equal (car it) tag)
                                    index)))]
              ;; some imenu implementations, like markdown, will only provide
              ;; a raw buffer position (an int) to move to
	      (list (or buf (get-file-buffer file)) pos))))
      (error
       (treemacs--log "Something went wrong when finding tag '%s': %s"
                      (propertize tag 'face 'treemacs-tags-face)
                      e)))))

;; Add this in treemacs-tags.el
(defun treemacs--tag-noselect (btn)
  "Return list of tag source buffer and position for BTN for future display."
  (require 'hmouse-tag) ;; from GNU Hyperbole, adds xref convenience functions used herein
  (-let [(tag-buf tag-pos)
         (treemacs--with-button-buffer btn
				       (-> btn (button-get 'marker) (treemacs--pos-from-marker)))]
    (if tag-buf
	(list tag-buf tag-pos)
      (-pcase treemacs-goto-tag-strategy
        [`refetch-index
         (let (file tag-path)
           (with-current-buffer (marker-buffer btn)
             (setq file (treemacs--nearest-path btn)
                   tag-path (treemacs--tags-path-of btn)))
           (treemacs--imenu-tag-noselect file tag-path))]
        [`call-xref
	 (let ((xref (xref-definition
		      (treemacs--with-button-buffer btn
						    (treemacs--get-label-of btn)))))
	   (when xref
	     (list (xref-item-buffer xref) (xref-item-position xref))))]
        [`issue-warning
         (treemacs--log "Tag '%s' is located in a buffer that does not exist."
                        (propertize (treemacs--with-button-buffer btn (treemacs--get-label-of btn)) 'face 'treemacs-tags-face))]
        [_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy)]))))

;; Replace this macro in treemacs-impl.el
(cl-defmacro treemacs--execute-button-action
    (&key save-window ensure-window-split split-function window dir-action file-action tag-action no-match-explanation)
  "Infrastructure macro for setting up actions on different button states.
Fetches the currently selected button and verifies it's in the correct state
based on the given state actions.
If it isn't it will log NO-MATCH-EXPLANATION, if it is it selects WINDOW (or
`next-window' if none is given) and splits it with SPLIT-FUNCTION if given.
DIR-ACTION, FILE-ACTION, and TAG-ACTION are inserted into a `pcase' statement
matching the buttons state.
If ENSURE-WINDOW-SPLIT is t treemacs will vertically split the window if
treemacs is the only window to make sure a buffer is opened next to it, not
under or below it."
  (let ((valid-states (list)))
    (when dir-action
      (push 'dir-node-open valid-states)
      (push 'dir-node-closed valid-states))
    (when file-action
      (push 'file-node-open valid-states)
      (push 'file-node-closed valid-states))
    (when tag-action
      (push 'tag-node valid-states))
    `(-when-let (btn (treemacs--current-button))
       (treemacs--without-following
        (let* ((state (button-get btn 'state))
               (current-window (selected-window)))
          (if (not (memq state ',valid-states))
              (treemacs--log "%s" ,no-match-explanation)
            (progn
              ,@(if ensure-window-split
                    `((when (one-window-p)
                        (save-selected-window
                          (split-window nil nil (if (eq 'left treemacs-position) 'right 'left))))))
              (select-window (or ,window (next-window (selected-window) nil nil)))
              ,@(if split-function
                    `((funcall ,split-function)
                      (other-window 1)))
	      ;; Return the result of the action
              (prog1 (pcase state
                       ,@(when dir-action
			   `(((or `dir-node-open `dir-node-closed)
			      ,dir-action)))
                       ,@(when file-action
			   `(((or `file-node-open `file-node-closed)
			      ,file-action)))
                       ,@(when tag-action
			   `((`tag-node
			      ,tag-action)))
                       (_ (error "No match achieved even though button's state %s was part of the set of valid states %s"
				 state ',valid-states)))
		(when ,save-window
                  (select-window current-window))))))))))

;; Reload source form of this library so updated version of
;; `treemacs--execute-button-action' above is used throughout.
(load "treemacs-interface.el")

;; Add to treemacs-interface.el.
;;;###autoload
(defun treemacs-node-buffer-and-position (&optional arg)
  "Return source buffer or list of buffer and position for the current node for future display.
Stay in the selected window and ignore any prefix argument ARG."
  (interactive "P")
  (let ((treemacs--no-messages t))
    (treemacs--execute-button-action
     :file-action (find-file-noselect (treemacs--safe-button-get btn 'abs-path))
     :dir-action (find-file-noselect (treemacs--safe-button-get btn 'abs-path))
     :tag-action (treemacs--tag-noselect btn)
     :window (selected-window)
     :save-window t
     :ensure-window-split nil
     :no-match-explanation "")))

(provide 'hytreemacs)
