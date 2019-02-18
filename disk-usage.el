;;; disk-usage.el --- Sort and browse disk usage listings -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/emacs-disk-usage
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: files, convenience, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Disk Usage is a file system analyzer: it offers a tabulated view of file
;; listings sorted by size.  Directory sizes are computed recursively.  The results
;; are cached for speed.
;;
;; Run `disk-usage' or `disk-usage-here' to display a listing.
;; See `describe-mode' for additional bindings, such as
;; `disk-usage-dired-at-point' to open a `dired' buffer for the current
;; directory.
;;
;; Instead of displaying only the current folder, ~disk-usage~ can also display
;; files in all subfolders recursively with `disk-usage-toggle-recursive'.
;;
;; Marked files can be trashed with `disk-usage-delete-marked-files'.  When
;; called with a prefix argument, files are deleted permanently.
;;
;; Run `disk-usage-by-types' to display statistics of disk usage by file
;; extensions.
;;
;; With a prefix argument, cache is updated when reverting the buffer.
;;
;; You can customize options in the 'disk-usage group.


;;; Code:
(require 'tabulated-list)
(require 'cl-macs)

;; TODO: Helm-FF does not work when file-name-nondirectory is on.
;; TODO: Add support for charts?

(defgroup disk-usage nil
  "Predefined configurations for `disk-usage'."
  :group 'files)

(defcustom disk-usage-discard-previous-buffer t
  "Whether to kill the current `disk-usage' buffer before moving directory."
  :group 'disk-usage
  :type 'boolean)

(defcustom disk-usage--directory-size-function #'disk-usage--directory-size-with-du
  "Function that returns the total disk usage of the directory passed as argument."
  :group 'disk-usage
  :type '(choice (function :tag "Native (slow)" disk-usage--directory-size-with-emacs)
                 (function :tag "System \"du\"" disk-usage--directory-size-with-du)))

(defface disk-usage-inaccessible
  '((t :foreground "red"
       :underline t))
  "Face for inaccessible folders."
  :group 'disk-usage)

(defface disk-usage-symlink
  '((t :foreground "orange"))
  "Face for symlinks."
  :group 'disk-usage)

(defface disk-usage-symlink-directory
  '((t :inherit disk-usage-symlink
       :underline t))
  "Face for symlinks."
  :group 'disk-usage)

(defvar disk-usage-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "S-<return>") #'disk-usage-find-file-at-point)
    (define-key map (kbd "<backspace>") #'disk-usage-up)
    (define-key map "^" #'disk-usage-up)
    (define-key map "d" #'disk-usage-dired-at-point)
    (define-key map "e" #'disk-usage-eshell-at-point)
    (define-key map "h" #'disk-usage-toggle-human-readable)
    (define-key map "f" #'disk-usage-toggle-full-path)
    (define-key map "R" #'disk-usage-toggle-recursive)
    (define-key map "m" #'disk-usage-mark-at-point)
    (define-key map "x" #'disk-usage-delete-marked-files)
    map)
  "Local keymap for `disk-usage-mode' buffers.")

(defvar disk-usage--cache nil)

(cl-defstruct (disk-usage--file-info
               (:constructor nil)
               (:constructor disk-usage--file-info-make))
  size
  name
  (marked nil))

(defun disk-usage-reset-cache ()
  (interactive)
  (clrhash disk-usage--cache))

(defun disk-usage--list (directory)
  (setq directory (or directory default-directory))
  (let ((listing (and (file-accessible-directory-p directory)
                      (directory-files-and-attributes directory 'full nil 'nosort))))
    (or (cl-loop for l in listing
                 for attributes = (cl-rest l)
                 for path = (cl-first l)
                 ;; Files
                 if (null (file-attribute-type attributes))
                 collect (disk-usage--file-info-make :name path
                                                     :size (file-attribute-size attributes))
                 ;; Symlinks
                 if (stringp (file-attribute-type attributes))
                 collect (disk-usage--file-info-make :name path
                                                     :size (file-attribute-size attributes))
                 ;; Folders
                 else if (and (eq t (file-attribute-type attributes))
                              (not (string= "." (file-name-base path)))
                              (not (string= ".." (file-name-base path))))
                 collect
                 (disk-usage--file-info-make :name path
                                             :size (disk-usage--directory-size path)))
        (list (disk-usage--file-info-make :size 0 :name directory)))))

(defvar disk-usage--du-command "du")
(defvar disk-usage--du-args "-sb")
(defvar disk-usage--find-command "find")

(defun disk-usage--list-recursively (directory)
  "This is the equivalent of running the shell command
$ find . -type f -exec du -sb {} +"
  (setq directory (or directory default-directory))
  (mapcar (lambda (s)
            (let ((pair (split-string s "\t")))
              (disk-usage--file-info-make
               :name (cadr pair)
               :size (string-to-number (cl-first pair)))))
          (split-string (with-temp-buffer
                          (call-process "find" nil '(t nil) nil
                                        directory
                                        "-type" "f"
                                        "-exec" disk-usage--du-command disk-usage--du-args "{}" "+")
                                (buffer-string))
                              "\n" 'omit-nulls)))

(defcustom disk-usage-list-function #'disk-usage--list
  "Function that returns a list of `disk-usage--file-info'.
It takes the directory to scan as argument."
  :group 'disk-usage
  :type '(choice (function :tag "Hierarchical" disk-usage--list)
                 (function :tag "Flat (recursive)" disk-usage--list-recursively)))

(defun disk-usage-toggle-recursive ()
  "Toggle between hierarchical and flat view."
  (interactive)
  (setq disk-usage-list-function
        (if (eq disk-usage-list-function #'disk-usage--list)
            #'disk-usage--list-recursively
          #'disk-usage--list))
  (tabulated-list-revert))

(defun disk-usage--total (listing)
  (cl-loop for file in listing
           sum (disk-usage--file-info-size file)))

(defun disk-usage--directory-size (path)
  (let ((size (unless current-prefix-arg
                (gethash path disk-usage--cache))))
    (unless size
      (message "Computing disk usage for %S..." path)
      (setq size (funcall disk-usage--directory-size-function path))
      (puthash path size disk-usage--cache))
    size))

(defun disk-usage--directory-size-with-emacs (path)
  "Return the total disk usage of directory PATH as a number.
This is slow but does not require any external process."
  (disk-usage--total (disk-usage--list path)))

(defun disk-usage--directory-size-with-du (path)
  "See `disk-usage--directory-size-function'."
  (string-to-number
   (cl-first
    (split-string
     (with-temp-buffer
       (with-output-to-string
         (call-process disk-usage--du-command
                       nil '(t nil) nil
                       disk-usage--du-args path))
       (buffer-string))))))

(defun disk-usage--sort-by-size (a b)
  (< (disk-usage--file-info-size (car a))
     (disk-usage--file-info-size (car b))))

(defcustom disk-usage-size-format-function #'file-size-human-readable
  "How to print size.
Takes a number and returns a string."
  :group 'disk-usage
  :type '(choice (function :tag "Human readable" file-size-human-readable)
                 (function :tag "In bytes" number-to-string)))

(defun disk-usage--set-tabulated-list-format (&optional total-size)
  (setq tabulated-list-format
        `[("Size"
           ,(if (eq disk-usage-size-format-function #'file-size-human-readable)
                8
              12)
           disk-usage--sort-by-size . (:right-align t))
          (,(format "Files %sin '%s'"
                    (if total-size
                        (format "totalling %sB (%s) "
                                total-size
                                (file-size-human-readable total-size))
                      "")
                    default-directory)
           0 t)]))

(defun disk-usage--refresh (&optional directory)
  (setq directory (or directory default-directory))
  (let ((listing (funcall disk-usage-list-function directory)))
    (disk-usage--set-tabulated-list-format (disk-usage--total listing))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (file-info)
                    (list file-info (vector (number-to-string (disk-usage--file-info-size file-info))
                                            (let ((name (disk-usage--file-info-name file-info)))
                                              (if (file-directory-p name)
                                                  ;; Make button.
                                                  (cons name
                                                        (list 'action
                                                              (lambda (_)
                                                                (disk-usage name))))
                                                name)))))
                  listing))))

(defvar disk-usage--format-files #'identity
  "How to print files.
Takes a string and returns a string.
`identity' and `file-name-nondirectory' are good candidates.")

(defun disk-usage-toggle-human-readable ()
  (interactive)
  (setq disk-usage-size-format-function
        (if (eq disk-usage-size-format-function #'file-size-human-readable)
            #'number-to-string
          #'file-size-human-readable))
  (tabulated-list-revert))

(defun disk-usage-toggle-full-path ()
  (interactive)
  (setq disk-usage--format-files
        (if (eq disk-usage--format-files #'identity)
            #'file-name-nondirectory
          #'identity))
  (tabulated-list-revert))

(defun disk-usage--print-file-col (file-entry)
  "Call `disk-usage--format-file' on FILE-ENTRY.
FILE-ENTRY may be a string or a button."
  (let* ((filename (if (listp file-entry)
                       (cl-first file-entry)
                     file-entry))
         (formatted-filename
          (cond
           ((stringp (file-attribute-type (file-attributes filename)))
            (propertize (funcall disk-usage--format-files filename)
                        'face (if (file-directory-p filename)
                                  'disk-usage-symlink-directory
                                'disk-usage-symlink)))
           ((and (not (null (file-attribute-type (file-attributes filename))))
                 (not (file-accessible-directory-p filename)))
            (propertize (funcall disk-usage--format-files filename)
                        'face 'disk-usage-inaccessible))
           (t (funcall disk-usage--format-files filename)))))
    (if (listp file-entry)
        (let ((copy (cl-copy-list file-entry)))
          (setcar copy formatted-filename)
          copy)
      formatted-filename)))

;; TODO: We could avoid defining our own `disk-usage--print-entry' by settings
;; `tabulated-list-entries' to a closure over the listing calling
;; `disk-usage-size-format-function' to generate the columns.
(defun disk-usage--print-entry (id cols)
  "Like `tabulated-list-print-entry' but formats size for human
beings."
  (let ((beg   (point))
        (x     (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (if (> tabulated-list-padding 0)
        (insert (make-string x ?\s)))
    (let ((tabulated-list--near-rows    ; Bind it if not bound yet (Bug#25506).
           (or (bound-and-true-p tabulated-list--near-rows)
               (list (or (tabulated-list-get-entry (point-at-bol 0))
                         cols)
                     cols))))
      (setq x (tabulated-list-print-col 0 (funcall disk-usage-size-format-function (string-to-number (aref cols 0))) x))
      (setq x (tabulated-list-print-col 1 (disk-usage--print-file-col (aref cols 1)) x))
      (cl-loop for i from 2 below ncols
               do (setq x (tabulated-list-print-col i (aref cols i) x))))
    (insert ?\n)
    ;; Ever so slightly faster than calling `put-text-property' twice.
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id tabulated-list-entry ,cols
                         ;; WARNING: The following is a workaround for Helm so that
                         ;; (helm-find-files-input
                         ;;  (helm-ffap-guesser)
                         ;;  (thing-at-point 'filename))
                         ;; guesses the right file.
                         help-echo ,(aref cols 1)))))

(define-derived-mode disk-usage-mode tabulated-list-mode "Disk Usage"
  "Mode to display disk usage.
With a prefix argument, cache is updated when reverting the buffer.

Also see `disk-usage-by-types-mode'."
  ;; TODO: Option to display extra attributes and default column to sort.
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Size" 'flip))
  (setq tabulated-list-printer #'disk-usage--print-entry)
  (add-hook 'tabulated-list-revert-hook 'disk-usage--refresh nil t))

(defvar disk-usage-buffer-name "disk-usage")

;;;###autoload
(defun disk-usage (&optional directory)
  (interactive "D")
  (unless (file-accessible-directory-p directory)
    (error "Directory cannot be opened: %S" directory))
  (unless disk-usage--cache
    (setq disk-usage--cache (make-hash-table :test #'equal)))
  (setq directory (file-truename (or (and (file-directory-p directory)
                                          directory)
                                     default-directory)))
  (switch-to-buffer
   (get-buffer-create (format "*%s<%s>*" disk-usage-buffer-name
                              (directory-file-name directory))))
  (disk-usage-mode)
  (setq default-directory directory)
  (tabulated-list-revert))

;;;###autoload
(defun disk-usage-here ()
  (interactive)
  (disk-usage default-directory))

(defun disk-usage-up (&optional discard-previous-buffer)
  (interactive)
  (let ((directory default-directory))
    (when (and (or discard-previous-buffer disk-usage-discard-previous-buffer)
               (eq major-mode 'disk-usage-mode))
      (kill-this-buffer))
    (disk-usage (expand-file-name ".." directory))))

(defun disk-usage--path-at-point ()
  (let* ((entry (tabulated-list-get-entry (point)))
         (path (aref entry 1)))
    (if (listp path)
        (setq path (cl-first path))
      path)))

(defun disk-usage--directory-at-point ()
  (let ((path (disk-usage--path-at-point)))
    (if (file-directory-p path)
        path
      (setq path (file-name-directory path)))))

(defun disk-usage-mark-at-point ()
  (interactive)
  (let ((file-info (tabulated-list-get-id (point))))
    (setf (disk-usage--file-info-marked file-info) t))
  (tabulated-list-put-tag "*" 'advance))

(defun disk-usage-delete-marked-files (&optional permanently)
  "Delete marked files.
By default, files are moved to trash unless PERMANENTLY is
non-nil or with prefix argument."
  (interactive "P")
  (when (yes-or-no-p "Delete marked files?")
    (cl-loop for entry in tabulated-list-entries
             if (disk-usage--file-info-marked (car entry))
             do (let ((delete-by-moving-to-trash (not permanently)))
                  (delete-file (disk-usage--file-info-name (car entry))))))
  (tabulated-list-revert))

(defun disk-usage-find-file-at-point ()
  (interactive)
  (find-file (disk-usage--path-at-point)))

(defun disk-usage-dired-at-point ()
  (interactive)
  (dired (disk-usage--directory-at-point)))

(defun disk-usage-eshell-at-point ()
  (interactive)
  (let ((default-directory (disk-usage--directory-at-point)))
    (eshell 'new-session)))

(defun disk-usage-shell-at-point ()
  (interactive)
  (let ((default-directory (disk-usage--directory-at-point)))
    (shell (get-buffer-create (generate-new-buffer-name "*shell*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defstruct (disk-usage--type-info
               (:constructor nil)
               (:constructor disk-usage--type-info-make))
  extension
  size
  (count 1))

(defun disk-usage-by-types--list (directory)
  "Return a hash table of (TYPE DISK-USAGE-TYPE-INFO).
TYPE is the file extension (lower case)."
  (setq directory (or directory default-directory))
  (let ((listing (disk-usage--list-recursively directory))
        (table (make-hash-table :test #'equal)))
    (dolist (file-info listing)
      (let* ((ext (downcase (or (file-name-extension (disk-usage--file-info-name file-info)) "")))
             (size (disk-usage--file-info-size file-info))
             (type (gethash ext table)))
        (puthash ext
                 (if (not type)
                     (disk-usage--type-info-make :extension ext
                                                :size size)
                   (setf
                    (disk-usage--type-info-count type) (1+ (disk-usage--type-info-count type))
                    (disk-usage--type-info-size type) (+ size (disk-usage--type-info-size type)))
                   type)
                 table)))
    table))

(defun disk-usage--type-average-size (type)
  (/ (float (disk-usage--type-info-size type))
     (disk-usage--type-info-count type)))

(defun disk-usage-by-types--sort-by-count (a b)
  (< (disk-usage--type-info-count (car a))
     (disk-usage--type-info-count (car b))))

(defun disk-usage-by-types--sort-by-size (a b)
  (< (disk-usage--type-info-size (car a))
     (disk-usage--type-info-size (car b))))

(defun disk-usage-by-types--sort-by-average (a b)
  (< (disk-usage--type-average-size (car a))
     (disk-usage--type-average-size (car b))))

(defun disk-usage-by-types--set-tabulated-list-format ()
  (setq tabulated-list-format
        `[("Extension" 12 t)
          ("Count" 12 disk-usage-by-types--sort-by-count)
          ("Total size" 12 disk-usage-by-types--sort-by-size)
          ("Average size" 15 disk-usage-by-types--sort-by-average)]))

(defun disk-usage-by-types--refresh (&optional directory)
  (setq directory (or directory default-directory))
  (let ((listing (disk-usage-by-types--list directory)))
    (disk-usage-by-types--set-tabulated-list-format)
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop for e being the hash-values of listing
                   collect (list e
                                 (vector
                                  (disk-usage--type-info-extension e)
                                  (number-to-string (disk-usage--type-info-count e))
                                  (funcall disk-usage-size-format-function
                                           (disk-usage--type-info-size e))
                                  (funcall disk-usage-size-format-function
                                           (string-to-number
                                            (format "%.2f"
                                                    (disk-usage--type-average-size e))))))))))

(define-derived-mode disk-usage-by-types-mode tabulated-list-mode "Disk Usage By Types"
  "Mode to display disk usage by file types.
Also see `disk-usage-mode'."
  (add-hook 'tabulated-list-revert-hook 'disk-usage-by-types--refresh nil t))

(defvar disk-usage-by-types-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "h" #'disk-usage-toggle-human-readable)
    map)
  "Local keymap for `disk-usage-by-types-mode' buffers.")

(defvar disk-usage-by-types-buffer-name "disk-usage-by-types")

;;;###autoload
(defun disk-usage-by-types (&optional directory)
  (interactive "D")
  (setq directory (file-truename (or (and (file-directory-p directory)
                                          directory)
                                     default-directory)))
  (switch-to-buffer
   (get-buffer-create (format "*%s<%s>*" disk-usage-by-types-buffer-name
                              (directory-file-name directory))))
  (disk-usage-by-types-mode)
  (setq default-directory directory)
  (tabulated-list-revert))

(provide 'disk-usage)
;;; disk-usage.el ends here
