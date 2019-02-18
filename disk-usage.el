;;; disk-usage.el --- Sort and browse disk usage listings -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://gitlab.com/Ambrevar/emacs-disk-usage
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, files, convenience, tools

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
;; Run `disk-usage' or `disk-usage-here' to display a listing.
;; `describe-mode' to display additional bindings, such as
;; `disk-usage-dired-at-point' to open a `dired' buffer for the current
;; directory.
;;
;; You can customize options in the 'disk-usage group.


;;; Code:
(require 'tabulated-list)
(require 'cl-macs)

;; TODO: Toggle display of full paths.  Make sur Helm-FF still works.
;; TODO: Graph support.
;; TODO: Include screenshots.
;; TODO: When going up, place cursor on right directory.

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
    map)
  "Local keymap for `disk-usage-mode' buffers.")

(defvar disk-usage--cache nil)

(defun disk-usage-reset-cache ()
  (interactive)
  (setq disk-usage--cache nil))

(defun disk-usage--list (directory)
  (setq directory (or directory default-directory))
  (let ((listing (directory-files-and-attributes directory 'full nil 'nosort)))
    (or (cl-loop for l in listing
                 for attributes = (cl-rest l)
                 for path = (cl-first l)
                 ;; Files
                 if (null (file-attribute-type attributes))
                 collect (vector (file-attribute-size attributes) path)
                 ;; Symlinks
                 if (stringp (file-attribute-type attributes))
                 collect (vector (file-attribute-size attributes)
                                 path)
                 ;; Folders
                 else if (and (eq t (file-attribute-type attributes))
                              (not (string= "." (file-name-base path)))
                              (not (string= ".." (file-name-base path))))
                 collect (vector
                          (disk-usage--directory-size path)
                          path))
        (list  (vector 0 directory)))))

(defun disk-usage--total (listing)
  (aref
   (cl-reduce (lambda (f1 f2)
                (vector (+ (aref f1 0) (aref f2 0))))
              listing)
   0))

(defun disk-usage--directory-size (path)
  (let ((size (gethash path disk-usage--cache)))
    (unless size
      (message "Computing disk usage for %S..." path)
      (setq size (funcall disk-usage--directory-size-function path))
      (puthash path size disk-usage--cache))
    size))

(defun disk-usage--directory-size-with-emacs (path)
  "Return the total disk usage of directory PATH as a number.
This is slow but does not require any external process."
  (disk-usage--total (disk-usage--list path)))

(defvar disk-usage--du-command "du")
(defvar disk-usage--du-args "-sb")
(defun disk-usage--directory-size-with-du (path)
  "See `disk-usage--directory-size-function'."
  (string-to-number
   (cl-first
    (split-string
     (with-temp-buffer
       (with-output-to-string
         (call-process disk-usage--du-command
                       nil t nil
                       disk-usage--du-args path))
       (buffer-string))))))

(defun disk-usage--set-format (&optional total-size)
  (setq tabulated-list-format
        `[("Size"
           ,(if (eq disk-usage--format-size #'file-size-human-readable)
                8
              12)
           ,disk-usage--sort . (:right-align t))
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
  ;; TODO: Set tabulated-list-entries to a function?
  (let ((listing (disk-usage--list directory)))
    (disk-usage--set-format (disk-usage--total listing))
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list e (vector (number-to-string (aref e 0))
                                    (if (file-directory-p (aref e 1))
                                        ;; Make button.
                                        (cons (aref e 1)
                                              (list 'action
                                                    (lambda (_)
                                                      (disk-usage (aref e 1)))))
                                      (aref e 1)))))
                  listing))))

(defun disk-usage--sort-size-< (a b)
  (let ((size-a (string-to-number (aref (cadr a) 0)))
        (size-b (string-to-number (aref (cadr b) 0))))
    (< size-a size-b)))

(defvar disk-usage--sort #'disk-usage--sort-size-<)

(defvar disk-usage--format-size #'file-size-human-readable
  "How to print size.
Takes a number and returns a string.
`file-size-human-readable' and `number-to-string' are good candidates.")

(defvar disk-usage--format-files #'identity
  "How to print files.
Takes a string and returns a string.
`identity' and `file-name-nondirectory' are good candidates.")

(defun disk-usage-toggle-human-readable ()
  (interactive)
  (setq disk-usage--format-size
        (if (eq disk-usage--format-size #'file-size-human-readable)
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
         (formatted-filename (if (stringp (file-attribute-type (file-attributes filename)))
                                 (propertize (funcall disk-usage--format-files filename)
                                             'face (if (file-directory-p filename)
                                                       'disk-usage-symlink-directory
                                                     'disk-usage-symlink))
                               (funcall disk-usage--format-files filename))))
    (if (listp file-entry)
        (let ((copy (cl-copy-list file-entry)))
          (setcar copy formatted-filename)
          copy)
      formatted-filename)))

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
      (setq x (tabulated-list-print-col 0 (funcall disk-usage--format-size (string-to-number (aref cols 0))) x))
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
  "Mode to display disk usage."
  ;; TODO: Option to display extra attributes and default column to sort.
  (setq tabulated-list-sort-key (cons "Size" 'flip))
  (setq tabulated-list-printer #'disk-usage--print-entry)
  (add-hook 'tabulated-list-revert-hook 'disk-usage--refresh nil t))

;;;###autoload
(defun disk-usage (&optional directory)
  (interactive "D")
  (unless disk-usage--cache
    (setq disk-usage--cache (make-hash-table :test #'equal)))
  (setq directory (file-truename (or (and (file-directory-p directory)
                                          directory)
                                     default-directory)))
  (switch-to-buffer
   (get-buffer-create (format "*disk-usage<%s>*" (directory-file-name directory))))
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

;; TODO: Mark files?
(defun disk-usage-delete-at-point ()
  (interactive)
  (delete-file (disk-usage--path-at-point))
  (tabulated-list-delete-entry))

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

(provide 'disk-usage)
;;; disk-usage.el ends here
