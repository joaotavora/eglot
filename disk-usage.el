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

;; TODO: Canonical paths
;; TODO: Graph support.
;; TODO: Print symlinks?
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

(defvar disk-usage-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "d" #'disk-usage-dired-at-point)
    (define-key map "e" #'disk-usage-eshell-at-point)
    map)
  "Local keymap for `disk-usage-mode' buffers.")

(defvar disk-usage--cache nil)

(defun disk-usage-reset-cache ()
  (interactive)
  (setq disk-usage--cache nil))

(defun disk-usage--list (directory)
  (setq directory (or directory default-directory))
  ;; TODO: 'full paths?
  (let ((listing (directory-files-and-attributes directory 'full nil 'nosort)))
    (or (cl-loop for l in listing
                 for attributes = (cl-rest l)
                 for path = (cl-first l)
                 if (null (file-attribute-type attributes))
                 collect (vector (file-attribute-size attributes) path)
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

(defun disk-usage--refresh (&optional directory)
  (setq directory (or directory default-directory))
  ;; TODO: Set tabulated-list-entries to a function?
  (setq tabulated-list-entries
        (mapcar (lambda (e) (list nil (vector (number-to-string (aref e 0))
                                              (if (file-directory-p (aref e 1))
                                                  (cons (aref e 1) (list 'action (lambda (s) (disk-usage (button-label s)))))
                                                (aref e 1)))))
                (disk-usage--list directory))))

(defun disk-usage--sort-size-< (a b)
  (let ((size-a (string-to-number (aref (cadr a) 0)))
        (size-b (string-to-number (aref (cadr b) 0))))
    (< size-a size-b)))

;; TODO: Customize sort options.
(defvar disk-usage--sort #'disk-usage--sort-size-<)

(defvar disk-usage--format #'file-size-human-readable
  "How to print size.
Takes a number and returns a string.
`file-size-human-readable' and `number-to-string' are good candidates.")

(defun disk-usage-toggle-human-readable ()
  (interactive)
  (if (eq disk-usage--format #'file-size-human-readable)
      (setq disk-usage--format #'number-to-string)
    (setq disk-usage--format #'file-size-human-readable)))

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
      (setq x (tabulated-list-print-col 0 (funcall disk-usage--format (string-to-number (aref cols 0))) x))
      (cl-loop for i from 1 below ncols
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
  ;; TODO: Option to display extra attributes.
  (setq tabulated-list-format
        `[("Size" 10 ,disk-usage--sort)
          ("File" 0 t)])
  (setq tabulated-list-sort-key (cons "Size" 'flip))
  (setq tabulated-list-printer #'disk-usage--print-entry)
  (add-hook 'tabulated-list-revert-hook 'disk-usage--refresh nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun disk-usage (&optional directory)
  (interactive "D")
  (unless disk-usage--cache
    (setq disk-usage--cache (make-hash-table :test #'equal)))
  (setq directory (or (and (file-directory-p directory)
                           directory)
                      default-directory))
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
