;;; eglot-tests.el --- Tests for eglot.el            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: tests

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for eglot.el

;;; Code:
(require 'eglot)
(require 'cl-lib)
(require 'ert)
(require 'edebug)

;; Helpers

(defmacro eglot--with-dirs-and-files (dirs &rest body)
  (declare (indent 1) (debug t))
  `(eglot--call-with-dirs-and-files
    ,dirs #'(lambda () ,@body)))

(defun eglot--make-file-or-dirs (ass)
  (let ((file-or-dir-name (car ass))
        (content (cdr ass)))
    (cond ((listp content)
           (make-directory file-or-dir-name 'parents)
           (let ((default-directory (concat default-directory "/" file-or-dir-name)))
             (mapc #'eglot--make-file-or-dirs content)))
          ((stringp content)
           (with-temp-buffer
             (insert content)
             (write-region nil nil file-or-dir-name nil 'nomessage)))
          (t
           (message "[yas] oops don't know this content")))))

(defun eglot--call-with-dirs-and-files (dirs fn)
  (let* ((default-directory (make-temp-file "eglot--fixture" t))
         new-buffers new-processes)
    (unwind-protect
        (let ((find-file-hook
               (cons (lambda () (push (current-buffer) new-buffers))
                     find-file-hook))
              (eglot-connect-hook
               (lambda (proc) (push proc new-processes))))
          (mapc #'eglot--make-file-or-dirs dirs)
          (funcall fn))
      (eglot--message "Killing buffers %s,  deleting %s, killing %s"
                      (mapconcat #'buffer-name new-buffers ", ")
                      default-directory
                      new-processes)
      (let ((eglot-autoreconnect nil))
        (mapc #'eglot-shutdown
              (cl-remove-if-not #'process-live-p new-processes)))
      (dolist (buf new-buffers) ;; have to save otherwise will get prompted
        (with-current-buffer buf (save-buffer) (kill-buffer)))
      (delete-directory default-directory 'recursive))))

(cl-defmacro eglot--with-test-timeout (timeout &body body)
  (declare (indent 1) (debug t))
  `(eglot--call-with-test-timeout ,timeout (lambda () ,@body)))

(defun eglot--call-with-test-timeout (timeout fn)
  (let* ((tag (make-symbol "tag"))
         (timed-out (make-symbol "timeout"))
         (timer )
         (eglot-request-timeout 1)
         (retval))
    (unwind-protect
        (setq retval
              (catch tag
                (setq timer
                      (run-with-timer timeout nil
                                      (lambda ()
                                        (unless edebug-active
                                          (throw tag timed-out)))))
                (funcall fn)))
      (cancel-timer timer)
      (when (eq retval timed-out)
        (error "Test timeout!")))))

(defun eglot--find-file-noselect (file &optional noerror)
  (unless (or noerror
              (file-readable-p file)) (error "%s does not exist" file))
  (find-file-noselect file))


;; `rust-mode' is not a part of emacs. So define these two shims which
;; should be more than enough for testing
(unless (functionp 'rust-mode)
  (define-derived-mode rust-mode prog-mode "Rust"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


(ert-deftest dummy () "A dummy test" (should t))

(ert-deftest auto-detect-running-server ()
  "Visit a file and M-x eglot, then visit a neighbour. "
  (skip-unless (executable-find "rls"))
  (let (proc)
    (eglot--with-dirs-and-files
        '(("project" . (("coiso.rs" . "bla")
                        ("merdix.rs" . "bla")))
          ("anotherproject" . (("cena.rs" . "bla"))))
      (eglot--with-test-timeout 2
        (with-current-buffer
            (eglot--find-file-noselect "project/coiso.rs")
          (setq proc
                (eglot 'rust-mode `(transient . ,default-directory)
                       '("rls")))
          (should (eglot--current-process)))
        (with-current-buffer
            (eglot--find-file-noselect "project/merdix.rs")
          (should (eglot--current-process))
          (should (eq (eglot--current-process) proc)))
        (with-current-buffer
            (eglot--find-file-noselect "anotherproject/cena.rs")
          (should-error (eglot--current-process-or-lose)))))))

(ert-deftest auto-reconnect ()
  "Start a server. Kill it. Watch it reconnect."
  (skip-unless (executable-find "rls"))
  (let (proc
        (eglot-autoreconnect 1))
    (eglot--with-dirs-and-files
        '(("project" . (("coiso.rs" . "bla")
                        ("merdix.rs" . "bla"))))
      (eglot--with-test-timeout 3
        (with-current-buffer
            (eglot--find-file-noselect "project/coiso.rs")
          (setq proc
                (eglot 'rust-mode `(transient . ,default-directory)
                       '("rls")))
          ;; In 1.2 seconds > `eglot-autoreconnect' kill servers. We
          ;; should have a automatic reconnection.
          (run-with-timer 1.2 nil (lambda () (delete-process proc)))
          (while (process-live-p proc) (accept-process-output nil 0.5))
          (should (eglot--current-process))
          ;; Now try again too quickly
          (setq proc (eglot--current-process))
          (run-with-timer 0.5 nil (lambda () (delete-process proc)))
          (while (process-live-p proc) (accept-process-output nil 0.5))
          (should (not (eglot--current-process))))))))

(ert-deftest basic-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (eglot--with-test-timeout 4
      (with-current-buffer
          (eglot--find-file-noselect "project/something.py")
        (eglot 'python-mode `(transient . ,default-directory) '("pyls"))
        (goto-char (point-max))
        (completion-at-point)
        (should (looking-back "sys.exit"))))))

(ert-deftest hover-after-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (eglot--with-test-timeout 4
      (with-current-buffer
          (eglot--find-file-noselect "project/something.py")
        (eglot 'python-mode `(transient . ,default-directory) '("pyls"))
        (goto-char (point-max))
        (setq eldoc-last-message nil)
        (completion-at-point)
        (should (looking-back "sys.exit"))
        (while (not eldoc-last-message) (accept-process-output nil 0.1))
        (should (string-match "^exit" eldoc-last-message))))))

(provide 'eglot-tests)
;;; eglot-tests.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
