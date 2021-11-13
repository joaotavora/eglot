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
(require 'ert-x) ; ert-simulate-command
(require 'edebug)
(require 'python) ; python-mode-hook
(require 'company nil t)
(require 'subr-x)

;;; Helpers

(defun eglot--have-eclipse-jdt-ls-p ()
  (and (getenv "CLASSPATH")
       (cl-some
        (lambda (x)
          (string-match-p "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$" x))
        (split-string (getenv "CLASSPATH") ":"))))

(defmacro eglot--with-fixture (fixture &rest body)
  "Setup FIXTURE, call BODY, teardown FIXTURE.
FIXTURE is a list.  Its elements are of the form (FILE . CONTENT)
to create a readable FILE with CONTENT.  FILE may be a directory
name and CONTENT another (FILE . CONTENT) list to specify a
directory hierarchy.  FIXTURE's elements can also be (SYMBOL
VALUE) meaning SYMBOL should be bound to VALUE during BODY and
then restored."
  (declare (indent 1) (debug t))
  `(eglot--call-with-fixture
    ,fixture #'(lambda () ,@body)))

(defun eglot--make-file-or-dir (ass)
  (let ((file-or-dir-name (car ass))
        (content (cdr ass)))
    (cond ((listp content)
           (make-directory file-or-dir-name 'parents)
           (let ((default-directory (concat default-directory "/" file-or-dir-name)))
             (mapcan #'eglot--make-file-or-dir content)))
          ((stringp content)
           (with-temp-buffer
             (insert content)
             (write-region nil nil file-or-dir-name nil 'nomessage))
           (list (expand-file-name file-or-dir-name)))
          (t
           (eglot--error "Expected a string or a directory spec")))))

(defun eglot--call-with-fixture (fixture fn)
  "Helper for `eglot--with-fixture'.  Run FN under FIXTURE."
  (let* ((fixture-directory (make-temp-file "eglot--fixture" t))
         (default-directory fixture-directory)
         file-specs created-files
         syms-to-restore
         new-servers
         test-body-successful-p)
    (dolist (spec fixture)
      (cond ((symbolp spec)
             (push (cons spec (symbol-value spec)) syms-to-restore)
             (set spec nil))
            ((symbolp (car spec))
             (push (cons (car spec) (symbol-value (car spec))) syms-to-restore)
             (set (car spec) (cadr spec)))
            ((stringp (car spec)) (push spec file-specs))))
    (unwind-protect
        (let ((process-environment
               ;; Prevent user-configuration to have an influence on
               ;; language servers. (See github#441)
               (cons "XDG_CONFIG_HOME=/dev/null" process-environment))
              ;; Prevent "Can't guess python-indent-offset ..." messages.
              (python-indent-guess-indent-offset-verbose . nil)
              (eglot-server-initialized-hook
               (lambda (server) (push server new-servers))))
          (setq created-files (mapcan #'eglot--make-file-or-dir file-specs))
          (prog1 (funcall fn)
            (setq test-body-successful-p t)))
      (eglot--message
       "Test body was %s" (if test-body-successful-p "OK" "A FAILURE"))
      (unwind-protect
          (let ((eglot-autoreconnect nil))
            (dolist (server new-servers)
              (when (jsonrpc-running-p server)
                (condition-case oops
                    (eglot-shutdown
                     server nil 3 (not test-body-successful-p))
                  (error
                   (eglot--message "Non-critical shutdown error after test: %S"
                                   oops))))
              (when (not test-body-successful-p)
                ;; We want to do this after the sockets have
                ;; shut down such that any pending data has been
                ;; consumed and is available in the process
                ;; buffers.
                (let ((buffers (delq nil (list
                                          ;; FIXME: Accessing "internal" symbol here.
                                          (process-buffer (jsonrpc--process server))
                                          (jsonrpc-stderr-buffer server)
                                          (jsonrpc-events-buffer server)))))
                  (cond (noninteractive
                         (dolist (buffer buffers)
                           (eglot--message "%s:" (buffer-name buffer))
                           (princ (with-current-buffer buffer (buffer-string))
                                  'external-debugging-output)))
                        (t
                         (eglot--message "Preserved for inspection: %s"
                                         (mapconcat #'buffer-name buffers ", "))))))))
        (eglot--cleanup-after-test fixture-directory created-files syms-to-restore)))))

(defun eglot--cleanup-after-test (fixture-directory created-files syms-to-restore)
  (let ((buffers-to-delete
         (delete nil (mapcar #'find-buffer-visiting created-files))))
    (eglot--message "Killing %s, wiping %s, restoring %s"
                    buffers-to-delete
                    fixture-directory
                    (mapcar #'car syms-to-restore))
    (cl-loop for (sym . val) in syms-to-restore
             do (set sym val))
    (dolist (buf buffers-to-delete) ;; have to save otherwise will get prompted
      (with-current-buffer buf (save-buffer) (kill-buffer)))
    (delete-directory fixture-directory 'recursive)))

(cl-defmacro eglot--with-timeout (timeout &body body)
  (declare (indent 1) (debug t))
  `(eglot--call-with-timeout ,timeout (lambda () ,@body)))

(defun eglot--call-with-timeout (timeout fn)
  (let* ((tag (gensym "eglot-test-timeout"))
         (timed-out (make-symbol "timeout"))
         (timeout-and-message
          (if (listp timeout) timeout
            (list timeout "waiting for test to finish")))
         (timeout (car timeout-and-message))
         (message (cadr timeout-and-message))
         (timer)
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
        (error "%s" (concat "Timed out " message))))))

(defun eglot--find-file-noselect (file &optional noerror)
  (unless (or noerror
              (file-readable-p file)) (error "%s does not exist" file))
  (find-file-noselect file))

(cl-defmacro eglot--sniffing ((&key server-requests
                                    server-notifications
                                    server-replies
                                    client-requests
                                    client-notifications
                                    client-replies)
                              &rest body)
  "Run BODY saving LSP JSON messages in variables, most recent first."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((log-event-ad-sym (make-symbol "eglot--event-sniff")))
    `(unwind-protect
         (let ,(delq nil (list server-requests
                               server-notifications
                               server-replies
                               client-requests
                               client-notifications
                               client-replies))
           (advice-add
            #'jsonrpc--log-event :before
            (lambda (_proc message &optional type)
              (cl-destructuring-bind (&key method id _error &allow-other-keys)
                  message
                (let ((req-p (and method id))
                      (notif-p method)
                      (reply-p id))
                  (cond
                   ((eq type 'server)
                    (cond (req-p ,(when server-requests
                                    `(push message ,server-requests)))
                          (notif-p ,(when server-notifications
                                      `(push message ,server-notifications)))
                          (reply-p ,(when server-replies
                                      `(push message ,server-replies)))))
                   ((eq type 'client)
                    (cond (req-p ,(when client-requests
                                    `(push message ,client-requests)))
                          (notif-p ,(when client-notifications
                                      `(push message ,client-notifications)))
                          (reply-p ,(when client-replies
                                      `(push message ,client-replies)))))))))
            '((name . ,log-event-ad-sym)))
           ,@body)
       (advice-remove #'jsonrpc--log-event ',log-event-ad-sym))))

(cl-defmacro eglot--wait-for ((events-sym &optional (timeout 1) message) args &body body)
  "Spin until FN match in EVENTS-SYM, flush events after it.
Pass TIMEOUT to `eglot--with-timeout'."
  (declare (indent 2) (debug (sexp sexp sexp &rest form)))
  `(eglot--with-timeout '(,timeout ,(or message
                                        (format "waiting for:\n%s" (pp-to-string body))))
     (let ((event
            (cl-loop thereis (cl-loop for json in ,events-sym
                                      for method = (plist-get json :method)
                                      when (keywordp method)
                                      do (plist-put json :method
                                                    (substring
                                                     (symbol-name method)
                                                     1))
                                      when (funcall
                                            (jsonrpc-lambda ,args ,@body) json)
                                      return (cons json before)
                                      collect json into before)
                     for i from 0
                     when (zerop (mod i 5))
                     ;; do (eglot--message "still struggling to find in %s"
                     ;;                    ,events-sym)
                     do
                     ;; `read-event' is essential to have the file
                     ;; watchers come through.
                     (read-event "[eglot] Waiting a bit..." nil 0.1)
                     (accept-process-output nil 0.1))))
       (setq ,events-sym (cdr event))
       (eglot--message "Event detected:\n%s"
                       (pp-to-string (car event))))))

;; `rust-mode' is not a part of emacs. So define these two shims which
;; should be more than enough for testing
(unless (functionp 'rust-mode)
  (define-derived-mode rust-mode prog-mode "Rust"))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(defun eglot--tests-connect (&optional timeout)
  (let* ((timeout (or timeout 2))
         (eglot-sync-connect t)
         (eglot-connect-timeout timeout))
    (apply #'eglot--connect (eglot--guess-contact))))


;;; Unit tests

(ert-deftest eclipse-connect ()
  "Connect to eclipse.jdt.ls server."
  (skip-unless (eglot--have-eclipse-jdt-ls-p))
  (eglot--with-fixture
      '(("project/src/main/java/foo" . (("Main.java" . "")))
        ("project/.git/" . nil))
    (with-current-buffer
        (eglot--find-file-noselect "project/src/main/java/foo/Main.java")
      (eglot--sniffing (:server-notifications s-notifs)
        (should (eglot--tests-connect 20))
        (eglot--wait-for (s-notifs 10)
            (&key _id method &allow-other-keys)
          (string= method "language/status"))))))

(ert-deftest eclipse-workspace-folders ()
  "Check eclipse connection with multi-root projects."
  (skip-unless (eglot--have-eclipse-jdt-ls-p))
  (eglot--with-fixture
      '(("project/main/src/main/java/foo" . (("Main.java" . "")))
        ("project/sub1/" . (("pom.xml" . "")))
        ("project/sub2/" . (("build.gradle" . "")))
        ("project/sub3/" . (("a.txt" . "")))
        ("project/.git/" . nil))
    (let ((root (file-name-as-directory default-directory)))
      (with-current-buffer
          (eglot--find-file-noselect "project/main/src/main/java/foo/Main.java")
        (eglot--sniffing (:client-requests c-reqs)
          (should (eglot--tests-connect 10))
          (eglot--wait-for (c-reqs 10)
              (&key _id method params &allow-other-keys)
            (when (string= method "initialize")
              (let ((folders (plist-get
                              (plist-get params :initializationOptions)
                              :workspaceFolders))
                    (default-directory root))
                (and
                 (cl-find (eglot--path-to-uri "project/") folders :test #'equal)
                 (cl-find (eglot--path-to-uri "project/sub1/") folders :test #'equal)
                 (cl-find (eglot--path-to-uri "project/sub2/") folders :test #'equal)
                 (= 3 (length folders)))))))))))

(defun eglot-tests--auto-detect-running-server-1 ()
  (let (server)
    (eglot--with-fixture
     `(("project" . (("coiso.py" . "bla")
                     ("merdix.py" . "bla")))
       ("anotherproject" . (("cena.py" . "bla"))))
     (with-current-buffer
         (eglot--find-file-noselect "project/coiso.py")
       (should (setq server (eglot--tests-connect)))
       (should (eglot-current-server)))
     (with-current-buffer
         (eglot--find-file-noselect "project/merdix.py")
       (should (eglot-current-server))
       (should (eq (eglot-current-server) server)))
     (with-current-buffer
         (eglot--find-file-noselect "anotherproject/cena.py")
       (should-error (eglot--current-server-or-lose))))))

(ert-deftest auto-detect-running-server ()
  "Visit a file and M-x eglot, then visit a neighbour. "
  (skip-unless (executable-find "pyls"))
  (eglot-tests--auto-detect-running-server-1))

(ert-deftest auto-shutdown ()
  "Visit a file and M-x eglot, then kill buffer. "
  (skip-unless (executable-find "pyls"))
  (let (server
        buffer)
    (eglot--with-fixture
        `(("project" . (("coiso.py" . "def coiso: pass"))))
      (with-current-buffer
          (setq buffer (eglot--find-file-noselect "project/coiso.py"))
        (should (setq server (eglot--tests-connect)))
        (should (eglot-current-server))
        (let ((eglot-autoshutdown nil)) (kill-buffer buffer))
        (should (jsonrpc-running-p server))
        ;; re-find file...
        (setq buffer (eglot--find-file-noselect (buffer-file-name buffer)))
        ;; ;; but now kill it with `eglot-autoshutdown' set to t
        (let ((eglot-autoshutdown t)) (kill-buffer buffer))
        (should (not (jsonrpc-running-p server)))))))

(ert-deftest auto-reconnect ()
  "Start a server. Kill it. Watch it reconnect."
  (skip-unless (executable-find "pyls"))
  (let (server (eglot-autoreconnect 1))
    (eglot--with-fixture
        `(("project" . (("coiso.py" . "bla")
                        ("merdix.py" . "bla"))))
      (with-current-buffer
          (eglot--find-file-noselect "project/coiso.py")
        (should (setq server (eglot--tests-connect)))
        ;; In 1.2 seconds > `eglot-autoreconnect' kill servers. We
        ;; should have a automatic reconnection.
        (run-with-timer 1.2 nil (lambda () (delete-process
                                            (jsonrpc--process server))))
        (while (jsonrpc-running-p server) (accept-process-output nil 0.5))
        (should (eglot-current-server))
        ;; Now try again too quickly
        (setq server (eglot-current-server))
        (let ((proc (jsonrpc--process server)))
          (run-with-timer 0.5 nil (lambda () (delete-process proc)))
          (while (process-live-p proc) (accept-process-output nil 0.5)))
        (should (not (eglot-current-server)))))))

(ert-deftest rls-watches-files ()
  "Start RLS server.  Notify it when a critical file changes."
  (skip-unless (executable-find "rls"))
  (skip-unless (executable-find "cargo"))
  (skip-unless (null (getenv "TRAVIS_TESTING")))
  (let ((eglot-autoreconnect 1))
    (eglot--with-fixture
        '(("watch-project" . (("coiso.rs" . "bla")
                              ("merdix.rs" . "bla"))))
      (with-current-buffer
          (eglot--find-file-noselect "watch-project/coiso.rs")
        (should (zerop (shell-command "cargo init")))
        (eglot--sniffing (
                          :server-requests s-requests
                          :client-notifications c-notifs
                          :client-replies c-replies
                          )
          (should (eglot--tests-connect))
          (let (register-id)
            (eglot--wait-for (s-requests 1)
                (&key id method &allow-other-keys)
              (setq register-id id)
              (string= method "client/registerCapability"))
            (eglot--wait-for (c-replies 1)
                (&key id error &allow-other-keys)
              (and (eq id register-id) (null error))))
          (delete-file "Cargo.toml")
          (eglot--wait-for
              (c-notifs 3 "waiting for didChangeWatchedFiles notification")
              (&key method params &allow-other-keys)
            (and (string= method "workspace/didChangeWatchedFiles")
                 (cl-destructuring-bind (&key uri type)
                     (elt (plist-get params :changes) 0)
                   (and (string= (eglot--path-to-uri "Cargo.toml") uri)
                        (= type 3))))))))))

(ert-deftest basic-diagnostics ()
  "Test basic diagnostics."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("diag-project" .
                                        ; colon missing after True
         (("main.py" . "def foo(): if True pass"))))
    (with-current-buffer
        (eglot--find-file-noselect "diag-project/main.py")
      (eglot--sniffing (:server-notifications s-notifs)
        (eglot--tests-connect)
        (eglot--wait-for (s-notifs 2)
            (&key _id method &allow-other-keys)
          (string= method "textDocument/publishDiagnostics"))
        (flymake-start)
        (goto-char (point-min))
        (flymake-goto-next-error 1 '() t)
        (should (eq 'flymake-error (face-at-point)))))))

(defun eglot--eldoc-on-demand ()
  ;; Trick Eldoc 1.1.0 into accepting on-demand calls.
  (eldoc t))

(defun eglot--tests-force-full-eldoc ()
  ;; FIXME: This uses some Eldoc implementation defatils.
  (when (buffer-live-p eldoc--doc-buffer)
    (with-current-buffer eldoc--doc-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (eglot--eldoc-on-demand)
  (cl-loop
   repeat 10
   for retval = (and (buffer-live-p eldoc--doc-buffer)
                     (with-current-buffer eldoc--doc-buffer
                       (let ((bs (buffer-string)))
                         (unless (zerop (length bs)) bs))))
   when retval return retval
   do (sit-for 0.1)
   finally (error "eglot--tests-force-full-eldoc didn't deliver.")))

(ert-deftest rls-hover-after-edit ()
  "Hover and highlightChanges are tricky in RLS."
  (skip-unless (executable-find "rls"))
  (skip-unless (executable-find "cargo"))
  (skip-unless (null (getenv "TRAVIS_TESTING")))
  (eglot--with-fixture
      '(("hover-project" .
         (("main.rs" .
           "fn test() -> i32 { let test=3; return te; }"))))
    (with-current-buffer
        (eglot--find-file-noselect "hover-project/main.rs")
      (should (zerop (shell-command "cargo init")))
      (eglot--sniffing (
                        :server-replies s-replies
                        :client-requests c-reqs
                        )
        (eglot--tests-connect)
        (goto-char (point-min))
        (search-forward "return te")
        (insert "st")
        (progn
          ;; simulate these two which don't happen when buffer isn't
          ;; visible in a window.
          (eglot--signal-textDocument/didChange)
          (eglot--eldoc-on-demand))
        (let (pending-id)
          (eglot--wait-for (c-reqs 2)
              (&key id method &allow-other-keys)
            (setq pending-id id)
            (string= method "textDocument/documentHighlight"))
          (eglot--wait-for (s-replies 2)
              (&key id &allow-other-keys)
            (eq id pending-id)))))))

(ert-deftest rename-a-symbol ()
  "Test basic symbol renaming"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("rename-project"
         . (("main.py" .
             "def foo (bar) : 1 + bar\n\ndef bar() : pass"))))
    (with-current-buffer
        (eglot--find-file-noselect "rename-project/main.py")
      (eglot--tests-connect)
      (goto-char (point-min)) (search-forward "bar")
      (eglot-rename "bla")
      (should (equal (buffer-string)
                     "def foo (bla) : 1 + bla\n\ndef bar() : pass")))))

(ert-deftest basic-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit")))))

(ert-deftest non-unique-completions ()
  "Test completion resulting in 'Complete, but not unique'"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      '(("project" . (("something.py" . "foo=1\nfoobar=2\nfoo"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point))
    ;; FIXME: `current-message' doesn't work here :-(
    (with-current-buffer (messages-buffer)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (should (looking-at "Complete, but not unique"))))))

(ert-deftest basic-imenu ()
  "Test basic imenu functionality in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "def foo(): pass\ndef bar(): foo()\n"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (should (imenu--make-index-alist))
      (should
       (equal imenu--index-alist
              `(("Function"
                 ("foo"
                  [(:name "foo" :containerName nil :location
                          (:uri ,(concat "file://" buffer-file-name) :range
                                (:start
                                 (:line 0 :character 0)
                                 :end
                                 (:line 1 :character 0)))
                          :kind 12)]
                  eglot--imenu-goto-function)
                 ("bar"
                  [(:name "bar" :containerName nil :location
                          (:uri ,(concat "file://" buffer-file-name) :range
                                (:start
                                 (:line 1 :character 0)
                                 :end
                                 (:line 2 :character 0)))
                          :kind 12)]
                  eglot--imenu-goto-function)))))
      (imenu (assoc "bar" (assoc "Function" imenu--index-alist)))
      (should (looking-at "def bar():")))))

(ert-deftest basic-xref ()
  "Test basic xref functionality in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "def foo(): pass\ndef bar(): foo()"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (search-forward "bar(): f")
      (call-interactively 'xref-find-definitions)
      (should (looking-at "foo(): pass")))))

(defvar eglot--test-python-buffer
  "\
def foobarquux(a, b, c=True): pass
def foobazquuz(d, e, f): pass
")

(ert-deftest snippet-completions ()
  "Test simple snippet completion in a python LSP"
  (skip-unless (and (executable-find "pyls")
                    (functionp 'yas-minor-mode)))
  (eglot--with-fixture
      `(("project" . (("something.py" . ,eglot--test-python-buffer))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (yas-minor-mode 1)
      (let ((eglot-workspace-configuration
             `((:pyls . (:plugins (:jedi_completion (:include_params t)))))))
        (should (eglot--tests-connect)))
      (goto-char (point-max))
      (insert "foobar")
      (completion-at-point)
      (should (looking-back "foobarquux("))
      (should (looking-at "a, b)")))))

(defvar company-candidates)

(ert-deftest snippet-completions-with-company ()
  "Test simple snippet completion in a python LSP"
  (skip-unless (and (executable-find "pyls")
                    (functionp 'yas-minor-mode)
                    (functionp 'company-complete)))
  (eglot--with-fixture
      `(("project" . (("something.py" . ,eglot--test-python-buffer))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (yas-minor-mode 1)
      (let ((eglot-workspace-configuration
             `((:pyls . (:plugins (:jedi_completion (:include_params t)))))))
        (should (eglot--tests-connect)))
      (goto-char (point-max))
      (insert "foo")
      (company-mode)
      (company-complete)
      (should (looking-back "fooba"))
      (should (= 2 (length company-candidates)))
      ;; this last one is brittle, since there it is possible that
      ;; pyls will change the representation of this candidate
      (should (member "foobazquuz(d, e, f)" company-candidates)))))

(ert-deftest eglot-eldoc-after-completions ()
  "Test documentation echo in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit"))
      (should (string-match "^exit" (eglot--tests-force-full-eldoc))))))

(ert-deftest eglot-multiline-eldoc ()
  "Test if suitable amount of lines of hover info are shown."
  :expected-result (if (getenv "TRAVIS_TESTING") :failed :passed)
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("hover-first.py" . "from datetime import datetime"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/hover-first.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      ;; one-line
      (let* ((eldoc-echo-area-use-multiline-p t)
             (captured-message (eglot--tests-force-full-eldoc)))
        (should (string-match "datetim" captured-message))
        (should (cl-find ?\n captured-message))))))

(ert-deftest eglot-single-line-eldoc ()
  "Test if suitable amount of lines of hover info are shown."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("hover-first.py" . "from datetime import datetime"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/hover-first.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      ;; one-line
      (let* ((eldoc-echo-area-use-multiline-p nil)
             (captured-message (eglot--tests-force-full-eldoc)))
        (should (string-match "datetim" captured-message))
        (should (not (cl-find ?\n eldoc-last-message)))))))

(ert-deftest python-autopep-formatting ()
  "Test formatting in the pyls python LSP.
pyls prefers autopep over yafp, despite its README stating the contrary."
  ;; Beware, default autopep rules can change over time, which may
  ;; affect this test.
  (skip-unless (and (executable-find "pyls")
                    (executable-find "autopep8")))
  (eglot--with-fixture
      `(("project" . (("something.py" . "def a():pass\n\ndef b():pass"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      ;; Try to format just the second line
      (search-forward "b():pa")
      (eglot-format (point-at-bol) (point-at-eol))
      (should (looking-at "ss"))
      (should
       (string= (buffer-string) "def a():pass\n\n\ndef b(): pass\n"))
      ;; now format the whole buffer
      (eglot-format-buffer)
      (should
       (string= (buffer-string) "def a(): pass\n\n\ndef b(): pass\n")))))

(ert-deftest python-yapf-formatting ()
  "Test formatting in the pyls python LSP"
  (skip-unless (and (executable-find "pyls")
                    (not (executable-find "autopep8"))
                    (executable-find "yapf")))
  (eglot--with-fixture
      `(("project" . (("something.py" . "def a():pass\ndef b():pass"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      ;; Try to format just the second line
      (search-forward "b():pa")
      (eglot-format (point-at-bol) (point-at-eol))
      (should (looking-at "ss"))
      (should
       (string= (buffer-string) "def a():pass\n\n\ndef b():\n    pass\n"))
      ;; now format the whole buffer
      (eglot-format-buffer)
      (should
       (string= (buffer-string) "def a():\n    pass\n\n\ndef b():\n    pass\n")))))

(ert-deftest javascript-basic ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "~/.yarn/bin/javascript-typescript-stdio"))
  (eglot--with-fixture
      '(("project" . (("hello.js" . "console.log('Hello world!');"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/hello.js")
      (let ((eglot-server-programs
             '((js-mode . ("~/.yarn/bin/javascript-typescript-stdio")))))
        (goto-char (point-max))
        (eglot--sniffing (:server-notifications
                          s-notifs
                          :client-notifications
                          c-notifs)
          (should (eglot--tests-connect))
          (eglot--wait-for (s-notifs 2) (&key method &allow-other-keys)
            (string= method "textDocument/publishDiagnostics"))
          (should (not (eq 'flymake-error (face-at-point))))
          (insert "{")
          (eglot--signal-textDocument/didChange)
          (eglot--wait-for (c-notifs 1) (&key method &allow-other-keys)
            (string= method "textDocument/didChange"))
          (eglot--wait-for (s-notifs 2) (&key params method &allow-other-keys)
            (and (string= method "textDocument/publishDiagnostics")
                 (cl-destructuring-bind (&key _uri diagnostics) params
                   (cl-find-if (jsonrpc-lambda (&key severity &allow-other-keys)
                                 (= severity 1))
                               diagnostics)))))))))

(ert-deftest json-basic ()
  "Test basic autocompletion in vscode-json-languageserver"
  (skip-unless (executable-find "vscode-json-languageserver"))
  (eglot--with-fixture
      '(("project" .
         (("p.json" . "{\"foo.b")
          ("s.json" . "{\"properties\":{\"foo.bar\":{\"default\":\"fb\"}}}")
          (".git" . nil))))
    (with-current-buffer
        (eglot--find-file-noselect "project/p.json")
      (yas-minor-mode)
      (goto-char 2)
      (insert "\"$schema\": \"file://"
              (file-name-directory buffer-file-name) "s.json\",")
      (let ((eglot-server-programs
             '((js-mode . ("vscode-json-languageserver" "--stdio")))))
        (goto-char (point-max))
        (should (eglot--tests-connect))
        (completion-at-point)
        (should (looking-back "\"foo.bar\": \""))
        (should (looking-at "fb\"$"))))))

(defun eglot-tests--lsp-abiding-column-1 ()
  (eglot--with-fixture
      '(("project" .
         (("foo.c" . "const char write_data[] = u8\"🚂🚃🚄🚅🚆🚈🚇🚈🚉🚊🚋🚌🚎🚝🚞🚟🚠🚡🛤🛲\";"))))
    (let ((eglot-server-programs
           '((c-mode . ("clangd")))))
      (with-current-buffer
          (eglot--find-file-noselect "project/foo.c")
        (setq-local eglot-move-to-column-function #'eglot-move-to-lsp-abiding-column)
        (setq-local eglot-current-column-function #'eglot-lsp-abiding-column)
        (eglot--sniffing (:client-notifications c-notifs)
          (eglot--tests-connect)
          (end-of-line)
          (insert "p ")
          (eglot--signal-textDocument/didChange)
          (eglot--wait-for (c-notifs 2) (&key params &allow-other-keys)
            (should (equal 71 (cadddr (cadadr (aref (cadddr params) 0))))))
          (beginning-of-line)
          (should (eq eglot-move-to-column-function #'eglot-move-to-lsp-abiding-column))
          (funcall eglot-move-to-column-function 71)
          (should (looking-at "p")))))))

(ert-deftest eglot-lsp-abiding-column ()
  "Test basic `eglot-lsp-abiding-column' and `eglot-move-to-lsp-abiding-column'"
  (skip-unless (executable-find "clangd"))
  (eglot-tests--lsp-abiding-column-1))

(ert-deftest eglot-ensure ()
  "Test basic `eglot-ensure' functionality"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("foo.py" . "import sys\nsys.exi")
                      ("bar.py" . "import sys\nsys.exi")))
        (python-mode-hook
         (eglot-ensure
          (lambda ()
            (remove-hook 'flymake-diagnostic-functions 'python-flymake)))))
    (let (server)
      ;; need `ert-simulate-command' because `eglot-ensure'
      ;; relies on `post-command-hook'.
      (with-current-buffer
          (ert-simulate-command
           '(find-file "project/foo.py"))
        (should (setq server (eglot-current-server))))
      (with-current-buffer
          (ert-simulate-command
           '(find-file "project/bar.py"))
        (should (eq server (eglot-current-server)))))))

(ert-deftest slow-sync-connection-wait ()
  "Connect with `eglot-sync-connect' set to t."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect t)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 1 && pyls")))))
        (should (eglot--tests-connect 3))))))

(ert-deftest slow-sync-connection-intime ()
  "Connect synchronously with `eglot-sync-connect' set to 2."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect 2)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 1 && pyls")))))
        (should (eglot--tests-connect 3))))))

(ert-deftest slow-async-connection ()
  "Connect asynchronously with `eglot-sync-connect' set to 2."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect 1)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 2 && pyls")))))
        (should-not (apply #'eglot--connect (eglot--guess-contact)))
        (eglot--with-timeout 3
          (while (not (eglot-current-server))
            (accept-process-output nil 0.2))
          (should (eglot-current-server)))))))

(ert-deftest slow-sync-timeout ()
  "Failed attempt at connection synchronously."
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      `(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect t)
            (eglot-connect-timeout 1)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 2 && pyls")))))
        (should-error (apply #'eglot--connect (eglot--guess-contact)))))))

(ert-deftest eglot-capabilities ()
  "Unit test for `eglot--server-capable'."
  (cl-letf (((symbol-function 'eglot--capabilities)
             (lambda (_dummy)
               ;; test data lifted from Golangserver example at
               ;; https://github.com/joaotavora/eglot/pull/74
               (list :textDocumentSync 2 :hoverProvider t
                     :completionProvider '(:triggerCharacters ["."])
                     :signatureHelpProvider '(:triggerCharacters ["(" ","])
                     :definitionProvider t :typeDefinitionProvider t
                     :referencesProvider t :documentSymbolProvider t
                     :workspaceSymbolProvider t :implementationProvider t
                     :documentFormattingProvider t :xworkspaceReferencesProvider t
                     :xdefinitionProvider t :xworkspaceSymbolByProperties t)))
            ((symbol-function 'eglot--current-server-or-lose)
             (lambda () nil)))
    (should (eql 2 (eglot--server-capable :textDocumentSync)))
    (should (eglot--server-capable :completionProvider :triggerCharacters))
    (should (equal '(:triggerCharacters ["."]) (eglot--server-capable :completionProvider)))
    (should-not (eglot--server-capable :foobarbaz))
    (should-not (eglot--server-capable :textDocumentSync :foobarbaz))))


(ert-deftest eglot-strict-interfaces ()
  (let ((eglot--lsp-interface-alist
         `((FooObject . ((:foo :bar) (:baz))))))
    (should
     (equal '("foo" . "bar")
            (let ((eglot-strict-mode nil))
              (eglot--dbind (foo bar) `(:foo "foo" :bar "bar")
                (cons foo bar)))))
    (should-error
     (let ((eglot-strict-mode '(disallow-non-standard-keys)))
       (eglot--dbind (foo bar) `(:foo "foo" :bar "bar" :fotrix bargh)
         (cons foo bar))))
    (should
     (equal '("foo" . "bar")
            (let ((eglot-strict-mode nil))
              (eglot--dbind (foo bar) `(:foo "foo" :bar "bar" :fotrix bargh)
                (cons foo bar)))))
    (should-error
     (let ((eglot-strict-mode '(disallow-non-standard-keys)))
       (eglot--dbind ((FooObject) foo bar) `(:foo "foo" :bar "bar" :fotrix bargh)
         (cons foo bar))))
    (should
     (equal '("foo" . "bar")
            (let ((eglot-strict-mode '(disallow-non-standard-keys)))
              (eglot--dbind ((FooObject) foo bar) `(:foo "foo" :bar "bar" :baz bargh)
                (cons foo bar)))))
    (should
     (equal '("foo" . nil)
            (let ((eglot-strict-mode nil))
              (eglot--dbind ((FooObject) foo bar) `(:foo "foo" :baz bargh)
                (cons foo bar)))))
    (should
     (equal '("foo" . "bar")
            (let ((eglot-strict-mode '(enforce-required-keys)))
              (eglot--dbind ((FooObject) foo bar) `(:foo "foo" :bar "bar" :baz bargh)
                (cons foo bar)))))
    (should-error
     (let ((eglot-strict-mode '(enforce-required-keys)))
       (eglot--dbind ((FooObject) foo bar) `(:foo "foo" :baz bargh)
         (cons foo bar))))))

(ert-deftest eglot-dcase ()
  (let ((eglot--lsp-interface-alist
         `((FooObject . ((:foo :bar) (:baz)))
           (CodeAction (:title) (:kind :diagnostics :edit :command))
           (Command ((:title . string) (:command . string)) (:arguments)))))
    (should
     (equal
      "foo"
      (eglot--dcase `(:foo "foo" :bar "bar")
        (((FooObject) foo)
         foo))))
    (should
     (equal
      (list "foo" '(:title "hey" :command "ho") "some edit")
      (eglot--dcase '(:title "foo"
                             :command (:title "hey" :command "ho")
                             :edit "some edit")
        (((Command) _title _command _arguments)
         (ert-fail "Shouldn't have destructured this object as a Command"))
        (((CodeAction) title edit command)
         (list title command edit)))))
    (should
     (equal
      (list "foo" "some command" nil)
      (eglot--dcase '(:title "foo" :command "some command")
        (((Command) title command arguments)
         (list title command arguments))
        (((CodeAction) _title _edit _command)
         (ert-fail "Shouldn't have destructured this object as a CodeAction")))))))

(ert-deftest eglot-dcase-issue-452 ()
  (let ((eglot--lsp-interface-alist
         `((FooObject . ((:foo :bar) (:baz)))
           (CodeAction (:title) (:kind :diagnostics :edit :command))
           (Command ((string . :title) (:command . string)) (:arguments)))))
    (should
     (equal
      (list "foo" '(:command "cmd" :title "alsofoo"))
      (eglot--dcase '(:title "foo" :command (:command "cmd" :title "alsofoo"))
        (((Command) _title _command _arguments)
         (ert-fail "Shouldn't have destructured this object as a Command"))
        (((CodeAction) title command)
         (list title command)))))))

(cl-defmacro eglot--guessing-contact ((interactive-sym
                                       prompt-args-sym
                                       guessed-class-sym guessed-contact-sym
                                       &optional guessed-lang-id-sym)
                                      &body body)
  "Evaluate BODY twice, binding results of `eglot--guess-contact'.

INTERACTIVE-SYM is bound to the boolean passed to
`eglot--guess-contact' each time. If the user would have been
prompted, PROMPT-ARGS-SYM is bound to the list of arguments that
would have been passed to `read-shell-command', else nil.
GUESSED-CLASS-SYM, GUESSED-CONTACT-SYM and GUESSED-LANG-ID-SYM
are bound to the useful return values of
`eglot--guess-contact'. Unless the server program evaluates to
\"a-missing-executable.exe\", this macro will assume it exists."
  (declare (indent 1) (debug t))
  (let ((i-sym (cl-gensym)))
    `(dolist (,i-sym '(nil t))
       (let ((,interactive-sym ,i-sym)
             (buffer-file-name "_")
             (,prompt-args-sym nil))
         (cl-letf (((symbol-function 'executable-find)
                    (lambda (name &optional _remote)
                      (unless (string-equal name "a-missing-executable.exe")
                        (format "/totally-mock-bin/%s" name))))
                   ((symbol-function 'read-shell-command)
                    (lambda (&rest args) (setq ,prompt-args-sym args) "")))
           (cl-destructuring-bind
               (_ _ ,guessed-class-sym ,guessed-contact-sym
                  ,(or guessed-lang-id-sym '_))
               (eglot--guess-contact ,i-sym)
             ,@body))))))

(ert-deftest eglot-server-programs-simple-executable ()
  (let ((eglot-server-programs '((foo-mode "some-executable")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("some-executable"))))))

(ert-deftest eglot-server-programs-simple-missing-executable ()
  (let ((eglot-server-programs '((foo-mode "a-missing-executable.exe")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (interactive-p prompt-args guessed-class guessed-contact)
      (should (equal (not prompt-args) (not interactive-p)))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("a-missing-executable.exe"))))))

(ert-deftest eglot-server-programs-executable-multiple-major-modes ()
  (let ((eglot-server-programs '(((bar-mode foo-mode) "some-executable")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("some-executable"))))))

(ert-deftest eglot-server-programs-executable-with-arg ()
  (let ((eglot-server-programs '((foo-mode "some-executable" "arg1")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("some-executable" "arg1"))))))

(ert-deftest eglot-server-programs-executable-with-args-and-autoport ()
  (let ((eglot-server-programs '((foo-mode "some-executable" "arg1"
                                           :autoport "arg2")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("some-executable" "arg1"
                                       :autoport "arg2"))))))

(ert-deftest eglot-server-programs-host-and-port ()
  (let ((eglot-server-programs '((foo-mode "somehost.example.com" 7777)))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("somehost.example.com" 7777))))))

(ert-deftest eglot-server-programs-host-and-port-and-tcp-args ()
  (let ((eglot-server-programs '((foo-mode "somehost.example.com" 7777
                                           :type network)))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("somehost.example.com" 7777
                                       :type network))))))

(ert-deftest eglot-server-programs-class-name-and-plist ()
  (let ((eglot-server-programs '((foo-mode bar-class :init-key init-val)))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'bar-class))
      (should (equal guessed-contact '(:init-key init-val))))))

(ert-deftest eglot-server-programs-class-name-and-contact-spec ()
  (let ((eglot-server-programs '((foo-mode bar-class "some-executable" "arg1"
                                           :autoport "arg2")))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'bar-class))
      (should (equal guessed-contact '("some-executable" "arg1"
                                       :autoport "arg2"))))))

(ert-deftest eglot-server-programs-function ()
  (let ((eglot-server-programs '((foo-mode . (lambda (&optional _)
                                               '("some-executable")))))
        (major-mode 'foo-mode))
    (eglot--guessing-contact (_ prompt-args guessed-class guessed-contact)
      (should (not prompt-args))
      (should (equal guessed-class 'eglot-lsp-server))
      (should (equal guessed-contact '("some-executable"))))))

(ert-deftest eglot-server-programs-guess-lang ()
  (let ((major-mode 'foo-mode))
    (let ((eglot-server-programs '((foo-mode . ("prog-executable")))))
      (eglot--guessing-contact (_ _ _ _ guessed-lang)
        (should (equal guessed-lang "foo"))))
    (let ((eglot-server-programs '(((foo-mode :language-id "bar")
                                    . ("prog-executable")))))
      (eglot--guessing-contact (_ _ _ _ guessed-lang)
        (should (equal guessed-lang "bar"))))
    (let ((eglot-server-programs '(((baz-mode (foo-mode :language-id "bar"))
                                    . ("prog-executable")))))
      (eglot--guessing-contact (_ _ _ _ guessed-lang)
        (should (equal guessed-lang "bar"))))))

(defun eglot--glob-match (glob str)
  (funcall (eglot--glob-compile glob t t) str))

(ert-deftest eglot--glob-test ()
  (should (eglot--glob-match "foo/**/baz" "foo/bar/baz"))
  (should (eglot--glob-match "foo/**/baz" "foo/baz"))
  (should-not (eglot--glob-match "foo/**/baz" "foo/bar"))
  (should (eglot--glob-match "foo/**/baz/**/quuz" "foo/baz/foo/quuz"))
  (should (eglot--glob-match "foo/**/baz/**/quuz" "foo/foo/foo/baz/foo/quuz"))
  (should-not (eglot--glob-match "foo/**/baz/**/quuz" "foo/foo/foo/ding/foo/quuz"))
  (should (eglot--glob-match "*.js" "foo.js"))
  (should-not (eglot--glob-match "*.js" "foo.jsx"))
  (should (eglot--glob-match "foo/**/*.js" "foo/bar/baz/foo.js"))
  (should-not (eglot--glob-match "foo/**/*.js" "foo/bar/baz/foo.jsx"))
  (should (eglot--glob-match "*.{js,ts}" "foo.js"))
  (should-not (eglot--glob-match "*.{js,ts}" "foo.xs"))
  (should (eglot--glob-match "foo/**/*.{js,ts}" "foo/bar/baz/foo.ts"))
  (should (eglot--glob-match "foo/**/*.{js,ts}x" "foo/bar/baz/foo.tsx"))
  (should (eglot--glob-match "?oo.js" "foo.js"))
  (should (eglot--glob-match "foo/**/*.{js,ts}?" "foo/bar/baz/foo.tsz"))
  (should (eglot--glob-match "foo/**/*.{js,ts}?" "foo/bar/baz/foo.tsz"))
  (should (eglot--glob-match "example.[!0-9]" "example.a"))
  (should-not (eglot--glob-match "example.[!0-9]" "example.0"))
  (should (eglot--glob-match "example.[0-9]" "example.0"))
  (should-not (eglot--glob-match "example.[0-9]" "example.a"))
  (should (eglot--glob-match "**/bar/" "foo/bar/"))
  (should-not (eglot--glob-match "foo.hs" "fooxhs"))

  ;; Some more tests
  (should (eglot--glob-match "**/.*" ".git"))
  (should (eglot--glob-match ".?" ".o"))
  (should (eglot--glob-match "**/.*" ".hidden.txt"))
  (should (eglot--glob-match "**/.*" "path/.git"))
  (should (eglot--glob-match "**/.*" "path/.hidden.txt"))
  (should (eglot--glob-match "**/node_modules/**" "node_modules/"))
  (should (eglot--glob-match "{foo,bar}/**" "foo/test"))
  (should (eglot--glob-match "{foo,bar}/**" "bar/test"))
  (should (eglot--glob-match "some/**/*" "some/foo.js"))
  (should (eglot--glob-match "some/**/*" "some/folder/foo.js"))

  ;; VSCode supposedly supports this, not sure if good idea.
  ;;
  ;; (should (eglot--glob-match "**/node_modules/**" "node_modules"))
  ;; (should (eglot--glob-match "{foo,bar}/**" "foo"))
  ;; (should (eglot--glob-match "{foo,bar}/**" "bar"))

  ;; VSCode also supports nested blobs.  Do we care?
  ;;
  ;; (should (eglot--glob-match "{**/*.d.ts,**/*.js}" "/testing/foo.js"))
  ;; (should (eglot--glob-match "{**/*.d.ts,**/*.js}" "testing/foo.d.ts"))
  ;; (should (eglot--glob-match "{**/*.d.ts,**/*.js,foo.[0-9]}" "foo.5"))
  ;; (should (eglot--glob-match "prefix/{**/*.d.ts,**/*.js,foo.[0-9]}" "prefix/foo.8"))
  )

(ert-deftest eglot--tramp-test ()
  "Ensure LSP servers can be used over TRAMP."
  (skip-unless (and (>= emacs-major-version 27) (executable-find "pyls")))
  ;; Set up a loopback TRAMP method that’s just a shell so the remote
  ;; host is really just the local host.
  (let ((tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))
        (tramp-methods '(("loopback"
                          (tramp-login-program "/bin/sh")
                          (tramp-remote-shell "/bin/sh")
                          (tramp-remote-shell-login ("-l"))
                          (tramp-remote-shell-args ("-c")))))
        (temporary-file-directory (concat "/loopback::"
                                          temporary-file-directory)))
    ;; With ‘temporary-file-directory’ bound to the ‘loopback’ TRAMP
    ;; method, fixtures will be automatically made “remote".
    (eglot-tests--auto-detect-running-server-1)))

(ert-deftest eglot--tramp-test-2 ()
  "Ensure LSP servers can be used over TRAMP."
  (skip-unless (or (>= emacs-major-version 27) (executable-find "clangd")))
  ;; Set up a loopback TRAMP method that’s just a shell so the remote
  ;; host is really just the local host.
  (let ((tramp-remote-path (cons 'tramp-own-remote-path tramp-remote-path))
        (tramp-methods '(("loopback"
                          (tramp-login-program "/bin/sh")
                          (tramp-remote-shell "/bin/sh")
                          (tramp-remote-shell-login ("-l"))
                          (tramp-remote-shell-args ("-c")))))
        (temporary-file-directory (concat "/loopback::"
                                          temporary-file-directory))
        (eglot-server-programs '((c-mode "clangd"))))
    (eglot-tests--lsp-abiding-column-1) ))

(ert-deftest eglot--path-to-uri-windows ()
  (should (string-prefix-p "file:///"
                           (eglot--path-to-uri "c:/Users/Foo/bar.lisp")))
  (should (string-suffix-p "c%3A/Users/Foo/bar.lisp"
                           (eglot--path-to-uri "c:/Users/Foo/bar.lisp"))))

(provide 'eglot-tests)
;;; eglot-tests.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
