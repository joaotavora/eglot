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

;; Helpers

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
           (list file-or-dir-name))
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
        (let ((eglot-connect-hook
               (lambda (server) (push server new-servers))))
          (setq created-files (mapcan #'eglot--make-file-or-dir file-specs))
          (prog1 (funcall fn)
            (setq test-body-successful-p t)))
      (eglot--message
       "Test body was %s" (if test-body-successful-p "OK" "A FAILURE"))
      (unwind-protect
          (let ((eglot-autoreconnect nil))
            (mapc (lambda (server)
                    (condition-case oops
                        (eglot-shutdown
                         server nil 3 (not test-body-successful-p))
                      (error
                       (message "[eglot] Non-critical shutdown error after test: %S"
                                oops))))
                  (cl-remove-if-not #'jsonrpc-running-p new-servers)))
        (let ((buffers-to-delete
               (delete nil (mapcar #'find-buffer-visiting created-files))))
          (eglot--message "Killing %s, wiping %s, restoring %s"
                          buffers-to-delete
                          default-directory
                          (mapcar #'car syms-to-restore))
          (cl-loop for (sym . val) in syms-to-restore
                   do (set sym val))
          (dolist (buf buffers-to-delete) ;; have to save otherwise will get prompted
            (with-current-buffer buf (save-buffer) (kill-buffer)))
          (delete-directory fixture-directory 'recursive))))))

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
                 (seq-contains folders (eglot--path-to-uri "project/"))
                 (seq-contains folders (eglot--path-to-uri "project/sub1/"))
                 (seq-contains folders (eglot--path-to-uri "project/sub2/"))
                 (= 3 (length folders)))))))))))

(ert-deftest auto-detect-running-server ()
  "Visit a file and M-x eglot, then visit a neighbour. "
  (skip-unless (executable-find "pyls"))
  (let (server)
    (eglot--with-fixture
        '(("project" . (("coiso.py" . "bla")
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

(ert-deftest auto-shutdown ()
  "Visit a file and M-x eglot, then kill buffer. "
  (skip-unless (executable-find "pyls"))
  (let (server
        buffer)
    (eglot--with-fixture
        '(("project" . (("coiso.py" . "def coiso: pass"))))
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
        '(("project" . (("coiso.py" . "bla")
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
      '(("diag-project" .
         (("main.py" . "def foo(): if True pass")))) ; colon missing after True
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
          (eglot-eldoc-function))
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
      '(("rename-project"
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
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit")))))

(ert-deftest basic-xref ()
  "Test basic xref functionality in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      '(("project" . (("something.py" . "def foo(): pass\ndef bar(): foo()"))))
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
      (beginning-of-line)
      (should (looking-at "foobarquux(a, b)")))))

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

(ert-deftest hover-after-completions ()
  "Test documentation echo in a python LSP"
  (skip-unless (executable-find "pyls"))
  ;; JT@19/06/21: We check with `eldoc-last-message' because it's
  ;; practical, which forces us to use
  ;; `eglot-put-doc-in-help-buffer' to nil.
  (let ((eglot-put-doc-in-help-buffer nil))
    (eglot--with-fixture
     '(("project" . (("something.py" . "import sys\nsys.exi"))))
     (with-current-buffer
         (eglot--find-file-noselect "project/something.py")
       (should (eglot--tests-connect))
       (goto-char (point-max))
       (setq eldoc-last-message nil)
       (completion-at-point)
       (should (looking-back "sys.exit"))
       (while (not eldoc-last-message) (accept-process-output nil 0.1))
       (should (string-match "^exit" eldoc-last-message))))))

(ert-deftest python-autopep-formatting ()
  "Test formatting in the pyls python LSP.
pyls prefers autopep over yafp, despite its README stating the contrary."
  ;; For some reason Travis will fail the part of the test where we
  ;; try to reformat just the second line, i.e. it will _not_ add
  ;; newlines before the region we asked to reformat.  I actually
  ;; think Travis' behaviour is more sensible, but I don't know how to
  ;; reproduce it locally.  Must be some Python version thing.
  ;; Beware, this test is brittle if ~/.config/pycodestyle exists, or
  ;; default autopep rules change, which has happened.
  (skip-unless (null (getenv "TRAVIS_TESTING")))
  (skip-unless (and (executable-find "pyls")
                    (executable-find "autopep8")))
  (eglot--with-fixture
      '(("project" . (("something.py" . "def a():pass\n\ndef b():pass"))))
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
      '(("project" . (("something.py" . "def a():pass\ndef b():pass"))))
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

(ert-deftest eglot-ensure ()
  "Test basic `eglot-ensure' functionality"
  (skip-unless (executable-find "pyls"))
  (eglot--with-fixture
      '(("project" . (("foo.py" . "import sys\nsys.exi")
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
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
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
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
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
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
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
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect t)
            (eglot-connect-timeout 1)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 2 && pyls")))))
        (should-error (apply #'eglot--connect (eglot--guess-contact)))))))



;;; Unit tests
;;; 
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
           (Command (:title :command) (:arguments)))))
    (should
     (equal
      "foo"
      (eglot--dcase `(:foo "foo" :bar "bar")
        (((FooObject) foo)
         foo))))
    (should
     (equal
      (list "foo" "some command" "some edit")
      (eglot--dcase '(:title "foo" :command "some command" :edit "some edit")
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

(provide 'eglot-tests)
;;; eglot-tests.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:

