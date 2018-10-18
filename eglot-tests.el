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

;; Helpers

(defun eglot--have-eclipse-jdt-ls-p ()
  (and (getenv "CLASSPATH")
       (cl-some
        (lambda (x)
          (string-match-p "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$" x))
        (split-string (getenv "CLASSPATH") ":"))))

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
  (let* ((fixture-directory (make-temp-file "eglot--fixture" t))
         (default-directory fixture-directory)
         new-buffers new-servers
         cleanup-events-et-cetera-p)
    (unwind-protect
        (let ((find-file-hook
               (cons (lambda () (push (current-buffer) new-buffers))
                     find-file-hook))
              (eglot-connect-hook
               (lambda (server) (push server new-servers))))
          (mapc #'eglot--make-file-or-dirs dirs)
          (funcall fn)
          (setq cleanup-events-et-cetera-p t))
      (unwind-protect
          (let ((eglot-autoreconnect nil))
            (mapc (lambda (server)
                    (eglot-shutdown
                     server nil nil (not cleanup-events-et-cetera-p)))
                  (cl-remove-if-not #'jsonrpc-running-p new-servers)))
        (eglot--message
         "Killing project buffers %s, deleting %s, killing server %s"
         (mapconcat #'buffer-name new-buffers ", ")
         default-directory
         (mapcar #'jsonrpc-name new-servers))
        (dolist (buf new-buffers) ;; have to save otherwise will get prompted
          (with-current-buffer buf (save-buffer) (kill-buffer)))
        (delete-directory fixture-directory 'recursive)))))

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
  (eglot--with-timeout (or timeout 2)
    (apply #'eglot--connect (eglot--guess-contact))))

(ert-deftest eclipse-connect ()
  "Connect to eclipse.jdt.ls server."
  (skip-unless (eglot--have-eclipse-jdt-ls-p))
  (eglot--with-dirs-and-files
   '(("project/src/main/java/foo" . (("Main.java" . "")))
     ("project/.git/" . nil))
   (with-current-buffer
       (eglot--find-file-noselect "project/src/main/java/foo/Main.java")
     (eglot--sniffing (:server-notifications s-notifs)
       (should (eglot--tests-connect 10))
       (eglot--wait-for (s-notifs 10)
           (&key _id method &allow-other-keys)
         (string= method "language/status"))
       (ignore-errors (eglot-shutdown (eglot--current-server) nil 10))))))

(ert-deftest eclipse-workspace-folders ()
  "Check eclipse connection with multi-root projects."
  (skip-unless (eglot--have-eclipse-jdt-ls-p))
  (eglot--with-dirs-and-files
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
                (= 3 (length folders))))))
         (ignore-errors (eglot-shutdown (eglot--current-server) nil 10)))))))

(ert-deftest auto-detect-running-server ()
  "Visit a file and M-x eglot, then visit a neighbour. "
  (skip-unless (executable-find "rls"))
  (let (server)
    (eglot--with-dirs-and-files
        '(("project" . (("coiso.rs" . "bla")
                        ("merdix.rs" . "bla")))
          ("anotherproject" . (("cena.rs" . "bla"))))
      (with-current-buffer
          (eglot--find-file-noselect "project/coiso.rs")
        (should (setq server (eglot--tests-connect)))
        (should (eglot--current-server)))
      (with-current-buffer
          (eglot--find-file-noselect "project/merdix.rs")
        (should (eglot--current-server))
        (should (eq (eglot--current-server) server)))
      (with-current-buffer
          (eglot--find-file-noselect "anotherproject/cena.rs")
        (should-error (eglot--current-server-or-lose))))))

(ert-deftest auto-reconnect ()
  "Start a server. Kill it. Watch it reconnect."
  (skip-unless (executable-find "rls"))
  (let (server (eglot-autoreconnect 1))
    (eglot--with-dirs-and-files
        '(("project" . (("coiso.rs" . "bla")
                        ("merdix.rs" . "bla"))))
      (with-current-buffer
          (eglot--find-file-noselect "project/coiso.rs")
        (should (setq server (eglot--tests-connect)))
        ;; In 1.2 seconds > `eglot-autoreconnect' kill servers. We
        ;; should have a automatic reconnection.
        (run-with-timer 1.2 nil (lambda () (delete-process
                                            (jsonrpc--process server))))
        (while (jsonrpc-running-p server) (accept-process-output nil 0.5))
        (should (eglot--current-server))
        ;; Now try again too quickly
        (setq server (eglot--current-server))
        (let ((proc (jsonrpc--process server)))
          (run-with-timer 0.5 nil (lambda () (delete-process proc)))
          (while (process-live-p proc) (accept-process-output nil 0.5)))
        (should (not (eglot--current-server)))))))

(ert-deftest rls-watches-files ()
  "Start RLS server.  Notify it when a critical file changes."
  (skip-unless (executable-find "rls"))
  (skip-unless (executable-find "cargo"))
  (skip-unless (null (getenv "TRAVIS_TESTING")))
  (let ((eglot-autoreconnect 1))
    (eglot--with-dirs-and-files
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

(ert-deftest rls-basic-diagnostics ()
  "Test basic diagnostics in RLS."
  (skip-unless (executable-find "rls"))
  (skip-unless (executable-find "cargo"))
  (eglot--with-dirs-and-files
      '(("diag-project" . (("main.rs" . "fn main() {\nprintfoo!(\"Hello, world!\");\n}"))))
    (with-current-buffer
        (eglot--find-file-noselect "diag-project/main.rs")
      (should (zerop (shell-command "cargo init")))
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
  (eglot--with-dirs-and-files
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

(ert-deftest rls-rename ()
  "Test renaming in RLS."
  (skip-unless (executable-find "rls"))
  (skip-unless (executable-find "cargo"))
  (eglot--with-dirs-and-files
      '(("rename-project"
         . (("main.rs" .
             "fn test() -> i32 { let test=3; return test; }"))))
    (with-current-buffer
        (eglot--find-file-noselect "rename-project/main.rs")
      (should (zerop (shell-command "cargo init")))
      (eglot--tests-connect)
      (goto-char (point-min)) (search-forward "return te")
      (eglot-rename "bla")
      (should (equal (buffer-string) "fn test() -> i32 { let bla=3; return bla; }")))))

(ert-deftest basic-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (completion-at-point)
      (should (looking-back "sys.exit")))))

(ert-deftest hover-after-completions ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (goto-char (point-max))
      (setq eldoc-last-message nil)
      (completion-at-point)
      (should (looking-back "sys.exit"))
      (while (not eldoc-last-message) (accept-process-output nil 0.1))
      (should (string-match "^exit" eldoc-last-message)))))

(ert-deftest formatting ()
  "Test formatting in a python LSP"
  (skip-unless (and (executable-find "pyls")
                    (or (executable-find "yapf")
                        (executable-find "autopep8"))))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "def a():pass\ndef b():pass"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (should (eglot--tests-connect))
      (search-forward "b():pa")
      (eglot-format (point-at-bol) (point-at-eol))
      (should (looking-at "ss"))
      (should
       (or
        ;; yapf
        (string= (buffer-string) "def a():pass\n\n\ndef b():\n    pass\n")
        ;; autopep8
        (string= (buffer-string) "def a():pass\n\n\ndef b(): pass\n")))
      (eglot-format-buffer)
      (should
       (or
        ;; yapf
        (string= (buffer-string) "def a():\n    pass\n\n\ndef b():\n    pass\n")
        ;; autopep8
        (string= (buffer-string) "def a(): pass\n\n\ndef b(): pass\n"))))))

(ert-deftest javascript-basic ()
  "Test basic autocompletion in a python LSP"
  (skip-unless (executable-find "~/.yarn/bin/javascript-typescript-stdio"))
  (eglot--with-dirs-and-files
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

(ert-deftest zzz-eglot-ensure ()
  "Test basic `eglot-ensure' functionality"
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("foo.py" . "import sys\nsys.exi")
                      ("bar.py" . "import sys\nsys.exi"))))
    (let ((saved-python-mode-hook python-mode-hook)
          server)
      (unwind-protect
          (progn
            (add-hook 'python-mode-hook 'eglot-ensure)
            ;; need `ert-simulate-command' because `eglot-ensure'
            ;; relies on `post-command-hook'.
            (with-current-buffer
                (ert-simulate-command
                 '(find-file "project/foo.py"))
              (should (setq server (eglot--current-server))))
            (with-current-buffer
                (ert-simulate-command
                 '(find-file "project/bar.py"))
              (should (eq server (eglot--current-server)))))
        (setq python-mode-hook saved-python-mode-hook)))))

(ert-deftest slow-sync-connection-wait ()
  "Connect with `eglot-sync-connect' set to t."
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
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
  (eglot--with-dirs-and-files
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
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
    (with-current-buffer
        (eglot--find-file-noselect "project/something.py")
      (let ((eglot-sync-connect 1)
            (eglot-server-programs
             `((python-mode . ("sh" "-c" "sleep 2 && pyls")))))
        (should-not (apply #'eglot--connect (eglot--guess-contact)))
        (eglot--with-timeout 3
          (while (not (eglot--current-server))
            (accept-process-output nil 0.2))
          (should (eglot--current-server)))))))

(ert-deftest slow-sync-timeout ()
  "Failed attempt at connection synchronously."
  (skip-unless (executable-find "pyls"))
  (eglot--with-dirs-and-files
      '(("project" . (("something.py" . "import sys\nsys.exi"))))
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

(provide 'eglot-tests)
;;; eglot-tests.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
