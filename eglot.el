;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Version: 1.4
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (jsonrpc "1.0.7") (flymake "1.0.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simply M-x eglot should be enough to get you started, but here's a
;; little info (see the accompanying README.md or the URL for more).
;;
;; M-x eglot starts a server via a shell-command guessed from
;; `eglot-server-programs', using the current major-mode (for whatever
;; language you're programming in) as a hint.  If it can't guess, it
;; prompts you in the mini-buffer for these things.  Actually, the
;; server needen't be locally started: you can connect to a running
;; server via TCP by entering a <host:port> syntax.
;;
;; Anyway, if the connection is successful, you should see an `eglot'
;; indicator pop up in your mode-line.  More importantly, this means
;; current *and future* file buffers of that major mode *inside your
;; current project* automatically become \"managed\" by the LSP
;; server, i.e.  information about their contents is exchanged
;; periodically to provide enhanced code analysis via
;; `xref-find-definitions', `flymake-mode', `eldoc-mode',
;; `completion-at-point', among others.
;;
;; To "unmanage" these buffers, shutdown the server with M-x
;; eglot-shutdown.
;;
;; You can also do:
;;
;;   (add-hook 'foo-mode-hook 'eglot-ensure)
;;
;; To attempt to start an eglot session automatically everytime a
;; foo-mode buffer is visited.
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(require 'flymake)
(require 'xref)
(eval-when-compile
  (require 'subr-x))
(require 'jsonrpc)
(require 'filenotify)
(require 'ert)


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-server-programs '((rust-mode . (eglot-rls "rls"))
                                (python-mode . ("pyls"))
                                ((js-mode
                                  js2-mode
                                  rjsx-mode
                                  typescript-mode)
                                 . ("javascript-typescript-stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                ((c++-mode c-mode) . ("ccls"))
                                ((caml-mode tuareg-mode reason-mode)
                                 . ("ocaml-language-server" "--stdio"))
                                (ruby-mode
                                 . ("solargraph" "socket" "--port"
                                    :autoport))
                                (php-mode . ("php" "vendor/felixfbecker/\
language-server/bin/php-language-server.php"))
                                (haskell-mode . ("hie-wrapper"))
                                (kotlin-mode . ("kotlin-language-server"))
                                (go-mode . ("go-langserver" "-mode=stdio"
                                            "-gocodecompletion"))
                                ((R-mode ess-r-mode) . ("R" "--slave" "-e"
                                                        "languageserver::run()"))
                                (java-mode . eglot--eclipse-jdt-contact)
                                (dart-mode . ("dart_language_server")))
  "How the command `eglot' guesses the server to start.
An association list of (MAJOR-MODE . CONTACT) pairs.  MAJOR-MODE
is a mode symbol, or a list of mode symbols.  The associated
CONTACT specifies how to connect to a server for managing buffers
of those modes.  CONTACT can be:

* In the most common case, a list of strings (PROGRAM [ARGS...]).
  PROGRAM is called with ARGS and is expected to serve LSP requests
  over the standard input/output channels.

* A list (HOST PORT [TCP-ARGS...]) where HOST is a string and PORT is
  na positive integer number for connecting to a server via TCP.
  Remaining ARGS are passed to `open-network-stream' for
  upgrading the connection with encryption or other capabilities.

* A list (PROGRAM [ARGS...] :autoport [MOREARGS...]), whereby a
  combination of the two previous options is used..  First, an
  attempt is made to find an available server port, then PROGRAM
  is launched with ARGS; the `:autoport' keyword substituted for
  that number; and MOREARGS.  Eglot then attempts to to establish
  a TCP connection to that port number on the localhost.

* A cons (CLASS-NAME . INITARGS) where CLASS-NAME is a symbol
  designating a subclass of `eglot-lsp-server', for representing
  experimental LSP servers.  INITARGS is a keyword-value plist
  used to initialize CLASS-NAME, or a plain list interpreted as
  the previous descriptions of CONTACT, in which case it is
  converted to produce a plist with a suitable :PROCESS initarg
  to CLASS-NAME.  The class `eglot-lsp-server' descends
  `jsonrpc-process-connection', which you should see for the
  semantics of the mandatory :PROCESS argument.

* A function of a single argument producing any of the above
  values for CONTACT.  The argument's value is non-nil if the
  connection was requested interactively (e.g. from the `eglot'
  command), and nil if it wasn't (e.g. from `eglot-ensure').  If
  the call is interactive, the function can ask the user for
  hints on finding the required programs, etc.  Otherwise, it
  should not ask the user for any input, and return nil or signal
  an error if it can't produce a valid CONTACT.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line.")

(defcustom eglot-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom eglot-connect-timeout 30
  "Number of seconds before timing out LSP connection attempts.
If nil, never time out."
  :type 'number)

(defcustom eglot-sync-connect 3
  "Control blocking of LSP connection attempts.
If t, block for `eglot-connect-timeout' seconds.  A positive
integer number means block for that many seconds, and then wait
for the connection in the background.  nil has the same meaning
as 0, i.e. don't block at all."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))

(defcustom eglot-events-buffer-size 2000000
  "Control the size of the Eglot events buffer.
If a number, don't let the buffer grow larger than that many
characters.  If 0, don't use an event's buffer at all.  If nil,
let the buffer grow forever."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Number of characters")))


;;; Constants
;;;
(defconst eglot--symbol-kind-names
  `((1 . "File") (2 . "Module")
    (3 . "Namespace") (4 . "Package") (5 . "Class")
    (6 . "Method") (7 . "Property") (8 . "Field")
    (9 . "Constructor") (10 . "Enum") (11 . "Interface")
    (12 . "Function") (13 . "Variable") (14 . "Constant")
    (15 . "String") (16 . "Number") (17 . "Boolean")
    (18 . "Array") (19 . "Object") (20 . "Key")
    (21 . "Null") (22 . "EnumMember") (23 . "Struct")
    (24 . "Event") (25 . "Operator") (26 . "TypeParameter")))

(defconst eglot--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Function") (4 . "Constructor")
    (5 . "Field") (6 . "Variable") (7 . "Class") (8 . "Interface")
    (9 . "Module") (10 . "Property") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "Keyword") (15 . "Snippet") (16 . "Color")
    (17 . "File") (18 . "Reference")))



;;; Message verification helpers
;;;
(eval-and-compile
  (defvar eglot--lsp-interface-alist
    `(
      (CodeAction (:title) (:kind :diagnostics :edit :command))
      (Command (:title :command) (:arguments))
      (CompletionItem (:label)
                      (:kind :detail :documentation :deprecated :preselect
                             :sortText :filterText :insertText :insertTextFormat
                             :textEdit :additionalTextEdits :commitCharacters
                             :command :data))
      (Diagnostic (:range :message) (:severity :code :source :relatedInformation))
      (DocumentHighlight (:range) (:kind))
      (FileSystemWatcher (:globPattern) (:kind))
      (Hover (:contents) (:range))
      (InitializeResult (:capabilities))
      (Location (:uri :range))
      (LogMessageParams (:type :message))
      (MarkupContent (:kind :value))
      (ParameterInformation (:label) (:documentation))
      (Position (:line :character))
      (Range (:start :end))
      (Registration (:id :method) (:registerOptions))
      (Registration (:id :method) (:registerOptions))
      (ResponseError (:code :message) (:data))
      (ShowMessageParams (:type :message))
      (ShowMessageRequestParams (:type :message) (:actions))
      (SignatureHelp (:signatures) (:activeSignature :activeParameter))
      (SignatureInformation (:label) (:documentation :parameters))
      (SymbolInformation (:name :kind :location) (:deprecated :containerName))
      (TextDocumentEdit (:textDocument :edits) ())
      (TextEdit (:range :newText))
      (VersionedTextDocumentIdentifier (:uri :version) ())
      (WorkspaceEdit () (:changes :documentChanges))
      )
    "Alist (INTERFACE-NAME . INTERFACE) of known external LSP interfaces.

INTERFACE-NAME is a symbol designated by the spec as
\"interface\".  INTERFACE is a list (REQUIRED OPTIONAL) where
REQUIRED and OPTIONAL are lists of keyword symbols designating
field names that must be, or may be, respectively, present in a
message adhering to that interface.

Here's what an element of this alist might look like:

    (CreateFile . ((:kind :uri) (:options)))"))

(eval-and-compile
  (defvar eglot-strict-mode (if load-file-name '()
                              '(disallow-non-standard-keys
                                ;; Uncomment these two for fun at
                                ;; compile-time or with flymake-mode.
                                ;;
                                ;; enforce-required-keys
                                ;; enforce-optional-keys
                                ))
    "How strictly to check LSP interfaces at compile- and run-time.

Value is a list of symbols (if the list is empty, no checks are
performed).

If the symbol `disallow-non-standard-keys' is present, an error
is raised if any extraneous fields are sent by the server.  At
compile-time, a warning is raised if a destructuring spec
includes such a field.

If the symbol `enforce-required-keys' is present, an error is
raised if any required fields are missing from the message sent
from the server.  At compile-time, a warning is raised if a
destructuring spec doesn't use such a field.

If the symbol `enforce-optional-keys' is present, nothing special
happens at run-time.  At compile-time, a warning is raised if a
destructuring spec doesn't use all optional fields.

If the symbol `disallow-unknown-methods' is present, Eglot warns
on unknown notifications and errors on unknown requests.
"))

(defun eglot--plist-keys (plist)
  (cl-loop for (k _v) on plist by #'cddr collect k))

(defun eglot--call-with-interface (interface object fn)
  "Call FN, checking that OBJECT conforms to INTERFACE."
  (when-let ((missing (and (memq 'enforce-required-keys eglot-strict-mode)
                           (cl-set-difference (car (cdr interface))
                                              (eglot--plist-keys object)))))
    (eglot--error "A `%s' must have %s" (car interface) missing))
  (when-let ((excess (and (memq 'disallow-non-standard-keys eglot-strict-mode)
                          (cl-set-difference
                           (eglot--plist-keys object)
                           (append (car (cdr interface)) (cadr (cdr interface)))))))
    (eglot--error "A `%s' mustn't have %s" (car interface) excess))
  (funcall fn))

(eval-and-compile
  (defun eglot--keywordize-vars (vars)
    (mapcar (lambda (var) (intern (format ":%s" var))) vars))

  (defun eglot--check-interface (interface-name vars)
    (let ((interface
           (assoc interface-name eglot--lsp-interface-alist)))
      (cond (interface
             (let ((too-many
                    (and
                     (memq 'disallow-non-standard-keys eglot-strict-mode)
                     (cl-set-difference
                      (eglot--keywordize-vars vars)
                      (append (car (cdr interface))
                              (cadr (cdr interface))))))
                   (ignored-required
                    (and
                     (memq 'enforce-required-keys eglot-strict-mode)
                     (cl-set-difference
                      (car (cdr interface))
                      (eglot--keywordize-vars vars))))
                   (missing-out
                    (and
                     (memq 'enforce-optional-keys eglot-strict-mode)
                     (cl-set-difference
                      (cadr (cdr interface))
                      (eglot--keywordize-vars vars)))))
               (when too-many (byte-compile-warn
                               "Destructuring for %s has extraneous %s"
                               interface-name too-many))
               (when ignored-required (byte-compile-warn
                                       "Destructuring for %s ignores required %s"
                                       interface-name ignored-required))
               (when missing-out (byte-compile-warn
                                  "Destructuring for %s is missing out on %s"
                                  interface-name missing-out))))
            (t
             (byte-compile-warn "Unknown LSP interface %s" interface-name))))))

(cl-defmacro eglot--dbind (vars object &body body)
  "Destructure OBJECT of binding VARS in BODY.
VARS is ([(INTERFACE)] SYMS...)
Honour `eglot-strict-mode'."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((interface-name (if (consp (car vars))
                            (car (pop vars))))
        (object-once (make-symbol "object-once"))
        (fn-once (make-symbol "fn-once")))
    (cond (interface-name
           (eglot--check-interface interface-name vars)
           `(let ((,object-once ,object))
              (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                (eglot--call-with-interface (assoc ',interface-name
                                                   eglot--lsp-interface-alist)
                                            ,object-once (lambda ()
                                                           ,@body)))))
          (t
           `(let ((,object-once ,object)
                  (,fn-once (lambda (,@vars) ,@body)))
              (if (memq 'disallow-non-standard-keys eglot-strict-mode)
                  (cl-destructuring-bind (&key ,@vars) ,object-once
                    (funcall ,fn-once ,@vars))
                (cl-destructuring-bind (&key ,@vars &allow-other-keys) ,object-once
                  (funcall ,fn-once ,@vars))))))))


(cl-defmacro eglot--lambda (cl-lambda-list &body body)
  "Function of args CL-LAMBDA-LIST for processing INTERFACE objects.
Honour `eglot-strict-mode'."
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (cl-gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (eglot--dbind ,cl-lambda-list ,e ,@body))))

(cl-defmacro eglot--dcase (obj &rest clauses)
  "Like `pcase', but for the LSP object OBJ.
CLAUSES is a list (DESTRUCTURE FORMS...) where DESTRUCTURE is
treated as in `eglot-dbind'."
  (declare (indent 1) (debug (sexp &rest (sexp &rest form))))
  (let ((obj-once (make-symbol "obj-once")))
    `(let ((,obj-once ,obj))
       (cond
        ,@(cl-loop
           for (vars . body) in clauses
           for vars-as-keywords = (eglot--keywordize-vars vars)
           for interface-name = (if (consp (car vars))
                                    (car (pop vars)))
           for condition =
           (cond (interface-name
                  (eglot--check-interface interface-name vars)
                  ;; In this mode, in runtime, we assume
                  ;; `eglot-strict-mode' is fully on, otherwise we
                  ;; can't disambiguate between certain types.
                  `(let* ((interface
                           (or (assoc ',interface-name eglot--lsp-interface-alist)
                               (eglot--error "Unknown LSP interface %s"
                                             ',interface-name)))
                          (object-keys (eglot--plist-keys ,obj-once))
                          (required-keys (car (cdr interface))))
                     (and (null (cl-set-difference required-keys object-keys))
                          (null (cl-set-difference
                                 (cl-set-difference object-keys required-keys)
                                 (cadr (cdr interface)))))))
                 (t
                  ;; In this interface-less mode we don't check
                  ;; `eglot-strict-mode' at all: just check that the object
                  ;; has all the keys the user wants to destructure.
                  `(null (cl-set-difference
                          ',vars-as-keywords
                          (eglot--plist-keys ,obj-once)))))
           collect `(,condition
                     (cl-destructuring-bind (&key ,@vars &allow-other-keys)
                         ,obj-once
                       ,@body)))
        (t
         (eglot--error "%s didn't match any of %s"
                       ,obj-once
                       ',(mapcar #'car clauses)))))))


;;; API (WORK-IN-PROGRESS!)
;;;
(cl-defmacro eglot--with-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro eglot--widening (&rest body)
  "Save excursion and restriction. Widen. Then run BODY." (declare (debug t))
  `(save-excursion (save-restriction (widen) ,@body)))

(cl-defgeneric eglot-handle-request (server method &rest params)
  "Handle SERVER's METHOD request with PARAMS.")

(cl-defgeneric eglot-handle-notification (server method &rest params)
  "Handle SERVER's METHOD notification with PARAMS.")

(cl-defgeneric eglot-execute-command (server command arguments)
  "Ask SERVER to execute COMMAND with ARGUMENTS.")

(cl-defgeneric eglot-initialization-options (server)
  "JSON object to send under `initializationOptions'"
  (:method (_s) nil)) ; blank default

(cl-defgeneric eglot-register-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (eglot--warn "Server tried to register unsupported capability `%s'"
                method)))

(cl-defgeneric eglot-unregister-capability (server method id &rest params)
  "Ask SERVER to register capability METHOD marked with ID."
  (:method
   (_s method _id &rest _params)
   (eglot--warn "Server tried to unregister unsupported capability `%s'"
                method)))

(cl-defgeneric eglot-client-capabilities (server)
  "What the EGLOT LSP client supports for SERVER."
  (:method (_s)
           (list
            :workspace (list
                        :applyEdit t
                        :executeCommand `(:dynamicRegistration :json-false)
                        :workspaceEdit `(:documentChanges :json-false)
                        :didChangeWatchedFiles `(:dynamicRegistration t)
                        :symbol `(:dynamicRegistration :json-false))
            :textDocument
            (list
             :synchronization (list
                               :dynamicRegistration :json-false
                               :willSave t :willSaveWaitUntil t :didSave t)
             :completion      (list :dynamicRegistration :json-false
                                    :completionItem
                                    `(:snippetSupport
                                      ,(if (eglot--snippet-expansion-fn)
                                           t
                                         :json-false))
                                    :contextSupport t)
             :hover              `(:dynamicRegistration :json-false)
             :signatureHelp      (list :dynamicRegistration :json-false
                                       :signatureInformation
                                       `(:parameterInformation
                                         (:labelOffsetSupport t)))
             :references         `(:dynamicRegistration :json-false)
             :definition         `(:dynamicRegistration :json-false)
             :documentSymbol     (list
                                  :dynamicRegistration :json-false
                                  :symbolKind `(:valueSet
                                                [,@(mapcar
                                                    #'car eglot--symbol-kind-names)]))
             :documentHighlight  `(:dynamicRegistration :json-false)
             :codeAction         (list
                                  :dynamicRegistration :json-false
                                  :codeActionLiteralSupport
                                  '(:codeActionKind
                                    (:valueSet
                                     ["quickfix"
                                      "refactor" "refactor.extract"
                                      "refactor.inline" "refactor.rewrite"
                                      "source" "source.organizeImports"])))
             :formatting         `(:dynamicRegistration :json-false)
             :rangeFormatting    `(:dynamicRegistration :json-false)
             :rename             `(:dynamicRegistration :json-false)
             :publishDiagnostics `(:relatedInformation :json-false))
            :experimental (list))))

(defclass eglot-lsp-server (jsonrpc-process-connection)
  ((project-nickname
    :documentation "Short nickname for the associated project."
    :accessor eglot--project-nickname)
   (major-mode
    :documentation "Major mode symbol."
    :accessor eglot--major-mode)
   (capabilities
    :documentation "JSON object containing server capabilities."
    :accessor eglot--capabilities)
   (shutdown-requested
    :documentation "Flag set when server is shutting down."
    :accessor eglot--shutdown-requested)
   (project
    :documentation "Project associated with server."
    :accessor eglot--project)
   (spinner
    :documentation "List (ID DOING-WHAT DONE-P) representing server progress."
    :initform `(nil nil t) :accessor eglot--spinner)
   (inhibit-autoreconnect
    :initform t
    :documentation "Generalized boolean inhibiting auto-reconnection if true."
    :accessor eglot--inhibit-autoreconnect)
   (file-watches
    :documentation "Map ID to list of WATCHES for `didChangeWatchedFiles'."
    :initform (make-hash-table :test #'equal) :accessor eglot--file-watches)
   (managed-buffers
    :documentation "List of buffers managed by server."
    :accessor eglot--managed-buffers)
   (saved-initargs
    :documentation "Saved initargs for reconnection purposes."
    :accessor eglot--saved-initargs)
   (inferior-process
    :documentation "Server subprocess started automatically."
    :accessor eglot--inferior-process))
  :documentation
  "Represents a server. Wraps a process for LSP communication.")


;;; Process management
(defvar eglot--servers-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(defun eglot-shutdown (server &optional _interactive timeout preserve-buffers)
  "Politely ask SERVER to quit.
Interactively, read SERVER from the minibuffer unless there is
only one and it's managing the current buffer.

Forcefully quit it if it doesn't respond within TIMEOUT seconds.
Don't leave this function with the server still running.

If PRESERVE-BUFFERS is non-nil (interactively, when called with a
prefix argument), do not kill events and output buffers of
SERVER.  ."
  (interactive (list (eglot--read-server "Shutdown which server"
                                         (eglot--current-server))
                     t nil current-prefix-arg))
  (eglot--message "Asking %s politely to terminate" (jsonrpc-name server))
  (unwind-protect
      (progn
        (setf (eglot--shutdown-requested server) t)
        (jsonrpc-request server :shutdown nil :timeout (or timeout 1.5))
        ;; this one is supposed to always fail, because it asks the
        ;; server to exit itself. Hence ignore-errors.
        (ignore-errors (jsonrpc-request server :exit nil :timeout 1)))
    ;; Turn off `eglot--managed-mode' where appropriate.
    (dolist (buffer (eglot--managed-buffers server))
      (eglot--with-live-buffer buffer (eglot--managed-mode-onoff server nil)))
    ;; Now ask jsonrpc.el to shut down the server (which under normal
    ;; conditions should return immediately).
    (jsonrpc-shutdown server (not preserve-buffers))
    (unless preserve-buffers (kill-buffer (jsonrpc-events-buffer server)))))

(defun eglot--on-shutdown (server)
  "Called by jsonrpc.el when SERVER is already dead."
  ;; Turn off `eglot--managed-mode' where appropriate.
  (dolist (buffer (eglot--managed-buffers server))
    (eglot--with-live-buffer buffer (eglot--managed-mode-onoff server nil)))
  ;; Kill any expensive watches
  (maphash (lambda (_id watches)
             (mapcar #'file-notify-rm-watch watches))
           (eglot--file-watches server))
  ;; Kill any autostarted inferior processes
  (when-let (proc (eglot--inferior-process server))
    (delete-process proc))
  ;; Sever the project/server relationship for `server'
  (setf (gethash (eglot--project server) eglot--servers-by-project)
        (delq server
              (gethash (eglot--project server) eglot--servers-by-project)))
  (cond ((eglot--shutdown-requested server)
         t)
        ((not (eglot--inhibit-autoreconnect server))
         (eglot--warn "Reconnecting after unexpected server exit.")
         (eglot-reconnect server))
        ((timerp (eglot--inhibit-autoreconnect server))
         (eglot--warn "Not auto-reconnecting, last one didn't last long."))))

(defun eglot--all-major-modes ()
  "Return all known major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defvar eglot--command-history nil
  "History of CONTACT arguments to `eglot'.")

(defun eglot--guess-contact (&optional interactive)
  "Helper for `eglot'.
Return (MANAGED-MODE PROJECT CLASS CONTACT).  If INTERACTIVE is
non-nil, maybe prompt user, else error as soon as something can't
be guessed."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (managed-mode
          (cond
           ((and interactive
                 (or (>= (prefix-numeric-value current-prefix-arg) 16)
                     (not guessed-mode)))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              (symbol-name guessed-mode) nil (symbol-name guessed-mode) nil)))
           ((not guessed-mode)
            (eglot--error "Can't guess mode to manage for `%s'" (current-buffer)))
           (t guessed-mode)))
         (project (or (project-current) `(transient . ,default-directory)))
         (guess (cdr (assoc managed-mode eglot-server-programs
                            (lambda (m1 m2)
                              (or (eq m1 m2)
                                  (and (listp m1) (memq m2 m1)))))))
         (guess (if (functionp guess)
                    (funcall guess interactive)
                  guess))
         (class (or (and (consp guess) (symbolp (car guess))
                         (prog1 (car guess) (setq guess (cdr guess))))
                    'eglot-lsp-server))
         (program (and (listp guess) (stringp (car guess)) (car guess)))
         (base-prompt
          (and interactive
               "Enter program to execute (or <host>:<port>): "))
         (program-guess
          (and program
               (combine-and-quote-strings (cl-subst ":autoport:"
                                                    :autoport guess))))
         (prompt
          (and base-prompt
               (cond (current-prefix-arg base-prompt)
                     ((null guess)
                      (format "[eglot] Sorry, couldn't guess for `%s'!\n%s"
                              managed-mode base-prompt))
                     ((and program (not (executable-find program)))
                      (concat (format "[eglot] I guess you want to run `%s'"
                                      program-guess)
                              (format ", but I can't find `%s' in PATH!" program)
                              "\n" base-prompt)))))
         (contact
          (or (and prompt
                   (let ((s (read-shell-command
                             prompt
                             program-guess
                             'eglot-command-history)))
                     (if (string-match "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                       (string-trim s))
                         (list (match-string 1 s)
                               (string-to-number (match-string 2 s)))
                       (cl-subst
                        :autoport ":autoport:" (split-string-and-unquote s)
                        :test #'equal))))
              guess
              (eglot--error "Couldn't guess for `%s'!" managed-mode))))
    (list managed-mode project class contact)))

;;;###autoload
(defun eglot (managed-major-mode project class contact &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server of CLASS started (or contacted) via CONTACT.  If
this operation is successful, current *and future* file buffers
of MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CLASS and CONTACT from
`eglot-server-programs' and PROJECT from `project-current'.  If
it can't guess, the user is prompted.  With a single
\\[universal-argument] prefix arg, it always prompt for COMMAND.
With two \\[universal-argument] prefix args, also prompts for
MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CLASS is a subclass of symbol `eglot-lsp-server'.

CONTACT specifies how to contact the server.  It is a
keyword-value plist used to initialize CLASS or a plain list as
described in `eglot-server-programs', which see.

INTERACTIVE is t if called interactively."
  (interactive (append (eglot--guess-contact t) '(t)))
  (let* ((current-server (eglot--current-server))
         (live-p (and current-server (jsonrpc-running-p current-server))))
    (if (and live-p
             interactive
             (y-or-n-p "[eglot] Live process found, reconnect instead? "))
        (eglot-reconnect current-server interactive)
      (when live-p (ignore-errors (eglot-shutdown current-server)))
      (eglot--connect managed-major-mode project class contact))))

(defun eglot-reconnect (server &optional interactive)
  "Reconnect to SERVER.
INTERACTIVE is t if called interactively."
  (interactive (list (eglot--current-server-or-lose) t))
  (when (jsonrpc-running-p server)
    (ignore-errors (eglot-shutdown server interactive nil 'preserve-buffers)))
  (eglot--connect (eglot--major-mode server)
                  (eglot--project server)
                  (eieio-object-class-name server)
                  (eglot--saved-initargs server))
  (eglot--message "Reconnected!"))

(defvar eglot--managed-mode) ; forward decl

;;;###autoload
(defun eglot-ensure ()
  "Start Eglot session for current buffer if there isn't one."
  (let ((buffer (current-buffer)))
    (cl-labels
        ((maybe-connect
          ()
          (remove-hook 'post-command-hook #'maybe-connect nil)
          (eglot--with-live-buffer buffer
            (unless eglot--managed-mode
              (apply #'eglot--connect (eglot--guess-contact))))))
      (when buffer-file-name
        (add-hook 'post-command-hook #'maybe-connect 'append nil)))))

(defun eglot-events-buffer (server)
  "Display events buffer for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (display-buffer (jsonrpc-events-buffer server)))

(defun eglot-stderr-buffer (server)
  "Display stderr buffer for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (display-buffer (jsonrpc-stderr-buffer server)))

(defun eglot-forget-pending-continuations (server)
  "Forget pending requests for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-forget-pending-continuations server))

(defvar eglot-connect-hook nil "Hook run after connecting in `eglot--connect'.")

(defvar eglot-server-initialized-hook
  '(eglot-signal-didChangeConfiguration)
  "Hook run after server is successfully initialized.
Each function is passed the server as an argument")

(defun eglot--connect (managed-major-mode project class contact)
  "Connect to MANAGED-MAJOR-MODE, PROJECT, CLASS and CONTACT.
This docstring appeases checkdoc, that's all."
  (let* ((default-directory (car (project-roots project)))
         (nickname (file-name-base (directory-file-name default-directory)))
         (readable-name (format "EGLOT (%s/%s)" nickname managed-major-mode))
         autostart-inferior-process
         (contact (if (functionp contact) (funcall contact) contact))
         (initargs
          (cond ((keywordp (car contact)) contact)
                ((integerp (cadr contact))
                 `(:process ,(lambda ()
                               (apply #'open-network-stream
                                      readable-name nil
                                      (car contact) (cadr contact)
                                      (cddr contact)))))
                ((and (stringp (car contact)) (memq :autoport contact))
                 `(:process ,(lambda ()
                               (pcase-let ((`(,connection . ,inferior)
                                            (eglot--inferior-bootstrap
                                             readable-name
                                             contact)))
                                 (setq autostart-inferior-process inferior)
                                 connection))))
                ((stringp (car contact))
                 `(:process ,(lambda ()
                               (make-process
                                :name readable-name
                                :command contact
                                :connection-type 'pipe
                                :coding 'utf-8-emacs-unix
                                :noquery t
                                :stderr (get-buffer-create
                                         (format "*%s stderr*" readable-name))))))))
         (spread (lambda (fn) (lambda (server method params)
                                (apply fn server method (append params nil)))))
         (server
          (apply
           #'make-instance class
           :name readable-name
           :events-buffer-scrollback-size eglot-events-buffer-size
           :notification-dispatcher (funcall spread #'eglot-handle-notification)
           :request-dispatcher (funcall spread #'eglot-handle-request)
           :on-shutdown #'eglot--on-shutdown
           initargs))
         (cancelled nil)
         (tag (make-symbol "connected-catch-tag")))
    (setf (eglot--saved-initargs server) initargs)
    (setf (eglot--project server) project)
    (setf (eglot--project-nickname server) nickname)
    (setf (eglot--major-mode server) managed-major-mode)
    (setf (eglot--inferior-process server) autostart-inferior-process)
    ;; Now start the handshake.  To honour `eglot-sync-connect'
    ;; maybe-sync-maybe-async semantics we use `jsonrpc-async-request'
    ;; and mimic most of `jsonrpc-request'.
    (unwind-protect
        (condition-case _quit
            (let ((retval
                   (catch tag
                     (jsonrpc-async-request
                      server
                      :initialize
                      (list :processId (unless (eq (jsonrpc-process-type server)
                                                   'network)
                                         (emacs-pid))
                            :rootPath (expand-file-name default-directory)
                            :rootUri (eglot--path-to-uri default-directory)
                            :initializationOptions (eglot-initialization-options
                                                    server)
                            :capabilities (eglot-client-capabilities server))
                      :success-fn
                      (eglot--lambda ((InitializeResult) capabilities)
                        (unless cancelled
                          (push server
                                (gethash project eglot--servers-by-project))
                          (setf (eglot--capabilities server) capabilities)
                          (jsonrpc-notify server :initialized `(:__dummy__ t))
                          (dolist (buffer (buffer-list))
                            (with-current-buffer buffer
                              (eglot--maybe-activate-editing-mode server)))
                          (setf (eglot--inhibit-autoreconnect server)
                                (cond
                                 ((booleanp eglot-autoreconnect)
                                  (not eglot-autoreconnect))
                                 ((cl-plusp eglot-autoreconnect)
                                  (run-with-timer
                                   eglot-autoreconnect nil
                                   (lambda ()
                                     (setf (eglot--inhibit-autoreconnect server)
                                           (null eglot-autoreconnect)))))))
                          (let ((default-directory (car (project-roots project)))
                                (major-mode managed-major-mode))
                            (hack-dir-local-variables-non-file-buffer)
                            (run-hook-with-args 'eglot-connect-hook server)
                            (run-hook-with-args 'eglot-server-initialized-hook server))
                          (eglot--message
                           "Connected! Server `%s' now managing `%s' buffers \
in project `%s'."
                           (jsonrpc-name server) managed-major-mode
                           (eglot--project-nickname server))
                          (when tag (throw tag t))))
                      :timeout eglot-connect-timeout
                      :error-fn (eglot--lambda ((ResponseError) code message)
                                  (unless cancelled
                                    (jsonrpc-shutdown server)
                                    (let ((msg (format "%s: %s" code message)))
                                      (if tag (throw tag `(error . ,msg))
                                        (eglot--error msg)))))
                      :timeout-fn (lambda ()
                                    (unless cancelled
                                      (jsonrpc-shutdown server)
                                      (let ((msg (format "Timed out")))
                                        (if tag (throw tag `(error . ,msg))
                                          (eglot--error msg))))))
                     (cond ((numberp eglot-sync-connect)
                            (accept-process-output nil eglot-sync-connect))
                           (eglot-sync-connect
                            (while t (accept-process-output nil 30)))))))
              (pcase retval
                (`(error . ,msg) (eglot--error msg))
                (`nil (eglot--message "Waiting in background for server `%s'"
                                      (jsonrpc-name server))
                      nil)
                (_ server)))
          (quit (jsonrpc-shutdown server) (setq cancelled 'quit)))
      (setq tag nil))))

(defun eglot--inferior-bootstrap (name contact &optional connect-args)
  "Use CONTACT to start a server, then connect to it.
Return a cons of two process objects (CONNECTION . INFERIOR).
Name both based on NAME.
CONNECT-ARGS are passed as additional arguments to
`open-network-stream'."
  (let* ((port-probe (make-network-process :name "eglot-port-probe-dummy"
                                           :server t
                                           :host "localhost"
                                           :service 0))
         (port-number (unwind-protect
                          (process-contact port-probe :service)
                        (delete-process port-probe)))
         inferior connection)
    (unwind-protect
        (progn
          (setq inferior
                (make-process
                 :name (format "autostart-inferior-%s" name)
                 :stderr (format "*%s stderr*" name)
                 :noquery t
                 :command (cl-subst
                           (format "%s" port-number) :autoport contact)))
          (setq connection
                (cl-loop
                 repeat 10 for i from 1
                 do (accept-process-output nil 0.5)
                 while (process-live-p inferior)
                 do (eglot--message
                     "Trying to connect to localhost and port %s (attempt %s)"
                     port-number i)
                 thereis (ignore-errors
                           (apply #'open-network-stream
                                  (format "autoconnect-%s" name)
                                  nil
                                  "localhost" port-number connect-args))))
          (cons connection inferior))
      (cond ((and (process-live-p connection)
                  (process-live-p inferior))
             (eglot--message "Done, connected to %s!" port-number))
            (t
             (when inferior (delete-process inferior))
             (when connection (delete-process connection))
             (eglot--error "Could not start and connect to server%s"
                           (if inferior
                               (format " started with %s"
                                       (process-command inferior))
                             "!")))))))


;;; Helpers (move these to API?)
;;;
(defun eglot--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[eglot] %s" (apply #'format format args)))

(defun eglot--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[eglot] %s" (apply #'format format args)))

(defun eglot--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'eglot--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'eglot (apply #'format format args) :warning)))

(defun eglot-current-column () (- (point) (point-at-bol)))

(defvar eglot-current-column-function #'eglot-current-column
  "Function to calculate the current column.

This is the inverse operation of
`eglot-move-to-column-function' (which see).  It is a function of
no arguments returning a column number.  For buffers managed by
fully LSP-compliant servers, this should be set to
`eglot-lsp-abiding-column', and `eglot-current-column' (the default)
for all others.")

(defun eglot-lsp-abiding-column ()
  "Calculate current COLUMN as defined by the LSP spec."
  (/ (- (length (encode-coding-region (line-beginning-position)
                                      (point) 'utf-16 t))
        2)
     2))

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (eglot--widening
   (list :line (1- (line-number-at-pos pos t)) ; F!@&#$CKING OFF-BY-ONE
         :character (progn (when pos (goto-char pos))
                           (funcall eglot-current-column-function)))))

(defvar eglot-move-to-column-function #'move-to-column
  "Function to move to a column reported by the LSP server.

According to the standard, LSP column/character offsets are based
on a count of UTF-16 code units, not actual visual columns.  So
when LSP says position 3 of a line containing just \"aXbc\",
where X is a multi-byte character, it actually means `b', not
`c'. However, many servers don't follow the spec this closely.

For buffers managed by fully LSP-compliant servers, this should
be set to `eglot-move-to-lsp-abiding-column', and
`move-to-column' (the default) for all others.")

(defun eglot-move-to-lsp-abiding-column (column)
  "Move to COLUMN abiding by the LSP spec."
  (cl-loop
   initially (move-to-column column)
   with lbp = (line-beginning-position)
   for diff = (- column
                 (/ (- (length (encode-coding-region lbp (point) 'utf-16 t))
                       2)
                    2))
   until (zerop diff)
   do (forward-char (/ (if (> diff 0) (1+ diff) (1- diff)) 2))))

(defun eglot--lsp-position-to-point (pos-plist &optional marker)
  "Convert LSP position POS-PLIST to Emacs point.
If optional MARKER, return a marker instead"
  (save-excursion
    (goto-char (point-min))
    (forward-line (min most-positive-fixnum
                       (plist-get pos-plist :line)))
    (unless (eobp) ;; if line was excessive leave point at eob
      (let ((tab-width 1))
        (funcall eglot-move-to-column-function (plist-get pos-plist :character))))
    (if marker (copy-marker (point-marker)) (point))))

(defun eglot--path-to-uri (path)
  "URIfy PATH."
  (url-hexify-string
   (concat "file://" (if (eq system-type 'windows-nt) "/") (file-truename path))
   url-path-allowed-chars))

(defun eglot--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-filename (url-generic-parse-url (url-unhex-string uri)))))
    (if (eq system-type 'windows-nt) (substring retval 1) retval)))

(defun eglot--snippet-expansion-fn ()
  "Compute a function to expand snippets.
Doubles as an indicator of snippet support."
  (and (boundp 'yas-minor-mode)
       (symbol-value 'yas-minor-mode)
       'yas-expand-snippet))

(defun eglot--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list (string-trim markup)
                                          (intern "gfm-view-mode"))
                 (list (plist-get markup :value)
                       major-mode))))
    (with-temp-buffer
      (insert string)
      (ignore-errors (funcall mode)) (font-lock-ensure) (buffer-string))))

(defcustom eglot-ignored-server-capabilites (list)
  "LSP server capabilities that Eglot could use, but won't.
You could add, for instance, the symbol
`:documentHighlightProvider' to prevent automatic highlighting
under cursor."
  :type '(repeat
          (choice
           (const :tag "Documentation on hover" :hoverProvider)
           (const :tag "Code completion" :completionProvider)
           (const :tag "Function signature help" :signatureHelpProvider)
           (const :tag "Go to definition" :definitionProvider)
           (const :tag "Go to type definition" :typeDefinitionProvider)
           (const :tag "Go to implementation" :implementationProvider)
           (const :tag "Find references" :referencesProvider)
           (const :tag "Highlight symbols automatically" :documentHighlightProvider)
           (const :tag "List symbols in buffer" :documentSymbolProvider)
           (const :tag "List symbols in workspace" :workspaceSymbolProvider)
           (const :tag "Execute code actions" :codeActionProvider)
           (const :tag "Code lens" :codeLensProvider)
           (const :tag "Format buffer" :documentFormattingProvider)
           (const :tag "Format portion of buffer" :documentRangeFormattingProvider)
           (const :tag "On-type formatting" :documentOnTypeFormattingProvider)
           (const :tag "Rename symbol" :renameProvider)
           (const :tag "Highlight links in document" :documentLinkProvider)
           (const :tag "Decorate color references" :colorProvider)
           (const :tag "Fold regions of buffer" :foldingRangeProvider)
           (const :tag "Execute custom commands" :executeCommandProvider)
           (symbol :tag "Other"))))

(defun eglot--server-capable (&rest feats)
  "Determine if current server is capable of FEATS."
  (unless (cl-some (lambda (feat)
                     (memq feat eglot-ignored-server-capabilites))
                   feats)
    (cl-loop for caps = (eglot--capabilities (eglot--current-server-or-lose))
             then (cadr probe)
             for (feat . more) on feats
             for probe = (plist-member caps feat)
             if (not probe) do (cl-return nil)
             if (eq (cadr probe) :json-false) do (cl-return nil)
             if (not (listp (cadr probe))) do (cl-return (if more nil (cadr probe)))
             finally (cl-return (or (cadr probe) t)))))

(defun eglot--range-region (range &optional markers)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let* ((st (plist-get range :start))
         (beg (eglot--lsp-position-to-point st markers))
         (end (eglot--lsp-position-to-point (plist-get range :end) markers)))
    (cons beg end)))

(defun eglot--read-server (prompt &optional dont-if-just-the-one)
  "Read a running Eglot server from minibuffer using PROMPT.
If DONT-IF-JUST-THE-ONE and there's only one server, don't prompt
and just return it.  PROMPT shouldn't end with a question mark."
  (let ((servers (cl-loop for servers
                          being hash-values of eglot--servers-by-project
                          append servers))
        (name (lambda (srv)
                (format "%s/%s" (eglot--project-nickname srv)
                        (eglot--major-mode srv)))))
    (cond ((null servers)
           (eglot--error "No servers!"))
          ((or (cdr servers) (not dont-if-just-the-one))
           (let* ((default (when-let ((current (eglot--current-server)))
                             (funcall name current)))
                  (read (completing-read
                         (if default
                             (format "%s (default %s)? " prompt default)
                           (concat prompt "? "))
                         (mapcar name servers)
                         nil t
                         nil nil
                         default)))
             (cl-find read servers :key name :test #'equal)))
          (t (car servers)))))


;;; Minor modes
;;;
(defvar eglot-mode-map (make-sparse-keymap))

(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer")

(defvar-local eglot--saved-bindings nil
  "Bindings saved by `eglot--setq-saving'.")

(defmacro eglot--setq-saving (symbol binding)
  `(progn (push (cons ',symbol (symbol-value ',symbol))
                eglot--saved-bindings)
          (setq-local ,symbol ,binding)))

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some EGLOT project."
  nil nil eglot-mode-map
  (cond
   (eglot--managed-mode
    (add-hook 'after-change-functions 'eglot--after-change nil t)
    (add-hook 'before-change-functions 'eglot--before-change nil t)
    (add-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'kill-buffer-hook 'eglot--managed-mode-onoff nil t)
    (add-hook 'before-revert-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-save-hook 'eglot--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook 'eglot--signal-textDocument/didSave nil t)
    (add-hook 'xref-backend-functions 'eglot-xref-backend nil t)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (add-hook 'change-major-mode-hook 'eglot--managed-mode-onoff nil t)
    (add-hook 'post-self-insert-hook 'eglot--post-self-insert-hook nil t)
    (add-hook 'pre-command-hook 'eglot--pre-command-hook nil t)
    (eglot--setq-saving eldoc-documentation-function #'eglot-eldoc-function)
    (eglot--setq-saving flymake-diagnostic-functions '(eglot-flymake-backend t))
    (add-function :before-until (local 'eldoc-message-function) #'eglot--eldoc-message)
    (add-function :around (local 'imenu-create-index-function) #'eglot-imenu)
    (flymake-mode 1)
    (eldoc-mode 1))
   (t
    (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
    (remove-hook 'after-change-functions 'eglot--after-change t)
    (remove-hook 'before-change-functions 'eglot--before-change t)
    (remove-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-save-hook 'eglot--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook 'eglot--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions 'eglot-xref-backend t)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-hook 'change-major-mode-hook #'eglot--managed-mode-onoff t)
    (remove-hook 'post-self-insert-hook 'eglot--post-self-insert-hook t)
    (remove-hook 'pre-command-hook 'eglot--pre-command-hook t)
    (remove-function (local 'eldoc-message-function) #'eglot--eldoc-message)
    (cl-loop for (var . saved-binding) in eglot--saved-bindings
             do (set (make-local-variable var) saved-binding))
    (setq eglot--current-flymake-report-fn nil))))

(defvar-local eglot--cached-current-server nil
  "A cached reference to the current EGLOT server.
Reset in `eglot--managed-mode-onoff'.")

(defun eglot--managed-mode-onoff (&optional server turn-on)
  "Proxy for function `eglot--managed-mode' with TURN-ON and SERVER."
  (let ((buf (current-buffer)))
    (cond ((and server turn-on)
           (eglot--managed-mode 1)
           (setq eglot--cached-current-server server)
           (cl-pushnew buf (eglot--managed-buffers server)))
          (t
           (eglot--managed-mode -1)
           (let ((server
                  (or server
                      eglot--cached-current-server)))
             (setq eglot--cached-current-server nil)
             (when server
               (setf (eglot--managed-buffers server)
                     (delq buf (eglot--managed-buffers server)))))))))

(defun eglot--current-server ()
  "Find the current logical EGLOT server."
  (or
   eglot--cached-current-server
   (let* ((probe (or (project-current)
                     `(transient . ,default-directory))))
     (cl-find major-mode (gethash probe eglot--servers-by-project)
              :key #'eglot--major-mode))))

(defun eglot--current-server-or-lose ()
  "Return current logical EGLOT server connection or error."
  (or (eglot--current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(defvar-local eglot--unreported-diagnostics nil
  "Unreported Flymake diagnostics for this buffer.")

(defun eglot--maybe-activate-editing-mode (&optional server)
  "Maybe activate mode function `eglot--managed-mode'.
If SERVER is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  (unless eglot--managed-mode
    (unless server
      (when eglot--cached-current-server
        (display-warning
         :eglot "`eglot--cached-current-server' is non-nil, but it shouldn't be!\n\
Please report this as a possible bug.")
        (setq eglot--cached-current-server nil)))
    ;; Called even when revert-buffer-in-progress-p
    (let* ((cur (and buffer-file-name (eglot--current-server)))
           (server (or (and (null server) cur) (and server (eq server cur) cur))))
      (when server
        (setq eglot--unreported-diagnostics `(:just-opened . nil))
        (eglot--managed-mode-onoff server t)
        (eglot--signal-textDocument/didOpen)))))


(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)
(add-hook 'after-change-major-mode-hook 'eglot--maybe-activate-editing-mode)

(defun eglot-clear-status (server)
  "Clear the last JSONRPC error for SERVER."
  (interactive (list (eglot--current-server-or-lose)))
  (setf (jsonrpc-last-error server) nil))


;;; Mode-line, menu and other sugar
;;;
(defvar eglot--mode-line-format `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mouse-call (what)
  "Make an interactive lambda for calling WHAT from mode-line."
  (lambda (event)
    (interactive "e")
    (let ((start (event-start event))) (with-selected-window (posn-window start)
                                         (save-excursion
                                           (goto-char (or (posn-point start)
                                                          (point)))
                                           (call-interactively what)
                                           (force-mode-line-update t))))))

(defun eglot--mode-line-props (thing face defs &optional prepend)
  "Helper for function `eglot--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (eglot--mouse-call def))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                                         face ,face
                                         keymap ,map help-echo ,(concat prepend blurb)
                                         mouse-face mode-line-highlight))))

(defun eglot--mode-line-format ()
  "Compose the EGLOT's mode-line."
  (pcase-let* ((server (eglot--current-server))
               (nick (and server (eglot--project-nickname server)))
               (pending (and server (hash-table-count
                                     (jsonrpc--request-continuations server))))
               (`(,_id ,doing ,done-p ,detail) (and server (eglot--spinner server)))
               (last-error (and server (jsonrpc-last-error server))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when nick
       `(":" ,(eglot--mode-line-props
               nick 'eglot-mode-line
               '((C-mouse-1 eglot-stderr-buffer "go to stderr buffer")
                 (mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when last-error
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-3 eglot-clear-status  "clear this status"))
                     (format "An error occured: %s\n" (plist-get last-error
                                                                 :message)))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     (format "%s%s" doing
                             (if detail (format ":%s" detail) ""))
                     'compilation-mode-line-run '())))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d outstanding requests" pending) 'warning
                     '((mouse-3 eglot-forget-pending-continuations
                                "fahgettaboudit"))))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))

(put 'eglot-note 'flymake-category 'flymake-note)
(put 'eglot-warning 'flymake-category 'flymake-warning)
(put 'eglot-error 'flymake-category 'flymake-error)

(defalias 'eglot--make-diag 'flymake-make-diagnostic)
(defalias 'eglot--diag-data 'flymake-diagnostic-data)

(cl-loop for i from 1
         for type in '(eglot-note eglot-warning eglot-error )
         do (put type 'flymake-overlay-control
                 `((mouse-face . highlight)
                   (priority . ,(+ 50 i))
                   (keymap . ,(let ((map (make-sparse-keymap)))
                                (define-key map [mouse-1]
                                            (eglot--mouse-call 'eglot-code-actions))
                                map)))))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(cl-defmethod eglot-handle-notification
  (_server method &key &allow-other-keys)
  "Handle unknown notification"
  (unless (or (string-prefix-p "$" (format "%s" method))
              (not (memq 'disallow-unknown-methods eglot-strict-mode)))
    (eglot--warn "Server sent unknown notification method `%s'" method)))

(cl-defmethod eglot-handle-request
  (_server method &key &allow-other-keys)
  "Handle unknown request"
  (when (memq 'disallow-unknown-methods eglot-strict-mode)
    (jsonrpc-error "Unknown request method `%s'" method)))

(cl-defmethod eglot-execute-command
  (server command arguments)
  "Execute COMMAND on SERVER with `:workspace/executeCommand'.
COMMAND is a symbol naming the command."
  (jsonrpc-request server :workspace/executeCommand
                   `(:command ,(format "%s" command) :arguments ,arguments)))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql window/showMessage)) &key type message)
  "Handle notification window/showMessage"
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defmethod eglot-handle-request
  (_server (_method (eql window/showMessageRequest)) &key type message actions)
  "Handle server request window/showMessageRequest"
  (or (completing-read
       (concat
        (format (propertize "[eglot] Server reports (type=%s): %s"
                            'face (if (<= type 1) 'error))
                type message)
        "\nChoose an option: ")
       (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
           '("OK"))
       nil t (plist-get (elt actions 0) :title))
      (jsonrpc-error :code -32800 :message "User cancelled")))

(cl-defmethod eglot-handle-notification
  (_server (_method (eql window/logMessage)) &key _type _message)
  "Handle notification window/logMessage") ;; noop, use events buffer

(cl-defmethod eglot-handle-notification
  (_server (_method (eql telemetry/event)) &rest _any)
  "Handle notification telemetry/event") ;; noop, use events buffer

(cl-defmethod eglot-handle-notification
  (server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (if-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
      (with-current-buffer buffer
        (cl-loop
         for diag-spec across diagnostics
         collect (eglot--dbind ((Diagnostic) range message severity source)
                     diag-spec
                   (setq message (concat source ": " message))
                   (pcase-let
                       ((sev severity)
                        (`(,beg . ,end) (eglot--range-region range)))
                     ;; Fallback to `flymake-diag-region' if server
                     ;; botched the range
                     (when (= beg end)
                       (if-let* ((st (plist-get range :start))
                                 (diag-region
                                  (flymake-diag-region
                                   (current-buffer) (1+ (plist-get st :line))
                                   (plist-get st :character))))
                           (setq beg (car diag-region) end (cdr diag-region))
                         (eglot--widening
                          (goto-char (point-min))
                          (setq beg
                                (point-at-bol
                                 (1+ (plist-get (plist-get range :start) :line))))
                          (setq end
                                (point-at-eol
                                 (1+ (plist-get (plist-get range :end) :line)))))))
                     (eglot--make-diag (current-buffer) beg end
                                       (cond ((<= sev 1) 'eglot-error)
                                             ((= sev 2)  'eglot-warning)
                                             (t          'eglot-note))
                                       message `((eglot-lsp-diag . ,diag-spec)))))
         into diags
         finally (cond (eglot--current-flymake-report-fn
                        (funcall eglot--current-flymake-report-fn diags
                                 ;; If the buffer hasn't changed since last
                                 ;; call to the report function, flymake won't
                                 ;; delete old diagnostics.  Using :region
                                 ;; keyword forces flymake to delete
                                 ;; them (github#159).
                                 :region (cons (point-min) (point-max)))
                        (setq eglot--unreported-diagnostics nil))
                       (t
                        (setq eglot--unreported-diagnostics (cons t diags))))))
    (jsonrpc--debug server "Diagnostics received for unvisited %s" uri)))

(cl-defun eglot--register-unregister (server things how)
  "Helper for `registerCapability'.
THINGS are either registrations or unregisterations (sic)."
  (cl-loop
   for thing in (cl-coerce things 'list)
   do (eglot--dbind ((Registration) id method registerOptions) thing
        (apply (cl-ecase how
                 (register 'eglot-register-capability)
                 (unregister 'eglot-unregister-capability))
               server (intern method) id registerOptions))))

(cl-defmethod eglot-handle-request
  (server (_method (eql client/registerCapability)) &key registrations)
  "Handle server request client/registerCapability"
  (eglot--register-unregister server registrations 'register))

(cl-defmethod eglot-handle-request
  (server (_method (eql client/unregisterCapability))
          &key unregisterations) ;; XXX: "unregisterations" (sic)
  "Handle server request client/unregisterCapability"
  (eglot--register-unregister server unregisterations 'unregister))

(cl-defmethod eglot-handle-request
  (_server (_method (eql workspace/applyEdit)) &key _label edit)
  "Handle server request workspace/applyEdit"
  (eglot--apply-workspace-edit edit 'confirm))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  `(:uri ,(eglot--path-to-uri (or buffer-file-name
                                  (ignore-errors
                                    (buffer-file-name
                                     (buffer-base-buffer)))))))

(defvar-local eglot--versioned-identifier 0)

(defun eglot--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--TextDocumentIdentifier)
          `(:version ,eglot--versioned-identifier)))

(defun eglot--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--VersionedTextDocumentIdentifier)
   (list :languageId
         (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
             (match-string 1 (symbol-name major-mode))
           "unknown")
         :text
         (eglot--widening
          (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (list :textDocument (eglot--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position)))

(defvar-local eglot--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun eglot--post-self-insert-hook ()
  "Set `eglot--last-inserted-char'."
  (setq eglot--last-inserted-char last-input-event))

(defun eglot--pre-command-hook ()
  "Reset `eglot--last-inserted-char'."
  (setq eglot--last-inserted-char nil))

(defun eglot--CompletionParams ()
  (append
   (eglot--TextDocumentPositionParams)
   `(:context
     ,(if-let (trigger (and (characterp eglot--last-inserted-char)
                            (cl-find eglot--last-inserted-char
                                     (eglot--server-capable :completionProvider
                                                            :triggerCharacters)
                                     :key (lambda (str) (aref str 0))
                                     :test #'char-equal)))
          `(:triggerKind 2 :triggerCharacter ,trigger) `(:triggerKind 1)))))

(defvar-local eglot--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(cl-defmethod jsonrpc-connection-ready-p ((_server eglot-lsp-server) _what)
  "Tell if SERVER is ready for WHAT in current buffer."
  (and (cl-call-next-method) (not eglot--recent-changes)))

(defvar-local eglot--change-idle-timer nil "Idle timer for didChange signals.")

(defun eglot--before-change (start end)
  "Hook onto `before-change-functions' with START and END."
  ;; Records START and END, crucially convert them into LSP
  ;; (line/char) positions before that information is lost (because
  ;; the after-change thingy doesn't know if newlines were
  ;; deleted/added)
  (when (listp eglot--recent-changes)
    (push `(,(eglot--pos-to-lsp-position start)
            ,(eglot--pos-to-lsp-position end))
          eglot--recent-changes)))

(defun eglot--after-change (start end pre-change-length)
  "Hook onto `after-change-functions'.
Records START, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (if (and (listp eglot--recent-changes)
           (null (cddr (car eglot--recent-changes))))
      (setf (cddr (car eglot--recent-changes))
            `(,pre-change-length ,(buffer-substring-no-properties start end)))
    (setf eglot--recent-changes :emacs-messup))
  (when eglot--change-idle-timer (cancel-timer eglot--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq eglot--change-idle-timer
          (run-with-idle-timer
           0.5 nil (lambda () (eglot--with-live-buffer buf
                                (when eglot--managed-mode
                                  (eglot--signal-textDocument/didChange)
                                  (setq eglot--change-idle-timer nil))))))))

;; HACK! Launching a deferred sync request with outstanding changes is a
;; bad idea, since that might lead to the request never having a
;; chance to run, because `jsonrpc-connection-ready-p'.
(advice-add #'jsonrpc-request :before
            (cl-function (lambda (_proc _method _params &key
                                        deferred &allow-other-keys)
                           (when (and eglot--managed-mode deferred)
                             (eglot--signal-textDocument/didChange))))
            '((name . eglot--signal-textDocument/didChange)))

(defvar-local eglot-workspace-configuration ()
  "Alist of (SETTING . VALUE) entries configuring the LSP server.
Setting should be a keyword, value can be any value that can be
converted to JSON.")

(put 'eglot-workspace-configuration 'safe-local-variable 'listp)

(defun eglot-signal-didChangeConfiguration (server)
  "Send a `:workspace/didChangeConfiguration' signal to SERVER.
When called interactively, use the currently active server"
  (interactive (list (eglot--current-server-or-lose)))
  (jsonrpc-notify
   server :workspace/didChangeConfiguration
   (list
    :settings
    (cl-loop for (k . v) in eglot-workspace-configuration
             collect (if (keywordp k)
                         k
                       (intern (format ":%s" k)))
             collect v))))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when eglot--recent-changes
    (let* ((server (eglot--current-server-or-lose))
           (sync-capability (eglot--server-capable :textDocumentSync))
           (sync-kind (if (numberp sync-capability) sync-capability
                        (plist-get sync-capability :change)))
           (full-sync-p (or (eq sync-kind 1)
                            (eq :emacs-messup eglot--recent-changes))))
      (jsonrpc-notify
       server :textDocument/didChange
       (list
        :textDocument (eglot--VersionedTextDocumentIdentifier)
        :contentChanges
        (if full-sync-p
            (vector `(:text ,(eglot--widening
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
          (cl-loop for (beg end len text) in (reverse eglot--recent-changes)
                   vconcat `[,(list :range `(:start ,beg :end ,end)
                                    :rangeLength len :text text)]))))
      (setq eglot--recent-changes nil)
      (setf (eglot--spinner server) (list nil :textDocument/didChange t))
      (jsonrpc--call-deferred server))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes nil eglot--versioned-identifier 0)
  (jsonrpc-notify
   (eglot--current-server-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (with-demoted-errors
      "[eglot] error sending textDocument/didClose: %s"
    (jsonrpc-notify
     (eglot--current-server-or-lose)
     :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier)))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((server (eglot--current-server-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (jsonrpc-notify server :textDocument/willSave params)
    (when (eglot--server-capable :textDocumentSync :willSaveWaitUntil)
      (ignore-errors
        (eglot--apply-text-edits
         (jsonrpc-request server :textDocument/willSaveWaitUntil params
                          :timeout 0.5))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (eglot--signal-textDocument/didChange)
  (jsonrpc-notify
   (eglot--current-server-or-lose)
   :textDocument/didSave
   (list
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "An EGLOT Flymake backend.
Calls REPORT-FN maybe if server publishes diagnostics in time."
  (setq eglot--current-flymake-report-fn report-fn)
  ;; Report anything unreported
  (when eglot--unreported-diagnostics
    (funcall report-fn (cdr eglot--unreported-diagnostics))
    (setq eglot--unreported-diagnostics nil)))

(defun eglot-xref-backend ()
  "EGLOT xref backend."
  (when (eglot--server-capable :definitionProvider) 'eglot))

(defvar eglot--xref-known-symbols nil)

(defun eglot--xref-reset-known-symbols (&rest _dummy)
  "Reset `eglot--xref-reset-known-symbols'.
DUMMY is ignored."
  (setq eglot--xref-known-symbols nil))

(advice-add 'xref-find-definitions :after #'eglot--xref-reset-known-symbols)
(advice-add 'xref-find-references :after #'eglot--xref-reset-known-symbols)

(defvar eglot--temp-location-buffers (make-hash-table :test #'equal)
  "Helper variable for `eglot--handling-xrefs'.")

(defmacro eglot--handling-xrefs (&rest body)
  "Properly sort and handle xrefs produced and returned by BODY."
  `(unwind-protect
       (sort (progn ,@body)
             (lambda (a b)
               (< (xref-location-line (xref-item-location a))
                  (xref-location-line (xref-item-location b)))))
     (maphash (lambda (_uri buf) (kill-buffer buf)) eglot--temp-location-buffers)
     (clrhash eglot--temp-location-buffers)))

(defun eglot--xref-make (name uri range)
  "Like `xref-make' but with LSP's NAME, URI and RANGE.
Try to visit the target file for a richer summary line."
  (pcase-let*
      ((file (eglot--uri-to-path uri))
       (visiting (or (find-buffer-visiting file)
                     (gethash uri eglot--temp-location-buffers)))
       (collect (lambda ()
                  (eglot--widening
                   (pcase-let* ((`(,beg . ,end) (eglot--range-region range))
                                (bol (progn (goto-char beg) (point-at-bol)))
                                (substring (buffer-substring bol (point-at-eol)))
                                (hi-beg (- beg bol))
                                (hi-end (- (min (point-at-eol) end) bol)))
                     (add-face-text-property hi-beg hi-end 'highlight
                                             t substring)
                     (list substring (1+ (current-line)) (eglot-current-column))))))
       (`(,summary ,line ,column)
        (cond
         (visiting (with-current-buffer visiting (funcall collect)))
         ((file-readable-p file) (with-current-buffer
                                     (puthash uri (generate-new-buffer " *temp*")
                                              eglot--temp-location-buffers)
                                   (insert-file-contents file)
                                   (funcall collect)))
         (t ;; fall back to the "dumb strategy"
          (let ((start (cl-getf range :start)))
            (list name (1+ (cl-getf start :line)) (cl-getf start :character)))))))
    (xref-make summary (xref-make-file-location file line column))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  (when (eglot--server-capable :documentSymbolProvider)
    (let ((server (eglot--current-server-or-lose))
          (text-id (eglot--TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (mapcar
                (eglot--lambda
                    ((SymbolInformation) name kind location containerName)
                  (propertize name
                              :textDocumentPositionParams
                              (list :textDocument text-id
                                    :position (plist-get
                                               (plist-get location :range)
                                               :start))
                              :locations (vector location)
                              :kind kind
                              :containerName containerName))
                (jsonrpc-request server
                                 :textDocument/documentSymbol
                                 `(:textDocument ,text-id))))
         (all-completions string eglot--xref-known-symbols))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  (when-let ((symatpt (symbol-at-point)))
    (propertize (symbol-name symatpt)
                :textDocumentPositionParams
                (eglot--TextDocumentPositionParams))))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) identifier)
  (let* ((rich-identifier
          (car (member identifier eglot--xref-known-symbols)))
         (definitions
          (if rich-identifier
              (get-text-property 0 :locations rich-identifier)
            (jsonrpc-request (eglot--current-server-or-lose)
                             :textDocument/definition
                             (get-text-property
                              0 :textDocumentPositionParams identifier))))
         (locations
          (and definitions
               (if (vectorp definitions) definitions (vector definitions)))))
    (eglot--handling-xrefs
     (mapcar (eglot--lambda ((Location) uri range)
               (eglot--xref-make identifier uri range))
             locations))))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) identifier)
  (unless (eglot--server-capable :referencesProvider)
    (cl-return-from xref-backend-references nil))
  (let ((params
         (or (get-text-property 0 :textDocumentPositionParams identifier)
             (let ((rich (car (member identifier eglot--xref-known-symbols))))
               (and rich (get-text-property 0 :textDocumentPositionParams rich))))))
    (unless params
      (eglot--error "Don' know where %s is in the workspace!" identifier))
    (eglot--handling-xrefs
     (mapcar
      (eglot--lambda ((Location) uri range)
        (eglot--xref-make identifier uri range))
      (jsonrpc-request (eglot--current-server-or-lose)
                       :textDocument/references
                       (append
                        params
                        (list :context
                              (list :includeDeclaration t))))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (eglot--handling-xrefs
     (mapcar
      (eglot--lambda ((SymbolInformation) name location)
        (eglot--dbind ((Location) uri range) location
          (eglot--xref-make name uri range)))
      (jsonrpc-request (eglot--current-server-or-lose)
                       :workspace/symbol
                       `(:query ,pattern))))))

(defun eglot-format-buffer ()
  "Format contents of current buffer."
  (interactive)
  (eglot-format nil nil))

(defun eglot-format (&optional beg end)
  "Format region BEG END.
If either BEG or END is nil, format entire buffer.
Interactively, format active region, or entire buffer if region
is not active."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (pcase-let ((`(,method ,cap ,args)
               (cond
                ((and beg end)
                 `(:textDocument/rangeFormatting
                   :documentRangeFormattingProvider
                   (:range ,(list :start (eglot--pos-to-lsp-position beg)
                                  :end (eglot--pos-to-lsp-position end)))))
                (t
                 '(:textDocument/formatting :documentFormattingProvider nil)))))
    (unless (eglot--server-capable cap)
      (eglot--error "Server can't format!"))
    (eglot--apply-text-edits
     (jsonrpc-request
      (eglot--current-server-or-lose)
      method
      (cl-list*
       :textDocument (eglot--TextDocumentIdentifier)
       :options (list :tabSize tab-width
                      :insertSpaces (if indent-tabs-mode :json-false t))
       args)
      :deferred method))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
        (server (eglot--current-server-or-lose))
        (completion-capability (eglot--server-capable :completionProvider))
        (sort-completions (lambda (completions)
                            (sort completions
                                  (lambda (a b)
                                    (string-lessp
                                     (or (get-text-property 0 :sortText a) "")
                                     (or (get-text-property 0 :sortText b) ""))))))
        (metadata `(metadata . ((display-sort-function . ,sort-completions))))
        strings)
    (when completion-capability
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (lambda (string pred action)
         (if (eq action 'metadata) metadata
           (funcall
            (completion-table-dynamic
             (lambda (_ignored)
               (let* ((resp (jsonrpc-request server
                                             :textDocument/completion
                                             (eglot--CompletionParams)
                                             :deferred :textDocument/completion
                                             :cancel-on-input t))
                      (items (if (vectorp resp) resp (plist-get resp :items))))
                 (setq
                  strings
                  (mapcar
                   (jsonrpc-lambda
                       (&rest all &key label insertText insertTextFormat
                              &allow-other-keys)
                     (let ((completion
                            (cond ((and (eql insertTextFormat 2)
                                        (eglot--snippet-expansion-fn))
                                   (string-trim-left label))
                                  (t
                                   (or insertText (string-trim-left label))))))
                       (put-text-property 0 1 'eglot--lsp-completion all completion)
                       completion))
                   items)))))
            string pred action)))
       :annotation-function
       (lambda (obj)
         (eglot--dbind ((CompletionItem) detail kind insertTextFormat)
             (get-text-property 0 'eglot--lsp-completion obj)
           (let* ((detail (and (stringp detail)
                               (not (string= detail ""))
                               detail))
                  (annotation
                   (or detail
                       (cdr (assoc kind eglot--kind-names)))))
             (when annotation
               (concat " "
                       (propertize annotation
                                   'face 'font-lock-function-name-face)
                       (and (eql insertTextFormat 2)
                            (eglot--snippet-expansion-fn)
                            " (snippet)"))))))
       :company-doc-buffer
       (lambda (obj)
         (let* ((documentation
                 (let ((lsp-comp
                        (get-text-property 0 'eglot--lsp-completion obj)))
                   (or (plist-get lsp-comp :documentation)
                       (and (eglot--server-capable :completionProvider
                                                   :resolveProvider)
                            (plist-get
                             (jsonrpc-request server :completionItem/resolve
                                              lsp-comp :cancel-on-input t)
                             :documentation)))))
                (formatted (and documentation
                                (eglot--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))
       :company-prefix-length
       (cl-some #'looking-back
                (mapcar #'regexp-quote
                        (plist-get completion-capability :triggerCharacters)))
       :exit-function
       (lambda (comp _status)
         (let ((comp (if (get-text-property 0 'eglot--lsp-completion comp)
                         comp
                       ;; When selecting from the *Completions*
                       ;; buffer, `comp' won't have any properties.  A
                       ;; lookup should fix that (github#148)
                       (cl-find comp strings :test #'string=))))
           (eglot--dbind ((CompletionItem) insertTextFormat
                          insertText
                          textEdit
                          additionalTextEdits)
               (get-text-property 0 'eglot--lsp-completion comp)
             (let ((snippet-fn (and (eql insertTextFormat 2)
                                    (eglot--snippet-expansion-fn))))
               (cond (textEdit
                      ;; Undo the just the completed bit.  If before
                      ;; completion the buffer was "foo.b" and now is
                      ;; "foo.bar", `comp' will be "bar".  We want to
                      ;; delete only "ar" (`comp' minus the symbol
                      ;; whose bounds we've calculated before)
                      ;; (github#160).
                      (delete-region (+ (- (point) (length comp))
                                        (if bounds (- (cdr bounds) (car bounds)) 0))
                                     (point))
                      (eglot--dbind ((TextEdit) range newText) textEdit
                        (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                          (delete-region beg end)
                          (goto-char beg)
                          (funcall (or snippet-fn #'insert) newText)))
                      (when (cl-plusp (length additionalTextEdits))
                        (eglot--apply-text-edits additionalTextEdits)))
                     (snippet-fn
                      ;; A snippet should be inserted, but using plain
                      ;; `insertText'.  This requires us to delete the
                      ;; whole completion, since `insertText' is the full
                      ;; completion's text.
                      (delete-region (- (point) (length comp)) (point))
                      (funcall snippet-fn insertText))))
             (eglot--signal-textDocument/didChange)
             (eglot-eldoc-function))))))))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--hover-info (contents &optional range)
  (let ((heading (and range (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                              (concat (buffer-substring beg end)  ": "))))
        (body (mapconcat #'eglot--format-markup
                         (if (vectorp contents) contents (list contents)) "\n")))
    (when (or heading (cl-plusp (length body))) (concat heading body))))

(defun eglot--sig-info (sigs active-sig active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat
   (eglot--dbind ((SignatureInformation) label documentation parameters) sig
     (with-temp-buffer
       (save-excursion (insert label))
       (let (params-start params-end)
         ;; Ad-hoc attempt to parse label as <name>(<params>)
         (when (looking-at "\\([^(]+\\)(\\([^)]+\\))")
           (setq params-start (match-beginning 2) params-end (match-end 2))
           (add-face-text-property (match-beginning 1) (match-end 1)
                                   'font-lock-function-name-face))
         (when (eql i active-sig)
           ;; Decide whether to add one-line-summary to signature line
           (when (and (stringp documentation)
                      (string-match "[[:space:]]*\\([^.\r\n]+[.]?\\)"
                                    documentation))
             (setq documentation (match-string 1 documentation))
             (unless (string-prefix-p (string-trim documentation) label)
               (goto-char (point-max))
               (insert ": " (eglot--format-markup documentation))))
           ;; Decide what to do with the active parameter...
           (when (and (eql i active-sig) active-param
                      (< -1 active-param (length parameters)))
             (eglot--dbind ((ParameterInformation) label documentation)
                 (aref parameters active-param)
               ;; ...perhaps highlight it in the formals list
               (when params-start
                 (goto-char params-start)
                 (pcase-let
                     ((`(,beg ,end)
                       (if (stringp label)
                           (let ((case-fold-search nil))
                             (and (re-search-forward
                                   (concat "\\<" (regexp-quote label) "\\>")
                                   params-end t)
                                  (list (match-beginning 0) (match-end 0))))
                         (mapcar #'1+ (append label nil)))))
                   (if (and beg end)
                       (add-face-text-property
                        beg end
                        'eldoc-highlight-function-argument))))
               ;; ...and/or maybe add its doc on a line by its own.
               (when documentation
                 (goto-char (point-max))
                 (insert "\n"
                         (propertize
                          label 'face 'eldoc-highlight-function-argument)
                         ": " (eglot--format-markup documentation))))))
         (buffer-string))))
   when moresigs concat "\n"))

(defvar eglot--eldoc-hint nil)

(defvar eglot--help-buffer nil)

(defun eglot--help-buffer ()
  (or (and (buffer-live-p eglot--help-buffer)
           eglot--help-buffer)
      (setq eglot--help-buffer (generate-new-buffer "*eglot-help*"))))

(defun eglot-help-at-point ()
  "Request \"hover\" information for the thing at point."
  (interactive)
  (eglot--dbind ((Hover) contents range)
      (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                       (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (let ((blurb (eglot--hover-info contents range))
          (sym (thing-at-point 'symbol)))
      (with-current-buffer (eglot--help-buffer)
        (with-help-window (current-buffer)
          (rename-buffer (format "*eglot-help for %s*" sym))
          (with-current-buffer standard-output (insert blurb)))))))

(defun eglot-doc-too-large-for-echo-area (string)
  "Return non-nil if STRING won't fit in echo area.
Respects `max-mini-window-height' (which see)."
  (let ((max-height
         (cond ((floatp max-mini-window-height) (* (frame-height)
                                                   max-mini-window-height))
               ((integerp max-mini-window-height) max-mini-window-height)
               (t 1))))
    (> (cl-count ?\n string) max-height)))

(defcustom eglot-put-doc-in-help-buffer
  #'eglot-doc-too-large-for-echo-area
  "If non-nil, put \"hover\" documentation in separate `*eglot-help*' buffer.
If nil, use whatever `eldoc-message-function' decides (usually
the echo area).  If t, use `*eglot-help; unconditionally.  If a
function, it is called with the docstring to display and should a
boolean producing one of the two previous values."
  :type '(choice (const :tag "Never use `*eglot-help*'" nil)
                 (const :tag "Always use `*eglot-help*'" t)
                 (function :tag "Ask a function")))

(defcustom eglot-auto-display-help-buffer nil
  "If non-nil, automatically display `*eglot-help*' buffer.
Buffer is displayed with `display-buffer', which obeys
`display-buffer-alist' & friends."
  :type 'boolean)

(defun eglot--eldoc-message (format &rest args)
  (let ((string (apply #'format format args))) ;; FIXME: overworking?
    (when (or (eq t eglot-put-doc-in-help-buffer)
              (funcall eglot-put-doc-in-help-buffer string))
      (with-current-buffer (eglot--help-buffer)
        (rename-buffer (format "*eglot-help for %s*" eglot--eldoc-hint))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert string)
          (goto-char (point-min))
          (if eglot-auto-display-help-buffer
              (display-buffer (current-buffer))
            (unless (get-buffer-window (current-buffer))
              (eglot--message
               "%s\n(...truncated. Full help is in `%s')"
               (truncate-string-to-width
                (replace-regexp-in-string "\\(.*\\)\n.*" "\\1" string)
                (frame-width) nil nil "...")
               (buffer-name eglot--help-buffer))))
          (help-mode)
          t)))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function.
If SKIP-SIGNATURE, don't try to send textDocument/signatureHelp."
  (let* ((buffer (current-buffer))
         (server (eglot--current-server-or-lose))
         (position-params (eglot--TextDocumentPositionParams))
         sig-showing
         (thing-at-point (thing-at-point 'symbol)))
    (setq eglot--eldoc-hint thing-at-point)
    (cl-macrolet ((when-buffer-window
                   (&body body) ; notice the exception when testing with `ert'
                   `(when (or (get-buffer-window buffer) (ert-running-test))
                      (with-current-buffer buffer ,@body))))
      (when (eglot--server-capable :signatureHelpProvider)
        (jsonrpc-async-request
         server :textDocument/signatureHelp position-params
         :success-fn
         (eglot--lambda ((SignatureHelp)
                         signatures activeSignature activeParameter)
           (when-buffer-window
            (when (cl-plusp (length signatures))
              (setq sig-showing t)
              (let ((eglot--eldoc-hint thing-at-point))
                (eldoc-message (eglot--sig-info signatures
                                                activeSignature
                                                activeParameter))))))
         :deferred :textDocument/signatureHelp))
      (when (eglot--server-capable :hoverProvider)
        (jsonrpc-async-request
         server :textDocument/hover position-params
         :success-fn (eglot--lambda ((Hover) contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (when-let (info (and contents
                                               (eglot--hover-info contents
                                                                  range)))
                            (let ((eglot--eldoc-hint thing-at-point))
                              (eldoc-message info))))))
         :deferred :textDocument/hover))
      (when (eglot--server-capable :documentHighlightProvider)
        (jsonrpc-async-request
         server :textDocument/documentHighlight position-params
         :success-fn
         (lambda (highlights)
           (mapc #'delete-overlay eglot--highlights)
           (setq eglot--highlights
                 (when-buffer-window
                  (mapcar
                   (eglot--lambda ((DocumentHighlight) range)
                     (pcase-let ((`(,beg . ,end)
                                  (eglot--range-region range)))
                       (let ((ov (make-overlay beg end)))
                         (overlay-put ov 'face 'highlight)
                         (overlay-put ov 'evaporate t)
                         ov)))
                   highlights))))
         :deferred :textDocument/documentHighlight))))
  eldoc-last-message)

(defun eglot-imenu (oldfun)
  "EGLOT's `imenu-create-index-function' overriding OLDFUN."
  (if (eglot--server-capable :documentSymbolProvider)
      (let ((entries
             (mapcar
              (eglot--lambda
                  ((SymbolInformation) name kind location containerName)
                (cons (propertize
                       name
                       :kind (alist-get kind eglot--symbol-kind-names
                                        "Unknown")
                       :containerName (and (stringp containerName)
                                           (not (string-empty-p containerName))
                                           containerName))
                      (eglot--lsp-position-to-point
                       (plist-get (plist-get location :range) :start))))
              (jsonrpc-request (eglot--current-server-or-lose)
                               :textDocument/documentSymbol
                               `(:textDocument ,(eglot--TextDocumentIdentifier))))))
        (mapcar
         (pcase-lambda (`(,kind . ,syms))
           (let ((syms-by-scope (seq-group-by
                                 (lambda (e)
                                   (get-text-property 0 :containerName (car e)))
                                 syms)))
             (cons kind (cl-loop for (scope . elems) in syms-by-scope
                                 append (if scope
                                            (list (cons scope elems))
                                          elems)))))
         (seq-group-by (lambda (e) (get-text-property 0 :kind (car e)))
                       entries)))
    (funcall oldfun)))

(defun eglot--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil."
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (jsonrpc-error "Edits on `%s' require version %d, you have %d"
                   (current-buffer) version eglot--versioned-identifier))
  (atomic-change-group
    (let* ((change-group (prepare-change-group))
           (howmany (length edits))
           (reporter (make-progress-reporter
                      (format "[eglot] applying %s edits to `%s'..."
                              howmany (current-buffer))
                      0 howmany))
           (done 0))
      (mapc (pcase-lambda (`(,newText ,beg . ,end))
              (let ((source (current-buffer)))
                (with-temp-buffer
                  (insert newText)
                  (let ((temp (current-buffer)))
                    (with-current-buffer source
                      (save-excursion
                        (save-restriction
                          (narrow-to-region beg end)

                          ;; On emacs versions < 26.2,
                          ;; `replace-buffer-contents' is buggy - it calls
                          ;; change functions with invalid arguments - so we
                          ;; manually call the change functions here.
                          ;;
                          ;; See emacs bugs #32237, #32278:
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
                          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
                          (let ((inhibit-modification-hooks t)
                                (length (- end beg)))
                            (run-hook-with-args 'before-change-functions
                                                beg end)
                            (replace-buffer-contents temp)
                            (run-hook-with-args 'after-change-functions
                                                beg (+ beg (length newText))
                                                length))))
                      (progress-reporter-update reporter (cl-incf done)))))))
            (mapcar (eglot--lambda ((TextEdit) range newText)
                      (cons newText (eglot--range-region range 'markers)))
                    (reverse edits)))
      (undo-amalgamate-change-group change-group)
      (progress-reporter-done reporter))))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (eglot--uri-to-path uri) edits version)))
                   documentChanges))
          edit)
      (cl-loop for (uri edits) on changes by #'cddr
               do (push (list (eglot--uri-to-path uri) edits) prepared))
      (if (or confirm
              (cl-notevery #'find-buffer-visiting
                           (mapcar #'car prepared)))
          (unless (y-or-n-p
                   (format "[eglot] Server wants to edit:\n  %s\n Proceed? "
                           (mapconcat #'identity (mapcar #'car prepared) "\n  ")))
            (eglot--error "User cancelled server edit")))
      (while (setq edit (car prepared))
        (pcase-let ((`(,path ,edits ,version)  edit))
          (with-current-buffer (find-file-noselect path)
            (eglot--apply-text-edits edits version))
          (pop prepared))
        t)
      (unwind-protect
          (if prepared (eglot--warn "Caution: edits of files %s failed."
                                    (mapcar #'car prepared))
            (eglot-eldoc-function)
            (eglot--message "Edit successful!"))))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jsonrpc-request (eglot--current-server-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))


(defun eglot-code-actions (&optional beg end)
  "Get and offer to execute code actions between BEG and END."
  (interactive
   (let (diags)
     (cond ((region-active-p) (list (region-beginning) (region-end)))
           ((setq diags (flymake-diagnostics (point)))
            (list (cl-reduce #'min (mapcar #'flymake-diagnostic-beg diags))
                  (cl-reduce #'max (mapcar #'flymake-diagnostic-end diags))))
           (t (list (point-min) (point-max))))))
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (jsonrpc-request
           server
           :textDocument/codeAction
           (list :textDocument (eglot--TextDocumentIdentifier)
                 :range (list :start (eglot--pos-to-lsp-position beg)
                              :end (eglot--pos-to-lsp-position end))
                 :context
                 `(:diagnostics
                   [,@(mapcar (lambda (diag)
                                (cdr (assoc 'eglot-lsp-diag
                                            (eglot--diag-data diag))))
                              (flymake-diagnostics beg end))]))))
         (menu-items
          (or (mapcar (jsonrpc-lambda (&rest all &key title &allow-other-keys)
                        (cons title all))
                      actions)
              (eglot--error "No code actions here")))
         (menu `("Eglot code actions:" ("dummy" ,@menu-items)))
         (action (if (listp last-nonmenu-event)
                     (x-popup-menu last-nonmenu-event menu)
                   (let ((never-mind (gensym)) retval)
                     (setcdr (cadr menu)
                             (cons `("never mind..." . ,never-mind) (cdadr menu)))
                     (if (eq (setq retval (tmm-prompt menu)) never-mind)
                         (keyboard-quit)
                       retval)))))
    (eglot--dcase action
        (((Command) command arguments)
         (eglot-execute-command server (intern command) arguments))
      (((CodeAction) edit command)
       (when edit (eglot--apply-workspace-edit edit))
       (when command
         (eglot--dbind ((Command) command arguments) command
           (eglot-execute-command server (intern command) arguments)))))))



;;; Dynamic registration
;;;
(defun eglot--wildcard-to-regexp (wildcard)
  "(Very lame attempt to) convert WILDCARD to a Elisp regexp."
  (cl-loop
   with substs = '(("{" . "\\\\(")
                   ("}" . "\\\\)")
                   ("," . "\\\\|"))
   with string = (wildcard-to-regexp wildcard)
   for (pattern . rep) in substs
   for target = string then result
   for result = (replace-regexp-in-string pattern rep target)
   finally return result))

(cl-defmethod eglot-register-capability
    (server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  (eglot-unregister-capability server method id)
  (let* (success
         (globs (mapcar (eglot--lambda ((FileSystemWatcher) globPattern)
                          globPattern)
                        watchers)))
    (cl-labels
        ((handle-event
          (event)
          (pcase-let ((`(,desc ,action ,file ,file1) event))
            (cond
             ((and (memq action '(created changed deleted))
                   (cl-find file globs
                            :test (lambda (f glob)
                                    (string-match (eglot--wildcard-to-regexp
                                                   (expand-file-name glob))
                                                  f))))
              (jsonrpc-notify
               server :workspace/didChangeWatchedFiles
               `(:changes ,(vector `(:uri ,(eglot--path-to-uri file)
                                          :type ,(cl-case action
                                                   (created 1)
                                                   (changed 2)
                                                   (deleted 3)))))))
             ((eq action 'renamed)
              (handle-event desc 'deleted file)
              (handle-event desc 'created file1))))))
      (unwind-protect
          (progn (dolist (dir (delete-dups (mapcar #'file-name-directory globs)))
                   (push (file-notify-add-watch dir '(change) #'handle-event)
                         (gethash id (eglot--file-watches server))))
                 (setq
                  success
                  `(:message ,(format "OK, watching %s watchers"
                                      (length watchers)))))
        (unless success
          (eglot-unregister-capability server method id))))))

(cl-defmethod eglot-unregister-capability
  (server (_method (eql workspace/didChangeWatchedFiles)) id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles"
  (mapc #'file-notify-rm-watch (gethash id (eglot--file-watches server)))
  (remhash id (eglot--file-watches server))
  (list t "OK"))


;;; Rust-specific
;;;
(defclass eglot-rls (eglot-lsp-server) () :documentation "Rustlang's RLS.")

(cl-defmethod jsonrpc-connection-ready-p ((server eglot-rls) what)
  "Except for :completion, RLS isn't ready until Indexing done."
  (and (cl-call-next-method)
       (or ;; RLS normally ready for this, even if building.
        (eq :textDocument/completion what)
        (pcase-let ((`(,_id ,what ,done ,_detail) (eglot--spinner server)))
          (and (equal "Indexing" what) done)))))

(cl-defmethod eglot-handle-notification
  ((server eglot-rls) (_method (eql window/progress))
   &key id done title message &allow-other-keys)
  "Handle notification window/progress"
  (setf (eglot--spinner server) (list id title done message)))


;;; cquery-specific
;;;
(defclass eglot-cquery (eglot-lsp-server) ()
  :documentation "Cquery's C/C++ langserver.")

(cl-defmethod eglot-initialization-options ((server eglot-cquery))
  "Passes through required cquery initialization options"
  (let* ((root (car (project-roots (eglot--project server))))
         (cache (expand-file-name ".cquery_cached_index/" root)))
    (list :cacheDirectory (file-name-as-directory cache)
          :progressReportFrequencyMs -1)))


;;; eclipse-jdt-specific
;;;
(defclass eglot-eclipse-jdt (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
  "Passes through required jdt initialization options"
  `(:workspaceFolders
    [,@(cl-delete-duplicates
        (mapcar #'eglot--path-to-uri
                (let* ((roots (project-roots (eglot--project server)))
                       (root (car roots)))
                  (append
                   roots
                   (mapcar
                    #'file-name-directory
                    (append
                     (file-expand-wildcards (concat root "*/pom.xml"))
                     (file-expand-wildcards (concat root "*/build.gradle"))
                     (file-expand-wildcards (concat root "*/.project")))))))
        :test #'string=)]
    ,@(if-let ((home (or (getenv "JAVA_HOME")
                         (ignore-errors
                           (expand-file-name
                            ".."
                            (file-name-directory
                             (file-chase-links (executable-find "javac"))))))))
          `(:settings (:java (:home ,home)))
        (ignore (eglot--warn "JAVA_HOME env var not set")))))

(defun eglot--eclipse-jdt-contact (interactive)
  "Return a contact for connecting to eclipse.jdt.ls server, as a cons cell.
If INTERACTIVE, prompt user for details."
  (cl-labels
      ((is-the-jar
        (path)
        (and (string-match-p
              "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$"
              (file-name-nondirectory path))
             (file-exists-p path))))
    (let* ((classpath (or (getenv "CLASSPATH") ":"))
           (cp-jar (cl-find-if #'is-the-jar (split-string classpath ":")))
           (jar cp-jar)
           (dir
            (cond
             (jar (file-name-as-directory
                   (expand-file-name ".." (file-name-directory jar))))
             (interactive
              (expand-file-name
               (read-directory-name
                (concat "Path to eclipse.jdt.ls directory (could not"
                        " find it in CLASSPATH): ")
                nil nil t)))
             (t (error "Could not find eclipse.jdt.ls jar in CLASSPATH"))))
           (repodir
            (concat dir
                    "org.eclipse.jdt.ls.product/target/repository/"))
           (repodir (if (file-directory-p repodir) repodir dir))
           (config
            (concat
             repodir
             (cond
              ((string= system-type "darwin") "config_mac")
              ((string= system-type "windows-nt") "config_win")
              (t "config_linux"))))
           (project (or (project-current) `(transient . ,default-directory)))
           (workspace
            (expand-file-name (md5 (car (project-roots project)))
                              (concat user-emacs-directory
                                      "eglot-eclipse-jdt-cache"))))
      (unless jar
        (setq jar
              (cl-find-if #'is-the-jar
                          (directory-files (concat repodir "plugins") t))))
      (unless (and jar (file-exists-p jar) (file-directory-p config))
        (error "Could not find required eclipse.jdt.ls files (build required?)"))
      (when (and interactive (not cp-jar)
                 (y-or-n-p (concat "Add path to the server program "
                                   "to CLASSPATH environment variable?")))
        (setenv "CLASSPATH" (concat (getenv "CLASSPATH") ":" jar)))
      (unless (file-directory-p workspace)
        (make-directory workspace t))
      (cons 'eglot-eclipse-jdt
            (list (executable-find "java")
                  "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                  "-Dosgi.bundles.defaultStartLevel=4"
                  "-Declipse.product=org.eclipse.jdt.ls.core.product"
                  "-jar" jar
                  "-configuration" config
                  "-data" workspace)))))

(cl-defmethod eglot-execute-command
  ((_server eglot-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
  "Eclipse JDT breaks spec and replies with edits as arguments."
  (mapc #'eglot--apply-workspace-edit arguments))

(provide 'eglot)
;;; eglot.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
