;;; jsonrpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: processes, languages, extensions

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

;; Originally extracted from eglot.el (Emacs LSP client)
;;
;; This library implements the JSONRPC 2.0 specification as described
;; in http://www.jsonrpc.org/.  As the name suggests, JSONRPC is a
;; generic Remote Procedure Call protocol designed around JSON
;; objects.
;;
;; Quoting from the spec: "[JSONRPC] is transport agnostic in that the
;; concepts can be used within the same process, over sockets, over
;; http, or in many various message passing environments."
;;
;; To approach this agnosticism, jsonrpc.el uses Emacs's "process"
;; abstraction since it mostly hides the underlying differences
;; between local subprocesses and network endpoints.  Thus everywhere
;; in this library (be it in the internals or in the user-visible
;; protocol), JSONRPC endpoint is an (augmented) process object.
;;
;; The main entry point is `jsonrpc-connect'.  It is passed a name
;; identifying the connection and a "contact", which will determine
;; the connection type to make.  It can a list of strings (a command
;; and arguments for creating subprocesses) or a (HOST PORT-NUMBER
;; PARAMS...) for connecting via TCP.  For flexibility, it can also be
;; a pre-connected process.
;;
;; `jsonrpc-connect' returns a process upon connection.  This value
;; should be saved to be later given to `jsonrpc-notify',
;; `jsonrpc-reply', `jsonrpc-request' and `jsonrpc-async-request' as a
;; way of contacting the connected remote endpoint.
;;
;; `jsonrpc-connect' is also passed a dispatcher function for handling
;; handling the contacts asynchronously initiated by the remote
;; endpoint's, as well as a optional function for cleaning up after
;; the tear-down of the JSONRPC connection.
;;
;; The JSON objects are passed to the dispatcher after being read by
;; `json-read' of Emacs's json.el library.  They are read as plists,
;; and, likewise, json.el-compatible plists should be given to
;; `jsonrpc-notify', `jsonrpc-request', etc...
;;
;; To facilitate handling of key-value plists, this library make
;; liberal use of cl-lib.el and suggests (but doesn't force) its
;; clients to do the same.  A macro `jsonrpc-lambda' can be used to
;; create a lambda for destructuring a JSON-object like in this
;; example:
;;
;;  (jsonrpc-async-request
;;   myproc :frobnicate `(:foo "trix")
;;   :success-fn (jsonrpc-lambda (&key bar baz &allow-other-keys)
;;                 (message "Server replied back %s and %s!"
;;                          bar baz))
;;   :error-fn (jsonrpc-lambda (&key code message _data)
;;               (message "Sadly, server reports %s: %s"
;;                        code message)))
;;
;; Finally, here's an example Emacs JSONRPC server that offers a (very
;; small) subset of Elisp for remote calling:
;;
;;   (defvar server) (defvar server-endpoint)
;;   (defvar server-allowed-functions '(+ - * / vconcat append sit-for))
;;
;;   (setq server
;;         (make-network-process
;;          :name "Emacs RPC server" :server t :host "localhost" :service 9393
;;          :log (lambda (_server client _message)
;;                 (jsonrpc-connect
;;                  (process-name client) client
;;                  (lambda (proc method id params)
;;                    (unless (memq method server-allowed-functions)
;;                      (signal 'error `((jsonrpc-error-message
;;                                        . "Sorry, this isn't allowed")
;;                                       (jsonrpc-error-code . 32601))))
;;                    (jsonrpc-reply proc id :result
;;                                   (apply method (append params nil))))))))

;;   (setq server-endpoint (jsonrpc-connect
;;                          "Emacs RPC client" '("localhost" 9393)
;;                          (lambda (_proc method id &rest params)
;;                            (message "server wants to %s" method))))
;;
;;   ;; returns 3
;;   (jsonrpc-request server-endpoint '+ '(1 2))
;;   ;; errors with -32601
;;   (jsonrpc-request server-endpoint 'delete-directory "~/tmp")
;;   ;; signals an -32603 JSONRPC error
;;   (jsonrpc-request server-endpoint '+ '(a 2))
;;   ;; times out
;;   (jsonrpc-request server-endpoint 'sit-for '(5))
;;   ;; stretching it, but works
;;   (jsonrpc-request server-endpoint 'vconcat '([1 2 3] [3 4 5]))
;;   ;; json.el can't serialize this, json.el errors and request isn't sent
;;   (jsonrpc-request server-endpoint 'append '((1 2 3) (3 4 5)))
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'warnings)
(require 'pcase)
(require 'array) ; xor

(defgroup jsonrpc nil
  "Interaction between JSONRPC endpoints"
  :prefix "jsonrpc-"
  :group 'applications)

(defcustom jsonrpc-request-timeout 3
  "How many seconds to wait for a JSONRPC from the server.
If nil, don't use a timeout (not recommended)."
  :type :integer)

(defvar jsonrpc-find-process-functions nil
  "Special hook to find an active JSON-RPC process.")

(defun jsonrpc-current-process ()
  "The current logical JSON-RPC process."
  (run-hook-with-args-until-success 'jsonrpc-find-process-functions))

(defun jsonrpc-current-process-or-lose ()
  "Return the current JSON-RPC process or error."
  (or (jsonrpc-current-process)
      (jsonrpc-error "No current JSON-RPC process")))

(defun jsonrpc-error (format &rest args)
  "Error out with FORMAT and ARGS.
If invoked inside a dispatcher function, this function is suitable
for replying to the remote endpoint with a -32603 error code and
FORMAT as the message."
  (signal 'error (format "[jsonrpc] %s" (apply #'format format args))))

(defun jsonrpc-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[jsonrpc] %s" (concat "[jsonrpc] %s" (apply #'format format args))))

(defun jsonrpc-warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jsonrpc-message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jsonrpc
                     (apply #'format format args)
                     :warning)))

(defmacro jsonrpc-define-process-var
    (var-sym initval &optional doc)
  "Define VAR-SYM as a generalized process-local variable.
INITVAL is the default value.  DOC is the documentation."
  (declare (indent 2) (doc-string 3))
  `(progn
     (defun ,var-sym (proc) ,doc
       (let* ((plist (process-plist proc))
              (probe (plist-member plist ',var-sym)))
         (if probe (cadr probe)
           (let ((def ,initval)) (process-put proc ',var-sym def) def))))
     (gv-define-setter ,var-sym (to-store process)
       `(let ((once ,to-store)) (process-put ,process ',',var-sym once) once))))

(jsonrpc-define-process-var jsonrpc-name nil
  "A name for the process")

(jsonrpc-define-process-var jsonrpc--dispatcher nil
  "Emacs-lisp function for server-invoked methods.")

(jsonrpc-define-process-var jsonrpc-status `(:unknown nil)
  "Status as declared by the server.
A list (WHAT SERIOUS-P).")

(jsonrpc-define-process-var jsonrpc--expected-bytes nil
  "How many bytes declared by server")

(jsonrpc-define-process-var jsonrpc--request-continuations (make-hash-table)
  "A hash table of request ID to continuation lambdas.")

(jsonrpc-define-process-var jsonrpc--server-request-ids nil
  "Server-initiated request id that client hasn't replied to.")

(jsonrpc-define-process-var jsonrpc--events-buffer nil
  "A buffer pretty-printing the JSON-RPC RPC events")

(jsonrpc-define-process-var jsonrpc-contact nil
  "Method used to contact a server.")

(jsonrpc-define-process-var jsonrpc--on-shutdown nil
  "Function run when JSONRPC server is dying.
Run after running any error handlers for outstanding requests.
A function passed the process object for the server.")

(jsonrpc-define-process-var jsonrpc--deferred-actions
    (make-hash-table :test #'equal)
  "Actions deferred to when server is thought to be ready.")

(defun jsonrpc-outstanding-request-ids (proc)
  "IDs of outstanding JSONRPC requests for PROC."
  (hash-table-keys (jsonrpc--request-continuations proc)))

(defun jsonrpc--make-process (name contact)
  "Make a process from CONTACT.
NAME is a name to give the inferior process or connection.
CONTACT is as explained in `jsonrpc-connect'.  Returns a process
object."
  (let* ((readable-name (format "JSON-RPC server (%s)" name)                                                            )
         (buffer (get-buffer-create (format "*%s stderr*" readable-name)))
         (proc
          (cond ((processp contact) contact)
                ((integerp (cadr contact))
                 (apply #'open-network-stream
                        readable-name buffer contact))
                (t
                 (make-process :name readable-name
                               :command contact
                               :connection-type 'pipe
                               :coding 'no-conversion
                               :stderr (get-buffer-create (format "*%s stderr*"
                                                                  name)))))))
    (set-process-buffer proc buffer)
    (set-marker (process-mark proc) (with-current-buffer buffer (point-min)))
    (set-process-filter proc #'jsonrpc--process-filter)
    (set-process-sentinel proc #'jsonrpc--process-sentinel)
    proc))

(defmacro jsonrpc-obj (&rest what)
  "Make WHAT a suitable argument for `json-encode'."
  (declare (debug (&rest form)))
  ;; FIXME: maybe later actually do something, for now this just fixes
  ;; the indenting of literal plists, i.e. is basically `list'
  `(list ,@what))

;;;###autoload
(cl-defun jsonrpc-connect (name contact dispatcher &optional on-shutdown)
  "Connect to JSON-RPC server hereafter known as NAME through CONTACT.

NAME is a string naming the server.

CONTACT is a list of strings (COMMAND ARGS...) specifying how to
start a server subprocess to connect to.  If the second element
in the list is an integer number instead of a string, the list is
interpreted as (HOST PORT PARAMETERS...) to connect to an
existing server via TCP, with the remaining PARAMETERS are given
to `open-network-stream's optional arguments.  CONTACT can also
be a live connected process object. In that case its buffer,
filter and sentinel are overwritten by `jsonrpc-connect'.

ON-SHUTDOWN, if non-nil, is a function called on server exit and
passed the moribund process object as a single argument.

DISPATCHER specifies how the server-invoked methods find their
Elisp counterpart. It is a function passed (PROC METHOD ID PARAMS
as arguments. PROC is the process object returned by this
function. ID is the server identifier for a server request, or
nil for a server notification. METHOD is a symbol. PARAMS
contains the method parameters as JSON data.

If ID is non-nil, DISPATCHER is expected to reply to the
request. If it doesn't, or if it signals an error before doing
so, jsonrpc.el will automatically reply with an error. If DISPATCHER
signals an error with alist elements `jsonrpc-error-message' and
`jsonrpc-error-code' in its DATA, the corresponding elements are
used for the automated error reply.

`jsonrpc-connect' returns a process object representing the server."
  (let* ((proc (jsonrpc--make-process name contact)))
    (setf (jsonrpc-contact proc) contact
          (jsonrpc-name proc) name
          (jsonrpc--dispatcher proc) dispatcher
          (jsonrpc--on-shutdown proc) on-shutdown)
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)) (erase-buffer) (read-only-mode t) proc))))

(defun jsonrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (jsonrpc-log-event proc `(:message "Process state changed" :change ,change))
  (when (not (process-live-p proc))
    (with-current-buffer (jsonrpc-events-buffer proc)
      (let ((inhibit-read-only t))
        (insert "\n----------b---y---e---b---y---e----------\n")))
    ;; Cancel outstanding timers
    (maphash (lambda (_id triplet)
               (pcase-let ((`(,_success ,_error ,timeout) triplet))
                 (when timeout (cancel-timer timeout))))
             (jsonrpc--request-continuations proc))
    (unwind-protect
        ;; Call all outstanding error handlers
        (maphash (lambda (_id triplet)
                   (pcase-let ((`(,_success ,error ,_timeout) triplet))
                     (funcall error `(:code -1 :message "Server died"))))
                 (jsonrpc--request-continuations proc))
      (jsonrpc-message "Server exited with status %s" (process-exit-status proc))
      (funcall (or (jsonrpc--on-shutdown proc) #'ignore) proc)
      (delete-process proc))))

(defun jsonrpc--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (expected-bytes (jsonrpc--expected-bytes proc)))
        ;; Insert the text, advancing the process marker.
        ;;
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        ;; Loop (more than one message might have arrived)
        ;;
        (unwind-protect
            (let (done)
              (while (not done)
                (cond
                 ((not expected-bytes)
                  ;; Starting a new message
                  ;;
                  (setq expected-bytes
                        (and (search-forward-regexp
                              "\\(?:.*: .*\r\n\\)*Content-Length: \
*\\([[:digit:]]+\\)\r\n\\(?:.*: .*\r\n\\)*\r\n"
                              (+ (point) 100)
                              t)
                             (string-to-number (match-string 1))))
                  (unless expected-bytes
                    (setq done :waiting-for-new-message)))
                 (t
                  ;; Attempt to complete a message body
                  ;;
                  (let ((available-bytes (- (position-bytes (process-mark proc))
                                            (position-bytes (point)))))
                    (cond
                     ((>= available-bytes
                          expected-bytes)
                      (let* ((message-end (byte-to-position
                                           (+ (position-bytes (point))
                                              expected-bytes))))
                        (unwind-protect
                            (save-restriction
                              (narrow-to-region (point) message-end)
                              (let* ((json-object-type 'plist)
                                     (json-message
                                      (condition-case-unless-debug oops
                                          (json-read)
                                        (error
                                         (jsonrpc-warn "Invalid JSON: %s %s"
                                                       (cdr oops) (buffer-string))
                                         nil))))
                                (when json-message
                                  ;; Process content in another
                                  ;; buffer, shielding proc buffer from
                                  ;; tamper
                                  (with-temp-buffer
                                    (jsonrpc--process-receive proc
                                                              json-message)))))
                          (goto-char message-end)
                          (delete-region (point-min) (point))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (jsonrpc--expected-bytes proc) expected-bytes))))))

(defun jsonrpc-events-buffer (process &optional interactive)
  "Display events buffer for current JSONRPC connection PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (jsonrpc-current-process-or-lose) t))
  (let* ((probe (jsonrpc--events-buffer process))
         (buffer (or (and (buffer-live-p probe)
                          probe)
                     (let ((buffer (get-buffer-create
                                    (format "*%s events*"
                                            (process-name process)))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (read-only-mode t)
                         (setf (jsonrpc--events-buffer process) buffer))
                       buffer))))
    (when interactive (display-buffer buffer))
    buffer))

(defun jsonrpc-log-event (proc message &optional type)
  "Log an jsonrpc-related event.
PROC is the current process.  MESSAGE is a JSON-like plist.  TYPE
is a symbol saying if this is a client or server originated."
  (with-current-buffer (jsonrpc-events-buffer proc)
    (cl-destructuring-bind (&key method id error &allow-other-keys) message
      (let* ((inhibit-read-only t)
             (subtype (cond ((and method id)       'request)
                            (method                'notification)
                            (id                    'reply)
                            (t                     'message)))
             (type
              (format "%s-%s" (or type :internal) subtype)))
        (goto-char (point-max))
        (let ((msg (format "%s%s%s:\n%s\n"
                           type
                           (if id (format " (id:%s)" id) "")
                           (if error " ERROR" "")
                           (pp-to-string message))))
          (when error
            (setq msg (propertize msg 'face 'error)))
          (insert-before-markers msg))))))

(defun jsonrpc--process-receive (proc message)
  "Process MESSAGE from PROC."
  (pcase-let ((`(,method ,id ,error ,params ,result)
               (condition-case-unless-debug oops
                   (cl-destructuring-bind
                       (&rest args &key method id error params result _jsonrpc)
                       message (list method id error params result))
                 (error (jsonrpc-warn "Invalid JSONRPC message %s: %s" message
                                      (cdr oops))
                        nil)))
              (continuations)
              (lisp-err))
    (jsonrpc-log-event proc message 'server)
    (when error (setf (jsonrpc-status proc) `(,error t)))
    (cond (method
           (condition-case-unless-debug oops
               (funcall (jsonrpc--dispatcher proc) proc (intern method) id params)
             (error (setq lisp-err oops)))
           (unless (or (member id (jsonrpc--server-request-ids proc))
                       (not (or id lisp-err)))
             (jsonrpc-reply
              proc id
              :error (jsonrpc-obj
                      :code (or (alist-get 'jsonrpc-error-code (cdr lisp-err))
                                -32603)
                      :message (or (alist-get 'jsonrpc-error-message
                                              (cdr lisp-err))
                                   "Internal error"))))
           (setf (jsonrpc--server-request-ids proc)
                 (delete id (jsonrpc--server-request-ids proc))))
          ((setq continuations
                 (and id (gethash id (jsonrpc--request-continuations proc))))
           (let ((timer (nth 2 continuations)))
             (when timer (cancel-timer timer)))
           (remhash id (jsonrpc--request-continuations proc))
           (if error (funcall (nth 1 continuations) error)
             (funcall (nth 0 continuations) result)))
          (id
           (jsonrpc-warn "No continuation for id %s" id)))
    (jsonrpc--call-deferred proc)))

(defun jsonrpc--process-send (proc message)
  "Send MESSAGE to PROC (ID is optional)."
  (let ((json-object-type 'plist)
        (json (json-encode message)))
    (process-send-string proc (format "Content-Length: %d\r\n\r\n%s"
                                      (string-bytes json)
                                      json))
    (jsonrpc-log-event proc message 'client)))

(defvar jsonrpc--next-request-id 0)

(defun jsonrpc--next-request-id ()
  "Compute the next id for a client request."
  (setq jsonrpc--next-request-id (1+ jsonrpc--next-request-id)))

(defun jsonrpc-forget-pending-continuations (proc)
  "Stop waiting for responses from the current JSONRPC PROC."
  (interactive (list (jsonrpc-current-process-or-lose)))
  (clrhash (jsonrpc--request-continuations proc)))

(defun jsonrpc-clear-status (process)
  "Clear most recent error message from PROCESS."
  (interactive (list (jsonrpc-current-process-or-lose)))
  (setf (jsonrpc-status process) nil))

(defun jsonrpc--call-deferred (proc)
  "Call PROC's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (jsonrpc--deferred-actions proc))))
    (jsonrpc-log-event proc `(:running-deferred ,(length actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defvar jsonrpc-ready-predicates '()
  "Special hook of predicates controlling deferred actions.
If one of these returns nil, a deferrable `jsonrpc-async-request'
will be deferred.  Each predicate is passed the symbol for the
request and a process object.")

(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(cl-defun jsonrpc-async-request (proc
                                 method
                                 params
                                 &rest args
                                 &key success-fn error-fn timeout-fn
                                 (timeout jsonrpc-request-timeout)
                                 (deferred nil))
  "Make a request to PROC, expecting a reply, return immediately.
The JSONRPC request is formed by METHOD, a symbol, and PARAMS a
JSON object.

The caller can expect SUCCESS-FN or ERROR-FN to be called with a
JSONRPC `:result' or `:error' object, respectively.  If this
doesn't happen after TIMEOUT seconds (defaults to
`jsonrpc-request-timeout'), the caller can expect TIMEOUT-FN to be
called with no arguments. The default values of SUCCESS-FN,
ERROR-FN and TIMEOUT-FN simply log the events into
`jsonrpc-events-buffer'.

If DEFERRED is non-nil, maybe defer the request to a future time
when the server is thought to be ready according to
`jsonrpc-ready-predicates' (which see).  The request might never be
sent at all, in case it is overridden in the meantime by a new
request with identical DEFERRED and for the same buffer.
However, in that situation, the original timeout is kept.

Return a list (ID TIMER). ID is the new request's ID, or nil if
the request was deferred. TIMER is a timer object set (or nil, if
TIMEOUT is nil)"
  (let* ((id (jsonrpc--next-request-id))
         (timer nil)
         (make-timer
          (lambda ( )
            (or timer
                (when timeout
                  (run-with-timer
                   timeout nil
                   (lambda ()
                     (remhash id (jsonrpc--request-continuations proc))
                     (funcall (or timeout-fn
                                  (lambda ()
                                    (jsonrpc-log-event
                                     proc `(:timed-out ,method :id ,id
                                                       :params ,params))))))))))))
    (when deferred
      (let* ((buf (current-buffer))
             (existing (gethash (list deferred buf)
                                (jsonrpc--deferred-actions proc))))
        (when existing (setq timer (cadr existing)))
        (if (run-hook-with-args-until-failure 'jsonrpc-ready-predicates
                                              deferred proc)
            (remhash (list deferred buf) (jsonrpc--deferred-actions proc))
          (jsonrpc-log-event proc `(:deferring ,method :id ,id :params ,params))
          (let* ((buf (current-buffer)) (point (point))
                 (later (lambda ()
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (save-excursion (goto-char point)
                                              (apply #'jsonrpc-async-request proc
                                                     method params args)))))))
            (puthash (list deferred buf)
                     (list later (setq timer (funcall make-timer)))
                     (jsonrpc--deferred-actions proc))
            ;; Non-local exit!
            (cl-return-from jsonrpc-async-request (list nil timer))))))
    ;; Really send it
    ;;
    (jsonrpc--process-send proc (jsonrpc-obj :jsonrpc "2.0"
                                             :id id
                                             :method method
                                             :params params))
    (puthash id
             (list (or success-fn
                       (jsonrpc-lambda (&rest _ignored)
                         (jsonrpc-log-event
                          proc (jsonrpc-obj :message "success ignored" :id id))))
                   (or error-fn
                       (jsonrpc-lambda (&key code message &allow-other-keys)
                         (setf (jsonrpc-status proc) `(,message t))
                         (jsonrpc-log-event
                          proc (jsonrpc-obj :message "error ignored, status set"
                                            :id id :error code))))
                   (setq timer (funcall make-timer)))
             (jsonrpc--request-continuations proc))
    (list id timer)))

(cl-defun jsonrpc-request (proc method params &key deferred)
  "Make a request to PROC, wait for a reply.
Like `jsonrpc-async-request' for PROC, METHOD and PARAMS, but
synchronous, i.e. doesn't exit until anything
interesting (success, error or timeout) happens.  Furthermore,
only exit locally (and return the JSONRPC result object) if the
request is successful, otherwise exit non-locally with an error.

DEFERRED is passed to `jsonrpc-async-request', which see."
  (let* ((tag (cl-gensym "jsonrpc-request-catch-tag")) id-and-timer
         (retval
          (unwind-protect ; protect against user-quit, for example
              (catch tag
                (setq
                 id-and-timer
                 (jsonrpc-async-request
                  proc method params
                  :success-fn (lambda (result) (throw tag `(done ,result)))
                  :error-fn
                  (jsonrpc-lambda
                      (&key code message data)
                    (throw tag `(error (jsonrpc-error-code . ,code)
                                       (jsonrpc-error-message . ,message)
                                       (jsonrpc-error-data . ,data))))
                  :timeout-fn
                  (lambda ()
                    (throw tag '(error (jsonrpc-error-message . "Timed out"))))
                  :deferred deferred))
                (while t (accept-process-output nil 30)))
            (pcase-let ((`(,id ,timer) id-and-timer))
              (when id (remhash id (jsonrpc--request-continuations proc)))
              (when timer (cancel-timer timer))))))
    (when (eq 'error (car retval))
      (signal 'error
              (cons
               (format "[jsonrpc] jsonrpc-request (%s) failed:" (car id-and-timer))
               (cdr retval))))
    (cadr retval)))

(cl-defun jsonrpc-notify (proc method params)
  "Notify PROC of something, don't expect a reply.e"
  (jsonrpc--process-send proc (jsonrpc-obj :jsonrpc  "2.0"
                                           :method method
                                           :params params)))

(cl-defun jsonrpc-reply (proc id &key (result nil result-supplied-p) error)
  "Reply to PROC's request ID with RESULT or ERROR."
  (unless id (jsonrpc-error "Need a non-nil ID"))
  (unless (xor result-supplied-p error)
    (jsonrpc-error "Can't pass both RESULT and ERROR!"))
  (push id (jsonrpc--server-request-ids proc))
  (jsonrpc--process-send
   proc `(:jsonrpc  "2.0" :id ,id
                    ,@(when result `(:result ,result))
                    ,@(when error `(:error ,error)))))

(provide 'jsonrpc)
;;; jsonrpc.el ends here
