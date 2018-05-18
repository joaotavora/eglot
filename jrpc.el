;;; jrpc.el --- JSON-RPC library                  -*- lexical-binding: t; -*-

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
;; To approach this agosticity, jrpc.el uses Emacs's "process"
;; abstraction since it mostly hides the underlying differences
;; between local subprocesses and network endpoints.  Thus everywhere
;; in this library (be it in the internals or in the user-visible
;; protocol), JSONRPC endpoint is an (augmented) process object.
;;
;; The main entry point is `jrpc-connect'.  It is passed a name
;; identifying the connection and a "contact", which will determine
;; the connection type to make.  It can a list of strings (a command
;; and arguments for creating subprocesses) or a (HOST PORT-NUMBER
;; PARAMS...) for connecting via TCP.  For flexibility, it can also be
;; a pre-connected process.
;;
;; `jrpc-connect' returns a process upon connection.  This value
;; should be saved to be later given to `jrpc-notify', `jrpc-reply',
;; `jrpc-request' and `jrpc-async-request' as a way of contacting the
;; connected remote endpoint.
;;
;; `jrpc-connect' is also passed a dispatcher function for handling
;; handling the contacts asynchronously initiated by the remote
;; endpoint's, as well as a optional function for cleaning up after
;; the teardown of the JSONRPC connection.
;;
;; The JSON objects are passed to the dispatcher after being read by
;; `json-read' of Emacs's json.el library.  They are read as plists,
;; and, likewise, json.el-compatible plists should be given to
;; `jrpc-notify', `jrpc-request', etc...
;;
;; To facilitate handling of key-value plists, this library make
;; liberal use of cl-lib.el and suggests (but doesn't force) its
;; clients to do the same.  A macro `jrpc-lambda' can be used to
;; create a lambda for destructuring a JSON-object like in this
;; example:
;;
;; (jrpc-async-request myproc :frobnicate `(:foo "trix")
;;                     :success-fn (jrpc-lambda (&key bar baz &allow-other-keys)
;;                                   (message "Server replied back %s and %s!"
;;                                            bar baz))
;;                     :error-fn (jrpc-lambda (&key code message _data)
;;                                 (message "Sadly, server reports %s: %s"
;;                                          code message)))
;;
;; Finally, here's an example Emacs JSONRPC server that offers a (very
;; small) subset of Elisp for remote calling:
;;
;; (defvar server) (defvar server-endpoint)
;; (defvar server-allowed-functions '(+ - * / vconcat append sit-for))
;;
;; (delete-process server)
;; (setq server
;;       (make-network-process
;;        :name "Emacs RPC server" :server t :host "localhost" :service 9393
;;        :log (lambda (_server client _message)
;;               (jrpc-connect
;;                (process-name client) client
;;                (lambda (proc method id params)
;;                  (unless (memq method server-allowed-functions)
;;                    (signal 'error `((jrpc-error-message . "Sorry, this isn't allowed")
;;                                     (jrpc-error-code . 32601))))
;;                  (jrpc-reply proc id :result (apply method (append params nil))))))))
;;
;; (setq server-endpoint (jrpc-connect "Emacs RPC client" '("localhost" 9393)
;;                                     (lambda (_proc method id &rest params)
;;                                       (message "server wants to %s" method))))
;;
;;   ;; returns 3
;;   (jrpc-request server-endpoint '+ '(1 2))
;;   ;; errors with -32601
;;   (jrpc-request server-endpoint 'delete-directory "~/tmp")
;;   ;; signals an -32603 JSONRPC error
;;   (jrpc-request server-endpoint '+ '(a 2))
;;   ;; times out
;;   (jrpc-request server-endpoint 'sit-for '(5))
;;   ;; stretching it, but works
;;   (jrpc-request server-endpoint 'vconcat '([1 2 3] [3 4 5]))
;;   ;; json.el can't serialize this, json.el errors and request isn't sent
;;   (jrpc-request server-endpoint 'append '((1 2 3) (3 4 5)))
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'warnings)
(require 'pcase)
(require 'array) ; xor

(defgroup jrpc nil
  "Interaction between JSONRPC endpoints"
  :prefix "jrpc-"
  :group 'applications)

(defcustom jrpc-request-timeout 3
  "How many seconds to wait for a JSONRPC from the server.
If nil, don't use a timeout (not recommended)."
  :type :integer)

(defvar jrpc-find-process-functions nil
  "Special hook to find an active JSON-RPC process.")

(defun jrpc-current-process ()
  "The current logical JSON-RPC process."
  (run-hook-with-args-until-success 'jrpc-find-process-functions))

(defun jrpc-current-process-or-lose ()
  "Return the current JSON-RPC process or error."
  (or (jrpc-current-process)
      (jrpc-error "No current JSON-RPC process")))

(defun jrpc-error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error (apply #'format format args)))

(defun jrpc-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message (concat "[jrpc] " (apply #'format format args))))

(defun jrpc-warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jrpc-message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jrpc
                     (apply #'format format args)
                     :warning)))

(defmacro jrpc-define-process-var
    (var-sym initval &optional doc)
  "Define VAR-SYM as a generalized process-local variable.
INITVAL is the default value.  DOC is the documentation."
  (declare (indent 2))
  `(progn
     (put ',var-sym 'function-documentation ,doc)
     (defun ,var-sym (proc)
       (let* ((plist (process-plist proc))
              (probe (plist-member plist ',var-sym)))
         (if probe
             (cadr probe)
           (let ((def ,initval))
             (process-put proc ',var-sym def)
             def))))
     (gv-define-setter ,var-sym (to-store process)
       `(let ((once ,to-store)) (process-put ,process ',',var-sym once) once))))

(jrpc-define-process-var jrpc-name nil
  "A name for the process")

(jrpc-define-process-var jrpc--dispatcher nil
  "Emacs-lisp function for server-invoked methods.")

(jrpc-define-process-var jrpc-status `(:unknown nil)
  "Status as declared by the server.
A list (WHAT SERIOUS-P).")

(jrpc-define-process-var jrpc--expected-bytes nil
  "How many bytes declared by server")

(jrpc-define-process-var jrpc--request-continuations (make-hash-table)
  "A hash table of request ID to continuation lambdas.")

(jrpc-define-process-var jrpc--server-request-ids nil
  "Server-initiated request id that client hasn't replied to.")

(jrpc-define-process-var jrpc--events-buffer nil
  "A buffer pretty-printing the JSON-RPC RPC events")

(jrpc-define-process-var jrpc-contact nil
  "Method used to contact a server.")

(jrpc-define-process-var jrpc--on-shutdown nil
  "Function run when JSON-RPC server is dying.
Run after running any error handlers for outstanding requests.
A function passed the process object for the server.")

(jrpc-define-process-var jrpc--deferred-actions
    (make-hash-table :test #'equal)
  "Actions deferred to when server is thought to be ready.")

(defun jrpc-outstanding-request-ids (proc)
  "IDs of outstanding JSON-RPC requests for PROC."
  (hash-table-keys (jrpc--request-continuations proc)))

(defun jrpc--make-process (name contact)
  "Make a process from CONTACT.
NAME is a name to give the inferior process or connection.
CONTACT is as explained in `jrpc-connect'.  Returns a process
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
    (set-process-filter proc #'jrpc--process-filter)
    (set-process-sentinel proc #'jrpc--process-sentinel)
    proc))

(defmacro jrpc-obj (&rest what)
  "Make WHAT a suitable argument for `json-encode'."
  (declare (debug (&rest form)))
  ;; FIXME: maybe later actually do something, for now this just fixes
  ;; the indenting of literal plists, i.e. is basically `list'
  `(list ,@what))

;;;###autoload
(cl-defun jrpc-connect (name contact dispatcher &optional on-shutdown)
  "Connect to JSON-RPC server hereafter known as NAME through CONTACT.

NAME is a string naming the server.

CONTACT is a list of strings (COMMAND ARGS...) specifying how to
start a server subprocess to connect to.  If the second element
in the list is an integer number instead of a string, the list is
interpreted as (HOST PORT PARAMETERS...) to connect to an
existing server via TCP, with the remaining PARAMETERS are given
to `open-network-stream's optional arguments.  CONTACT can also
be a live connected process object. In that case its buffer,
filter and sentinel are overwritten by `jrpc-connect'.

ON-SHUTDOWN, if non-nil, is a function called on server exit and
passed the moribund process object as a single argument.

DISPATCHER specifies how the server-invoked methods find their
Elisp counterpart. It is a function passed (PROC METHOD ID PARAMS
as arguments:

PROC is the process object returned by this function.

ID is server identifier for a server request, or nil for a server
notification. In the case of a server request, DISPATCHER is
reponsible using ID and `jrpc-reply' (which see) to reply.

METHOD is a symbol.

PARAMS contains the method parameters: an object, array, or other
type.

`jrpc-connect' returns a process object representing the server."
  (let* ((proc (jrpc--make-process name contact)))
    (setf (jrpc-contact proc) contact
          (jrpc-name proc) name
          (jrpc--dispatcher proc) dispatcher
          (jrpc--on-shutdown proc) on-shutdown)
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)) (erase-buffer) (read-only-mode t) proc))))

(defun jrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (jrpc-log-event proc `(:message "Process state changed" :change ,change))
  (when (not (process-live-p proc))
    (with-current-buffer (jrpc-events-buffer proc)
      (let ((inhibit-read-only t))
        (insert "\n----------b---y---e---b---y---e----------\n")))
    ;; Cancel outstanding timers
    (maphash (lambda (_id triplet)
               (pcase-let ((`(,_success ,_error ,timeout) triplet))
                 (when timeout (cancel-timer timeout))))
             (jrpc--request-continuations proc))
    (unwind-protect
        ;; Call all outstanding error handlers
        (maphash (lambda (_id triplet)
                   (pcase-let ((`(,_success ,error ,_timeout) triplet))
                     (funcall error `(:code -1 :message "Server died"))))
                 (jrpc--request-continuations proc))
      (jrpc-message "Server exited with status %s" (process-exit-status proc))
      (funcall (or (jrpc--on-shutdown proc) #'ignore) proc)
      (delete-process proc))))

(defun jrpc--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (expected-bytes (jrpc--expected-bytes proc)))
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
                                         (jrpc-warn "Invalid JSON: %s %s"
                                                    (cdr oops) (buffer-string))
                                         nil))))
                                (when json-message
                                  ;; Process content in another
                                  ;; buffer, shielding proc buffer from
                                  ;; tamper
                                  (with-temp-buffer
                                    (jrpc--process-receive proc json-message)))))
                          (goto-char message-end)
                          (delete-region (point-min) (point))
                          (setq expected-bytes nil))))
                     (t
                      ;; Message is still incomplete
                      ;;
                      (setq done :waiting-for-more-bytes-in-this-message))))))))
          ;; Saved parsing state for next visit to this filter
          ;;
          (setf (jrpc--expected-bytes proc) expected-bytes))))))

(defun jrpc-events-buffer (process &optional interactive)
  "Display events buffer for current LSP connection PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (jrpc-current-process-or-lose) t))
  (let* ((probe (jrpc--events-buffer process))
         (buffer (or (and (buffer-live-p probe)
                          probe)
                     (let ((buffer (get-buffer-create
                                    (format "*%s events*"
                                            (process-name process)))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (read-only-mode t)
                         (setf (jrpc--events-buffer process) buffer))
                       buffer))))
    (when interactive (display-buffer buffer))
    buffer))

(defun jrpc-log-event (proc message &optional type)
  "Log an jrpc-related event.
PROC is the current process.  MESSAGE is a JSON-like plist.  TYPE
is a symbol saying if this is a client or server originated."
  (with-current-buffer (jrpc-events-buffer proc)
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

(defun jrpc--process-receive (proc message)
  "Process MESSAGE from PROC."
  (pcase-let ((`(,method ,id ,error ,params ,result)
               (condition-case-unless-debug oops
                   (cl-destructuring-bind
                       (&rest args &key method id error params result _jsonrpc)
                       message (list method id error params result))
                 (error (jrpc-warn "Invalid JSONRPC message %s: %s" message
                                   (cdr oops))
                        nil)))
              (continuations))
    (jrpc-log-event proc message 'server)
    (when error (setf (jrpc-status proc) `(,error t)))
    (cond (method
           (unwind-protect
               (funcall (jrpc--dispatcher proc) proc method id params)
             (unless (or (not id)
                         (member id (jrpc--server-request-ids proc)))
               (jrpc-reply
                proc id
                :error (jrpc-obj :code -32603 :message "Internal error")))
             (setf (jrpc--server-request-ids proc)
                   (delete id (jrpc--server-request-ids proc)))))
          ((setq continuations
                 (and id (gethash id (jrpc--request-continuations proc))))
           (let ((timer (nth 2 continuations)))
             (when timer (cancel-timer timer)))
           (remhash id (jrpc--request-continuations proc))
           (if error (funcall (nth 1 continuations) error)
             (funcall (nth 0 continuations) result)))
          (id
           (jrpc-warn "No continuation for id %s" id)))
    (jrpc--call-deferred proc)))

(defun jrpc--process-send (proc message)
  "Send MESSAGE to PROC (ID is optional)."
  (let ((json-object-type 'plist)
        (json (json-encode message)))
    (process-send-string proc (format "Content-Length: %d\r\n\r\n%s"
                                      (string-bytes json)
                                      json))
    (jrpc-log-event proc message 'client)))

(defvar jrpc--next-request-id 0)

(defun jrpc--next-request-id ()
  "Compute the next id for a client request."
  (setq jrpc--next-request-id (1+ jrpc--next-request-id)))

(defun jrpc-forget-pending-continuations (proc)
  "Stop waiting for responses from the current LSP PROC."
  (interactive (list (jrpc-current-process-or-lose)))
  (clrhash (jrpc--request-continuations proc)))

(defun jrpc-clear-status (process)
  "Clear most recent error message from PROCESS."
  (interactive (list (jrpc-current-process-or-lose)))
  (setf (jrpc-status process) nil))

(defun jrpc--call-deferred (proc)
  "Call PROC's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (jrpc--deferred-actions proc))))
    (jrpc-log-event proc `(:running-deferred ,(length actions)))
    (mapc #'funcall (mapcar #'car actions))))

(defvar jrpc-ready-predicates '()
  "Special hook of predicates controlling deferred actions.
If one of these returns nil, a deferrable `jrpc-async-request'
will be deferred.  Each predicate is passed the symbol for the
request request and a process object.")

(cl-defmacro jrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (gensym "jrpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(cl-defun jrpc-async-request (proc
                              method
                              params
                              &rest args
                              &key success-fn error-fn timeout-fn
                              (timeout jrpc-request-timeout)
                              (deferred nil))
  "Make a request to PROC, expecting a reply, return immediately.
The JSONRPC request is formed by METHOD, a symbol, and PARAMS a
JSON object.

The caller can expect SUCCESS-FN or ERROR-FN to be called with a
JSONRPC `:result' or `:error' object, respectively.  If this
doesn't happen after TIMEOUT seconds (defaults to
`jrpc-request-timeout'), the caller can expect TIMEOUT-FN to be
called with no arguments. The default values of SUCCESS-FN,
ERROR-FN and TIMEOUT-FN simply log the events into
`jrpc-events-buffer'.

If DEFERRED is non-nil, maybe defer the request to a future time
when the server is thought to be ready according to
`jrpc-ready-predicates' (which see).  The request might never be
sent at all, in case it is overriden by a new request with
identical DEFERRED and for the same buffer happens in the
meantime.  However, in that situation, the original timeout is
kept.

Return the request ID, or nil, in case the request was deferred."
  (let* ((id (jrpc--next-request-id))
         (existing-timer nil)
         (make-timeout
          (lambda ( )
            (or existing-timer
                (when timeout
                  (run-with-timer
                   timeout nil
                   (lambda ()
                     (remhash id (jrpc--request-continuations proc))
                     (funcall (or timeout-fn
                                  (lambda ()
                                    (jrpc-log-event
                                     proc `(:timed-out ,method :id id
                                                       :params ,params))))))))))))
    (when deferred
      (let* ((buf (current-buffer))
             (existing (gethash (list deferred buf) (jrpc--deferred-actions proc))))
        (when existing (setq existing-timer (cadr existing)))
        (if (run-hook-with-args-until-failure 'jrpc-ready-predicates
                                              deferred proc)
            (remhash (list deferred buf) (jrpc--deferred-actions proc))
          (jrpc-log-event proc `(:deferring ,method :id ,id :params ,params))
          (let* ((buf (current-buffer)) (point (point))
                 (later (lambda ()
                          (when (buffer-live-p buf)
                            (with-current-buffer buf
                              (save-excursion (goto-char point)
                                              (apply #'jrpc-async-request proc
                                                     method params args)))))))
            (puthash (list deferred buf) (list later (funcall make-timeout))
                     (jrpc--deferred-actions proc))
            (cl-return-from jrpc-async-request nil)))))
    ;; Really send it
    ;;
    (jrpc--process-send proc (jrpc-obj :jsonrpc "2.0"
                                       :id id
                                       :method method
                                       :params params))
    (puthash id
             (list (or success-fn
                       (jrpc-lambda (&rest _ignored)
                         (jrpc-log-event
                          proc (jrpc-obj :message "success ignored" :id id))))
                   (or error-fn
                       (jrpc-lambda (&key code message &allow-other-keys)
                         (setf (jrpc-status proc) `(,message t))
                         (jrpc-log-event
                          proc (jrpc-obj :message "error ignored, status set"
                                         :id id :error code))))
                   (funcall make-timeout))
             (jrpc--request-continuations proc))
    id))

(cl-defun jrpc-request (proc method params &key deferred)
  "Make a request to PROC, wait for a reply.
Like `jrpc-async-request' for PROC, METHOD and PARAMS, but
synchronous, i.e. doesn't exit until anything
interesting (success, error or timeout) happens.  Furthermore,
only exit locally (and return the JSONRPC result object) if the
request is successful, otherwise exit non-locally with an error.

DEFERRED is passed to `jrpc-async-request', which see."
  (let* ((tag (cl-gensym "jrpc-request-catch-tag"))
         (retval
          (catch tag
            (jrpc-async-request
             proc method params
             :success-fn (lambda (result) (throw tag `(done ,result)))
             :error-fn (jrpc-lambda (&key code message _data)
                         (throw tag `(error ,(format "%s: %s" code message))))
             :timeout-fn (lambda () (throw tag '(error "Timed out")))
             :deferred deferred)
            (while t (accept-process-output nil 30)))))
    (when (eq 'error (car retval)) (jrpc-error (cadr retval)))
    (cadr retval)))

(cl-defun jrpc-notify (proc method params)
  "Notify PROC of something, don't expect a reply.e"
  (jrpc--process-send proc (jrpc-obj :jsonrpc  "2.0"
                                     :method method
                                     :params params)))

(cl-defun jrpc-reply (proc id &key result error)
  "Reply to PROC's request ID with RESULT or ERROR."
  (push id (jrpc--server-request-ids proc))
  (unless (xor result error) (jrpc-error "Can't pass both RESULT and ERROR!"))
  (jrpc--process-send
   proc `(:jsonrpc  "2.0" :id ,id
                    ,@(when result `(:result ,result))
                    ,@(when error `(:error ,error)))))

(provide 'jrpc)
;;; jrpc.el ends here
