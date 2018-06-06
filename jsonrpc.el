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
;; To approach this agnosticism, jsonrpc.el uses objects derived from
;; an abstract class, `jsonrpc-connection' to represent the connection
;; to the remote JSON endpoint.  Abstract operations such as sending
;; and receiving are modelled as generic functions, so that users of
;; JSONRPC working in complicated transport infrastructures can
;; specify a subclass of `jsonrpc-connection' and write specific
;; methods for it.  Nevertheless, jsonrpc.el comes built-in with
;; `jsonrpc-process-connection' class that works both with local
;; subprocesses (through stdin/stdout) and TCP hosts (using
;; sockets). This uses some simple HTTP-style envelopping for JSON
;; objects travelling through the wire.
;;
;; The entry point `jsonrpc-connect', returns one of these objects by
;; default.  It is passed a name identifying the connection and a
;; "contact", which will determine the connection type to make.  This
;; contact can a list of strings (a command and arguments for creating
;; subprocesses) or a list of the form (HOST PORT-NUMBER PARAMS...)
;; for connecting via TCP.  For the providing the aforementioned
;; flexibility, it can also be a any object of a subclass of
;; `jsonrpc-connection'.
;;
;; `jsonrpc-connect' returns a connection upon connection.  This value
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
;;;; Usage example:
;;
;; Finally, here's an example Emacs JSONRPC server that offers a (very
;; small) subset of Elisp for remote calling:
;;
;; (defvar server-server) (defvar server-endpoint)
;; (defvar server-allowed-functions '(+ - * / vconcat append sit-for))
;;
;; (setq server-server
;;       (make-network-process
;;        :name "Emacs RPC server" :server t :host "localhost" :service 44444
;;        :log (lambda (_server client _message)
;;               (jsonrpc-connect
;;                (process-name client)
;;                (make-instance 'jsonrpc-process-connection :process client)
;;                (lambda (endpoint method id params)
;;                  (unless (memq method '(+ - * / vconcat append sit-for))
;;                    (signal 'jsonrpc-error `((jsonrpc-error-message
;;                                              . "Sorry, this isn't allowed")
;;                                             (jsonrpc-error-code . -32601))))
;;                  (jsonrpc-reply endpoint id :result
;;                                 (apply method (append params nil))))))))
;;
;; (setq server-endpoint (jsonrpc-connect
;;                        "Emacs RPC client" '("localhost" 9393)
;;                        (lambda (endpoint method id &rest params)
;;                          (message "server wants to %s" method))))
;;
;; ;; returns 3
;; (jsonrpc-request server-endpoint '+ '(1 2))
;; ;; errors with -32601
;; (jsonrpc-request server-endpoint 'delete-directory "~/tmp")
;; ;; signals an -32603 JSONRPC error
;; (jsonrpc-request server-endpoint '+ '(a 2))
;; ;; times out
;; (jsonrpc-request server-endpoint 'sit-for '(5))
;; ;; stretching it, but works
;; (jsonrpc-request server-endpoint 'vconcat '([1 2 3] [3 4 5]))
;; ;; json.el can't serialize this, json.el errors and request isn't sent
;; (jsonrpc-request server-endpoint 'append '((1 2 3) (3 4 5)))
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'eieio)
(require 'subr-x)
(require 'warnings)
(require 'pcase)
(require 'ert) ; to escape a `condition-case-unless-debug'
(require 'array) ; xor

(defvar jsonrpc-find-connection-functions nil
  "Special hook to find an active JSON-RPC connection.")

(defun jsonrpc-current-connection ()
  "The current logical JSON-RPC connection."
  (run-hook-with-args-until-success 'jsonrpc-find-connection-functions))

(defun jsonrpc-current-connection-or-lose ()
  "Return the current JSON-RPC connection or error."
  (or (jsonrpc-current-connection)
      (jsonrpc-error "No current JSON-RPC connection")))

(define-error 'jsonrpc-error "jsonrpc-error")

(defun jsonrpc-error (format &rest args)
  "Error out with FORMAT and ARGS.
If invoked inside a dispatcher function, this function is suitable
for replying to the remote endpoint with a -32603 error code and
FORMAT as the message."
  (signal 'error
          (list (apply #'format-message (concat "[jsonrpc] " format) args))))

(defun jsonrpc-message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[jsonrpc] %s" (concat "[jsonrpc] %s" (apply #'format format args))))

(defun jsonrpc--debug (server format &rest args)
  "Debug message for SERVER with FORMAT and ARGS."
  (jsonrpc-log-event
   server (if (stringp format)`(:message ,(format format args)) format)))

(defun jsonrpc-warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'jsonrpc-message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'jsonrpc
                     (apply #'format format args)
                     :warning)))

(defclass jsonrpc-connection ()
  ((name
    :accessor jsonrpc-name
    :documentation "A name for the connection")
   (-dispatcher
    :accessor jsonrpc--dispatcher
    :documentation "Emacs-lisp function for server-invoked methods.")
   (status
    :initform `(:unknown nil) :accessor jsonrpc-status
    :documentation "Status (WHAT SERIOUS-P) as declared by the server.")
   (-request-continuations
    :initform (make-hash-table)
    :accessor jsonrpc--request-continuations
    :documentation "A hash table of request ID to continuation lambdas.")
   (-server-request-ids
    :accessor jsonrpc--server-request-ids
    :documentation "Server-initiated request ids that client hasn't replied to.")
   (-events-buffer
    :accessor jsonrpc--events-buffer
    :documentation "A buffer pretty-printing the JSON-RPC RPC events")
   (contact
    :accessor jsonrpc-contact
    :documentation "Method used to contact a server.")
   (-on-shutdown
    :accessor jsonrpc--on-shutdown
    :documentation "Function run when JSONRPC server is dying.")
   (-deferred-actions
    :initform (make-hash-table :test #'equal)
    :accessor jsonrpc--deferred-actions
    :documentation "Map (DEFERRED BUF) to (FN TIMER ID).  FN is\
a saved DEFERRED `async-request' from BUF, to be sent not later\
than TIMER as ID.")
   (-next-request-id
    :initform 0
    :accessor jsonrpc--next-request-id
    :documentation "Next number used for a request")))

(defclass jsonrpc-process-connection (jsonrpc-connection)
  ((-process
    :initarg :process :accessor jsonrpc--process
    ;; :initform (error "`:process' is a required initarg") ; doesn't work
    :documentation "Process object wrapped by the this connection.")
   (-expected-bytes
    :accessor jsonrpc--expected-bytes
    :documentation "How many bytes declared by server")))

(defmacro jsonrpc-obj (&rest what)
  "Make WHAT a suitable argument for `json-encode'."
  (declare (debug (&rest form)))
  ;; FIXME: maybe later actually do something, for now this just fixes
  ;; the indenting of literal plists, i.e. is basically `list'
  `(list ,@what))

;;;###autoload
(cl-defun jsonrpc-connect (name contact dispatcher &optional on-shutdown)
  "Connect to JSONRPC endpoint NAME through CONTACT.

This function creates an object (subprocess or network
connection) wrapped in a `jsonrpc-process-connection' object.

NAME is a string naming the connection.

In the most common case CONTACT is a list of strings (COMMAND
ARGS...) specifying how to locally start a server subprocess to
talk to via JSONRPC.  If the second element in the list is an
integer number instead of a string, the list is interpreted
as (HOST PORT PARAMETERS...) and an attempt is made to contact
HOST on PORT, with the remaining PARAMETERS are given to
`open-network-stream's optional arguments.

Moreover, if in either of these cases the first element in the
list is a symbol, that symbol is taken to name a subclass of
`jsonrpc-process-connection' which is used to create the object
returned by this function. The remaining arguments are processed
as described in the previous paragraph.

CONTACT can also be a an object of the type
`jsonrpc-process-connection' (or a subclass thereof) containing a
pre-connected process object. In that case the processes buffer,
filter and sentinel are henceforth overwritten and managed by
`jsonrpc-connect'.

ON-SHUTDOWN, if non-nil, is a function called on server exit and
passed the moribund connection object as a single argument.

DISPATCHER specifies how the server-invoked methods find their
Elisp counterpart. It is a function passed (PROC METHOD ID PARAMS
as arguments. PROC is the connection object returned by this
function. ID is the server identifier for a server request, or
nil for a server notification. METHOD is a symbol. PARAMS
contains the method parameters as JSON data.

If ID is non-nil, DISPATCHER is expected to reply to the
request. If it doesn't, or if it signals an error before doing
so, jsonrpc.el will automatically reply with an error. If DISPATCHER
signals an error with alist elements `jsonrpc-error-message' and
`jsonrpc-error-code' in its DATA, the corresponding elements are
used for the automated error reply.

If successful, `jsonrpc-connect' returns a
`jsonrpc-process-connection' object representing the remote
endpoint."
  (let* ((readable-name (format "JSON-RPC server (%s)" name))
         (buffer (get-buffer-create (format "*%s output*" readable-name)))
         (stderr)
         (original-contact contact)
         (connection
          (cond
           ((cl-typep contact 'jsonrpc-process-connection)
            (unless (process-live-p (jsonrpc--process contact))
              (error "%s doesn't have a live process" contact))
            contact)
           ((listp contact)
            (make-instance
             (if (symbolp (car contact))
                 (prog1 (car contact) (setq contact (cdr contact)))
               'jsonrpc-process-connection)
             :process
             (cond ((integerp (cadr contact))
                    (apply #'open-network-stream readable-name buffer contact))
                   (t
                    (make-process :name readable-name
                                  :command contact
                                  :connection-type 'pipe
                                  :coding 'no-conversion
                                  :stderr (setq stderr
                                                (get-buffer-create
                                                 (format "*%s stderr*" name))))))))))
         (proc (jsonrpc--process connection)))
    (set-process-buffer proc buffer)
    (process-put proc 'jsonrpc-stderr stderr)
    (set-marker (process-mark proc) (with-current-buffer buffer (point-min)))
    (set-process-filter proc #'jsonrpc--process-filter)
    (set-process-sentinel proc #'jsonrpc--process-sentinel)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer) (read-only-mode t) proc))
    (process-put proc 'jsonrpc-connection connection)
    (setf (jsonrpc--process connection) proc
          (jsonrpc-contact connection) original-contact
          (jsonrpc-name connection) name
          (jsonrpc--dispatcher connection) dispatcher
          (jsonrpc--on-shutdown connection) (or on-shutdown #'ignore))
    connection))

(defun jsonrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (let ((connection (process-get proc 'jsonrpc-connection)))
    (jsonrpc--debug connection `(:message "Connection state changed" :change ,change))
    (when (not (process-live-p proc))
      (with-current-buffer (jsonrpc-events-buffer connection)
        (let ((inhibit-read-only t))
          (insert "\n----------b---y---e---b---y---e----------\n")))
      ;; Cancel outstanding timers
      (maphash (lambda (_id triplet)
                 (pcase-let ((`(,_success ,_error ,timeout) triplet))
                   (when timeout (cancel-timer timeout))))
               (jsonrpc--request-continuations connection))
      (unwind-protect
          ;; Call all outstanding error handlers
          (maphash (lambda (_id triplet)
                     (pcase-let ((`(,_success ,error ,_timeout) triplet))
                       (funcall error `(:code -1 :message "Server died"))))
                   (jsonrpc--request-continuations connection))
        (jsonrpc-message "Server exited with status %s" (process-exit-status proc))
        (delete-process proc)
        (funcall (jsonrpc--on-shutdown connection) connection)))))

(defun jsonrpc--process-filter (proc string)
  "Called when new data STRING has arrived for PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let* ((inhibit-read-only t)
             (connection (process-get proc 'jsonrpc-connection))
             (expected-bytes (jsonrpc--expected-bytes connection)))
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
                                    (jsonrpc--connection-receive connection
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
          (setf (jsonrpc--expected-bytes connection) expected-bytes))))))

(defun jsonrpc-events-buffer (connection &optional interactive)
  "Display events buffer for current JSONRPC connection CONNECTION.
INTERACTIVE is t if called interactively."
  (interactive (list (jsonrpc-current-connection-or-lose) t))
  (let* ((probe (jsonrpc--events-buffer connection))
         (buffer (or (and (buffer-live-p probe)
                          probe)
                     (let ((buffer (get-buffer-create
                                    (format "*%s events*"
                                            (jsonrpc-name connection)))))
                       (with-current-buffer buffer
                         (buffer-disable-undo)
                         (read-only-mode t)
                         (setf (jsonrpc--events-buffer connection) buffer))
                       buffer))))
    (when interactive (display-buffer buffer))
    buffer))

(defun jsonrpc-stderr-buffer (connection)
  "Pop to stderr of CONNECTION, if it exists, else error."
  (interactive (list (jsonrpc-current-connection-or-lose)))
  (if-let ((b (process-get (jsonrpc--process connection) 'jsonrpc-stderr)))
      (pop-to-buffer b) (user-error "[eglot] No stderr buffer!")))

(defun jsonrpc-log-event (connection message &optional type)
  "Log an jsonrpc-related event.
CONNECTION is the current connection.  MESSAGE is a JSON-like
plist.  TYPE is a symbol saying if this is a client or server
originated."
  (with-current-buffer (jsonrpc-events-buffer connection)
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

(defun jsonrpc--connection-receive (connection message)
  "Connection MESSAGE from CONNECTION."
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
    (jsonrpc-log-event connection message 'server)
    (when error (setf (jsonrpc-status connection) `(,error t)))
    (cond (method
           (let ((debug-on-error
                  (and debug-on-error
                       (not (ert-running-test)))))
             (condition-case-unless-debug oops
                 (funcall (jsonrpc--dispatcher connection)
                          connection (intern method) id params)
               (error (setq lisp-err oops))))
           (unless (or (member id (jsonrpc--server-request-ids connection))
                       (not (or id lisp-err)))
             (jsonrpc-reply
              connection id
              :error (jsonrpc-obj
                      :code (or (alist-get 'jsonrpc-error-code (cdr lisp-err))
                                -32603)
                      :message (or (alist-get 'jsonrpc-error-message
                                              (cdr lisp-err))
                                   "Internal error"))))
           (setf (jsonrpc--server-request-ids connection)
                 (delete id (jsonrpc--server-request-ids connection))))
          ((setq continuations
                 (and id (gethash id (jsonrpc--request-continuations connection))))
           (let ((timer (nth 2 continuations)))
             (when timer (cancel-timer timer)))
           (remhash id (jsonrpc--request-continuations connection))
           (if error (funcall (nth 1 continuations) error)
             (funcall (nth 0 continuations) result)))
          (id
           (jsonrpc-warn "No continuation for id %s" id)))
    (jsonrpc--call-deferred connection)))

(cl-defmethod jsonrpc-connection-send ((connection jsonrpc-process-connection)
                                       message)
  "Send MESSAGE, a JSON object, to CONNECTION."
  (let ((json-object-type 'plist)
        (json (json-encode message)))
    (process-send-string (jsonrpc--process connection)
                         (format "Content-Length: %d\r\n\r\n%s"
                                 (string-bytes json)
                                 json))
    (jsonrpc-log-event connection message 'client)))

(defun jsonrpc-forget-pending-continuations (connection)
  "Stop waiting for responses from the current JSONRPC CONNECTION."
  (interactive (list (jsonrpc-current-connection-or-lose)))
  (clrhash (jsonrpc--request-continuations connection)))

(defun jsonrpc-clear-status (connection)
  "Clear most recent error message from CONNECTION."
  (interactive (list (jsonrpc-current-connection-or-lose)))
  (setf (jsonrpc-status connection) nil))

(defun jsonrpc--call-deferred (connection)
  "Call CONNECTION's deferred actions, who may again defer themselves."
  (when-let ((actions (hash-table-values (jsonrpc--deferred-actions connection))))
    (jsonrpc--debug connection `(:maybe-run-deferred ,(mapcar #'caddr actions)))
    (mapc #'funcall (mapcar #'car actions))))

(cl-defgeneric jsonrpc-connection-ready-p (connection what) ;; API
  "Tell if CONNECTION is ready for WHAT in current buffer.
If it isn't, a deferrable `jsonrpc-async-request' will be
deferred to the future.  By default, all connections are ready
for sending requests immediately."
  (:method (_s _what) t)) ; by default all connections are ready

(cl-defmacro jsonrpc-lambda (cl-lambda-list &body body)
  (declare (indent 1) (debug (sexp &rest form)))
  (let ((e (gensym "jsonrpc-lambda-elem")))
    `(lambda (,e) (apply (cl-function (lambda ,cl-lambda-list ,@body)) ,e))))

(defconst jrpc-default-request-timeout 10
  "Time in seconds before timing out a JSONRPC request.")

(cl-defun jsonrpc-async-request (connection
                                 method
                                 params
                                 &rest args
                                 &key _success-fn _error-fn
                                 _timeout-fn
                                 _timeout _deferred)
  "Make a request to CONNECTION, expecting a reply, return immediately.
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
`jsonrpc-connection-ready-p' (which see).  The request might
never be sent at all, in case it is overridden in the meantime by
a new request with identical DEFERRED and for the same buffer.
However, in that situation, the original timeout is kept.

Returns nil."
  (apply #'jsonrpc--async-request-1 connection method params args)
  nil)

(cl-defun jsonrpc--async-request-1 (connection
                                    method
                                    params
                                    &rest args
                                    &key success-fn error-fn timeout-fn
                                    (timeout jrpc-default-request-timeout)
                                    (deferred nil))
  "Does actual work for `jsonrpc-async-request'.

Return a list (ID TIMER). ID is the new request's ID, or nil if
the request was deferred. TIMER is a timer object set (or nil, if
TIMEOUT is nil)."
  (pcase-let* ((buf (current-buffer)) (point (point))
               (`(,_ ,timer ,old-id)
                (and deferred (gethash (list deferred buf)
                                       (jsonrpc--deferred-actions connection))))
               (id (or old-id (cl-incf (jsonrpc--next-request-id connection))))
               (make-timer
                (lambda ( )
                  (when timeout
                    (run-with-timer
                     timeout nil
                     (lambda ()
                       (remhash id (jsonrpc--request-continuations connection))
                       (remhash (list deferred buf)
                                (jsonrpc--deferred-actions connection))
                       (if timeout-fn (funcall timeout-fn)
                         (jsonrpc--debug
                          connection `(:timed-out ,method :id ,id
                                                  :params ,params)))))))))
    (when deferred
      (if (jsonrpc-connection-ready-p connection deferred)
          ;; Server is ready, we jump below and send it immediately.
          (remhash (list deferred buf) (jsonrpc--deferred-actions connection))
        ;; Otherwise, save in `eglot--deferred-actions' and exit non-locally
        (unless old-id
          (jsonrpc--debug connection `(:deferring ,method :id ,id :params
                                                  ,params)))
        (puthash (list deferred buf)
                 (list (lambda ()
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (save-excursion (goto-char point)
                                             (apply #'jsonrpc-async-request
                                                    connection
                                                    method params args)))))
                       (or timer (setq timer (funcall make-timer))) id)
                 (jsonrpc--deferred-actions connection))
        (cl-return-from jsonrpc--async-request-1 (list id timer))))
    ;; Really send it
    ;;
    (jsonrpc-connection-send connection (jsonrpc-obj :jsonrpc "2.0"
                                                     :id id
                                                     :method method
                                                     :params params))
    (puthash id
             (list (or success-fn
                       (jsonrpc-lambda (&rest _ignored)
                         (jsonrpc--debug
                          connection (jsonrpc-obj :message "success ignored" :id id))))
                   (or error-fn
                       (jsonrpc-lambda (&key code message &allow-other-keys)
                         (setf (jsonrpc-status connection) `(,message t))
                         (jsonrpc--debug
                          connection (jsonrpc-obj :message "error ignored, status set"
                                                  :id id :error code))))
                   (setq timer (funcall make-timer)))
             (jsonrpc--request-continuations connection))
    (list id timer)))

(cl-defun jsonrpc-request (connection method params &key deferred timeout)
  "Make a request to CONNECTION, wait for a reply.
Like `jsonrpc-async-request' for CONNECTION, METHOD and PARAMS, but
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
                 (jsonrpc--async-request-1
                  connection method params
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
                  :deferred deferred
                  :timeout timeout))
                (while t (accept-process-output nil 30)))
            (pcase-let* ((`(,id ,timer) id-and-timer))
              (remhash id (jsonrpc--request-continuations connection))
              (remhash (list deferred (current-buffer))
                       (jsonrpc--deferred-actions connection))
              (when timer (cancel-timer timer))))))
    (when (eq 'error (car retval))
      (signal 'jsonrpc-error
              (cons
               (format "request id=%s failed:" (car id-and-timer))
               (cdr retval))))
    (cadr retval)))

(cl-defun jsonrpc-notify (connection method params)
  "Notify CONNECTION of something, don't expect a reply.e"
  (jsonrpc-connection-send connection (jsonrpc-obj :jsonrpc  "2.0"
                                                   :method method
                                                   :params params)))

(cl-defun jsonrpc-reply (connection id &key (result nil result-supplied-p) error)
  "Reply to CONNECTION's request ID with RESULT or ERROR."
  (unless id (jsonrpc-error "Need a non-nil ID"))
  (unless (xor result-supplied-p error)
    (jsonrpc-error "Can't pass both RESULT and ERROR!"))
  (push id (jsonrpc--server-request-ids connection))
  (jsonrpc-connection-send
   connection `(:jsonrpc  "2.0" :id ,id
                          ,@(when result `(:result ,result))
                          ,@(when error `(:error ,error)))))

(provide 'jsonrpc)
;;; jsonrpc.el ends here
