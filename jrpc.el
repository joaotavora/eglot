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
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'warnings)

(defgroup jrpc nil
  "Interaction with Language Server Protocol servers"
  :prefix "jrpc-"
  :group 'applications)

(defcustom jrpc-request-timeout 10
  "How many seconds to wait for a reply from the server."
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
         (buffer (get-buffer-create (format "*%s inferior*" readable-name)))
         (proc
          (cond ((processp contact) contact)
                ((integerp (cadr contact))
                 (apply #'open-network-stream
                        readable-name buffer contact))
                (t
                 (make-process :name readable-name
                               :command contact
                               :connection-type 'pipe
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

(cl-defun jrpc-connect (name contact dispatcher &optional on-shutdown)
  "Connect to JSON-RPC server hereafter known as NAME through CONTACT.

NAME is a string naming the server.

CONTACT is a list of strings (COMMAND ARGS...) specifying how to
start a server subprocess to connect to.  If the second element
in the list is an integer number instead of a string, the list is
interpreted as (HOST PORT PARAMETERS...) to connect to an
existing server via TCP, with the remaining PARAMETERS are given
to `open-network-stream's optional arguments.  CONTACT can also
be a live connected process object.

ON-SHUTDOWN, when non-nil, is a function called on server exit
and passed the moribund process object.

DISPATCHER specifies how the server-invoked methods find their
Elisp counterpart. It is a function which is passed (PROC METHOD
ID PARAMS...) as arguments.

PROC is the process object returned by this function.

ID is server identifier for a server request, or nil for a server
notification.

METHOD is a symbol.

PARAMS contains the method parameters.  If the parameters are a
JSON object, PARAMS... is a plist of the form (KEY1 VALUE1 KEY2
VALUE2...).  It they are an array, a string or a number, the
first and only element of PARAMS is a vector, string or number,
respectively. If the parameters are a single boolean, PARAMS is
either the symbol `:json-false' or `t'. In the case of a server
request, DISPATCHER is reponsible for replying to it with
`jrpc-reply' (which see).

`jrpc-connect' returns a process object representing the server."
  (let* ((proc (jrpc--make-process name contact))
         (buffer (process-buffer proc)))
    (setf (jrpc-contact proc) contact
          (jrpc-name proc) name
          (jrpc--dispatcher proc) dispatcher
          (jrpc--on-shutdown proc) on-shutdown)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (read-only-mode t)
        proc))))

(defun jrpc--process-sentinel (proc change)
  "Called when PROC undergoes CHANGE."
  (jrpc-log-event proc `(:message "Process state changed" :change ,change))
  (when (not (process-live-p proc))
    (with-current-buffer (jrpc-events-buffer proc)
      (let ((inhibit-read-only t))
        (insert "\n----------b---y---e---b---y---e----------\n")))
    ;; Cancel outstanding timers
    (maphash (lambda (_id triplet)
               (cl-destructuring-bind (_success _error timeout) triplet
                 (cancel-timer timeout)))
             (jrpc--request-continuations proc))
    (unwind-protect
        ;; Call all outstanding error handlers
        (maphash (lambda (_id triplet)
                   (cl-destructuring-bind (_success error _timeout) triplet
                     (funcall error :code -1 :message (format "Server died"))))
                 (jrpc--request-continuations proc))
      (jrpc-message "Server exited with status %s" (process-exit-status proc))
      (funcall (or (jrpc--on-shutdown proc) #'identity) proc)
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
                                     (json-message (json-read)))
                                ;; Process content in another buffer,
                                ;; shielding buffer from tamper
                                ;;
                                (with-temp-buffer
                                  (jrpc--process-receive proc json-message))))
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
  (cl-destructuring-bind (&key method id error params &allow-other-keys) message
    (let* ((continuations (and id
                               (not method)
                               (gethash id (jrpc--request-continuations proc)))))
      (jrpc-log-event proc message 'server)
      (when error (setf (jrpc-status proc) `(,error t)))
      (cond (method
             (unwind-protect
                 (if (listp params)
                     (apply (jrpc--dispatcher proc) proc method id params)
                   (funcall (jrpc--dispatcher proc) proc method id params))
               (unless (or (not id)
                           (member id (jrpc--server-request-ids proc)))
                 (jrpc-reply
                  proc id
                  :error (jrpc-obj :code -32603 :message "Internal error")))
               (setf (jrpc--server-request-ids proc)
                     (delete id (jrpc--server-request-ids proc)))))
            (continuations
             (cancel-timer (cl-third continuations))
             (remhash id (jrpc--request-continuations proc))
             (if error
                 (apply (cl-second continuations) error)
               (let ((res (plist-get message :result)))
                 (if (listp res)
                     (apply (cl-first continuations) res)
                   (funcall (cl-first continuations) res)))))
            (id
             (jrpc-warn "Ooops no continuation for id %s" id)))
      (jrpc--call-deferred proc)
      (force-mode-line-update t))))

(defun jrpc--process-send (proc message)
  "Send MESSAGE to PROC (ID is optional)."
  (let ((json (json-encode message)))
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
  `(cl-function (lambda ,cl-lambda-list ,@body)))

(cl-defun jrpc-async-request (proc
                              method
                              params
                              &rest args
                              &key success-fn error-fn timeout-fn
                              (timeout jrpc-request-timeout)
                              (deferred nil))
  "Make a request to PROCESS, expecting a reply.
Return the ID of this request. Wait TIMEOUT seconds for response.
If DEFERRED, maybe defer request to the future, or never at all,
in case a new request with identical DEFERRED and for the same
buffer overrides it. However, if that happens, the original
timeout keeps counting."
  (let* ((id (jrpc--next-request-id))
         (existing-timer nil)
         (make-timeout
          (lambda ( )
            (or existing-timer
                (run-with-timer
                 timeout nil
                 (lambda ()
                   (remhash id (jrpc--request-continuations proc))
                   (funcall (or timeout-fn
                                (lambda ()
                                  (jrpc-log-event
                                   proc `(:timed-out ,method :id id
                                                     :params ,params)))))))))))
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
    ;; Really run it
    ;;
    (puthash id
             (list (or success-fn
                       (jrpc-lambda (&rest _ignored)
                         (jrpc-log-event
                          proc (jrpc-obj :message "success ignored" :id id))))
                   (or error-fn
                       (jrpc-lambda (&key code message &allow-other-keys)
                         (setf (jrpc-status proc) `(,message t))
                         proc (jrpc-obj :message "error ignored, status set"
                                        :id id :error code)))
                   (funcall make-timeout))
             (jrpc--request-continuations proc))
    (jrpc--process-send proc (jrpc-obj :jsonrpc "2.0"
                                       :id id
                                       :method method
                                       :params params))))

(defun jrpc-request (proc method params &optional deferred)
  "Like `jrpc-async-request' for PROC, METHOD and PARAMS, but synchronous.
Meaning only return locally if successful, otherwise exit non-locally.
DEFERRED is passed to `jrpc-async-request', which see."
  ;; Launching a deferred sync request with outstanding changes is a
  ;; bad idea, since that might lead to the request never having a
  ;; chance to run, because `jrpc-ready-predicates'.
  (let* ((tag (cl-gensym "jrpc-request-catch-tag"))
         (retval
          (catch tag
            (jrpc-async-request
             proc method params
             :success-fn (lambda (&rest args)
                           (throw tag
                                  `(done ,(if (vectorp (car args))
                                              (car args) args))))
             :error-fn (jrpc-lambda (&key code message &allow-other-keys)
                         (throw tag
                                `(error ,(format "Oops: %s: %s" code message))))
             :timeout-fn (lambda ()
                           (throw tag
                                  '(error "Timed out")))
             :deferred deferred)
            (while t (accept-process-output nil 30)))))
    (when (eq 'error (car retval)) (jrpc-error (cadr retval)))
    (cadr retval)))

(cl-defun jrpc-notify (process method params)
  "Notify PROCESS of something, don't expect a reply.e"
  (jrpc--process-send process (jrpc-obj :jasonrpc  "2.0"
                                        :method method
                                        :params params)))

(cl-defun jrpc-reply (proc id &key result error)
  "Reply to PROCESS's request ID with RESULT or ERROR."
  (push id (jrpc--server-request-ids proc))
  (jrpc--process-send
   proc`(:jasonrpc  "2.0" :id ,id
                    ,@(when result `(:result ,result))
                    ,@(when error `(:error ,error)))))

(defun jrpc-mapply (fun seq)
  "Apply FUN to every element of SEQ."
  (mapcar (lambda (e) (apply fun e)) seq))

(provide 'jrpc)
;;; jrpc.el ends here
