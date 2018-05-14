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
;; code	message	meaning
;; -32700	Parse error	Invalid JSON was received by the server.
;; An error occurred on the server while parsing the JSON text.
;; -32600	Invalid Request	The JSON sent is not a valid Request object.
;; -32601	Method not found	The method does not exist / is not available.
;; -32602	Invalid params	Invalid method parameter(s).
;; -32603	Internal error	Internal JSON-RPC error.

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

(jrpc-define-process-var jrpc--method-prefix nil
  "Emacs-lisp function prefix for server-invoked methods.")

(jrpc-define-process-var jrpc-status `(:unknown nil)
  "Status as declared by the server.
A list (WHAT SERIOUS-P).")

(jrpc-define-process-var jrpc--expected-bytes nil
  "How many bytes declared by server")

(jrpc-define-process-var jrpc--pending-continuations (make-hash-table)
  "A hash table of request ID to continuation lambdas")

(jrpc-define-process-var jrpc--events-buffer nil
  "A buffer pretty-printing the JSON-RPC RPC events")

(jrpc-define-process-var jrpc-contact nil
  "Method used to contact a server.")

(jrpc-define-process-var jrpc--shutdown-hook nil
  "Hook run when JSON-RPC server is dying.
Run after running any error handlers for outstanding requests.
Each hook function is passed the process object for the server.")

(jrpc-define-process-var jrpc--deferred-actions
    (make-hash-table :test #'equal)
  "Actions deferred to when server is thought to be ready.")

(defun jrpc-outstanding-request-ids (proc)
  "IDs of outstanding JSON-RPC requests for PROC."
  (hash-table-keys (jrpc--pending-continuations proc)))

(defun jrpc--make-process (name contact)
  "Make a process from CONTACT.
NAME is a name to give the inferior process or connection.
CONTACT is as `jrpc-contact'.  Returns a process object."
  (let* ((readable-name (format "JSON-RPC server (%s)" name)                                                            )
         (buffer (get-buffer-create
                  (format "*%s inferior*" readable-name)))
         singleton
         (proc
          (if (and (setq singleton (and (null (cdr contact)) (car contact)))
                   (string-match "^[\s\t]*\\(.*\\):\\([[:digit:]]+\\)[\s\t]*$"
                                 singleton))
              (open-network-stream readable-name
                                   buffer
                                   (match-string 1 singleton)
                                   (string-to-number
                                    (match-string 2 singleton)))
            (make-process :name readable-name
                          :buffer buffer
                          :command contact
                          :connection-type 'pipe
                          :stderr (get-buffer-create (format "*%s stderr*"
                                                             name))))))
    (set-process-filter proc #'jrpc--process-filter)
    (set-process-sentinel proc #'jrpc--process-sentinel)
    proc))

(defmacro jrpc-obj (&rest what)
  "Make WHAT a suitable argument for `json-encode'."
  (declare (debug (&rest form)))
  ;; FIXME: maybe later actually do something, for now this just fixes
  ;; the indenting of literal plists, i.e. is basically `list'
  `(list ,@what))

(cl-defun jrpc-connect (name contact prefix &optional on-shutdown)
  "Connect to JSON-RPC server hereafter known as NAME through CONTACT.

NAME is a string naming the server.

CONTACT is either a list of strings (a shell command and
arguments), or a list of a single string of the form
<host>:<port>.

PREFIX specifies how the server-invoked methods find their Elisp
counterpart. If a server invokes method \"FooBar\" and PREFIX is
\"fancy-mode-\", then the function `fancy-mode-FooBar' will be
called with arguments (PROCESS [JSON]). JSON is either a plist of
key-value pairs or, for JSON arrays, a non-list sequence.

ON-SHUTDOWN, when non-nil, is a function called on server exit
and passed the moribund process object.

Returns a process object representing the server."
  (let* ((proc (jrpc--make-process name contact))
         (buffer (process-buffer proc)))
    (setf (jrpc-contact proc) contact
          (jrpc-name proc) name
          (jrpc--method-prefix proc) prefix
          (jrpc--shutdown-hook proc) on-shutdown)
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
             (jrpc--pending-continuations proc))
    (unwind-protect
        ;; Call all outstanding error handlers
        (maphash (lambda (_id triplet)
                   (cl-destructuring-bind (_success error _timeout) triplet
                     (funcall error :code -1 :message (format "Server died"))))
                 (jrpc--pending-continuations proc))
      (jrpc-message "Server exited with status %s" (process-exit-status proc))
      (funcall (or (jrpc--shutdown-hook proc) #'identity) proc)
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
  (cl-destructuring-bind (&key method id error &allow-other-keys) message
    (let* ((continuations (and id
                               (not method)
                               (gethash id (jrpc--pending-continuations proc)))))
      (jrpc-log-event proc message 'server)
      (when error (setf (jrpc-status proc) `(,error t)))
      (cond (method
             ;; a server notification or a server request
             (let* ((handler-sym (intern (concat (jrpc--method-prefix proc)
                                                 method))))
               (if (functionp handler-sym)
                   (apply handler-sym proc (append
                                            (plist-get message :params)
                                            (if id `(:id ,id))))
                 (jrpc-warn "No implementation of method %s yet" method)
                 (when id
                   (jrpc-reply
                    proc id
                    :error (jrpc-obj :code -32601
                                     :message "Method unimplemented"))))))
            (continuations
             (cancel-timer (cl-third continuations))
             (remhash id (jrpc--pending-continuations proc))
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

(defun jrpc-process-send (proc message)
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

(defun jrpc-forget-pending-continuations (process)
  "Stop waiting for responses from the current LSP PROCESS."
  (interactive (list (jrpc-current-process-or-lose)))
  (clrhash (jrpc--pending-continuations process)))

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
                   (remhash id (jrpc--pending-continuations proc))
                   (funcall (or timeout-fn
                                (lambda ()
                                  (jrpc-error
                                   "Tired of waiting for reply to %s, id=%s"
                                   method id))))))))))
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
             (jrpc--pending-continuations proc))
    (jrpc-process-send proc (jrpc-obj :jsonrpc "2.0"
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
  (jrpc-process-send process (jrpc-obj :jasonrpc  "2.0"
                                       :method method
                                       :params params)))

(cl-defun jrpc-reply (process id &key result error)
  "Reply to PROCESS's request ID with MESSAGE."
  (jrpc-process-send
   process `(:jasonrpc  "2.0" :id ,id
                        ,@(when result `(:result ,result))
                        ,@(when error `(:error ,error)))))

(defun jrpc-mapply (fun seq)
  "Apply FUN to every element of SEQ."
  (mapcar (lambda (e) (apply fun e)) seq))

(provide 'jrpc)
;;; jrpc.el ends here
