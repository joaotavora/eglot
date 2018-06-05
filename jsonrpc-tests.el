;;; jsonrpc-tests.el --- tests for jsonrpc.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
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

;; 

;;; Code:

(require 'ert)
(require 'jsonrpc)
(require 'eieio)

(defclass jsonrpc-test-conn (jsonrpc-process-connection)
  ((hold-deferred :initform t :accessor jsonrpc--hold-deferred)
   (shutdown-complete-p :initform nil :accessor jsonrpc--shutdown-complete-p)))

(cl-defmacro jsonrpc--with-emacsrpc-fixture ((endpoint-sym) &body body)
  (declare (indent 1) (debug t))
  (let ((server (gensym "server-")) (listen-server (gensym "listen-server-")))
    `(let* (,server
            (,listen-server
             (make-network-process
              :name "Emacs RPC server" :server t :host "localhost"
              :service 44444
              :log (lambda (_server client _message)
                     (setq ,server
                           (jsonrpc-connect
                            (process-name client)
                            (make-instance 'jsonrpc-test-conn :process client)
                            (lambda (endpoint method id params)
                              (unless (memq method '(+ - * / vconcat append
                                                       sit-for ignore))
                                (signal 'jsonrpc-error
                                        `((jsonrpc-error-message
                                           . "Sorry, this isn't allowed")
                                          (jsonrpc-error-code . -32601))))
                              (jsonrpc-reply endpoint id :result
                                             (apply method (append params nil))))
                            (lambda (conn)
                              (setf (jsonrpc--shutdown-complete-p conn) t)))))))
            (,endpoint-sym (jsonrpc-connect
                            "Emacs RPC client"
                            '(jsonrpc-test-conn "localhost" 44444)
                            (lambda (_endpoint method _id &rest _params)
                              (message "server wants to %s" method))
                            (lambda (conn)
                              (setf (jsonrpc--shutdown-complete-p conn) t)))))
       (unwind-protect
           (progn
             (cl-assert ,endpoint-sym)
             ,@body
             (kill-buffer (jsonrpc--events-buffer ,endpoint-sym))
             (when ,server
               (kill-buffer (jsonrpc--events-buffer ,server))))
         (unwind-protect
             (cl-loop do (delete-process (jsonrpc--process ,endpoint-sym))
                      while (progn (accept-process-output nil 0.1)
                                   (not (jsonrpc--shutdown-complete-p ,endpoint-sym)))
                      do (jsonrpc-message
                          "test client is still running, waiting"))
           (unwind-protect
               (cl-loop do (delete-process (jsonrpc--process ,server))
                        while (progn (accept-process-output nil 0.1)
                                     (not (jsonrpc--shutdown-complete-p ,server)))
                        do (jsonrpc-message
                            "test server is still running, waiting"))
             (cl-loop do (delete-process ,listen-server)
                      while (progn (accept-process-output nil 0.1)
                                   (process-live-p ,listen-server))
                      do (jsonrpc-message
                          "test listen-server is still running, waiting"))))))))

(ert-deftest returns-3 ()
  "returns 3"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should (= 3 (jsonrpc-request conn '+ '(1 2))))))

(ert-deftest errors-with--32601 ()
  "errors with -32601"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (condition-case err
        (progn
          (jsonrpc-request conn 'delete-directory "~/tmp")
          (ert-fail "A `jsonrpc-error' should have been signalled!"))
      (jsonrpc-error
       (should (= -32601 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest signals-an--32603-JSONRPC-error ()
  "signals an -32603 JSONRPC error"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (condition-case err
        (progn
          (jsonrpc-request conn '+ '(a 2))
          (ert-fail "A `jsonrpc-error' should have been signalled!"))
      (jsonrpc-error
       (should (= -32603 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest times-out ()
  "times out"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should-error
     (jsonrpc-request conn 'sit-for '(5) :timeout 2))))

(ert-deftest stretching-it-but-works ()
  "stretching it, but works"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should (equal
             [1 2 3 3 4 5]
             (jsonrpc-request conn 'vconcat '([1 2 3] [3 4 5]))))))

(ert-deftest json-el-cant-serialize-this ()
  "json.el can't serialize this, json.el errors and request isn't sent"
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should-error
     (jsonrpc-request conn 'append '((1 2 3) (3 4 5))))))

(cl-defmethod jsonrpc-connection-ready-p
  ((conn jsonrpc-test-conn) what)
  (and (cl-call-next-method)
       (or (not (string-match "deferred" what))
           (not (jsonrpc--hold-deferred conn)))))

(ert-deftest deferred-action-intime ()
  "Deferred request barely makes it after event clears a flag."
  ;; Send an async request, which returns immediately. However the
  ;; success fun which sets the flag only runs after some time.
  (jsonrpc--with-emacsrpc-fixture (conn)
    (jsonrpc-async-request conn
                           'sit-for '(0.5)
                           :success-fn
                           (lambda (_result)
                             (setf (jsonrpc--hold-deferred conn) nil)))
    ;; Now wait for an answer to this request, which should be sent as
    ;; soon as the previous one is answered.
    (should
     (= 3 (jsonrpc-request conn '+ '(1 2)
                           :deferred "deferred"
                           :timeout 1)))))

(ert-deftest deferred-action-toolate ()
  "Deferred request times out, flag cleared too late."
  ;; Send an async request, which returns immediately. However the
  ;; success fun which sets the flag only runs after some time.
  (jsonrpc--with-emacsrpc-fixture (conn)
    (let (n-deferred-1 n-deferred-2)
      (jsonrpc-async-request
       conn
       'sit-for '(0.1)
       :success-fn
       (lambda (_result)
         (setq n-deferred-1 (hash-table-count (jsonrpc--deferred-actions conn)))))
      (should-error
       (jsonrpc-request conn 'ignore '("first deferred")
                        :deferred "first deferred"
                        :timeout 0.5)
       :type 'jsonrpc-error)
      (jsonrpc-async-request
       conn
       'sit-for '(0.1)
       :success-fn
       (lambda (_result)
         (setq n-deferred-2 (hash-table-count (jsonrpc--deferred-actions conn)))
         (setf (jsonrpc--hold-deferred conn) nil)))
      (jsonrpc-async-request conn 'ignore '("second deferred")
                             :deferred "second deferred"
                             :timeout 1)
      (jsonrpc-request conn 'ignore '("third deferred")
                       :deferred "third deferred"
                       :timeout 1)
      (should (eq 1 n-deferred-1))
      (should (eq 2 n-deferred-2))
      (should (eq 0 (hash-table-count (jsonrpc--deferred-actions conn)))))))

(ert-deftest deferred-action-timeout ()
  "Deferred request fails because noone clears the flag."
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should-error
     (jsonrpc-request conn '+ '(1 2)
                      :deferred "deferred-testing" :timeout 0.5)
     :type 'jsonrpc-error)
    (should
     (= 3 (jsonrpc-request conn '+ '(1 2)
                           :timeout 0.5)))))

(provide 'jsonrpc-tests)
;;; jsonrpc-tests.el ends here
