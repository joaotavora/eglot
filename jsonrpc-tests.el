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

(cl-defmacro jsonrpc--with-emacsrpc-fixture ((endpoint-sym) &body body)
  (declare (indent 1) (debug t))
  (let ((server (gensym)))
    `(let* ((,server (make-network-process
                      :name "Emacs RPC server" :server t :host "localhost" :service 44444
                      :log (lambda (_server client _message)
                             (jsonrpc-connect
                              (process-name client) client
                              (lambda (endpoint method id params)
                                (unless (memq method '(+ - * / vconcat append sit-for))
                                  (signal 'jsonrpc-error `((jsonrpc-error-message
                                                            . "Sorry, this isn't allowed")
                                                           (jsonrpc-error-code . -32601))))
                                (jsonrpc-reply endpoint id :result
                                               (apply method (append params nil))))))))
            (,endpoint-sym (jsonrpc-connect
                            "Emacs RPC client" '("localhost" 44444)
                            (lambda (_endpoint method _id &rest _params)
                              (message "server wants to %s" method)))))
       (unwind-protect
           ,@body
         (unwind-protect
             (delete-process ,server)
           (delete-process (jsonrpc--process ,endpoint-sym)))))))

(ert-deftest returns-3 ()
  "returns 3"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (should (= 3 (jsonrpc-request server-endpoint '+ '(1 2))))))

(ert-deftest errors-with--32601 ()
  "errors with -32601"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (condition-case err
        (progn
          (jsonrpc-request server-endpoint 'delete-directory "~/tmp")
          (ert-fail "A `jsonrpc-error' should have been signalled!"))
      (jsonrpc-error
       (should (= -32601 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest signals-an--32603-JSONRPC-error ()
  "signals an -32603 JSONRPC error"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (condition-case err
        (progn
          (jsonrpc-request server-endpoint '+ '(a 2))
          (ert-fail "A `jsonrpc-error' should have been signalled!"))
      (jsonrpc-error
       (should (= -32603 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest times-out ()
  "times out"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (should-error
     (jsonrpc-request server-endpoint 'sit-for '(5) :timeout 2))))

(ert-deftest stretching-it-but-works ()
  "stretching it, but works"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (should (equal
             [1 2 3 3 4 5]
             (jsonrpc-request server-endpoint 'vconcat '([1 2 3] [3 4 5]))))))

(ert-deftest json-el-cant-serialize-this ()
  "json.el can't serialize this, json.el errors and request isn't sent"
  (jsonrpc--with-emacsrpc-fixture (server-endpoint)
    (should-error
     (jsonrpc-request server-endpoint 'append '((1 2 3)
                                                (3 4 5))))))


(provide 'jsonrpc-tests)
;;; jsonrpc-tests.el ends here
