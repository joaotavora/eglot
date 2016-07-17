;;; knode.el --- Generic nodes for use as elements in data structures
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    5/1/93
;;
;; Copyright (C) 1993-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;
;;; Knodes
;;;

(defun knode:create (contents &optional prop-list)
  "Return a new knode which stores CONTENTS and optional PROP-LIST."
  (list   'knode
	  'contents contents
	  'plist prop-list))

(defun knode:contents (knode)
   "Return KNODE's contents."
   (if (knode:is-p knode)
       (car (cdr (memq 'contents knode)))
     (error "(knode:contents): Argument must be a knode.")))

(defalias 'knode:copy 'copy-tree)

(defun knode:is-p (object)
  "Is OBJECT a knode?"
  (and (listp object) (eq (car object) 'knode)))

(defun knode:set-contents (knode contents)
  "Set KNODE's CONTENTS."
  (if (knode:is-p knode)
      (setcar (cdr (memq 'contents knode)) contents)
    (error "(knode:set-contents): First arg must be a knode.")))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun knode:get-attr (obj attribute)
  "Return the value of OBJECT's ATTRIBUTE."
  (car (cdr (memq attribute obj))))

(defun knode:remove-attr (obj attribute)
  "Remove OBJECT's ATTRIBUTE, if any, and return modified OBJECT.
Use (setq object (knode:remove-attr object attribute)) to ensure that OBJECT
is updated."
  (let ((tail obj)
	sym
	prev)
    (setq sym (car tail))
    (while (and sym (eq sym attribute))
      (setq tail (cdr (cdr tail))
	    sym (car tail)))
    (setq obj tail
	  prev tail
	  tail (cdr (cdr tail)))
    (while tail
      (setq sym (car tail))
      (if (eq sym attribute)
	  (setcdr (cdr prev) (cdr (cdr tail))))
      (setq prev tail
	    tail (cdr (cdr tail))))
    obj))

(defun knode:set-attr (obj attribute value)
  "Set OBJECT's ATTRIBUTE to VALUE and return OBJECT."
  (let ((attr (memq attribute obj)))
    (if attr
	(setcar (cdr attr) value)
      (setq obj (nconc obj (list attribute value)))))
  obj)

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(provide 'knode)


;;; knode.el ends here
