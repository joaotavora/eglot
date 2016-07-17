;;; kprop-em.el --- Koutline text property handling under GNU Emacs
;;
;; AUTHOR:       Bob Weiner
;;
;; Orig-Date:    7/27/93
;;
;; Copyright (C) 1993-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'hversion)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defalias 'kproperty:get 'get-text-property)

(defun kproperty:map (function property value)
  "Apply FUNCTION to each character with PROPERTY `eq' to VALUE in the current buffer.
FUNCTION is called with the start and end points of the text span with the matching PROPERTY
and with point at the start."
  (let ((result)
	(start (point-min))
	end)
    (save-excursion
      (while (and (< start (point-max))
		  (setq start (text-property-any start (point-max) property value)))
	(goto-char start)
	(setq end (or (text-property-not-all start (point-max) property value) (point-max))
	      result (cons (funcall function start end) result)
	      start end)))
    (nreverse result)))

(defalias 'kproperty:next-single-change 'next-single-property-change)

(defalias 'kproperty:previous-single-change 'previous-single-property-change)

(defalias 'kproperty:properties 'text-properties-at)

(defun kproperty:put (start end property-list &optional object)
  "From START to END, add PROPERTY-LIST properties to the text.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  Text inserted before or after this region does not inherit the added
properties."
  (add-text-properties
   start end (append property-list '(rear-nonsticky t)) object))

(defun kproperty:remove (start end property-list &optional object)
  "From START to END, remove the text properties in PROPERTY-LIST.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  PROPERTY-LIST should be a plist; if the value of a property is
non-nil, then only a property with a matching value will be removed.
Returns t if any property was changed, nil otherwise."
  (let ((changed) plist property value next)
    (while property-list
      (setq property (car property-list)
	    value (car (cdr property-list))
	    plist (list property value)
	    property-list (nthcdr 2 property-list)
	    next start)
      (while (setq next (text-property-any next end property value object))
	(remove-text-properties next (1+ next) plist object)
	(setq changed t next (1+ next))))
    changed))

(defun kproperty:replace-separator (pos label-separator old-sep-len)
  "Replace at POS the cell label separator with LABEL-SEPARATOR.
OLD-SEP-LEN is the length of the separator being replaced."
  (let (properties)
    (while (setq pos (kproperty:next-single-change (point) 'kcell))
      (goto-char pos)
      (setq properties (text-properties-at pos))
      ;; Replace label-separator while maintaining cell properties.
      (insert label-separator)
      (add-text-properties pos (+ pos 2) properties)
      (delete-region (point) (+ (point) old-sep-len)))))

(defun kproperty:set (property value)
  "Set PROPERTY of character at point to VALUE."
  (kproperty:put (point) (min (+ 2 (point)) (point-max))
		 (list property value)))

;;; kprop-em.el ends here
