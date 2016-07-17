;;; kprop-xe.el --- Koutline text property handling under XEmacs
;;
;; Author:       Bob Weiner
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

;; (get-text-property (pos prop &optional object))
;; Return the value of position POS's property PROP, in OBJECT.
;; OBJECT is optional and defaults to the current buffer.
;; If POSITION is at the end of OBJECT, the value is nil.
(defalias 'kproperty:get 'get-text-property)

(defun kproperty:map (function property &optional value)
  "Apply FUNCTION to each character with PROPERTY `eq' to VALUE in the current buffer.
FUNCTION is called with the start and end points of the text span with the matching PROPERTY
and with point at the start."
  (let ((result)
	(start) end)
    (save-excursion
      (map-extents (lambda (extent unused)
		       (if (setq start (extent-start-position extent))
			   (progn (goto-char start)
				  (setq end (extent-end-position extent)
					result (cons (funcall function start end) result))))
		       nil)
		   nil nil nil nil nil property value))
    (nreverse result)))

;; (next-single-property-change (pos prop &optional object))
;; Return the position of next property change for a specific property.
;; Scans characters forward from POS till it finds
;; a change in the PROP property, then returns the position of the change.
;; The optional third argument OBJECT is the string or buffer to scan.
;; Return nil if the property is constant all the way to the end of OBJECT.
;; If the value is non-nil, it is a position greater than POS, never equal.
(defalias 'kproperty:next-single-change 'next-single-property-change)

;; (previous-single-property-change (pos prop &optional object))
;; Return the position of previous property change for a specific property.
;; Scans characters backward from POS till it finds
;; a change in the PROP property, then returns the position of the change.
;; The optional third argument OBJECT is the string or buffer to scan.
;; Return nil if the property is constant all the way to the start of OBJECT.
;; If the value is non-nil, it is a position less than POS, never equal.
(defalias 'kproperty:previous-single-change 'previous-single-property-change)

(defalias 'kproperty:properties 'extent-properties-at)

(defun kproperty:put (start end property-list &optional object)
  "From START to END, add PROPERTY-LIST properties to the text.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  Text inserted before or after this region does not inherit the added
properties."
  ;; Don't use text properties internally because they don't work as desired
  ;; when copied to a string and then reinserted, at least in some versions
  ;; of XEmacs.
  (let ((extent (make-extent start end object)))
    (if (null extent)
	(error "(kproperty:put): No extent at %d-%d to add properties %s" 
	       start end property-list))
    (if (/= (mod (length property-list) 2) 0)
	(error "(kproperty:put): Property-list has odd number of elements, %s"
	       property-list))
    (set-extent-property extent 'text-prop (car property-list))
    (set-extent-property extent 'duplicable t)
    (set-extent-property extent 'start-open t)
    (set-extent-property extent 'end-open t)
    (while property-list
      (set-extent-property
       extent (car property-list) (car (cdr property-list)))
      (setq property-list (nthcdr 2 property-list)))
    extent))

(defun kproperty:remove (start end property-list &optional object)
  "From START to END, remove the text properties in PROPERTY-LIST.
The optional fourth argument, OBJECT, is the string or buffer containing the
text.  PROPERTY-LIST should be a plist; if the value of a property is
non-nil, then only a property with a matching value will be removed.
Returns t if any property was changed, nil otherwise."
  ;; Don't use text property functions internally because they only look for
  ;; closed extents, which kproperty does not use.
  (let ((changed) property value)
    (while property-list
      (setq property (car property-list)
	    value (car (cdr property-list))
	    property-list (nthcdr 2 property-list))
      (map-extents
       (lambda (extent maparg)
	 (if (extent-live-p extent)
	     (progn (setq changed t)
		    (delete-extent extent)))
	 nil)
       object start end nil nil property value))
    changed))

(defun kproperty:replace-separator (pos label-separator old-sep-len)
  "Replace at POS the cell label separator with LABEL-SEPARATOR.
OLD-SEP-LEN is the length of the separator being replaced."
  (let (extent)
    (while (setq pos (kproperty:next-single-change (point) 'kcell))
      (goto-char pos)
      (setq extent (extent-at pos))
      ;; Replace label-separator while maintaining cell properties.
      (insert label-separator)
      (set-extent-endpoints extent pos (+ pos 2))
      (delete-region (point) (+ (point) old-sep-len)))))

(defun kproperty:set (property value)
  "Set PROPERTY of character at point to VALUE."
  (kproperty:put (point) (min (+ 2 (point)) (point-max))
		 (list property value)))

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; kprop-xe.el ends here
