;;; kcell.el --- Internal representation of koutline kcells used by kviews
;;
;; Author:       Bob Weiner
;;
;; Orig-Date:    5/1/1993
;;
;; Copyright (C) 1993-2016  Free Software Foundation, Inc.
;; See the "../HY-COPY" file for license information.
;;
;; This file is part of GNU Hyperbole.

;;; Commentary:
;;
;;   Defines kcells, nodes in Koutlines, along with a persistent representation
;;   for writing to files called kcell-data.
;;

;;; Code:
;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(eval-and-compile (mapc #'require '(hinit htz klabel knode kview)))

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar kcell:read-only-attributes
  '(idstamp creator create-time modifier mod-time)
  "List of kcell attributes which may not be modified by a user.
Add to this list but don't remove any of the default elements.")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;;
;;; kcell
;;;

(defalias 'kcell:contents     'knode:contents)

(defun kcell:copy (kcell)
  "Return a copy of KCELL."
  (knode:copy kcell))

(defun kcell:create (contents idstamp &optional plist)
  "Return a new kcell which stores CONTENTS (a string or nil), has permanent IDSTAMP (an integer), and optional additional property list, PLIST.
User id of `creator' of cell and `create-time' are added to cell's PLIST if
not already there."
  (and contents (not (stringp contents))
       (error "(kcell:create): Invalid `contents' argument: %s" contents))
  (unless (klabel:idstamp-p idstamp)
      (error "(kcell:create): Invalid `idstamp' argument: %s" idstamp))
  (knode:create
   contents (nconc (list 'idstamp idstamp)
		   (if (memq 'creator plist)
		       nil
		     (list 'creator hyperb:user-email
			   'create-time (htz:date-sortable-gmt)))
		   plist)))

(defun kcell:create-top (&optional file counter)
  "Return a new koutline top cell optionally attached to FILE with current idstamp COUNTER."
  (kcell:create nil 0
		;; id-counter = max idstamp value given out in this koutline
		(list 'id-counter (or counter 0) 'file file)))

(defun kcell:get-attr (kcell attribute)
  "Return the value of KCELL's ATTRIBUTE."
  (knode:get-attr (kcell:plist kcell) attribute))

(defun kcell:idstamp (kcell)
  "Return permanent idstamp of KCELL as an integer."
  (kcell:get-attr kcell 'idstamp))

(defalias 'kcell:is-p      'knode:is-p)

(defun kcell:plist (kcell)
  (knode:get-attr kcell 'plist))

(defun kcell:ref-to-id (cell-ref)
  "Returns a CELL-REF string converted to a cell identifier string.
If CELL-REF contains both a relative and a permanent id, the permanent id is
returned.  If CELL-REF is invalid, nil is returned.

CELL-REF may be of any of the following forms:
  1b        - relative id, augment style
  1.2       - relative id, legal style
  012       - permanent idstamp
  1a=012    - both relative and permanent ids (in that order) separated by =
  |viewspec - a viewspec setting, rather than a cell reference
  :viewspec - an augment viewspec, ignored for now.

Optionally, any of the above id forms may be followed by a period and some
alpha characters indicating a location relative to the id.

Optionally, any of these id forms (or the relative form) may be followed by
zero or more whitespace characters, a | and some view specification
characters.  Augment viewspec characters preceded by a colon are ignored, for
now."

  (if (not (stringp cell-ref))
      nil
    (setq cell-ref (hypb:replace-match-string "\\s +" cell-ref "" t))
    (let ((specs) result)
      ;; Ignore Augment :viewspecs.
      (if (string-match ":" cell-ref)
	  (setq cell-ref (substring cell-ref 0 (match-beginning 0))))
      ;; Separate koutline |viewspecs from cell id.
      (if (string-match "\\(\\.[a-zA-Z]\\||\\)" cell-ref)
	  (setq specs (substring cell-ref (match-beginning 1))
		cell-ref (substring cell-ref 0 (match-beginning 0))))
      (setq result
	    (cond
	     ((string-match "[^.= \t\n\r\f0-9a-zA-Z]" cell-ref) nil)
	     ((string-match "^\\([.0-9a-zA-Z]+\\)=\\(0[0-9]*\\)$"
			    cell-ref)
	      (substring cell-ref (match-beginning 2) (match-end 2)))
	     ((string-match "^\\([.0-9a-zA-Z]+\\)$" cell-ref)
	      (substring cell-ref (match-beginning 1) (match-end 1)))))
      (cond (result
	     (if specs (concat result specs) result))
	    (specs
	     (if (eq ?| (aref specs 0)) specs))))))
	
(defun kcell:remove-attr (kcell attribute)
  "Remove KCELL's ATTRIBUTE, if any, return modified KCELL."
  (knode:set-attr
   kcell 'plist (knode:remove-attr (kcell:plist kcell) attribute)))

(defun kcell:set-attr (kcell attribute value)
  "Set KCELL's ATTRIBUTE to VALUE and return modified KCELL."
  (knode:set-attr
   kcell 'plist (knode:set-attr (kcell:plist kcell)
				attribute value)))

(defun kcell:set-create-time (kcell)
  "Store the time of creation of KCELL."
  (kcell:set-attr kcell 'create-time (htz:date-sortable-gmt)))

(defun kcell:set-creator (kcell)
  "Store the current user's id as the creator of KCELL."
  (kcell:set-attr kcell 'creator hyperb:user-email))

(defun kcell:set-idstamp (kcell idstamp)
  "Set KCELL's permanent IDSTAMP (an integer) and return IDSTAMP."
  (kcell:set-attr kcell 'idstamp idstamp)
  (kcell:idstamp kcell))

;;;
;;; kcell-data - Persistent representation of kotl cells (written to files).
;;;

(defun kcell-data:create (cell)
  "Given a kotl CELL, return a kcell-data structure to write to a file.
If CELL, its idstamp, or its property list are nil, this repairs the cell by
assuming it is the cell at point and filling in the missing information."
   (let ((idstamp (kcell:idstamp cell))
	 (plist (nthcdr 2 (kcell:plist cell))))
     (if (and cell idstamp plist)
	 (vector idstamp plist)
       (kcell-data:create
	(kcell:create nil (or idstamp (kview:id-increment kview)) plist)))))

(defun kcell-data:idstamp (kcell-data)
  (aref kcell-data 0))

(defun kcell-data:plist-v2 (kcell-data)
  (aref kcell-data 2))

(defun kcell-data:plist-v3 (kcell-data)
  (aref kcell-data 1))

(defun kcell-data:to-kcell-v2 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create
       ;; Cell contents are no longer put into cells themselves by default
       ;; when a file is read.  The contents are stored within the kview
       ;; buffer, so use nil as a place-holder.
       nil
       ;; Repair invalid idstamps on the fly.
       (or (kcell-data:idstamp kcell-data) (kview:id-increment kview))
       (kcell-data:plist-v2 kcell-data))
    ;; Repair invalid cells on the fly.
    (kcell:create nil (kview:id-increment kview))))

(defun kcell-data:to-kcell-v3 (kcell-data)
  (if (vectorp kcell-data)
      (kcell:create
       ;; Cell contents are no longer put into cells themselves by default
       ;; when a file is read.  The contents are stored within the kview
       ;; buffer, so use nil as a place-holder.
       nil
       ;; Repair invalid idstamps on the fly.
       (or (kcell-data:idstamp kcell-data) (kview:id-increment kview))
       (kcell-data:plist-v3 kcell-data))
    ;; Repair invalid cells on the fly.
    (kcell:create nil (kview:id-increment kview))))

(provide 'kcell)

;;; kcell.el ends here
