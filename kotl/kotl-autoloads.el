;;; kotl-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (kexport:html) "kexport" "kexport.el" (22412 20800))
;;; Generated autoloads from kexport.el

(autoload (quote kexport:html) "kexport" "\
Export a koutline buffer or file in EXPORT-FROM to html format in OUTPUT-TO.
By default, this retains newlines within cells as they are.  With optional prefix arg, SOFT-NEWLINES-P, 
hard newlines are not used.  Also converts Urls and Klinks into Html hyperlinks.
STILL TODO:
  Make delimited pathnames into file links (but not if within klinks).
  Copy attributes stored in cell 0 and attributes from each cell.

\(fn EXPORT-FROM OUTPUT-TO &optional SOFT-NEWLINES-P)" t nil)

;;;***

;;;### (autoloads (kfile:view kfile:is-p kfile:find) "kfile" "kfile.el"
;;;;;;  (22412 20800))
;;; Generated autoloads from kfile.el

(autoload (quote kfile:find) "kfile" "\
Find a file FILE-NAME containing a kotl or create one if none exists.
Return the new kview.

\(fn FILE-NAME)" t nil)

(autoload (quote kfile:is-p) "kfile" "\
Iff current buffer contains an unformatted or formatted koutline, return file format version string, else nil.

\(fn)" nil nil)

(autoload (quote kfile:view) "kfile" "\
View an existing kotl version-2 file FILE-NAME in a read-only mode.

\(fn FILE-NAME)" t nil)

;;;***

;;;### (autoloads (kimport:text kimport:star-outline kimport:aug-post-outline
;;;;;;  kimport:file) "kimport" "kimport.el" (22412 23155))
;;; Generated autoloads from kimport.el

(defvar kimport:mode-alist (quote ((t . kimport:text) (outline-mode . kimport:star-outline))) "\
Alist of (major-mode . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called if the major mode of the import file matches the car of an element in
this list.  If there is no match, then `kimport:suffix-alist' is checked.  If
that yields no match, the element in this list whose car is 't is used.  It
normally does an import of a koutline or text file.

Each importation-function must take two arguments, a buffer/file to import
and a buffer/file into which to insert the imported elements and a third
optional argument, CHILDREN-P, which when non-nil means insert imported cells
as the initial set of children of the current cell, if any.

   outline-mode  - imported as an Emacs outline whose entries begin with
                   asterisks; 
   .kot
   .kotl         - imported as a structured koutline

   all others    - imported as text.")

(defvar kimport:suffix-alist (quote (("\\.otl$" . kimport:star-outline) ("\\.aug$" . kimport:aug-post-outline))) "\
Alist of (buffer-name-suffix-regexp . importation-function) elements.
This determines the type of importation done on a file when `kimport:file' is
called.  Each importation-function must take two arguments, a buffer/file to
import and a buffer/file into which to insert the imported elements and a
third optional argument, CHILDREN-P, which when non-nil means insert imported
cells as the initial set of children of the current cell, if any.

   .otl  - imported as an Emacs outline whose entries begin with asterisks;
   .kot
   .kotl - imported as a structured koutline
   .aug  - imported as an Augment post-numbered outline.")

(autoload (quote kimport:file) "kimport" "\
Import a buffer or file IMPORT-FROM into the koutline in buffer or file OUTPUT-TO.

Any suffix in IMPORT-FROM's buffer name is used to determine the type of
importation.  All others are imported as text, one paragraph per cell.

See the documentation for the variable, `kimport:suffix-alist' for
information on specific importation formats.

\(fn IMPORT-FROM OUTPUT-TO &optional CHILDREN-P)" t nil)

(autoload (quote kimport:aug-post-outline) "kimport" "\
Insert Augment outline statements from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

If OUTPUT-TO is a new koutline, the first statement inserted will be the
first cell.  Otherwise, it will be the successor of the current cell.

Each statement to be imported is delimited by an Augment relative id at the
end of the statement.  \"1\" = level 1, \"1a\" = level 2 in outline and so
on.

\(fn IMPORT-FROM OUTPUT-TO &optional CHILDREN-P)" t nil)

(autoload (quote kimport:star-outline) "kimport" "\
Insert star outline nodes from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

\"* \" = level 1, \"** \" = level 2 in outline and so on.

\(fn IMPORT-FROM OUTPUT-TO &optional CHILDREN-P)" t nil)

(autoload (quote kimport:text) "kimport" "\
Insert text paragraphs from IMPORT-FROM into koutline OUTPUT-TO.
Displays and leaves point in OUTPUT-TO.  See documentation for
`kimport:initialize' for valid values of IMPORT-FROM and OUTPUT-TO and for
an explanation of where imported cells are placed.

Text paragraphs are imported as a sequence of same level cells.  Koutlines
are imported with their structure intact.

The variable, `paragraph-start,' is used to determine paragraphs.

\(fn IMPORT-FROM OUTPUT-TO &optional CHILDREN-P)" t nil)

;;;***

;;;### (autoloads (klink:create) "klink" "klink.el" (22412 23155))
;;; Generated autoloads from klink.el

(autoload (quote klink:create) "klink" "\
Insert at point an implicit link to REFERENCE.
REFERENCE should be a cell-ref or a string containing \"filename, cell-ref\".
See documentation for `kcell:ref-to-id' for valid cell-ref formats.

\(fn REFERENCE)" t nil)

;;;***

;;;### (autoloads (kotl-mode:is-p kotl-mode:show-tree kotl-mode:hide-tree
;;;;;;  kotl-mode:top-cells kotl-mode:show-all kotl-mode:overview
;;;;;;  kotl-mode:example kotl-mode) "kotl-mode" "kotl-mode.el" (22417
;;;;;;  27935))
;;; Generated autoloads from kotl-mode.el

(autoload (quote kotl-mode) "kotl-mode" "\
The major mode used to edit and view koutlines.
It provides the following keys:
\\{kotl-mode-map}

\(fn)" t nil)

(autoload (quote kotl-mode:example) "kotl-mode" "\
Display the Koutliner example file for demonstration use by a user.

\(fn)" t nil)

(autoload (quote kotl-mode:overview) "kotl-mode" "\
Show the first line of each cell.
With optional prefix ARG, toggle display of blank lines between cells.

\(fn &optional ARG)" t nil)

(autoload (quote kotl-mode:show-all) "kotl-mode" "\
Show (expand) all cells in the current view.
With optional prefix ARG, toggle display of blank lines between cells.

\(fn &optional ARG)" t nil)

(autoload (quote kotl-mode:top-cells) "kotl-mode" "\
Collapse all level 1 cells in view and hide any deeper sublevels.
With optional prefix ARG, toggle display of blank lines between cells.

\(fn &optional ARG)" t nil)

(autoload (quote kotl-mode:hide-tree) "kotl-mode" "\
Collapse tree rooted at optional CELL-REF (defaults to cell at point).
With optional SHOW-FLAG, expand the tree instead.

\(fn &optional CELL-REF SHOW-FLAG)" t nil)

(autoload (quote kotl-mode:show-tree) "kotl-mode" "\
Display fully expanded tree rooted at CELL-REF.

\(fn &optional CELL-REF)" t nil)

(autoload (quote kotl-mode:is-p) "kotl-mode" "\
Signal an error if current buffer is not a Hyperbole outline, else return t.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("kcell.el" "kfill.el" "klabel.el" "kmenu.el"
;;;;;;  "knode.el" "kprop-em.el" "kprop-xe.el" "kproperty.el" "kview.el"
;;;;;;  "kvspec.el") (22418 18873 274718))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; kotl-autoloads.el ends here
