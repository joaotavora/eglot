;;; kproperty.el --- Wrapper for koutline text property implementations
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

;; Ensures kotl/ is in load-path.
(require 'hyperbole)

(load (if hyperb:emacs-p "kprop-em" "kprop-xe"))

(provide 'kproperty)

;;; kproperty.el ends here
