;; -*- no-byte-compile: t -*-
(define-package "hyperbole" "7.0.2" "GNU Hyperbole: The Everyday Hypertextual Information Manager"
  '((emacs "24.4"))
  :url "http://www.gnu.org/software/hyperbole"
  :keywords '("comm" "convenience" "files" "frames" "hypermedia" "languages"
	      "mail" "matching" "mouse" "multimedia" "outlines" "tools" "wp"))

(setq byte-compile-warnings '(not interactive-only find-tag free-vars unresolved))
