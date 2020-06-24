### Makefile for EGLOT
### 
# Variables
#
EMACS=emacs
SELECTOR=t
ERROR_ON_WARN=nil

LOAD_PATH=-L .

ELFILES := eglot.el eglot-tests.el
ELCFILES := $(ELFILES:.el=.elc)

ELPADEPS ?=--eval '(dolist (el (directory-files "./tramp/lisp" t "\\.el$")) (load-file el))'			\
           --eval '(package-refresh-contents)'			\
           --eval '(package-refresh-contents)'			\
           --eval '(package-install (quote jsonrpc))'		\
           --eval '(package-install (quote project))'		\
           --eval '(package-install (quote xref))'		\
           --eval '(package-install (quote eldoc))'		\
           --eval '(package-install (quote company))'		\
           --eval '(package-install (quote yasnippet))'		\
           --eval '(package-install 				\
                      (cadr (assoc (quote flymake)		\
                                   package-archive-contents)))'

BYTECOMP_ERROR_ON_WARN := \
	--eval '(setq byte-compile-error-on-warn $(ERROR_ON_WARN))'

all: compile

# Compilation.  Note BYTECOMP_ERROR_ON_WARN after ELPADEPS 
# so deps can still warn on compilation.
#
%.elc: %.el
	$(EMACS) -Q $(ELPADEPS) $(BYTECOMP_ERROR_ON_WARN) $(LOAD_PATH) \
		--batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
eglot-check: compile
	$(EMACS) -Q --batch						\
		$(ELPADEPS)						\
		$(LOAD_PATH)						\
		-l eglot						\
		-l eglot-tests						\
		--eval '(setq ert-batch-backtrace-right-margin 200)'	\
		--eval '(setq default-directory (concat "/ssh:localhost:" default-directory))'	\
		--eval '(setq tramp-verbose 10)'	\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

check: eglot-check

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
