### Makefile for EGLOT
### 
# Variables
#
EMACS?=emacs
SELECTOR?=t
ERROR_ON_WARN=nil

LOAD_PATH=-L .

ELFILES := eglot.el eglot-tests.el
ELCFILES := $(ELFILES:.el=.elc)
ELPADIR := $(shell mktemp -d elpa-eglot-test.XXXXXXXX --tmpdir)
ELPADEPS ?=--eval '(setq package-user-dir "$(ELPADIR)")'        \
           --eval '(setq package-check-signature nil)'          \
           --eval '(package-initialize)'                        \
           --eval '(package-refresh-contents)'                  \
           --eval '(defun install-latest (p)                    \
                     (let ((desc (cadr                          \
                                 (assoc p                       \
                                        package-archive-contents\
                                        (quote equal)))))       \
                       (unless (package-installed-p             \
                                  p                             \
                                  (package-desc-version desc))  \
                        (package-install desc))))'              \
           --eval '(install-latest (quote jsonrpc))'            \
           --eval '(install-latest (quote project))'            \
           --eval '(install-latest (quote xref))'               \
           --eval '(install-latest (quote seq))'                \
           --eval '(install-latest (quote eldoc))'              \
           --eval '(unintern                                    \
                     (quote eldoc-documentation-function))'     \
           --eval '(load "eldoc")'                              \
           --eval '(install-latest (quote company))'            \
           --eval '(install-latest (quote yasnippet))'          \
           --eval '(install-latest (quote external-completion))'\
           --eval '(install-latest (quote flymake))'

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
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

eglot-check-noelpa: ELPADEPS=-f package-initialize
eglot-check-noelpa: eglot-check

interactive: compile
	$(EMACS) -Q							\
		--eval '(setq debug-on-error t)'			\
		$(ELPADEPS)						\
		$(LOAD_PATH)						\
		-l eglot						\
		-l eglot-tests						\

check: eglot-check-noelpa

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
