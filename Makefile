### Makefile for EGLOT
### 
# Variables
#
EMACS=emacs
SELECTOR=t

LOAD_PATH=-L .

ELFILES := eglot.el eglot-tests.el
ELCFILES := $(ELFILES:.el=.elc)

ELPADEPS ?=--eval '(package-initialize)'			\
           --eval '(package-refresh-contents)'			\
           --eval '(package-install (quote company))'		\
           --eval '(package-install (quote jsonrpc))'		\
           --eval '(package-install (quote yasnippet))'		\
           --eval '(package-install 				\
                      (cadr (assoc (quote flymake)		\
                                   package-archive-contents)))'

all: compile

# Compilation
#
%.elc: %.el
	$(EMACS) -Q $(ELPADEPS) $(LOAD_PATH) --batch -f batch-byte-compile $<

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

check: eglot-check

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
