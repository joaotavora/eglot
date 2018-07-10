### Makefile for EGLOT
### 
# Variables
#
EMACS=emacs
SELECTOR=t

LOAD_PATH=-L .

ELFILES := eglot.el eglot-tests.el
ELCFILES := $(ELFILES:.el=.elc)

all: compile

# Compilation
#
%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
eglot-check: compile
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		--eval '(package-initialize)'				\
		--eval '(package-install (quote jsonrpc))'		\
		-l eglot-tests						\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

check: eglot-check

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
