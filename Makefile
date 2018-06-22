### Makefile for EGLOT
### 
# Variables
#
EMACS=emacs
SELECTOR=t

LOAD_PATH=-L .

ELFILES := eglot.el jsonrpc.el eglot-tests.el jsonrpc-tests.el
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
		-l eglot-tests						\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

jsonrpc-check: jsonrpc.elc jsonrpc-tests.elc
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		-l jsonrpc-tests					\
		--eval '(ert-run-tests-batch-and-exit (quote $(SELECTOR)))'

check: eglot-check jsonrpc-check

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
