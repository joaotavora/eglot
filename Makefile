### Makefile for EGLOT
### 
# Variables
#
EMACS=emacs

LOAD_PATH=-L .

ELFILES := eglot.el jsonrpc.el eglot-tests.el
ELCFILES := $(ELFILES:.el=.elc)

all: compile

# Compilation
#
%.elc: %.el
	$(EMACS) -Q $(LOAD_PATH) --batch -f batch-byte-compile $<

compile: $(ELCFILES)

# Automated tests
#
check: compile

check: SELECTOR=t
check: compile
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		-l eglot-tests						\
		-f ert-run-tests-batch-and-exit				\

jsonrpc-check: jsonrpc.elc
	$(EMACS) -Q --batch $(LOAD_PATH)				\
		-l jsonrpc-tests					\
		-f ert-run-tests-batch-and-exit				\

# Cleanup
#
clean:
	find . -iname '*.elc' -exec rm {} \;
.PHONY: all compile clean check
