# This Makefile is used to generate the info documentation and preprocessed
# Lisp data for Emacs Chess.
#
# If you update chess.texi or chess-eco.pos, run make on this file.
#
# NOTE: The "test" target is unnecessarily slow as it requires compiled Lisp
# files for decent performance.

EMACS = emacs --batch --no-site-file
MAKEINFO = makeinfo
INSTALL_INFO = install-info

all: chess-eco.fen chess.info dir

test:
	$(EMACS) -L . -l chess-perft -f ert-run-tests-batch

chess-eco.fen: chess-eco.pos
	$(EMACS) -L . -l chess-eco.el -f chess-generate-fen-table $< $@

chess.info: chess.texi
	$(MAKEINFO) --no-split -o $@ $<

dir: chess.info
	$(INSTALL_INFO) $< $@

