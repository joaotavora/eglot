# This Makefile is used to generate the info documentation and preprocessed
# Lisp data for Emacs Chess.
#
# If you update chess.texi or chess-eco.pos, run make on this file.

EMACS = emacs --batch --no-site-file
MAKEINFO_FLAGS = --no-split
INSTALL_INFO = install-info

all: chess-eco.fen chess.info dir

test: chess-test.elc
	$(EMACS) -L . -l chess-test -f ert-run-tests-batch

chess-eco.fen: chess-eco.pos
	$(EMACS) -L . -l chess-eco.el -f chess-generate-fen-table $< $@

dir: chess.info
	$(INSTALL_INFO) $< $@

chess-database.elc: chess-message.elc chess-file.elc chess-scid.elc
chess-file.elc: chess-fen.elc chess-pgn.elc
chess-perft.elc: chess-fen.elc chess-ply.elc chess-pos.elc
chess-test.elc: chess-database.elc chess-game.elc chess-perft.elc

.el.elc:
	@$(EMACS) -L . -f batch-byte-compile $<

clean:
	rm -f *.elc
