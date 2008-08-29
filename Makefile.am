BUILT_SOURCES  = chess-auto.el
CLEANFILES     = TAGS chess-auto.el auto-autoloads.el
EXTRA_DIST     = autogen.sh doc/chess.pdf
DISTCLEANFILES = chess-eco.fen doc/chess.info doc/chess.pdf
info_TEXINFOS  = doc/chess.texi
lisp_DATA      = _pkg.el chess-eco.fen

dist_lisp_LISP =	   \
	chess.el	   \
	chess-ai.el	   \
	chess-algebraic.el \
	chess-announce.el  \
	chess-auto.el      \
	chess-autosave.el  \
	chess-chat.el	   \
	chess-clock.el	   \
	chess-common.el	   \
	chess-crafty.el	   \
	chess-database.el  \
	chess-display.el   \
	chess-engine.el	   \
	chess-epd.el	   \
	chess-fen.el	   \
	chess-file.el	   \
	chess-game.el	   \
	chess-german.el	   \
	chess-gnuchess.el  \
	chess-ics.el	   \
	chess-ics1.el	   \
	chess-images.el	   \
	chess-input.el	   \
	chess-irc.el	   \
	chess-kibitz.el	   \
	chess-link.el	   \
	chess-log.el	   \
	chess-message.el   \
	chess-module.el	   \
	chess-network.el   \
	chess-none.el	   \
	chess-pgn.el	   \
	chess-phalanx.el   \
	chess-plain.el	   \
	chess-ply.el	   \
	chess-pos.el	   \
	chess-puzzle.el	   \
	chess-random.el	   \
	chess-scid.el	   \
	chess-sjeng.el	   \
	chess-sound.el	   \
	chess-transport.el \
	chess-tutorial.el  \
	chess-ucb.el	   \
	chess-var.el	   \
	chess-eco.el

chess-auto.el: $(ELFILES) chess-auto.el.in
	cp -p $(srcdir)/chess-auto.el.in $@
	-rm $(top_builddir)/chess-auto.elc
	$(EMACS) --no-init-file --no-site-file -batch \
		-L $(top_builddir) -l chess-auto -f generate-autoloads \
		$(top_builddir)/chess-auto.el $(srcdir)

chess-eco.fen: chess-eco.pos chess-eco.el
	$(EMACS) --no-init-file --no-site-file -batch \
		-L $(srcdir) -l chess-eco -f chess-generate-fen-table \
		chess-eco.pos chess-eco.fen

TESTS = chess-test

chess-test:
	echo "$(EMACS) -batch -L . -l chess-test.el --eval '(chess-test)'" > $@
	chmod u+x $@

TAGS: $(dist_lisp_LISP)
	@etags $(dist_lisp_LISP)
	@echo TAGS rebuilt.

# Makefile.am ends here
