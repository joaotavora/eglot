BUILT_SOURCES  = chess-auto.el
CLEANFILES     = TAGS chess-auto.el auto-autoloads.el
EXTRA_DIST     = autogen.sh chess-auto.el.in chess-eco.pos doc/chess.pdf
DISTCLEANFILES = chess-eco.fen doc/chess.info doc/chess.pdf $(TESTS)
info_TEXINFOS  = doc/chess.texi
dist_lisp_DATA = _pkg.el chess-eco.fen

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
	chess-ics2.el	   \
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
	chess-test.el      \
	chess-transport.el \
	chess-tutorial.el  \
	chess-ucb.el	   \
	chess-var.el	   \
	chess-eco.el

chess-auto.el: $(ELFILES)
	echo ";;; DO NOT MODIFY THIS FILE" > $(top_builddir)/chess-auto.el
	echo "(if (featurep 'chess-auto) (error \"Already loaded\"))" \
		>> $(top_builddir)/chess-auto.el
	$(EMACS) -batch -L $(srcdir) -l chess-maint.el \
		-f chess-generate-autoloads \
		$(top_builddir)/chess-auto.el $(srcdir)
	echo "(provide 'chess-auto)" >> $(top_builddir)/chess-auto.el
	ln -f $(top_builddir)/chess-auto.el $(top_builddir)/auto-autoloads.el

chess-eco.fen: chess-eco.pos chess-eco.el
	$(EMACS) --no-init-file --no-site-file -batch \
		-L $(srcdir) -l chess-eco -f chess-generate-fen-table \
		chess-eco.pos chess-eco.fen

TESTS	 = chess-test
DATABASE = $(shell test -r test/largedb.sg3 && echo test/largedb || echo test/historic.pgn)
START	 = $(shell test -r test/largedb.sg3 && perl -e 'print int(rand(4000000)), "\n";' || echo 0)
COUNT	 = 100000

# Note: There are 4,209,433 games in test/largedb, if you download the files
# from:
#
#   ftp://ftp.newartisans.com/pub/chess/largedb.7z
#
# If you have a dual-core CPU (or more), you'll get the best performance by
# running a separate Emacs for each core.  Here's how I run all the tests on
# my dual-core MacBook Pro: (Note, removing chess-test is just a precaution,
# since the Makefile will not regenerate it if you aborted a previous test
# run).
#
#   chess1 $ rm -f chess-test; nice -n 20 make START=0 COUNT=2104715 check
#   chess2 $ rm -f chess-test; nice -n 20 make START=2104715 COUNT=0 check
#
# I run both of these using `screen', with a vertical split so I can watch
# them both running.  I type C-a H in each screen window before starting, so
# that all the output is logged to a file I can examine afterward.
#
# Note that these tests can take days to run.  My MacBook Pro gets around
# 2,000 plies per second.  If you're a Lisp hacker and want to improve the
# speed of that, the slowness is pretty much all in `chess-search-position',
# in the file chess-pos.el.

chess-test:
	echo "$(EMACS) -batch -L $(srcdir) -l chess-test.el -f chess-test '$(DATABASE)' $(START) $(COUNT); rm -f $(top_builddir)/chess-test" > $@
	chmod u+x $@

TAGS: $(dist_lisp_LISP)
	@etags $(dist_lisp_LISP)
	@echo TAGS rebuilt.

# Makefile.am ends here
