SPECIAL = chess-auto.el
SOURCE	= $(filter-out $(SPECIAL),$(wildcard *.el))
TARGET	= $(patsubst %.el,%.elc,$(SPECIAL) $(SOURCE))
EMACS   = emacs

MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
ENVADD = TEXINPUTS="$(TEXINPUTS)" MAKEINFO="$(MAKEINFO) -I$(srcdir)"

all: $(TARGET) chess.info

chess-auto.el: chess-auto.in $(SOURCE)
	cp chess-auto.in chess-auto.el
	-rm chess-auto.elc
	$(EMACS) --no-init-file --no-site-file -batch \
		-l $(shell pwd)/chess-auto \
		-f generate-autoloads \
		$(shell pwd)/chess-auto.el .

%.elc: %.el
	$(EMACS) --no-init-file --no-site-file -batch \
		-l $(shell pwd)/chess-maint \
		-f batch-byte-compile $<

chess-final.texi: chess.texi $(SOURCE)
	$(EMACS) --no-init-file --no-site-file -batch \
		-l $(shell pwd)/chess-maint \
		-f chess-generate-texinfo-file

chess.info: chess-final.texi
	$(MAKEINFO) chess-final.texi

info: chess.info

chess.dvi: chess-final.texi
	$(ENVADD) $(TEXI2DVI) chess-final.texi

clean:
	rm -f $(TARGET) *~ chess.dvi chess.info chess-final.*
	rm -f *.aux *.cp *.cps *.fn *.fns *.ky *.log *.pg *.toc *.tp *.vr

fullclean: clean
	-rm *.elc chess-auto.el
