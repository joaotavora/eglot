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
	rm -f *~ chess.dvi chess-final.* game.* log.*
	rm -f *.aux *.cp *.cps *.fn *.fns *.ky *.log *.pg *.toc *.tp *.vr

fullclean: clean
	-rm $(TARGET) chess.info chess-auto.el

VERSION=$(shell perl -ne 'print $$1 if /chess-version.*"([^"]+)"/;' chess.el)
TAG=$(shell echo $(VERSION) | sed 's/\./-/g')
CAT=$(shell echo $(VERSION) | perl -ne 'print $$1 if /[-0-9]+([ab])[0-9]+/;')
SUB=$(shell echo $(VERSION) | perl -ne 'print $$1 if /[-0-9]+[ab]([0-9]+)/;')
NEXT=$(shell expr $(SUB) + 1)

dist: fullclean all clean
	cvs tag $(TAG)
	cp -ar . /var/tmp/chess-$(VERSION)
	tar cvjfXC /var/tmp/chess-$(VERSION).tar.bz2 \
		.exclude /var/tmp chess-$(VERSION)
	rm -fr /var/tmp/chess-$(VERSION)
	mv /var/tmp/chess-$(VERSION).tar.bz2 \
		$(HOME)/public_html/Emacs/packages
	perl -ne 's/(chess-version.*)"([0-9.]+)[ab][0-9]+"/$$1"$$2$(CAT)$(NEXT)"/;' chess.el
	cvs commit -m "bumped minor rev" chess.el

update:
	make dist
	sitecopy
	make fullclean
