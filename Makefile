SPECIAL = chess-auto.el
SOURCE	= $(filter-out $(SPECIAL),$(wildcard *.el))
TARGET	= $(patsubst %.el,%.elc,$(SPECIAL) $(SOURCE))
EMACS   = emacs

all: $(TARGET)

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

clean:
	rm -f $(TARGET) *~

fullclean: clean
	-rm *.elc chess-auto.el
