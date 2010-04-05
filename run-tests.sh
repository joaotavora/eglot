#!/bin/sh

CPUS=8
SOURCE=$HOME/src/emacs-chess
PRODUCTS=$HOME/Products/emacs-chess
EMACS=/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs

# No need to change these values
TOTAL=4209433
PART=$((TOTAL / CPUS))

for i in $(seq 1 $CPUS); do
    # Make sure the sources are all up to date
    rsync -av --delete --exclude=.git/ --delete-excluded \
	$SOURCE/ $PRODUCTS/$i/

    (cd $PRODUCTS/$i; rm -f chess-test; make EMACS=$EMACS; \
     nice -n 20 make EMACS=$EMACS \
	START=$(((i - 1) * PART)) COUNT=$PART check > test.out 2>&1) &
done

wait

for i in $(seq 1 $CPUS); do
    cat $PRODUCTS/$i/test.out >> test.out
done

cat test.out
