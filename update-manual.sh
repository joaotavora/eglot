#!/bin/sh
EMACS=~/Source/Emacs/emacs
texi2any -I $EMACS/doc/emacs --html --no-split --css-ref eglot-manual.css -o index.html $EMACS/doc/misc/eglot.texi
