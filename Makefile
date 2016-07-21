# Makefile --- Build GNU Hyperbole directories and package distributions
#
# Author:       Bob Weiner
#
# Orig-Date:    15-Jun-94 at 03:42:38
#
# Copyright (C) 1994-2016  Free Software Foundation, Inc.
# See the file HY-COPY for license information.
#
# This file is part of GNU Hyperbole.

# Commentary:
#
#   **********
#   READ THIS:
#   **********
#
#   Only Hyperbole developers (those who develop the source code) need
#   to use this file for building Hyperbole package distributions.  Others
#   may ignore it.
#
#   GNU Hyperbole is now installed for use via the Emacs package system; see
#   the "INSTALL" file for installation instructions and the Info node,
#   "(emacs)Packages", if you are unfamiliar with the Emacs package system.
#
#   **********
#
#   Before doing your first make, edit the CONFIGURABLE SECTION below.
#   Make any needed changes now and save the file.  Then select from the
#   USAGE lines immediately following.
#
#   USAGE:	For those installing GNU Hyperbole, use:
#   	             make help
#
#               For OO-Browser maintainers:
#                 To assemble the Hyperbole Emacs package for release:
#		     make pkg
#
#               The Hyperbole Manual is included in the package in four forms:
#
#                  "man/hyperbole.info"   - GNU browsable version
#                  "man/hyperbole.html"   - Web browsable version
#                  "man/hyperbole.pdf"    - Printable version
#                  "man/hyperbole.texi"   - source form

# Code:
##########################################################################
#                         CONFIGURABLE SECTION                           #
##########################################################################

# This ver setup won't work under any make except GNU make, so set it manually.
#HYPB_VERSION = "`head -3 hversion.el | tail -1 | sed -e 's/.*|\(.*\)|.*/\1/'`"
HYPB_VERSION = 6.0.1

# Emacs executable used to byte-compile .el files into .elc's.
# Possibilities include: emacs, infodock, xemacs, etc.
EMACS = emacs

# Site-specific Emacs Lisp libraries to load before byte-compiling any files
# from this package.  Typically the only reason to set this is to get Emacs
# to include the directory of this package into its load-path variable, which
# determines where it will find Lisp library files to load.  This is now
# handled automatically by Hyperbole for most modern versions of Emacs.
#
# You must include the .el or .elc file suffix on each library name and each
# must be preceded by the `-l' command-line flag.  If the directory in which
# the library is stored will not be in your Emacs load-path when Emacs
# attempts to load the library, you must include the full pathname to the
# library.  Here is an example setting.
#
# SITE_PRELOADS = -l ~/.emacs -l set-load-path.el
#
SITE_PRELOADS =

# Command used to build the .info version of the user manual.
TEXI2INFO = makeinfo --no-split

# Command used to build the .html version of the user manual.
# TEXI2HTML = id-texi2html -html_only -number -split_chapter # InfoDock-specific command
# TEXI2HTML = makeinfo --html --split=chapter # Chapter splitting doesn't seem to work in 6.0
TEXI2HTML = makeinfo --html --no-split

# Command used to build the .pdf version of the user manual.
TEXI2PDF = makeinfo --pdf --no-split

# Where to find the parent tree of the Hyperbole source directory.
id_dir = $(HOME)/sw-dev/emacs
# Where to find the .texi source of the user manual.
man_dir := $(shell pwd)/man
# Where to install the Hyperbole mouse key help file
data_dir = $(id_dir)/id-etc
# Where to install the Info version of the user manual.
info_dir = $(id_dir)/id-info
# Where to install the HTML version of the user manual.
html_dir = $(id_dir)/id-html

# Shell used to process this Makefile.  Bourne shell syntax is required.
SHELL = /bin/sh

# UNIX commands you may want to change for your particular system.
CP = \cp -p
ETAGS = \etags
INSTALL = \install -m 644 -c
MKDIR = \mkdir -p
MAKE = \make
RM = \rm -f
TAR = \tar
GPG = \gpg
ZIP = \zip -qry

# Directory in which to create new package distributions of Hyperbole.
pkg_dir = /tmp
pkg_hyperbole = $(pkg_dir)/hyperbole-$(HYPB_VERSION)

# Temp file to use to build .elc files.
ELISP_TO_COMPILE = $(pkg_dir)/elc-${USER}

##########################################################################
#                     NO CHANGES REQUIRED BELOW HERE.                    #
##########################################################################

# Libraries that must be pre-loaded before trying to byte-compile anything.
PRELOADS = $(SITE_PRELOADS) -l ./hload-path.el -l ./hversion.el -l ./hyperbole.el 

# Compile in batch mode. Under Emacs and XEmacs, load
# site-lisp/site-start.el, which may set load-path.
BATCHFLAGS = -batch -nw -Q

# Directories other than the current directory in which to find files.
# This doesn't seem to work in all versions of make, so we also add kotl/
# explicitly to those files which need it.
VPATH = kotl man

EL_SRC = hui-em-but.el hui-xe-but.el

EL_COMPILE = hact.el hactypes.el hargs.el hbdata.el hbmap.el hbut.el \
	     hgnus.el hhist.el hib-debbugs.el hib-doc-id.el hib-kbd.el \
	     hib-social.el hibtypes.el \
	     hinit.el hload-path.el hlvar.el hmail.el hmh.el hmoccur.el hmouse-info.el \
	     hmouse-drv.el hmouse-key.el hmouse-mod.el hmouse-sh.el hmouse-tag.el \
	     hpath.el hrmail.el hsite.el hsmail.el hsys-org.el hsys-w3.el htz.el \
	     hycontrol.el hui-jmenu.el hui-menu.el hui-mini.el hui-mouse.el hui-select.el \
	     hui-window.el hui.el hvar.el hversion.el hvm.el hypb.el hyperbole.el \
	     hyrolo-logic.el hyrolo-menu.el hyrolo.el hywconfig.el set.el

EL_KOTL = kotl/kexport.el kotl/kfile.el kotl/kfill.el kotl/kimport.el kotl/klabel.el \
	  kotl/klink.el kotl/kmenu.el kotl/knode.el kotl/kotl-mode.el \
          kotl/kcell.el kotl/kproperty.el kotl/kprop-em.el \
	  kotl/kprop-xe.el kotl/kview.el kotl/kvspec.el

ELC_COMPILE =  hactypes.elc hibtypes.elc hib-debbugs.elc hib-doc-id.elc hib-kbd.elc \
	     hib-social.elc hact.elc \
	     hargs.elc hbdata.elc hbmap.elc hbut.elc hgnus.elc hhist.elc \
	     hinit.elc hload-path.elc hlvar.elc hmail.elc hmh.elc hmoccur.elc hmouse-info.elc \
	     hmouse-drv.elc hmouse-key.elc hmouse-mod.elc hmouse-sh.elc hmouse-tag.elc \
	     hpath.elc hrmail.elc hsite.elc hsmail.elc hsys-org.elc hsys-w3.elc htz.elc \
	     hycontrol.elc hui-jmenu.elc hui-menu.elc hui-mini.elc hui-mouse.elc hui-select.elc \
	     hui-window.elc hui.elc hvar.elc hversion.elc hvm.elc hypb.elc hyperbole.elc \
	     hyrolo-logic.elc hyrolo-menu.elc hyrolo.elc hywconfig.elc set.elc

ELC_KOTL = kotl/kexport.elc kotl/kfile.elc kotl/kfill.elc kotl/kimport.elc kotl/klabel.elc \
	   kotl/klink.elc kotl/kmenu.elc kotl/knode.elc kotl/kotl-mode.elc \
           kotl/kcell.elc kotl/kproperty.elc \
	   kotl/kprop-xe.elc kotl/kview.el kotl/kvspec.elc

HYPERBOLE_FILES = dir hyperbole-pkg.el info html $(EL_SRC) $(EL_COMPILE) $(EL_KOTL) \
	$(ELC_COMPILE) ChangeLog COPYING Makefile HY-ABOUT HY-NEWS \
	HY-README HY-WHY.kotl DEMO MANIFEST _hypb .hypb file-newer smart-clib-sym \
	hyperbole-banner.png $(man_dir)/hkey-help.txt \
	$(man_dir)/hyperbole.texi $(man_dir)/version.texi

EL_TAGS = $(EL_SRC) $(EL_COMPILE) $(EL_KOTL)

.SUFFIXES:            # Delete the default suffixes
.SUFFIXES: .el .elc   # Define the list of file suffixes to match to rules

help: 
	@ echo "Use the Emacs Package Manager to build and install GNU Hyperbole."
	@ echo "See \"$(shell pwd)/INSTALL\" for installation instructions."
	@ echo "For help with Emacs packages, see the GNU Emacs Info Manual section, \"(emacs)Packages\"."
	@ echo ""
	@ echo "For Hyperbole maintainers, the Hyperbole distribution package is built with:"
	@ echo "     make pkg"

all: help

install: elc install-info install-html $(data_dir)/hkey-help.txt

install-info: $(info_dir)/hyperbole.info
$(info_dir)/hyperbole.info: $(man_dir)/hyperbole.info
	$(MKDIR) $(info_dir)/im; \
	  cd $(man_dir); $(INSTALL) hyperbole.info* $(info_dir); \
	  $(INSTALL) im/*.{png,eps} $(info_dir)/im

install-html: $(html_dir)/hyperbole.html
$(html_dir)/hyperbole.html: $(man_dir)/hyperbole.html
	$(MKDIR) $(html_dir)/im; \
	  cd $(man_dir); $(INSTALL) hyperbole.html* $(html_dir); \
	  $(INSTALL) im/*.{png,eps} $(html_dir)/im

$(data_dir)/hkey-help.txt: $(man_dir)/hkey-help.txt
	$(INSTALL) hkey-help.txt $(data_dir)

# Record any .el files that need to be compiled.
.el.elc:
	@ echo $< >> $(ELISP_TO_COMPILE)

# Compile all recorded .el files.
elc: elc-init $(ELC_KOTL) $(ELC_COMPILE)
	@- \test ! -f $(ELISP_TO_COMPILE) \
            || (echo "These files will be compiled: " \
                 && echo "`cat $(ELISP_TO_COMPILE)`" \
                 && $(EMACS) $(BATCHFLAGS) $(PRELOADS) \
                       -f batch-byte-compile `cat $(ELISP_TO_COMPILE)`)
	@ $(RM) $(ELISP_TO_COMPILE)

elc-init:
	@ $(RM) $(ELISP_TO_COMPILE)

# Remove and then rebuild all byte-compiled .elc files, even those .elc files
# which do not yet exist.
all-elc:
	$(RM) *.elc kotl/*.elc
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $(EL_KOTL) $(EL_COMPILE)

tags: TAGS
TAGS: $(EL_TAGS)
	$(ETAGS) $(EL_TAGS)

version: doc
	@ echo ""
	@ echo "Any fgrep output means the version number has not been updated in that file."
	fgrep -L $(HYPB_VERSION) Makefile HY-NEWS HY-ABOUT hversion.el hyperbole-pkg.el \
	  $(man_dir)/hyperbole.texi $(man_dir)/version.texi
	@ echo ""

# Build the Info, HTML and Postscript versions of the user manual.
doc: info html pdf

info: $(man_dir)/hyperbole.info
$(man_dir)/hyperbole.info: $(man_dir)/hyperbole.texi $(man_dir)/version.texi $(man_dir)/hkey-help.txt
	cd $(man_dir) && $(TEXI2INFO) hyperbole.texi

html: $(man_dir)/hyperbole.html
$(man_dir)/hyperbole.html: $(man_dir)/hyperbole.texi $(man_dir)/version.texi $(man_dir)/hkey-help.txt
	cd ${man_dir} && $(TEXI2HTML) hyperbole.texi

pdf: $(man_dir)/hyperbole.pdf
$(man_dir)/hyperbole.pdf: $(man_dir)/hyperbole.texi $(man_dir)/version.texi $(man_dir)/hkey-help.txt
	cd $(man_dir) && $(TEXI2PDF) hyperbole.texi

pkg: doc package
package: $(pkg_dir)/hyperbole-$(HYPB_VERSION).tar.sig

$(pkg_dir)/hyperbole-$(HYPB_VERSION).tar.sig: $(pkg_dir)/hyperbole-$(HYPB_VERSION).tar
	cd $(pkg_dir) && $(GPG) -ba -o hyperbole-$(HYPB_VERSION).tar.sig hyperbole-$(HYPB_VERSION).tar
	@ echo; echo "Hyperbole package built successfully:"
	@ ls -l $(pkg_dir)/hyperbole-$(HYPB_VERSION).tar*

$(pkg_dir)/hyperbole-$(HYPB_VERSION).tar: $(HYPERBOLE_FILES)
	make version
	$(RM) -r $(pkg_hyperbole)
	cd .. && COPYFILE_DISABLE=1 $(TAR) -clf $(pkg_dir)/h.tar hyperbole-$(HYPB_VERSION)
	cd $(pkg_dir) && COPYFILE_DISABLE=1 $(TAR) xf h.tar && cd $(pkg_hyperbole) && $(MAKE) packageclean
	cd $(pkg_dir) && $(RM) h.tar; \
	  COPYFILE_DISABLE=1 $(TAR) -clf $(pkg_dir)/hyperbole-$(HYPB_VERSION).tar hyperbole-$(HYPB_VERSION)
	$(INSTALL) HY-NEWS HY-README HY-WHY.kotl $(pkg_dir)/; chmod 644 $(pkg_dir)/*.tar

pkgclean: packageclean
packageclean:
	if [ -d $(pkg_hyperbole) ]; then \
	  cd $(pkg_hyperbole) && $(RM) -r .git* ChangeLog.* *autoloads.* *.elc TAGS TODO* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore GNUmakefile.id; fi
	if [ -d $(pkg_hyperbole)/kotl ]; then \
	  cd $(pkg_hyperbole)/kotl && $(RM) -r *autoloads.* *.elc TAGS TODO* .DS_Store \
	    core .place* ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man ]; then \
	  cd $(pkg_hyperbole)/man && $(RM) -r .DS_Store core .place* hyperbole.{log,aux,cp*,fn*,ky*,toc,vr*} \
	    ._* .*~ *~ *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi
	if [ -d $(pkg_hyperbole)/man/im ]; then \
	  cd $(pkg_hyperbole)/man/im && $(RM) -r .DS_Store core .place* ._* .*~ *~ \
	    *.ps *\# *- *.orig *.rej .nfs* CVS .cvsignore; fi

