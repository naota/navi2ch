# Makefile.in generated automatically by automake 1.5 from Makefile.am.

# Copyright 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
# Free Software Foundation, Inc.
# This Makefile.in is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.



SHELL = /bin/sh

srcdir = .
top_srcdir = ..

prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man
includedir = ${prefix}/include
oldincludedir = /usr/include
pkgdatadir = $(datadir)/navi2ch
pkglibdir = $(libdir)/navi2ch
pkgincludedir = $(includedir)/navi2ch
top_builddir = ..

ACLOCAL = ${SHELL} /home/rui/src/navi2ch/missing --run aclocal
AUTOCONF = ${SHELL} /home/rui/src/navi2ch/missing --run autoconf
AUTOMAKE = ${SHELL} /home/rui/src/navi2ch/missing --run automake
AUTOHEADER = ${SHELL} /home/rui/src/navi2ch/missing --run autoheader

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL}
INSTALL_HEADER = $(INSTALL_DATA)
transform = s,x,x,
NORMAL_INSTALL = :
PRE_INSTALL = :
POST_INSTALL = :
NORMAL_UNINSTALL = :
PRE_UNINSTALL = :
POST_UNINSTALL = :
host_alias = 
host_triplet = i686-pc-linux-gnu
AMTAR = ${SHELL} /home/rui/src/navi2ch/missing --run tar
AWK = gawk
DEPDIR = .deps
EMACS = emacs
EXEEXT = 
INSTALL_STRIP_PROGRAM = ${SHELL} $(install_sh) -c -s
MAINT = #
OBJEXT = o
PACKAGE = navi2ch
VERSION = 1.5.1-DEV
am__include = @am__include@
am__quote = @am__quote@
install_sh = /home/rui/src/navi2ch/install-sh
lispdir = /usr/local/share/emacs/21.1/site-lisp/navi2ch

icondir = ${datadir}/pixmaps/navi2ch
icon_DATA = plugged.xpm unplugged.xpm

EXTRA_DIST = $(icon_DATA)
subdir = icons
mkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs
CONFIG_CLEAN_FILES =
DIST_SOURCES =
DATA = $(icon_DATA)

DIST_COMMON = Makefile.am Makefile.in
all: all-am

.SUFFIXES:
$(srcdir)/Makefile.in: # Makefile.am  $(top_srcdir)/configure.in $(ACLOCAL_M4)
	cd $(top_srcdir) && \
	  $(AUTOMAKE) --gnu  icons/Makefile
uninstall-info-am:
install-iconDATA: $(icon_DATA)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(icondir)
	@list='$(icon_DATA)'; for p in $$list; do \
	  if test -f "$$p"; then d=; else d="$(srcdir)/"; fi; \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " $(INSTALL_DATA) $$d$$p $(DESTDIR)$(icondir)/$$f"; \
	  $(INSTALL_DATA) $$d$$p $(DESTDIR)$(icondir)/$$f; \
	done

uninstall-iconDATA:
	@$(NORMAL_UNINSTALL)
	@list='$(icon_DATA)'; for p in $$list; do \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  echo " rm -f $(DESTDIR)$(icondir)/$$f"; \
	  rm -f $(DESTDIR)$(icondir)/$$f; \
	done
tags: TAGS
TAGS:


DISTFILES = $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)

top_distdir = ..
distdir = $(top_distdir)/$(PACKAGE)-$(VERSION)

distdir: $(DISTFILES)
	@for file in $(DISTFILES); do \
	  if test -f $$file; then d=.; else d=$(srcdir); fi; \
	  dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	  if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	    $(mkinstalldirs) "$(distdir)/$$dir"; \
	  fi; \
	  if test -d $$d/$$file; then \
	    cp -pR $$d/$$file $(distdir) \
	    || exit 1; \
	  else \
	    test -f $(distdir)/$$file \
	    || cp -p $$d/$$file $(distdir)/$$file \
	    || exit 1; \
	  fi; \
	done
check-am: all-am
check: check-am
all-am: Makefile $(DATA)

installdirs:
	$(mkinstalldirs) $(DESTDIR)$(icondir)

install: install-am
install-exec: install-exec-am
install-data: install-data-am
uninstall: uninstall-am

install-am: all-am
	@$(MAKE) $(AM_MAKEFLAGS) install-exec-am install-data-am

installcheck: installcheck-am
install-strip:
	$(MAKE) $(AM_MAKEFLAGS) INSTALL_PROGRAM="$(INSTALL_STRIP_PROGRAM)" \
	  `test -z '$(STRIP)' || \
	    echo "INSTALL_PROGRAM_ENV=STRIPPROG='$(STRIP)'"` install
mostlyclean-generic:

clean-generic:

distclean-generic:
	-rm -f Makefile $(CONFIG_CLEAN_FILES) stamp-h stamp-h[0-9]*

maintainer-clean-generic:
	@echo "This command is intended for maintainers to use"
	@echo "it deletes files that may require special tools to rebuild."
clean: clean-am

clean-am: clean-generic mostlyclean-am

distclean: distclean-am

distclean-am: clean-am distclean-generic

dvi: dvi-am

dvi-am:

info: info-am

info-am:

install-data-am: install-iconDATA

install-exec-am:

install-info: install-info-am

install-man:

installcheck-am:

maintainer-clean: maintainer-clean-am

maintainer-clean-am: distclean-am maintainer-clean-generic

mostlyclean: mostlyclean-am

mostlyclean-am: mostlyclean-generic

uninstall-am: uninstall-iconDATA uninstall-info-am

.PHONY: all all-am check check-am clean clean-generic distclean \
	distclean-generic distdir dvi dvi-am info info-am install \
	install-am install-data install-data-am install-exec \
	install-exec-am install-iconDATA install-info install-info-am \
	install-man install-strip installcheck installcheck-am \
	installdirs maintainer-clean maintainer-clean-generic \
	mostlyclean mostlyclean-generic uninstall uninstall-am \
	uninstall-iconDATA uninstall-info-am


Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:
