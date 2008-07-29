AUTOMAKE_OPTIONS = 1.8
lisp_LISP = navi2ch-version.el \
	navi2ch-vars.el navi2ch-face.el navi2ch-util.el \
	navi2ch-net.el navi2ch-list.el navi2ch-article.el \
	navi2ch-popup-article.el navi2ch-board-misc.el \
	navi2ch-board.el navi2ch-articles.el navi2ch-bookmark.el \
	navi2ch-history.el navi2ch-search.el navi2ch-message.el \
	navi2ch.el \
	navi2ch-head.el navi2ch-mona.el navi2ch-e21.el navi2ch-xmas.el \
	navi2ch-splash.el navi2ch-directory.el \
	navi2ch-be2ch.el navi2ch-multibbs.el \
	navi2ch-jbbs-net.el navi2ch-jbbs-shitaraba.el navi2ch-machibbs.el \
	navi2ch-futaba.el navi2ch-megabbs.el \
	navi2ch-http-date.el navi2ch-localfile.el navi2ch-oyster.el \
	navi2ch-auto-modify.el
lisp_DATA = navi2ch-config.el

EXTRA_DIST = Makefile.old bcomp.el $(lisp_LISP) \
	bcomp.el.in navi2ch-version.el.in ChangeLog.1 ChangeLog.2
BUILT_SOURCES = $(srcdir)/navi2ch-version.el $(srcdir)/bcomp.el
SUBDIRS = contrib doc icons

$(srcdir)/navi2ch-version.el: @MAINTAINER_MODE_TRUE@ $(srcdir)/navi2ch-version.el.in $(srcdir)/configure.in
	rm -f $@.tmp && \
	sed -e 's/@\(VERSION\)@/$(VERSION)/' \
		$(srcdir)/navi2ch-version.el.in > $@.tmp && \
	mv $@.tmp $@

snap:
	rm -f $(srcdir)/navi2ch-version.el && \
	$(MAKE) $(AM_MAKEFLAGS) VERSION=@VERSION@-`date +%y%m%d` dist elcdist && \
	rm -f $(srcdir)/navi2ch-version.el && \
	$(MAKE) $(AM_MAKEFLAGS) $(srcdir)/navi2ch-version.el

elcdist: $(ELCFILES)
	rm -rf $(distdir) && \
	mkdir $(distdir) && \
	cp -p $(ELCFILES) $(distdir) && \
	$(AMTAR) chof - $(distdir) | GZIP=$(GZIP_ENV) gzip -c >$(distdir)-elc.tar.gz && \
	rm -rf $(distdir)

$(srcdir)/bcomp.el: @MAINTAINER_MODE_TRUE@ $(srcdir)/bcomp.el.in $(srcdir)/Makefile.am
	rm -f $@ && \
	list=`for file in $(lisp_LISP); do echo -n "\"$$file\" "; done` && \
	sed -e "s/@\(lisp_LISP\)@/$$list/" < $(srcdir)/bcomp.el.in > $@

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$@ $(SHELL) ./config.status

Makefiles: $(top_builddir)/config.status
	$(SHELL) $(top_builddir)/config.status

update:
	( cd $(srcdir); $(CVS) update -P -d ) && \
	$(MAKE) $(AM_MAKEFLAGS) Makefiles
