lisp_LISP = navi2ch-version.el \
	navi2ch-vars.el navi2ch-face.el navi2ch-util.el \
	navi2ch-net.el navi2ch-list.el navi2ch-article.el \
	navi2ch-popup-article.el navi2ch-board-misc.el \
	navi2ch-board.el navi2ch-articles.el navi2ch-bookmark.el \
	navi2ch-history.el navi2ch-search.el navi2ch-message.el \
	navi2ch.el \
	navi2ch-head.el navi2ch-mona.el

EXTRA_DIST = Makefile.old bcomp.el $(lisp_LISP) \
	bcomp.el.in navi2ch-version.el.in
BUILT_SOURCES = navi2ch-version.el bcomp.el
SUBDIRS = doc

navi2ch-version.el: $(srcdir)/navi2ch-version.el.in $(srcdir)/configure.in
	rm -f $@.tmp && \
	sed -e 's/@\(VERSION\)@/$(VERSION)/' \
		$(srcdir)/navi2ch-version.el.in > $@.tmp && \
	mv $@.tmp $@

snap:
	rm -f navi2ch-version.el && \
	$(MAKE) $(AM_MAKEFLAGS) VERSION=@VERSION@-`date +%y%m%d` dist elcdist && \
	rm -f navi2ch-version.el

recompile: $(lisp_LISP)
	rm -f $(ELCFILES) && \
	EMACS=$(EMACS) $(SHELL) $(srcdir)/elisp-comp $(lisp_LISP) && \
	EMACS=$(EMACS) $(SHELL) $(srcdir)/elisp-comp $(lisp_LISP)

elcdist: recompile
	$(AMTAR) chof - $(ELCFILES) | GZIP=$(GZIP_ENV) gzip -c >$(distdir)-elc.tar.gz

bcomp.el: @MAINTAINER_MODE_TRUE@ bcomp.el.in Makefile.am
	rm -f $@ && \
	list=`for file in $(lisp_LISP); do echo -n "\"$$file\" "; done` && \
	sed -e "s/@\(lisp_LISP\)@/$$list/" < bcomp.el.in > $@

$(ELCFILES): $(lisp_LISP)
	$(MAKE) $(AM_MAKEFLAGS) recompile

Makefile: $(srcdir)/Makefile.in $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$@ $(SHELL) ./config.status
