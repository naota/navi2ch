lisp_LISP = navi2ch-article.el navi2ch-articles.el \
	navi2ch-board-misc.el navi2ch-board.el navi2ch-bookmark.el \
	navi2ch-face.el navi2ch-head.el navi2ch-history.el \
	navi2ch-list.el navi2ch-message.el navi2ch-mona.el \
	navi2ch-net.el navi2ch-popup-article.el navi2ch-search.el \
	navi2ch-util.el navi2ch-vars.el navi2ch-version.el navi2ch.el
EXTRA_DIST = Makefile.old bcomp.el $(lisp_LISP) navi2ch-version.el.in
BUILT_SOURCES = navi2ch-version.el
SUBDIRS = doc

navi2ch-version.el: $(srcdir)/navi2ch-version.el.in $(srcdir)/configure.in
	rm -f $@.tmp
	sed -e 's/@\(VERSION\)@/$(VERSION)/' $(srcdir)/navi2ch-version.el.in > $@.tmp
	mv $@.tmp $@

snap:
	rm -f navi2ch-version.el
	make VERSION=@VERSION@-`date +%y%m%d` dist
	rm -f navi2ch-version.el

# defmacro / defsubst dependencies
# should be fixed: thre are many defsubst cross references between files.
navi2ch-article.elc: navi2ch-util.elc
navi2ch-board-misc.elc: navi2ch-article.elc navi2ch-util.elc
navi2ch-list.elc: navi2ch-util.elc
navi2ch-board.elc: navi2ch-article.elc navi2ch-util.elc
navi2ch-bookmark.elc: navi2ch-article.elc
navi2ch-search.elc: navi2ch-article.elc
navi2ch-popup-article.elc: navi2ch-article.elc
