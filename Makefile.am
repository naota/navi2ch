lisp_LISP = navi2ch-article.el navi2ch-articles.el \
	navi2ch-board-misc.el navi2ch-board.el navi2ch-bookmark.el \
	navi2ch-face.el navi2ch-head.el navi2ch-history.el \
	navi2ch-list.el navi2ch-message.el navi2ch-mona.el \
	navi2ch-net.el navi2ch-popup-article.el navi2ch-search.el \
	navi2ch-util.el navi2ch-vars.el navi2ch.el
EXTRA_DIST = Makefile.old bcomp.el $(lisp_LISP)
SUBDIRS = doc

# defmacro / defsubst dependencies
# should be fixed: thre are many defsubst cross references between files.
navi2ch-article.elc: navi2ch-util.elc
navi2ch-board-misc.elc: navi2ch-article.elc navi2ch-util.elc
navi2ch-list.elc: navi2ch-util.elc
navi2ch-board.elc: navi2ch-article.elc navi2ch-util.elc
navi2ch-bookmark.elc: navi2ch-article.elc
navi2ch-search.elc: navi2ch-article.elc
navi2ch-popup-article.elc: navi2ch-article.elc
