EXTRA_DIST = gikope.el izonmoji-mode.el navi2ch-logo.el \
	navi2ch-migemo.el navi2ch-track-mouse.el

all:

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
