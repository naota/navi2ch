## アイコンは Wanderlust (ftp://ftp.gohome.org/pub/elisp/) からもらった。
icondir = @icondir@
icon_DATA = plugged.xpm unplugged.xpm

EXTRA_DIST = $(icon_DATA)

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
