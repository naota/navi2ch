AUTOMAKE_OPTIONS = no-texinfo.tex

info_TEXINFOS = navi2ch.texi
EXTRA_DIST = Makefile.pmake batch-texi2info.el

.texi.info:
	rm -f $@; \
	  ${EMACS} -q -batch -l ./batch-texi2info.el -f batch-texi2info $<

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status
