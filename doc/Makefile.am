info_TEXINFOS = navi2ch.texi
EXTRA_DIST = Makefile.pmake batch-texi2info.el
CLEANFILES = navi2ch.pdf navi2ch.html

.texi.info:
	rm -f $(@)*
	$(EMACS) -q -batch -l $(srcdir)/batch-texi2info.el \
		-f batch-texi2info $<

Makefile: $(srcdir)/Makefile.in  $(top_builddir)/config.status
	cd $(top_builddir) && \
	CONFIG_HEADERS= CONFIG_LINKS= \
	CONFIG_FILES=$(subdir)/$@ $(SHELL) ./config.status

# create PDF file
pdf: navi2ch.pdf
.texi.dvi:
	TEX=ptex texi2dvi $<
.dvi.pdf:
	rm -f $@ && dvipdfm $<

# create HTML file
html: navi2ch.html
.texi.html:
	$(MAKEINFO) $(AM_MAKEINFOFLAGS) $(MAKEINFOFLAGS) \
		--html --no-split -o $@ $<
