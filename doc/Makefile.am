AUTOMAKE_OPTIONS = no-texinfo.tex

info_TEXINFOS = navi2ch.texi
EXTRA_DIST = Makefile.pmake batch-texi2info.el

.texi.info:
	rm -f $@; \
	  ${EMACS} -q -batch -l ./batch-texi2info.el -f batch-texi2info $<
