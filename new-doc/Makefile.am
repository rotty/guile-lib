clean: clean-am
	rm -f $(doc).texi
	rm -f $(doc).info
	rm -f html-stamp
	rm -rf html

doc=guile-library

$(doc).texi: $(doc).scm
	$(top_srcdir)/dev-environ $(srcdir)/make-texinfo.scm $(doc).scm >$@

info_TEXINFOS=guile-library.texi
guile_library_TEXINFOS=fdl.texi

html: html-stamp $(package).scm
html-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ $(srcdir)/make-html.scm $(doc).scm
	touch $@

