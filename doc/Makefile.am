clean: clean-am
	rm -f $(doc).texi
	rm -f $(doc).info
	rm -f scmfiles
	rm -f html-stamp
	rm -rf html

EXTRA_DIST=guile-library.scm make-texinfo.scm make-html.scm base.css
DISTCLEANFILES=guile-library.texi scmfiles

doc=guile-library

scmfiles:
	guile --debug --use-srfi=13 -l $(srcdir)/guile-library.scm \
	 -c '(for-each (lambda (m) (format #t "~a.scm\n" (string-join (map symbol->string m) "/"))) (map car *modules*))' \
	 > $@
depfiles=$(addprefix $(top_srcdir)/src/,$(shell test ! -f scmfiles || cat scmfiles))

$(doc).texi: $(srcdir)/$(doc).scm scmfiles $(depfiles)
	$(top_srcdir)/dev-environ $(srcdir)/make-texinfo.scm $(srcdir)/$(doc).scm >$@

info_TEXINFOS=guile-library.texi
guile_library_TEXINFOS=fdl.texi

html: html-stamp $(srcdir)/$(doc).scm $(depfiles)
html-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ $(srcdir)/make-html.scm $(srcdir)/$(doc).scm
	touch $@

