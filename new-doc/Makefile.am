clean: clean-am
	rm -f $(doc).texi
	rm -f $(doc).info
	rm -f scmfiles
	rm -f html-stamp
	rm -rf html

doc=guile-library

scmfiles:
	guile --debug --use-srfi=13 -l guile-library.scm \
	 -c '(for-each (lambda (m) (format #t "~a.scm\n" (string-join (map symbol->string m) "/"))) (map car *modules*))' \
	 > $@
depfiles=$(addprefix $(top_srcdir)/src/,$(shell cat scmfiles))

$(doc).texi: $(doc).scm scmfiles $(depfiles)
	$(top_srcdir)/dev-environ $(srcdir)/make-texinfo.scm $(doc).scm >$@

info_TEXINFOS=guile-library.texi
guile_library_TEXINFOS=fdl.texi

html: html-stamp $(doc).scm $(depfiles)
html-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ $(srcdir)/make-html.scm $(doc).scm
	touch $@

