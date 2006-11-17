clean-docs:
	rm -f $(doc).texi
	rm -f $(doc).info
	rm -f $(doc)scmfiles
	rm -f html-stamp
	rm -rf html
	rm -f $(addprefix $(doc).,aux cp cps fn fns ky log pdf pg pg toc tp tps vr vrs)
	rm -rf $(doc).html

EXTRA_DIST=$(doc).scm make-texinfo.scm make-html.scm docs.mk
DISTCLEANFILES=$(doc).texi $(doc)scmfiles

$(doc)scmfiles:
	guile --debug --use-srfi=13 -l $(srcdir)/$(doc).scm \
	 -c '(for-each (lambda (m) (format #t "~a.scm\n" (string-join (map symbol->string m) "/"))) (map car *modules*))' \
	 > $@
depfiles=$(addprefix $(top_srcdir)/src/,$(shell test ! -f $(doc)scmfiles || cat $(doc)scmfiles))

$(doc).texi: $(srcdir)/$(doc).scm $(doc)scmfiles $(depfiles)
	$(top_srcdir)/dev-environ $(srcdir)/make-texinfo.scm $(srcdir)/$(doc).scm >$@

html: html-stamp $(srcdir)/$(doc).scm $(depfiles)
html-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ $(srcdir)/make-html.scm $(srcdir)/$(doc).scm
	touch $@

