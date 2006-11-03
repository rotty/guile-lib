CLEANFILES=texi-stamp html-stamp

texi: texi-stamp
texi-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ guile -l $(srcdir)/index.scm \
	   -c '(make-texinfo)'
	touch $@

html: html-stamp
html-stamp: $(scm-module-files)
	$(top_srcdir)/dev-environ guile -l $(srcdir)/index.scm \
	   -c '(make-html)'
	touch $@

# arch-tag: 90a459b2-5a89-41d2-8a0a-6832d1276aaf

