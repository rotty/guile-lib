TESTS= \
	api.scm				\
	container.async-queue.scm	\
	graph.topological-sort.scm	\
	htmlprag.scm			\
	logging.logger.scm		\
	match-bind.scm			\
	math.minima.scm			\
	math.primes.scm			\
	md5.scm				\
	os.process.scm			\
	search.basic.scm		\
	srfi.srfi-35.scm		\
	statprof.scm			\
	scheme.kwargs.scm		\
	string.completion.scm		\
	string.transform.scm		\
	string.wrap.scm			\
	sxml.fold.scm			\
	sxml.ssax.scm			\
	sxml.transform.scm		\
	sxml.xpath.scm			\
	term.ansi-color.scm		\
	texinfo.scm			\
	texinfo.docbook.scm		\
	texinfo.serialize.scm		\
	text.parse-lalr.scm

TESTS_ENVIRONMENT=\
	API_FILE=$(srcdir)/guile-library.api \
	DOC_GUILE_LIBRARY_SCM=$(top_srcdir)/doc/guile-library.scm \
	$(top_srcdir)/dev-environ guile -s

script := '(load (getenv "DOC_GUILE_LIBRARY_SCM"))\
(for-each \
(lambda (mod) \
(display " \"") (write (car mod)) (display "\" ")) \
 *modules*)'

guile-library.api.update:
	$(top_srcdir)/dev-environ $(srcdir)/update-api \
	$(shell DOC_GUILE_LIBRARY_SCM=$(top_srcdir)/doc/guile-library.scm \
	   guile -c $(script)) > $(srcdir)/guile-library.api

%.check: %
	$(TESTS_ENVIRONMENT) $(srcdir)/$*

EXTRA_DIST=$(TESTS) guile-library.api
