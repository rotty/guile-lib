TESTS= \
	graph.topological-sort.scm	\
	htmlprag.scm			\
	logging.logger.scm		\
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
	sxml.ssax.scm			\
	sxml.transform.scm		\
	sxml.xpath.scm			\
	term.ansi-color.scm		\
	texinfo.scm			\
	texinfo.serialize.scm		\
	text.parse-lalr.scm

TESTS_ENVIRONMENT=$(top_srcdir)/dev-environ guile -s

%.check: %
	$(TESTS_ENVIRONMENT) $*

EXTRA_DIST=$(TESTS)
