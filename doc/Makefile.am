doc=guile-library

include docs.mk

info_TEXINFOS=guile-library.texi
guile_library_TEXINFOS=fdl.texi

www: html guile-library.pdf clean-www
	find www -name 'index.scm' -print \
	  | while read f; do \
              guile -l "$$f" -c '(make-index)' \
	        > `echo $$f | sed -e s,\.scm,\.html,`; \
	    done
	cp -a html www/doc/ref
	cp guile-library.pdf www/doc/

www-commit: www
	svn rm -m 'make www in guile-lib/docs' svn+ssh://wingo@svn.gna.org/svn/guile-lib/website
	svn import -m 'make www in guile-lib/docs' www svn+ssh://wingo@svn.gna.org/svn/guile-lib/website

clean-www:
	find www -name 'index.html' -print \
	  | while read f; do rm -f "$$f"; done
	rm -rf www/doc/ref
	rm -f www/doc/guile-library.pdf

clean: clean-docs clean-www
