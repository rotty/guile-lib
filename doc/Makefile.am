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
	( export CVSDIR=:ext:wingo@cvs.sv.nongnu.org:/sources/guile-lib; cd www; cvs -d $$CVSDIR import -m 'make www in guile-lib/docs' guile-lib gnu $(shell date "+D%Y%m%d%H%M%S"))

clean-www:
	find www -name 'index.html' -print \
	  | while read f; do rm -f "$$f"; done
	rm -rf www/doc/ref
	rm -f www/doc/guile-library.pdf

clean: clean-docs clean-www
