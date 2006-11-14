#!/bin/sh
exec guile --debug -s $0 "$@"
!#

(use-modules (texinfo reflection)
             (texinfo serialize))

(define (main config-scm)
  (load config-scm)
  (display
   (stexi->texi
    (package-stexi-documentation
     (map car *modules*)
     *name*
     (string-append *texinfo-basename* ".info")
     (package-stexi-standard-prologue
      *name*
      (string-append *texinfo-basename* ".info")
      *texinfo-category*
      *description*
      (package-stexi-standard-copying
       *name* *version* *updated* *years* *copyright-holder* *permissions*)
      (package-stexi-standard-titlepage
       *name* *version* *updated* *authors*)
      (package-stexi-standard-menu
       *name* (map car *modules*) (map cdr *modules*)
       *extra-texinfo-menu-entries*))
     *texinfo-epilogue*))))

(apply main (cdr (command-line)))
