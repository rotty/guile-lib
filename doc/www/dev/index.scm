(define main-archive-url "http://arch.gna.org/guile-lib/guile-lib.bzr")

(define page
  `((h2 "developer information")

    (h3 "dependencies")

    (p (code "guile-lib") " works with the current "
       (a (@ (href "http://www.gnu.org/software/guile/")) "Guile") " 1.8"
       ", as well as with the old 1.6 stable series.")

    (h3 "source repository")

    (p "Guile-lib is managed with "
       (a (@ (href "http://bazaar-vcs.org/")) "bzr") ", a distributed "
       "version control system. To grab guile-lib, run the following:")

    (pre "bzr get " ,main-archive-url " guile-lib\n"
         "cd guile-lib\n"
         "./autogen.sh --prefix=... && make")

    (p "At that point you can install guile-lib with " 
       (code "make install") ", or run it uninstalled using the "
       (code "dev-environ") " script.")

    (h3 "patches")

    (p "Send patches to the " (code "guile-lib-dev") " list. "
       "It's best to send bundles:")

    (pre "# hack hack hack\n"
         "bzr commit -m 'fixed my thing'\n"
         "bzr bundle > bundle-file\n"
         "# then attach the bundle to a mail to the list")
    
    (p "This will bundle up any commits to your repo that are not
in upstream, that is, that are not in the place you initially
pulled your packages from. You can specify other options
to " (code "bundle") ", see " (code "bzr help commands") ".")

    (h3 "gna project page")

    (p "We also have
a " (a (@ (href "http://gna.org/projects/guile-lib")) "page
on GNA!") ".")))

(load "../template.scm")
(define (make-index)
  (output-html page "guile-lib: developers" "developers" "../"))
