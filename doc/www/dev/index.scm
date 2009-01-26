(define main-archive-url "git://git.sv.gnu.org/guile-lib.git")
(define dev-archive-url "ssh://git.sv.gnu.org/srv/git/guile-lib.git")

(define page
  `((h2 "developer information")

    (h3 "dependencies")

    (p (code "guile-lib") " works with the current "
       (a (@ (href "http://www.gnu.org/software/guile/")) "Guile") " 1.8"
       ", as well as with the old 1.6 stable series.")

    (h3 "source repository")

    (p "Guile-lib is managed with "
       (a (@ (href "http://git-scm.com/")) "git") ", a distributed "
       "version control system. To grab guile-lib, run the following:")

    (pre "git clone " ,main-archive-url " guile-lib\n"
         "cd guile-lib\n"
         "./autogen.sh --prefix=... && make")

    (p "At that point you can install guile-lib with " 
       (code "make install") ", or run it uninstalled using the "
       (code "dev-environ") " script.")

    (p "Developers with SSH access should check out "
       (code ,dev-archive-url) " instead.")

    (h3 "web history browser")

    (p "Catch up with what's been happening by visiting "
       (code "guile-lib") "'s "
       (a (@ (href "http://git.savannah.nongnu.org/gitweb/?p=guile-lib.git"))
          "gitweb") ".")

    (h3 "patches")

    (p "Send patches to the " (code "guile-devel") " list. The list "
       "itself belongs to Guile, but most people that have commit access "
       "to Guile also have commit access to " (code "guile-lib") ".")

    (p "Git has a little bit of a learning curve. If you aren't comfortable "
       "with committing, just do a " (code "git diff > mypatch.patch") ". "
       "Otherwise, send your patches using " (code "git-format-patch") ":")

    (pre "# hack hack hack\n"
         "git commit -a -m 'fixed my thing'\n"
         "git format-patch origin/master..HEAD\n"
         "# then attach the generated patch files to a mail to the list")
    
    (h3 "savannah project page")

    (p "We also have
a " (a (@ (href "http://savannah.nongnu.org/projects/guile-lib")) "page
on Savannah") ".")))

(load "../template.scm")
(define (make-index)
  (output-html page "guile-lib: developers" "developers" "../"))
