(define page
  '((h2 "releases")

    (p "Releases are available from our "
       (a (@ (href "http://download.gna.org/guile-lib/"))
          "release area")
       " on gna.org.")))

(load "../template.scm")
(define (make-index)
  (output-html page "guile-lib: download" "download" "../"))
