(define page
  '((h2 "releases")

    (p "Releases are available from our "
       (a (@ (href "http://download.savannah.nongnu.org/releases/guile-lib/"))
          "release area")
       " on " (code "savannah.nongnu.org."))))

(load "../template.scm")
(define (make-index)
  (output-html page "guile-lib: download" "download" "../"))
