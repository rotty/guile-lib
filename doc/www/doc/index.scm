(define page
  '((h2 "documentation")

    (h3 (a (@ (href "ref/"))
           "guile-library reference manual"))
    (p "The guile-lib reference manual, in HTML format. Also available in "
       (a (@ (href "guile-library.pdf")) "PDF format") ", and available at "
       "runtime via the " (code "help") " system.")))

(load "../template.scm")
(define (make-index)
  (output-html page "guile-lib: docs" "documentation" "../"))
