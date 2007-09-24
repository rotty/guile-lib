(define page
  '((h2 "news")

    (news
     (@ (date "24 September 2007")
        (title "guile-lib 0.1.6 available"))
     (code "guile-lib") " 0.1.6 has been released. Check the "
     (a (@ (href "http://download.gna.org/guile-lib/NEWS")) "NEWS")
     " for details.")
    
    (news
     (@ (date "9 August 2007")
        (title "guile-lib 0.1.5 available"))
     (code "guile-lib") " 0.1.5 has been released featuring a "
     (rlink "doc/ref/container.async-queue/" "new module")
     " plus a few bugfixes.")
    
    (news
     (@ (date "20 July 2007")
        (title "guile-lib 0.1.4 available"))
     (code "guile-lib") " 0.1.4 has been released, featuring two "
     (rlink "doc/ref/match-bind/" "new") " "
     (rlink "doc/ref/scheme.kwargs/" "modules") " "
     " and other bugfixes and improvements.")
    
    ))


(load "../template.scm")

(define (news tag args . body)
  `(div (h4 ,@(assq-ref (cdr args) 'date) ": "
            ,@(assq-ref (cdr args) 'title))
        (p ,@body)))

(define (make-index)
  (output-html page "guile-lib: news" "news" "../"
               #:transform-rules `((news . ,news))))
