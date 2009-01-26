(define page
  '((h2 "guile-lib")

    (p (code "guile-lib") " is intended as an accumulation place for
pure-scheme Guile modules, allowing for people to cooperate
integrating their generic Guile modules into a coherent library.
Think \"a down-scaled,
limited-scope " (a (@ (href "http://www.cpan.org/")) "CPAN") "
for Guile\".")

    (p "Also, it can be seen as a code staging area for Guile;
the Guile developers could decide to integrate some of the code
into guile-core. An example for a possible candidate is
SRFI-35.")

    (h3 (@ (style "text-align: center")) "latest news")

    (latest-news)

    (h4 (rlink "news/" "older news..."))))

(define this-page page)
(load "news/index.scm")
(define news-page page)
(define page this-page)

(define (news tag args . body)
  `(div (h4 ,@(assq-ref (cdr args) 'date) ": "
            ,@(assq-ref (cdr args) 'title))
        (p ,@body)))

(define (latest-news . body)
  (cadr news-page))

(define (make-index)
  (output-html page "guile-lib" "guile-lib" ""
               #:transform-rules `((news . ,news)
                                   (latest-news *macro* . ,latest-news))))
