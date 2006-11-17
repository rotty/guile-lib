(use-modules (ice-9 optargs) (sxml simple) (sxml transform))

(define* (templatize body title category relative-root-path #:key (scm-path "index.scm"))
  (define (href . args)
    `(href ,(apply string-append relative-root-path args)))
  (define (rev? text)
    (if (string=? category text)
        '((class "reversed"))
        '()))
  (define (list-join l infix)
    "Infixes @var{infix} into list @var{l}."
    (if (null? l) l
        (let lp ((in (cdr l)) (out (list (car l))))
          (cond ((null? in) (reverse out))
                (else (lp (cdr in) (cons* (car in) infix out)))))))
  (define (make-navbar)
    `(div (@ (id "menu-bar"))
          ,@(list-join
             (map (lambda (x)
                    `(a (@ ,(href (cdr x) "/") ,@(rev? (car x))) ,(car x)))
                  '(("documentation" . "doc")
                    ("download" . "download")
                    ("developers" . "dev")))
             " ")))
  `(html
    (head (title ,title)
          (meta (@ (name "Generator")
                   (content "The Guile SXML Toolkit")))
          (link (@ (rel "stylesheet") (type "text/css")
                   ,(href "base.css"))))
    (body
     (div (@ (id "body"))
          (div (@ (id "heading"))
               (h1 (a (@ ,(href "")) "guile-gnome"))
               ,(make-navbar))
          (div (@ (id "text"))
               ,@body)
          (div (@ (id "footer"))
               "(powered by " (a (@ (href ,scm-path)) "sxml") ")"
               (br)
               "Copyright (C) 2003,2004,2005,2006 Richard Todd, Andreas Rottmann, Andy Wingo."
               (br)
               "Verbatim copying and distribution of this entire article "
               "is permitted in any medium, provided this notice is preserved.")))))

(define *xhtml-doctype*
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define (preprocess page transform-rules relative-root-path)
  (define (rlink tag path . body)
    `(a (@ (href ,(string-append relative-root-path path))) ,body))
  (pre-post-order
   page
   `(,@transform-rules
     (rlink . ,rlink)
     (*text* . ,(lambda (tag text) text))
     (*default* . ,(lambda args args)))))

(define* (output-html page title category relative-root-path
                      #:key (scm-path "index.scm") (transform-rules '()))
  (display *xhtml-doctype*)
  (sxml->xml
   (templatize (preprocess page transform-rules relative-root-path)
               title category relative-root-path #:scm-path scm-path)))
