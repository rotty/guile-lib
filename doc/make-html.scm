#!/bin/sh
exec guile --debug -s $0 "$@"
!#

(use-modules (texinfo reflection)
             (texinfo html)
             (sxml simple)
             (sxml transform)
             ((srfi srfi-13) :select (string-join)))

(define (makedirs path)
  (let loop ((path ".") (components (string-split path #\/)))
    (if (not (null? components))
        (let ((sub-path (string-append path "/" (car components))))
          (if (or (not (file-exists? sub-path))
                  (not (file-is-directory? sub-path)))
              (mkdir sub-path))
          (loop sub-path (cdr components))))))

(define (wrap-html title root-path scm-url body)
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (@ (name "Generator")
              (content "The Guile SXML Toolkit")))
     (style (@ (type "text/css") (media "screen"))
       "@import url("
       ,(string-append root-path "base.css")
       ");"))
    (body
     (div (@ (id "body"))
          (h1 (@ (id "heading"))
              (a (@ (href ,root-path)) "guile-lib"))
          (div (@ (id "text"))
               (h2 (@ (class "centered")) ,title)
               ,@body)
          (div (@ (id "footer"))
               "powered by "
               (a (@ (href ,scm-url)) "sxml"))))))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"))

(define (module->str scm)
  (call-with-output-string (lambda (p) (display scm p))))
(define (module->ustr scm)
  (string-append (string-join (map symbol->string scm) ".") "/"))

(define (make-html-index)
  (with-output-to-file "html/index.html"
    (lambda ()
      (display xhtml-doctype)
      (sxml->xml
       (pre-post-order
        (stexi->shtml
         `(texinfo
           (% (title "unused"))
           ,@(cdr
              (package-stexi-standard-copying
               *name* *version* *updated* *years* *copyright-holder*
               *permissions*))
           (table
            (% (formatter (bold)))
            ,@(map
               (lambda (module description)
                 `(entry
                   (% (heading
                       (uref (% (url ,(module->ustr module))
                                (title ,(module->str module))))))
                   ,@description))
               (map car *modules*) (map cdr *modules*)))))
        `((html . ,(lambda (tag attrs head body)
                     (wrap-html
                      *name*
                      *html-relative-root-path*
                      "index.scm"
                      (cdr body)))) ;; cdr past the 'body tag
          (*text* . ,(lambda (tag text) text))
          (*default* . ,(lambda args args))))))))

(define (make-html-module-pages)
  (for-each
   (lambda (module)
     (let* ((ustr (string-append "./html/" (module->ustr module)))
            (port (begin
                    (makedirs ustr)
                    (open-output-file (string-append ustr "index.html")))))
       (display xhtml-doctype port)
       (sxml->xml
        (pre-post-order
         (stexi->shtml (module-stexi-documentation module))
         `((html . ,(lambda (tag attrs head body)
                      (wrap-html
                       (module->str module)
                       (string-append "../" *html-relative-root-path*)
                       "../index.scm"
                       (cdr body)))) ;; cdr past the 'body tag
           (*text* . ,(lambda (tag text) text))
           (*default* . ,(lambda args args))))
        port)))
   (map car *modules*)))

(define (main config-scm)
  (load config-scm)
  (makedirs "./html")
  (make-html-index)
  (make-html-module-pages))

(apply main (cdr (command-line)))
