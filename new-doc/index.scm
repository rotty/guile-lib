;; create HTML docs in ./html/:
;;  guile -l index.scm -c '(make-html)'
;; create Texinfo docs in ./texinfo/:
;;  guile -l index.scm -c '(make-texinfo)'

(use-modules (sxml simple)
             (sxml transform)
             (sxml xpath)
             (texinfo reflection)
             (texinfo html)
             (texinfo serialize)
             (srfi srfi-13))

;; The documentation is written in a domain-specific SXML language.
;; `pre-post-order' rules are defined later to translate this language
;; to stexi, as well as into HTML directly.

(define page
  `(page
    (@ (name "guile-lib Reference Manual") (relative-root-path "../"))

    (p (link "http://home.gna.org/guile-lib/" (code "guile-lib")) " is
an attempt to provide a repository for useful code written in Guile
Scheme. It aims to be the central point for distribution of third-party
code for Guile. It's already in "
       (link "http://packages.debian.org/guile-lib/" "Debian") ".")

    (h3 "Modules")

    (modules
     (module (container nodal-tree)
             "A tree consisting of nodes with attributes. Implemented as
a tagged alist.")

     (module (container delay-tree)
             "A nodal tree whose fields can be promises created by the "
             (code "delay") " operator.")

     (module (debugging time)
             "A simple macro to time the execution of an expression.
FIXME: ice-9 also has a time macro. Who knew?")

     (module (scheme documentation)
             "Macros to define different kinds of variables with
documentation.")

     (module (texinfo)
             "Routines for parsing texinfo files or fragments into an
SXML representation.")
     
     (module (texinfo html)
             "Code to transform " (code "stexi") ", the SXML
representation of texinfo, into HTML.")

     (module (texinfo indexing)
             "Code to extract an index from a piece of " (code "stexi") ".")

     (module (texinfo nodal-tree)
             "Code to chunk a " (code "stexi") " document into pieces,
suitable for integrating with a custom GtkTreeModel.")

     (module (texinfo plain-text)
             "Code to render " (code "stexi") " as plain text.")

     (module (texinfo serialize)
             "Code to render " (code "stexi") " as texinfo.")

     (module (texinfo reflection)
             "Integrates texinfo into guile's help via "
             (code "(scheme session)") ". See the article, "
             (link
              "http://ambient.2y.net/wingo/archives/2004/07/24/literate-programming-with-guile-lib"
              "literate programming with guile-lib") ".")

     (module (sxml simple)
             "Some convenience routines built on top of SSAX.")

     (lmodule (htmlprag) "http://neilvandyke.org/htmlprag/"
              "Neil Van Dyke's permissive (\"pragmatic\") HTML parser.")

     (lmodule (sxml ssax) "http://ssax.sourceforge.net/"
              "A functional-style XML parser for Scheme.")

     (lmodule (sxml xpath) "http://ssax.sourceforge.net/"
              "An implementation of XPath for SXML.")

     (lmodule (sxml transform) "http://ssax.sourceforge.net/"
              "A higher-order SXML transformation operator, "
              (code "pre-post-order") ".")
    
     (lmodule (sxml apply-templates) "http://ssax.sourceforge.net/"
              "A more XSLT-like approach to SXML transformations.")

     (module (sxml ssax input-parse)
             "Oleg's " (code "input-parse.scm") ", but with some routines
sped up by use of " (code "(ice-9 rdelim)") ".")

     (module (debugging assert)
             "Oleg's assert macro that will print out values of variables
referenced in the assert expression.")

     (module (statprof)
             "A statistical profiler for Guile.")

     (module (io string)
             "IO routines dealing with strings, stolen from SLIB.")

     (module (scheme session)
             "The original " (code "(ice-9 session)") ", but with some
hooks to make the help system extensible."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformation code follows...

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"))

(define (list-join l infix)
  "Infixes @var{infix} into list @var{l}."
  (if (null? l) l
      (let lp ((in (cdr l)) (out (list (car l))))
        (cond ((null? in) (reverse out))
              (else (lp (cdr in) (cons* (car in) infix out)))))))

(define (module->str scm)
  (call-with-output-string (lambda (p) (display scm p))))
(define (module->ustr scm)
  (string-append (string-join (map symbol->string scm) "-") "/"))
(define (module->dstr scm)
  (string-join (map symbol->string scm) "-"))

(define (make-module-list)
  ;; dunno why the ssax one is broken -- need to update to newest
  ;; sxml-tools, instead of oleg's sxpath
  (define (my-select-kids test-pred?)
    (lambda (node)
      (cond 
       ((null? node) node)
       ((not (pair? node)) '())
       ;;((symbol? (car node))
       ;; ((filter test-pred?) (cdr node)))
       (else (map-union (select-kids test-pred?) node)))))

  (map cadr
       (apply append
              (map (lambda (type)
                     ((my-select-kids (node-typeof? type)) page))
                   '(module lmodule)))))

(define (maybe-mkdir path)
  (let loop ((path ".") (components (string-split path #\/)))
    (if (not (null? components))
        (let ((sub-path (string-append path "/" (car components))))
          (if (or (not (file-exists? sub-path))
                  (not (file-is-directory? sub-path)))
              (mkdir sub-path))
          (loop sub-path (cdr components))))))

;;; HTML generation

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

(define (make-html-index)
  (with-output-to-file "html/index.html"
    (lambda ()
      (display xhtml-doctype)
      (sxml->xml
       (pre-post-order
        page
        `((page . ,(lambda (tag args . body)
                     (wrap-html
                      (cadr (assq 'name (cdr args)))
                      (cadr (assq 'relative-root-path (cdr args)))
                      "index.scm"
                      body)))
          (module* . ,(lambda (tag header . body)
                        `((dt ,@header)
                          (dd ,@body))))
          (module *macro* . ,(lambda (tag module . body)
                               `(module*
                                 ((a (@ (href ,(module->ustr module))
                                        (name ,(module->ustr module)))
                                     ,(module->str module)))
                                 ,@body)))
          (lmodule *macro* . ,(lambda (tag module link . body)
                                `(module*
                                  ((a (@ (href ,(module->ustr module))
                                         (name ,(module->ustr module)))
                                      ,(module->str module))
                                   " "
                                   (a (@ (href ,link)) "(source)"))
                                  ,@body)))
          (link . ,(lambda (tag href name)
                     `(a (@ (href ,href)) ,name)))
          (*text* . ,(lambda (tag text) text))
          (*default* . ,(lambda args args))))))))

(define (make-html-module-pages)
  (let ((modules (make-module-list)))
    (for-each
     (lambda (module)
       (let* ((ustr (string-append "./html/" (module->ustr module)))
              (port (begin
                      (maybe-mkdir ustr)
                      (open-output-file (string-append ustr "index.html")))))
         (display xhtml-doctype port)
         (sxml->xml
          (pre-post-order
           (stexi->shtml (module-stexi-documentation module))
           `((html . ,(lambda (tag attrs head body)
                        (wrap-html
                         (module->str module)
                         "../../"
                         "../index.scm"
                         (cdr body)))) ;; cdr past the 'body tag
             (*text* . ,(lambda (tag text) text))
             (*default* . ,(lambda args args))))
          port)))
     modules)))

(define (make-html)
  (maybe-mkdir "./html")
  (make-html-index)
  (make-html-module-pages))

;;; Texinfo generation

(define (make-texi-module-files)
  (maybe-mkdir "./texi")
  (let ((modules (make-module-list)))
    (for-each
     (lambda (module)
       (let ((port (open-output-file
                    (string-append "./texi/" (module->dstr module) ".texi"))))
         (format #t "writing .texi for ~S\n" module)
         (display (stexi->texi (module-stexi-documentation module)) port)))
     modules)))

(define (make-texinfo)
  (make-texi-module-files))

;; arch-tag: e493ad42-ad58-451c-a2d6-b17ba6c1d1d0
