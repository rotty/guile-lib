(define-module (text structured html)
  :use-module (text structured)
  :use-module (scheme documentation)
  :use-module (srfi srfi-13)
  :export (stext->html html-ref-resolvers))

(define-with-docs html-ref-resolvers 
  "A list of procedures which, when passed the name of a node and
manual, return either a link address or @code{#f}."
  '())

(define (default-stext->html-func tag tokens loop)
  (let inside-loop ((out (if (list? tag) (reverse tag) (list tag)))
                    (in tokens))
    ;;(pk tag tokens)
    (cond
     ((null? in)
      (reverse out))
     ((ignored? (car in))
      (inside-loop out (cdr in)))
     ((list? (car in))
      (inside-loop (cons (loop (car in)) out) (cdr in)))
     (else
      (inside-loop (cons (car in) out) (cdr in))))))

(define (c-stext->html-func tag tokens loop)
  (list
   'raw
   (string-append
    (let loop ((str "\n<!--") (tokens tokens))
      (if (null? tokens)
          str
          (loop (string-append str (with-output-to-string
                                     (lambda () (display (car tokens)))))
                (cdr tokens))))
    "-->\n")))

(define (urlify str)
  (string-downcase
   (string-map
    (lambda (c)
      (case c
        ((#\space #\/ #\:) #\-)
        (else c)))
    str)))

(define (ref-stext->html-func head tokens loop)
  (let* ((node-name (or (argument-ref tokens 0) (error "No node name:" type tokens)))
         (info-ref-name (or (argument-ref tokens 1) node-name))
         (section-name (or (argument-ref tokens 2) node-name))
         (info-file (argument-ref tokens 3))
         (manual (argument-ref tokens 4))
         (target (or (or-map
                      (lambda (x)
                        (x node-name manual))
                      html-ref-resolvers)
                     (urlify
                      (if manual
                          (string-append manual ".html#" section-name)
                          (string-append "#" section-name))))))
    (append
     (case head
       ((xref)
        '(span "See "))
       ((pxref)
        '(span "see "))
       (else
        '(span)))
     `((a :href ,target
          ,section-name)))))

(define (uref-stext->html-func head tokens loop)
  (let* ((uri (or (argument-ref tokens 0) (error "No URI:" head tokens)))
         (title (or (argument-ref tokens 1) uri)))
    `(a :href ,uri ,title)))

(define (node-stext->html-func head tokens loop)
  (let ((name (urlify (or (argument-ref tokens 0) (error "No node name:" head tokens)))))
    `(a :name ,name)))

(define (dl-stext->html-func head tokens loop)
  (let inner-loop ((out '(dl)) (dd #f) (in tokens))
    (cond
     ((null? in)
      (reverse (if dd
                   (cons (reverse dd) out)
                   out)))
     ((list? (car in))
      (if (eq? (caar in) 'item)
          (inner-loop (cons (default-stext->html-func 'dt (cdar in) loop)
                            (if dd
                                (cons (reverse dd) out)
                                out))
                      '(dd)
                      (cdr in))
          (inner-loop out
                      (cons (loop (car in)) dd)
                      (cdr in))))
     (else
      (inner-loop out
                  ;; Ignore superfluous space between the (table) and
                  ;; the first (item)
                  (if dd (cons (car in) dd) dd)
                  (cdr in))))))

(define (l-stext->html-func head tokens loop)
  (let inner-loop ((out (list (if (eq? (if (list? head) (car head) head)
                                       'itemize)
                                  'ul
                                  'ol)))
                   (li #f)
                   (in tokens))
    (cond
     ((null? in)
      (reverse (if li
                   (cons (reverse li) out)
                   out)))
     ((list? (car in))
      (if (eq? (caar in) 'item)
          (inner-loop (if li
                          (cons (reverse li) out)
                          out)
                      (reverse (default-stext->html-func 'li (cdar in) loop))
                      (cdr in))
          (inner-loop out
                      (cons (loop (car in))
                            li)
                      (cdr in))))
     (else
      (inner-loop out
                  ;; Same as above
                  (if li (cons (car in) li) li)
                  (cdr in))))))

(define (def-stext->html-func head tokens loop)
  (define (split-args)
    (let loop ((args (cdr head)) (ret '()))
      (cond
       ((null? args)
        (reverse ret))
       ((and (string? (car args))
             (string-index (car args) #\space))
        (loop (append (string-split (car args) #\space) (cdr args)) ret))
       (else
        (loop (cdr args) (cons (car args) ret))))))

  (let* ((args (split-args))
         (right (case (car head)
                  ((deftp) (list-ref args 0))
                  ((defun) "Function")
                  ((defspec) "Special Form")
                  ((defvar) "Variable")
                  (else (string-capitalize (substring (symbol->string tag) 3)))))
         (left-td (case (car head)
                    ((deftp)
                     `(td (code (b ,(list-ref args 1)))))
                    ((defun)
                     `(td (code (b ,(list-ref args 0)))
                          " "
                          ,(cons 'var (list-join (cdr args) " "))))
                    ((defspec)
                     `(td (code (b ,(list-ref args 0)))
                          " "
                          ,(cons 'text (list-join (cdr args) " "))))
                    ((defvar)
                     `(td (code (b ,(list-ref args 0)))))
                    (else
                     (cons 'td (list-join args " "))))))
    `(div
      (table :cellpadding 0 :cellspacing 0 :width "100%" :class def
             (tr
              ,left-td
              (td (p :class right "[" ,right "]"))))
      ,(default-stext->html-func '(div :class description) tokens loop))))

;; If the assoc-ref is a symbol, that symbol will be the HTML tag. If
;; it's a list, the first element is the tag and the kw-args are
;; attributes. If it's a procedure, that procedure handles the body.
(define tag-action-alist
  `((titlepage (div :class titlepage))
    (title . (h2 :class title))
    (subtitle . (h3 :class subtitle))
    (author . (h3 :class author))
    (center . center)
    (example . pre)
    (lisp . pre)
    (smallexample . (pre :class smaller))
    (smalllisp . (pre :class smaller))
    (cartouche . (div :class cartouche))
    (chapter . h2)
    (section . h3)
    (subsection . h4)
    (subsubsection . h5)
    (appendix . h2)
    (appendixsec . h3)
    (appendixsubsec . h4)
    (appendixsubsubsec . h5)
    (unnumbered . h2)
    (unnumberedsec . h3)
    (unnumberedsubsec . h4)
    (unnumberedsubsubsec . h5)
    (quotation . blockquote)

    (bold . b)
    (item . #f)
    (itemx . #f)
    (sample . samp)
    (samp . samp)
    (code . code)
    (kbd . kbd)
    (key . (code :class key))
    (var . var)
    (env . (code :class env))
    (file . (code :class file))
    (command . (code :class command))
    (option . (code :class option))
    (dfn . dfn)
    (cite . cite)
    (acro . acronym)
    (url . #f)
    (email . (code :class email))
    (emph . em)
    (strong . strong)
    (sc . (span :class small-caps))
    (text . span) ;; something of a hack...

    (body . ,default-stext->html-func) ;; a special one to start the
				       ;; process
    (copyright . ,(lambda (head tokens loop) "&copy;"))
    (result . ,(lambda (head tokens loop) "&rArr;"))
    (comment . ,c-stext->html-func)
    (c . ,c-stext->html-func)
    (xref . ,ref-stext->html-func)
    (ref . ,ref-stext->html-func)
    (pxref . ,ref-stext->html-func)
    (uref . ,uref-stext->html-func)
    (node . ,node-stext->html-func)
    (anchor . ,node-stext->html-func)
    (table . ,dl-stext->html-func)
    (itemize . ,l-stext->html-func)
    (enumerate . ,l-stext->html-func)
    (deftp . ,def-stext->html-func)
    (defcv . ,def-stext->html-func)
    (defivar . ,def-stext->html-func)
    (deftypeivar . ,def-stext->html-func)
    (defop . ,def-stext->html-func)
    (deftypeop . ,def-stext->html-func)
    (defmethod . ,def-stext->html-func)
    (deftypemethod . ,def-stext->html-func)
    (defopt . ,def-stext->html-func)
    (defvr . ,def-stext->html-func)
    (defvar . ,def-stext->html-func)
    (deftypevr . ,def-stext->html-func)
    (deftypevar . ,def-stext->html-func)
    (deffn . ,def-stext->html-func)
    (deftypefn . ,def-stext->html-func)
    (defmac . ,def-stext->html-func)
    (defspec . ,def-stext->html-func)
    (defun . ,def-stext->html-func)
    (deftypefun . ,def-stext->html-func)))

(define ignore-list
  '(page setfilename setchapternewpage iftex ifinfo ifplaintext ifxml sp vskip
    menu ignore syncodeindex stext))
(define (ignored? token)
  (and (list? token) (memq (car token) ignore-list)))
(for-each
 (lambda (sym)
   (set! tag-action-alist (acons sym noop tag-action-alist)))
 ignore-list)

(define (get-stext->html-func head)
  (let ((tag (assq-ref tag-action-alist (if (list? head) (car head) head))))
    (cond
     ((or (symbol? tag) (list? tag))
      (lambda (tokens loop)
        (default-stext->html-func tag tokens loop)))
     ((procedure? tag)
      (lambda (tokens loop)
        (tag head tokens loop)))
     (else
      (error "Unknown stext element" head)))))

(define (stext->html stext . rest)
  "Format @var{stext} as HTML. Returns a string.

Processing actually occurs in two phases. First, the stext list is
transformed into an HTML list corresponding to the body of an HTML
document. The first element of an HTML list is a symbol corresponding to
an HTML tag (e.g. @code{head} or @code{p}). Keyword arguments after the
symbol are interpreted as attributes. The remainder of the list are
either strings or HTML lists. After the HTML list is made, it is written
to a string.

Any optional arguments should be symbol-value pairs. Valid symbols
include:

@table @code
@item post-processor
This procedure steps in after the first phase of processing. Its
arguments are the first element of @var{stext} and the processed HTML
body of the document (with @code{body} as the head of the list). This
procedure is responsible for attaching the header to the document and
enclosing the whole document in an @code{html} tag, if desired. The
default post-processor will add a minimal header to the document. Note
that any text within a @code{raw} tag will be output as-is, without any
special quoting.

@item ugly?
If true, don't add any extra newlines to the output. This is more
correct, but makes the HTML difficult to read.
@end table
"
  (define (default-post-processor stext-head body)
    `(html
      (head
       (title ,(cadr stext-head)))
      ,(cons*
        'body
        `(h1 ,(cadr stext-head))
        (cdr body))))
  (define (filter-paragraphs l)
    (let loop ((out '()) (p #f) (in l))
      (cond
       ((null? in)
        (reverse (if p
                     (cons (reverse p) out)
                     out)))
       ((and (list? (car in))
             (eq? (caar in) 'pre))
        (loop (cons (car in)
                    (if p
                        (cons (reverse p) out)
                        out))
              #f
              (cdr in)))
       ((and (list? (car in))
             (not (inline-html? (caar in))))
        (loop (cons (filter-paragraphs (car in))
                    (if p
                        (cons (reverse p) out)
                        out))
              #f
              (cdr in)))
       ((and (equal? (car in) "\n")
             (not (null? (cdr in)))
             (equal? (cadr in) "\n"))
        ;; We might have a new paragraph. We don't if the next element
        ;; is not a string or inline.
        (let inner-loop ((in (cddr in)))
          (cond
           ((null? in)
            (loop out p in)) ;; get out of here
           ((equal? (car in) "\n")
            (inner-loop (cdr in))) ;; keep going...
           ((string? (car in))
            (loop (if p
                      (cons (reverse p) out)
                      out)
                  (list (car in) 'p)
                  (cdr in)))
           ((and (list? (car in)) (inline-html? (caar in)))
            (loop (cons (car in)
                        (if p
                            (cons (reverse p) out)
                            out))
                  #f
                  (cdr in)))
           (else
            ;; no paragraph.
            (loop out p in)))))
       (else
        (if p
            (loop out (cons (car in) p) (cdr in))
            (loop (cons (car in) out) p (cdr in)))))))

  (write-html
   ((or (assq-ref rest 'post-processor) default-post-processor)
    (car stext)
    (filter-paragraphs
     (let loop ((text (cons 'body (cdr stext))))
       ((get-stext->html-func (car text)) (cdr text) loop))))
   (assq-ref rest 'ugly?)))

;; From the 4.01 strict DTD
(define inline-html-elements
  '(tt i b big small em strong dfn code samp kbd var cite abbr acronym
    a img object br script map q sub sup span bdo input select textarea
    label button))
(define (inline-html? tag)
  (memq tag inline-html-elements))

(define (write-html html ugly?)
  (define (display-escaping scm)
    (let loop ((out '())
               (in (string->list (with-output-to-string (lambda () (display scm))))))
      (if (null? in)
          (display (list->string (reverse! out)))
          (case (car in)
            ;; remember to cons in reverse order
            ((#\<)
             (loop (cons* #\; #\t #\l #\& out) (cdr in)))
            ((#\>)
             (loop (cons* #\; #\t #\g #\& out) (cdr in)))
            (else
             (loop (cons (car in) out) (cdr in)))))))
  (define (write-head l)
    (display "<")
    (display (car l))
    ((lambda (outlist)
       (display ">")
       outlist)
     (let loop ((in (cdr l)))
       (if (and (not (null? in)) (keyword? (car in)))
           (begin
             (display " ")
             (display (keyword->symbol (car in)))
             (cond
              ((null? (cdr in))
               => noop)
              ((keyword? (cadr in))
               (if (eq? (cadr in) :%no-args)
                   (loop (cddr in))
                   (error "Expected an argument, but got a keyword instead"
                          (cadr in) l)))
              (else
               (display "=\"")
               (display-escaping (cadr in))
               (display "\"")
               (loop (cddr in)))))
           in))))

  (define (run)
    (let loop ((in html))
      (cond
       ((null? in)
        *unspecified*)
       ((symbol? (car in))
        ;; We're at the head of our list, starting a new tag
        (cond
         ((eq? (car in) 'raw)
          (for-each display (cdr in)))
         (else
          (if (and (not ugly?) (not (inline-html? (car in))))
              (display "\n"))
          (loop (write-head in))
          (display "</")
          (display (car in))
          (display ">")
          (if (and (not ugly?) (not (inline-html? (car in))))
              (display "\n")))))
       ((list? (car in))
        (loop (car in))
        (loop (cdr in)))
       (else
        (display-escaping (car in))
        (loop (cdr in))))))

  (with-output-to-string
    run))

;;; arch-tag: 005de15e-ae55-4237-8685-c686fb91ce59
