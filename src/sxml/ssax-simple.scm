;; (sxml ssax-simple) -- SSAX, without syncase
;; Written 2001,2002,2003,2004 by Oleg Kiselyov <oleg at pobox dot com> as SSAX.sxm.
;; Modified 2004 by Andy Wingo <wingo at pobox dot com>.

;; This file is in the public domain.

;;; Commentary:
;;
;; This module is the same as (sxml ssax), except that it avoids loading
;; R5RS macros due to the slow load-time of (ice-9 syncase). As a result
;; it does not export the @code{ssax:make-parser},
;; @code{ssax:make-pi-parser}, and @code{ssax:make-elem-parser} macros,
;; which create custom SAX parsers.
;;
;; If you need to make a custom SAX parser, use the (sxml ssax) module
;; instead.
;;
;;; Code:

(define-module (sxml ssax-simple)
  #:use-module (sxml ssax input-parse)
  #:use-module (sxml unicode)
  #:use-module (debugging assert)
  #:use-module (io string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export     (xml-token? xml-token-kind xml-token-head
                make-empty-attlist attlist-add
                attlist-null?
                attlist-remove-top
                attlist->alist attlist-fold
                ssax:uri-string->symbol
                ssax:skip-internal-dtd
                ssax:read-pi-body-as-string
                ssax:reverse-collect-str-drop-ws
                ssax:reverse-collect-str
                ssax:read-markup-token
                ssax:read-cdata-body
                ssax:predefined-parsed-entities
                ssax:read-char-ref
                ssax:read-attributes
                ssax:complete-start-tag
                ssax:read-external-id
                ssax:read-char-data
                ssax:xml->sxml)
  
  ;; We don't want to load up syncase.
  ;; #:export-syntax (ssax:make-parser ssax:make-pi-parser ssax:make-elem-parser)
  )

(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))
(define ascii->char integer->char)
(define char->ascii char->integer)

(define (ssax:warn port msg . args)
  (warn msg port args))

;; Well, so this isn't correct for other unicode encodings. Something to
;; fix in the future, I guess.
(define ucscode->string unichar->utf-8)

(define char-newline #\newline)
(define char-return #\return)
(define char-tab #\tab)
(define nl "\n")

(define (load-filtered accept-list file)
  (with-input-from-file (%search-load-path file)
    (lambda ()
      (let loop ((sexp (read)))
        (cond
         ((eof-object? sexp))
         ((and (pair? sexp) (memq (car sexp) accept-list))
          (eval sexp (current-module))
          (loop (read)))
         (else
          (loop (read))))))))

;; if condition is true, execute stmts in turn and return the result of
;; the last statement otherwise, return #f
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))

;; Execute a sequence of forms and return the result of the _first_ one.
;; Like PROG1 in Lisp. Typically used to evaluate one or more forms with
;; side effects and return a value that must be computed before some or
;; all of the side effects happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

; Like let* but allowing for multiple-value bindings
(define-macro (let*-values bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply
       (lambda (vars initializer)
	 (let ((cont 
		(cons 'let*-values
		      (cons (cdr bindings) body))))
	   (cond
	    ((not (pair? vars))		; regular let case, a single var
	     `(let ((,vars ,initializer)) ,cont))
	    ((null? (cdr vars))		; single var, see the prev case
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(call-with-values (lambda () ,initializer)
	      (lambda ,vars ,cont))))))
       (car bindings))))

(define ascii->char integer->char)

(load-filtered '(define) "sxml/upstream/SSAX-expanded.scm")

;;; arch-tag: 4e4c450f-ea27-4a1c-86b7-df644da40079
;;; ssax.scm ends here
