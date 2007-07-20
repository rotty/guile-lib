;; guile-lib
;; Copyright (C) 2003,2004,2007 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;; 
;; @c
;; Support for defining functions that take python-like keyword
;; arguments.
;; 
;; In one of his early talks, Paul Graham wrote about a large system
;; called "Rtml":
;;
;; @quotation
;;
;; Most of the operators in Rtml were designed to take keyword
;; parameters, and what a help that turned out to be. If I wanted to add
;; another dimension to the behavior of one of the operators, I could
;; just add a new keyword parameter, and everyone's existing templates
;; would continue to work. A few of the Rtml operators didn't take
;; keyword parameters, because I didn't think I'd ever need to change
;; them, and almost every one I ended up kicking myself about later. If
;; I could go back and start over from scratch, one of the things I'd
;; change would be that I'd make every Rtml operator take keyword
;; parameters.
;;
;; @end quotation
;;
;; @xref{scheme kwargs lambda/kwargs,,lambda/kwargs}, for documentation
;; and examples.
;;
;; @xref{Optional Arguments,,,guile,Guile Reference Manual}, for more
;; information on Guile's standard support for optional and keyword
;; arguments.
;; 
;; Quote taken from
;; @uref{http://lib.store.yahoo.net/lib/paulgraham/bbnexcerpts.txt}.
;;
;;; Code:

(define-module (scheme kwargs)
  :use-module (ice-9 optargs)
  :use-module (scheme documentation)
  :export (lambda/kwargs define/kwargs))

(define (until pred? list)
  "Returns the first elements of @var{list} for which @var{pred?} is false."
  (if (or (eq? list '()) (pred? (car list)))
      '()
      (cons (car list) (until pred? (cdr list)))))

(define-macro-with-docs (lambda/kwargs BINDINGS . BODY)
  "Defines a function that takes keyword arguments.

@var{bindings} is a list of bindings, each of which may either be a
symbol or a two-element symbol-and-default-value list. Symbols without
specified default values will default to @code{#f}.

For example:
@example
 (define frobulate (lambda/kwargs (foo (bar 13) (baz 42))
                     (list foo bar baz)))
 (frobulate) @result{} (#f 13 42)
 (frobulate #:baz 3) @result{} (#f 13 3)
 (frobulate #:foo 3) @result{} (3 13 42)
 (frobulate 3 4) @result{} (3 4 42)
 (frobulate 1 2 3) @result{} (1 2 3)
 (frobulate #:baz 2 #:bar 1) @result{} (#f 1 2)
 (frobulate 10 20 #:foo 3) @result{} (3 20 42)
@end example

This function differs from the standard @code{lambda*} provided by Guile
in that invoking the function will accept positional arguments.
As an example, the @code{lambda/kwargs} behaves more intuitively in the
following case:

@example
 ((lambda* (#:optional (bar 42) #:key (baz 73))
    (list bar baz))
  1 2) @result{} (1 73)
 ((lambda/kwargs ((bar 42) (baz 73))
    (list bar baz))
  1 2) @result{} (1 2)
@end example

The fact that @code{lambda*} accepts the extra @samp{2} argument is
probably just a bug. In any case, @code{lambda/kwargs} does the right
thing.
"
  (or (list? BINDINGS)
      (error "lambda/kwargs bindings must be a list"))
  (let ((lambda-gensym (gensym))
        (args-gensym (gensym))
        (positional (gensym))
        (keyword (gensym))
        (nbindings (length BINDINGS))
        (CANONICAL-BINDINGS (map (lambda (x)
                                   (if (list? x) x (list x #f)))
                                 BINDINGS))
        (VARIABLES (map (lambda (x) (if (list? x) (car x) x))
                        BINDINGS)))
    `(let ((,lambda-gensym
            (lambda ,args-gensym
              ,@(if (string? (car BODY)) (list (car BODY)) '())
              (let* ((,positional (,until keyword? ,args-gensym))
                     (,keyword (list-tail ,args-gensym (length ,positional))))
                (if (> (length ,positional) ,nbindings)
                    (error "Too many positional arguments."))
                (,let-optional ,positional
                  ,CANONICAL-BINDINGS
                  ,@(map car CANONICAL-BINDINGS)
                  (,let-keywords ,keyword
                    #f
                    ,(map list VARIABLES VARIABLES)
                    ,@(if (string? (car BODY)) (cdr BODY) BODY)))))))
       (set-procedure-property! ,lambda-gensym
                                'arglist
                                '(() () ,CANONICAL-BINDINGS #f #f))
       ,lambda-gensym)))
       
(define-macro-with-docs (define/kwargs what . body)
  "Defines a function that takes kwargs. @xref{scheme kwargs
lambda/kwargs}, for more information.
"
  `(define ,(car what) (,lambda/kwargs ,(cdr what) ,@body)))
