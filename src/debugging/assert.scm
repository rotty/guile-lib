;; guile-lib
;; Written 2001 by Oleg Kiselyov <oleg at pobox dot com>.
;; Modified 2004 by Andy Wingo <wingo at pobox dot com>.

;; This file is based on SSAX's myenv-scm.scm, and is in the public
;; domain.

;;; Commentary:
;;
;; Defines an @code{assert} macro, and the @code{cout} and @code{cerr}
;; utility functions.
;;
;;; Code:

(define-module (debugging assert)
  #:use-module (scheme documentation)
  #:export (assert cout cerr))

(define (cout . args)
  "Similar to @code{cout << arguments << args}, where @var{argument} can
be any Scheme object. If it's a procedure (e.g. @code{newline}), it's
called without args rather than printed."
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  "Similar to @code{cerr << arguments << args}, where @var{argument} can
be any Scheme object. If it's a procedure (e.g. @code{newline}), it's
called without args rather than printed."
  (for-each (lambda (x)
              (if (procedure? x) (x (current-error-port)) (display x (current-error-port))))
            args))

(define nl (string #\newline))

(define-macro-with-docs (assert expr . others)
  "Assert the truth of an expression (or of a sequence of expressions).

syntax: @code{assert @var{?expr} @var{?expr} ... [report: @var{?r-exp} @var{?r-exp} ...]}

If @code{(and @var{?expr} @var{?expr} ...)} evaluates to anything but
@code{#f}, the result is the value of that expression. Otherwise, an
error is reported.

The error message will show the failed expressions, as well as the
values of selected variables (or expressions, in general). The user may
explicitly specify the expressions whose values are to be printed upon
assertion failure -- as @var{?r-exp} that follow the identifier
@code{report:}.

Typically, @var{?r-exp} is either a variable or a string constant. If
the user specified no @var{?r-exp}, the values of variables that are
referenced in @var{?expr} will be printed upon the assertion failure."

  ;; given the list of expressions or vars, make the list appropriate
  ;; for cerr
  (define (make-print-list prefix lst)
    (cond
     ((null? lst) '())
     ((symbol? (car lst))
      (cons #\newline
	(cons (list 'quote (car lst))
	  (cons ": " (cons (car lst) (make-print-list #\newline (cdr lst)))))))
     (else 
      (cons prefix (cons (car lst) (make-print-list "" (cdr lst)))))))

  ;; return the list of all unique "interesting" variables in the expr.
  ;; Variables that are certain to be bound to procedures are not
  ;; interesting.
  (define (vars-of expr)
    (let loop ((expr expr) (vars '()))
      (cond
       ((not (pair? expr)) vars)	; not an application -- ignore
       ((memq (car expr) 
	      '(quote let let* letrec let-values* lambda cond quasiquote
		      case define do assert))
	vars)				; won't go there
       (else				; ignore the head of the application
	(let inner ((expr (cdr expr)) (vars vars))
	  (cond 
	   ((null? expr) vars)
	   ((symbol? (car expr))
	    (inner (cdr expr)
		   (if (memq (car expr) vars) vars (cons (car expr) vars))))
	   (else
	    (inner (cdr expr) (loop (car expr) vars)))))))))

  (cond
   ((null? others)		; the most common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr ,nl "bindings"
			    ,@(make-print-list #\newline (vars-of expr)) ,nl)
		      (error "assertion failure"))))
   ((eq? (car others) 'report:) ; another common case
    `(or ,expr (begin (cerr "failed assertion: " ',expr
			    ,@(make-print-list #\newline (cdr others)) ,nl)
		      (error "assertion failure"))))
   ((not (memq 'report: others))
    `(or (and ,expr ,@others)
	 (begin (cerr "failed assertion: " '(,expr ,@others) ,nl "bindings"
		      ,@(make-print-list #\newline
			 (vars-of (cons 'and (cons expr others)))) ,nl)
		      (error "assertion failure"))))
   (else			; report: occurs somewhere in 'others'
    (let loop ((exprs (list expr)) (reported others))
      (cond
       ((eq? (car reported) 'report:)
	`(or (and ,@(reverse exprs))
	     (begin (cerr "failed assertion: " ',(reverse exprs)
			  ,@(make-print-list #\newline (cdr reported)) ,nl)
		    (error "assertion failure"))))
       (else (loop (cons (car reported) exprs) (cdr reported))))))))
    
;;; arch-tag: e3b450e8-1af2-4f09-a36e-e4dd48fc363c
;;; assert.scm ends here
