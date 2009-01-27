;; (sxml ssax input-parse) -- a simple lexer
;; Written 2003 by Oleg Kiselyov <oleg at pobox dot com> as input-parse.scm.
;; Modified 2004 by Andy Wingo <wingo at pobox dot com>.

;; This file is in the public domain.

;;; Commentary:
;;
;; A simple lexer.
;;
;; The procedures in this module surprisingly often suffice to parse an
;; input stream. They either skip, or build and return tokens, according
;; to inclusion or delimiting semantics. The list of characters to
;; expect, include, or to break at may vary from one invocation of a
;; function to another. This allows the functions to easily parse even
;; context-sensitive languages.
;;
;; EOF is generally frowned on, and thrown up upon if encountered.
;; Exceptions are mentioned specifically. The list of expected
;; characters (characters to skip until, or break-characters) may
;; include an EOF "character", which is to be coded as the symbol,
;; @code{*eof*}.
;;
;; The input stream to parse is specified as a @dfn{port}, which is
;; usually the last (and optional) argument. It defaults to the current
;; input port if omitted.
;;
;; If the parser encounters an error, it will throw an exception to the
;; key @code{parser-error}. The arguments will be of the form
;; @code{(@var{port} @var{message} @var{specialising-msg}*)}.
;;
;; The first argument is a port, which typically points to the offending
;; character or its neighborhood. You can then use @code{port-column}
;; and @code{port-line} to query the current position. @var{message} is
;; the description of the error. Other arguments supply more details
;; about the problem.
;;
;;; Code:

(define-module (sxml ssax input-parse)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 rdelim)
  #:export (peek-next-char
            assert-curr-char
            skip-until
            skip-while
            next-token
            next-token-of
            read-text-line
            read-string))

(define ascii->char integer->char)
(define char->ascii char->integer)
(define char-newline #\newline)
(define char-return #\return)
(define inc 1+)
(define dec 1-)

;; rewrite oleg's define-opt into define* style
(define-macro (define-opt bindings body . body-rest)
  (let* ((rev-bindings (reverse bindings))
         (opt-bindings
          (and (pair? rev-bindings) (pair? (car rev-bindings))
               (eq? 'optional (caar rev-bindings))
               (cdar rev-bindings))))
    (if opt-bindings
	`(define* ,(append (reverse (cons #:optional (cdr rev-bindings)))
			  opt-bindings)
	   ,body ,@body-rest)
	`(define* ,bindings ,body ,@body-rest))))

(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))

(load-from-path "sxml/upstream/input-parse.scm")

;; This version for guile is quite speedy, due to read-delimited (which
;; is implemented in C).
(define-opt (next-token prefix-skipped-chars break-chars
			(optional (comment "") (port (current-input-port))) )
  (let ((delims (list->string (delete '*eof* break-chars))))
    (if (eof-object? (if (null? prefix-skipped-chars)
                         (peek-char port)
                         (skip-while prefix-skipped-chars port)))
        (if (memq '*eof* break-chars)
            ""
            (parser-error port "EOF while reading a token " comment))
        (let ((token (read-delimited delims port 'peek)))
          (if (and (eof-object? (peek-char port))
                   (not (memq '*eof* break-chars)))
              (parser-error port "EOF while reading a token " comment)
              token)))))

(define-opt (read-text-line (optional (port (current-input-port))) )
  (read-line port))


;;; arch-tag: 73fa0dc1-9f01-45e1-80fa-4d9a7ab83f92
;;; input-parse.scm ends here
