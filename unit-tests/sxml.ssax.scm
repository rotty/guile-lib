;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001,2002,2003,2004 Oleg Kiselyov <oleg at pobox dot com>

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
;; Unit tests for (sxml ssax).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (sxml ssax input-parse)
             (debugging assert)
             (io string)
             (srfi srfi-1)
             (srfi srfi-13)
             (sxml ssax)
             (ice-9 pretty-print))

(define-class <test-ssax> (<test-case>))

(define-macro (import module . symbols)
  `(begin
     ,@(map (lambda (sym)
              `(define ,sym (module-ref (resolve-module ',module) ',sym)))
            symbols)))

(import (sxml ssax)
        ssax:read-NCName
        ssax:read-QName
        ssax:largest-unres-name
        ssax:Prefix-XML
        ssax:resolve-name
        ssax:scan-Misc
        ssax:assert-token
        ssax:handle-parsed-entity
        ssax:warn
        ssax:skip-pi
        ssax:S-chars
        ssax:skip-S
        ssax:ncname-starting-char?
        when
        equal_?
        make-xml-token
        nl
        unesc-string
        parser-error
        ascii->char
        char->ascii
        char-newline
        char-return
        char-tab
        name-compare
        ;; weird stuff below. it's expansion-specific.
        _eqv?_7
        _cons_23
        _append_24
        _list_25
        _vector_26
        _list->vector_27)

(define pp pretty-print)

(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
              (append args disposition-msgs))
    (newline port)))

(define-macro (failed? . stmts)
  `(not (false-if-exception (begin ,@stmts #t))))

(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
      (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
        (if (null? l) (reverse dest)
            (loop (cdr l) (cons (car l) (cons elem dest)))))))

(define (load-rejecting reject-list file)
  (with-input-from-file (%search-load-path file)
    (lambda ()
      (let loop ((sexp (read)))
        (cond
         ((eof-object? sexp))
         ((and (pair? sexp) (not (memq (car sexp) reject-list)))
          (eval sexp (current-module))
          (loop (read)))
         (else
          (loop (read))))))))

(define-method (test-ssax (self <test-ssax>))
  (load-rejecting '(define)
                  (%search-load-path "sxml/upstream/SSAX-expanded.scm")))

(test-ssax (make <test-ssax>))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 72a04035-11ce-4505-9fe8-f63d1db060a1
;;; xml.ssax.scm ends here
