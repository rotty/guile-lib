;; guile-lib
;; Copyright (C) 2007 Andy Wingo <wingo at pobox dot com>

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
;; Unit tests for (scheme kwargs).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (scheme kwargs))

(define-class <test-scheme-kwargs> (<test-case>))

(define-method (test-lambda/kwargs (self <test-scheme-kwargs>))
 (define frobulate (lambda/kwargs (foo (bar 13) (baz 42))
                     (list foo bar baz)))
 (assert-equal '(#f 13 42)
               (frobulate))
 (assert-equal '(#f 13 3)
               (frobulate #:baz 3))
 (assert-equal '(3 13 42)
               (frobulate #:foo 3))
 (assert-equal '(3 4 42)
               (frobulate 3 4))
 (assert-equal '(1 2 3)
               (frobulate 1 2 3))
 (assert-equal '(#f 1 2)
               (frobulate #:baz 2 #:bar 1))
 (assert-equal '(3 20 42)
               (frobulate 10 20 #:foo 3)))

(exit-with-summary (run-all-defined-test-cases))
