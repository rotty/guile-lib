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
;; Unit tests for (match-bind).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (match-bind))

(define-class <test-match-bind> (<test-case>))

(define-method (test-s/// (self <test-match-bind>))
  (assert-equal "bar bar baz qux foo"
                ((s/// "foo" "bar") "foo bar baz qux foo"))

  (assert-equal "foo bar baz qux foo"
                ((s/// "zag" "bar") "foo bar baz qux foo"))

  (assert-equal "foo oo bar baz qux foo"
                ((s/// "(f(o+)) (zag)?" "$1 $2 $3")
                 "foo bar baz qux foo")))

(define-method (test-s///g (self <test-match-bind>))
  (assert-equal "bar bar baz qux bar"
                ((s///g "foo" "bar") "foo bar baz qux foo"))

  (assert-equal "foo bar baz qux foo"
                ((s///g "zag" "bar") "foo bar baz qux foo"))

  (assert-equal "foo oo bar baz qux foo"
                ((s///g "(f(o+)) (zag)?" "$1 $2 $3")
                 "foo bar baz qux foo"))

  (assert-equal "foo oo bar baz qux foo oo"
                ((s///g "(f(o+))( zag)?" "$1 $2$3")
                 "foo bar baz qux foo")))

(exit-with-summary (run-all-defined-test-cases))
