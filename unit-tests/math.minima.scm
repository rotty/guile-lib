;;; ----------------------------------------------------------------------
;;;    unit test
;;;    Copyright (C) 2003 Richard Todd
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; ----------------------------------------------------------------------
(use-modules (math minima)
             (unit-test)
             (oop goops))


;; **********************************************************************
;; Test of factorization 
;; **********************************************************************
(define-class <test-golden-sect> (<test-case>))

(define-method (test-gs-function (self <test-golden-sect>))
  (let ((func (lambda (x) (+ (* x (+ (* x x) -2)) -5))))
    (assert-numeric-= 816.4883855245578e-3 
                  (car (golden-section-search func 0 1 (/ 10000)))
                  (/ 10000))
    (assert-numeric-= -6.0886621077391165
                      (cdr (golden-section-search func 0 1 (/ 10000)))
                      (/ 10000))
    (assert-numeric-= 819.6601125010515e-3
                      (car (golden-section-search func 0 1 -5))
                      (/ 10000))
    (assert-numeric-= -6.088637561916407
                      (cdr (golden-section-search func 0 1 -5))
                      (/ 10000))

    (assert-numeric-= 816.4965933140557e-3
                      (car (golden-section-search func 0 1
                                         (lambda (a b c d e f g ) (= g 500))))
                      (/ 10000))
    (assert-numeric-= -6.088662107903635
                      (cdr (golden-section-search func 0 1
                                         (lambda (a b c d e f g ) (= g 500))))
                      (/ 10000))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: eef6ce35-0886-434f-9a52-b3f2d216019a
