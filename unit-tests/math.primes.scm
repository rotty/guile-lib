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
(use-modules (math primes)
             (unit-test)
             (oop goops))


;; **********************************************************************
;; Test of factorization 
;; **********************************************************************
(define-class <test-factorization> (<test-case>))

(define-method (test-factor-function (tc <test-factorization>))
  (for-each
   (lambda (lst)
     (assert-equal (sort lst                    <)
                   (sort (factor (apply * lst)) <)))
   '((2) 
     (2 3) 
     (2 3 23)
     (7 13 29 43))))

;; **********************************************************************
;; Test of primes listings... 
;; **********************************************************************
(define-class <test-prime-lists> (<test-case>)
  (first-hundred #:accessor first100))

(define-method (set-up-test (tpl <test-prime-lists>))
  (set! (first100 tpl)
        (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 
              53 59 61 67 71 73 79 83 89 97 101 103 107 
              109 113 127 131 137 139 149 151 157 163 
              167 173 179 181 191 193 197 199 211 223 
              227 229 233 239 241 251 257 263 269 271 
              277 281 283 293 307 311 313 317 331 337 
              347 349 353 359 367 373 379 383 389 397 
              401 409 419 421 431 433 439 443 449 457 
              461 463 467 479 487 491 499 503 509 521 
              523 541)))

(define-method (test-recognizes-first100 (tpl <test-prime-lists>))
  (for-each (lambda (n)
              (assert-true (prime? n)))
            (first100 tpl)))

(define-method (test-prime> (tpl <test-prime-lists>))
  (let loop ((lst (first100 tpl)))
    (if (>= (length lst) 2)
        (begin
          (assert-equal (cadr lst)
                        (prime> (car lst)))
          (loop (cdr lst))))))

(define-method (test-prime< (tpl <test-prime-lists>))
  (let loop ((lst (first100 tpl)))
    (if (>= (length lst) 2)
        (begin
          (assert-equal (car lst)
                        (prime< (cadr lst)))
          (loop (cdr lst)))))
  (assert-equal #f
                (prime< 2)))

(define-method (test-primes> (tpl <test-prime-lists>))
  (assert-equal (first100 tpl)
                (primes> 0 100)))

(define-method (test-primes< (tpl <test-prime-lists>))
  (assert-equal (first100 tpl)
                (primes< 542 100))
  ;; it returns a shorter list if you ask for more than there are...
  (assert-equal 1
                (length (primes< 3 100))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 6014a90f-7104-4807-815a-17325809b3ad
