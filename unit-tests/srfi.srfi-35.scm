#!/bin/sh
exec ${srcdir:-.}/guile-test-env guile -s "$0" "$@"
!#

(use-modules (srfi srfi-35)
             (unit-test))

(define-condition-type &c &condition
  c?
  (x c-x))

(define-condition-type &c1 &c
  c1?
  (a c1-a))

(define-condition-type &c2 &c
  c2?
  (b c2-b))

(define-class <test-basics> (<test-case>)
  v1 v2 v3 v4 v5)
  
(define-method (set-up-test (self <test-basics>))
  (slot-set! self 'v1 (make-condition &c1 'x "V1" 'a "a1"))
  (slot-set! self 'v2 (condition (&c2 (x "V2") (b "b2"))))
  (slot-set! self 'v3 (condition (&c1 (x "V3/1") (a "a3"))
                        (&c2 (b "b3"))))
  (slot-set! self 'v4 (make-compound-condition v1 v2))
  (slot-set! self 'v5 (make-compound-condition v2 v3)))

(define-method (test-v1 (self <test-basics>))
  (let ((v1 (slot-ref self 'v1)))
    (assert-true (c? v1))
    (assert-true (c1? v1))
    (assert-true (not (c2? v1)))
    (assert-tru2 (string=? (c-x v1) "V1"))
    (assert-true (string=? (c1-a v1) "a1"))))

(define-method (test-v2 (self <test-basics>))
  (let ((v2 (slot-ref self 'v2)))
    (assert-true (c? v2))
    (assert-true (c2? v2))
    (assert-true (not (c1? v2)))
    (assert-true (string=? (c-x v2) "V2"))
    (assert-true (string=? (c2-b v2) "b2"))))
  
(define-method (test-v3 (self <test-basics>))
  (let ((v3 (slot-ref self 'v3)))
    (assert-true (c? v3))
    (assert-true (c1? v3))
    (assert-true (c2? v3))
    (assert-true (string=? (c-x v3) "V3/1"))
    (assert-true (string=? (c1-a v3) "a3"))
    (assert-true (string=? (c2-b v3) "b3"))))

(define-method (test-v4 (self <test-basics>))
  (let ((v4 (slot-ref self 'v4)))
    (assert-true (c? v4))
    (assert-true (c1? v4))
    (assert-true (c2? v4))
    (assert-true (string=? (c-x v4) "V1"))
    (assert-true (string=? (c1-a v4) "a1"))
    (assert-true (string=? (c2-b v4) "b2"))))

(define-method (test-v5 (self <test-basics>))
  (let ((v5 (slot-ref self 'v5)))
    (assert-true (c? v5))
    (assert-true (c1? v5))
    (assert-true (c2? v5))
    (assert-true (string=? (c-x v5) "V2"))
    (assert-true (string=? (c1-a v5) "a3"))
    (assert-true (string=? (c2-b v5) "b2"))))

(exit-with-summary (run-all-defined-test-cases))

;; Local Variables:
;; mode: scheme
;; End:

;;; arch-tag: 774c4de9-d9f8-4754-8d40-38912ec7f3a1
