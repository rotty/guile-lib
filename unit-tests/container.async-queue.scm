#!/bin/sh
exec ${srcdir:-.}/guile-test-env guile -s "$0" "$@"
!#

(use-modules (oop goops)
             (ice-9 threads)
             (container async-queue)
             (unit-test))

(case (catch #t
        (lambda ()
          (call-with-new-thread (lambda () 'thread)
                                (lambda (e) 'error)))
        (lambda (key . args)
          'exception))
  ((exception error)
   (display "Thread support apparently disabled, will skip this test") (newline)
   (exit 0)))

(define-class <test-async-queue> (<test-case>)
  producer
  q)

(define (producer queue)
  (do ((i 0 (+ i 1)))
      ((>= i 100))
    (usleep 100)
    (async-enqueue! queue i))
  (async-enqueue! queue #f))

(define-method (set-up-test (self <test-async-queue>))
  (slot-set! self 'q (make-async-queue))
  (slot-set! self 'producer (make-thread producer (slot-ref self 'q))))

(define-method (test-consume (self <test-async-queue>))
  (let ((q (slot-ref self 'q)))
    (do ((i 0 (+ i 1)))
        ((>= i 100))
      (assert-equal i (async-dequeue! q)))
    (assert-equal (async-dequeue! q) #f)))

(exit-with-summary (run-all-defined-test-cases))

;; Local Variables:
;; mode: scheme
;; End:

