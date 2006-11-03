;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001 Rob Browning <rlb at defaultvalue dot org>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;; Unit tests for (debugging statprof).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (statprof))

(define-class <test-statprof> (<test-case>))

(debug-enable 'debug)
(trap-enable 'traps)

(define (fail reason . args)
  (throw 'test-failed-exception
         (with-output-to-string
           (lambda ()
             (display reason)
             (for-each
              (lambda (x) (display " ") (write x))
              args)))))

(define-method (test-call-frequencies (self <test-statprof>))
  ;; test to see that if we call 3 identical functions equally, they
  ;; show up equally in the call count +- 10%
  ;; from test-call-frequencies.scm

  (define (func-a n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  (define (func-b n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  (define (func-c n) (do ((i 0 (+ i 1))) ((= 200 i)) (+ i i)))
  
  (let ((num-calls 333)
        (func func-a))

    ;; Run test.
    (statprof-reset 0 30000 #t)
    (statprof-start)
    (let loop ((x num-calls))
      (cond
       ((positive? x)
        (func x)
        (cond
         ((eq? func func-a) (set! func func-b))
         ((eq? func func-b) (set! func func-c))
         ((eq? func func-c) (set! func func-a)))
        (loop (- x 1)))))
    (statprof-stop)

    (let* ((a-data (statprof-proc-call-data func-a))
           (b-data (statprof-proc-call-data func-b))
           (c-data (statprof-proc-call-data func-c))
           ;;
           (samples (map statprof-call-data-cum-samples
                         (list a-data b-data c-data)))
           (average (/ (apply + samples) 3))
           (max-allowed-drift (/ (sqrt average) average))
           ;;
           (diffs (map (lambda (x) (abs (- x average)))
                       samples))
           (max-diff (apply max diffs)))

      (let ((drift-fraction (/ max-diff average)))
        (if (> drift-fraction max-allowed-drift)
            (fail 
             "call frequencies too far apart"
             (* 100 drift-fraction))
            (simple-format 
             #t
             "  call-frequencies: within tolerance ~A<~A%\n"
             (* 100 drift-fraction) (* 100 max-allowed-drift)))))))

(define-method (test-call-counts (self <test-statprof>))
  ;; Test to see that if we call a function N times while the profiler
  ;; is active, it shows up N times.
  (debug-set! stack 0)
  (let ((num-calls 2000))

    (define (do-nothing n)
      (simple-format #f "FOO ~A\n" (+ n n)))
    
    ;; Run test.
    (statprof-reset 0 50000 #t)
    (statprof-start)
    (let loop ((x num-calls))
      (cond
       ((positive? x)
        (do-nothing x)
        (loop (- x 1))
        #t)))
    (statprof-stop)
    
    ;;(statprof-display)

    ;; Check result.
    (let ((proc-data (statprof-proc-call-data do-nothing)))
      (if (and proc-data
               (= (statprof-call-data-calls proc-data)
                  num-calls))
          'ok
          (fail "Expected ~A calls, got ~A.\n"
                num-calls
                (and proc-data
                     (statprof-call-data-calls proc-data)))))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 5e304f00-699f-481b-9be0-17b6034abe9c
