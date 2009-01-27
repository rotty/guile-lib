;; (unit-test) -- a JUnit-like testing framework for Guile
;; Original code by John Maxwell <jmax@toad.net>
;; Copyright (C) 2003  Richard Todd
;; Copyright (C) 2004,2005  Andreas Rottmann

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This code originally came from
;; http://article.gmane.org/gmane.lisp.guile.user/1728.

(define-module (unit-test)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 pretty-print)
  #:export (assert-equal
            assert-true
            assert-numeric-=
            <test-result> 
            tests-run 
            tests-failed
            tests-log 
            failure-messages 
            test-started 
            test-failed 
            summary
            <test-case> 
            name 
            set-up-test 
            tear-down-test
            run 
            <test-suite> 
            tests 
            add
            run-all-defined-test-cases
            exit-with-summary)
  
  #:export-syntax (assert-exception))


 ;; Utility method for finding an object's method given its name. The
 ;; equivalent probably already exists somewhere in the MOP, but the doc
 ;; is a little sketchy.
(define-method (lookup-method (object <object>) (name <string>))
  (or
   (any (lambda (method)
          (let ((gf (method-generic-function method)))
            (if (string=? name (symbol->string (generic-function-name gf)))
                gf
                #f)))
        (class-direct-methods (class-of object)))
   (throw 'no-such-method-exception
          (string-append name
                         ": no such method in class "
                         (symbol->string (class-name (class-of object)))))))


;; Utility method for finding out whether a method is a slot-accessor
;; method for a particular class.
(define-method (slot-accessor? (object <object>) (method-name <string>))
  (any
   (lambda (slot)
     (or
      (and (slot-definition-getter slot)
           (string=? method-name
                     (symbol->string (generic-function-name 
                                      (slot-definition-getter slot)))))
      (and (slot-definition-setter slot)
           (string=? method-name
                     (symbol->string (generic-function-name 
                                      (slot-definition-setter slot)))))
      (and (slot-definition-accessor slot)
           (string=? method-name
                     (symbol->string (generic-function-name 
                                      (slot-definition-accessor slot)))))))
   (class-slots (class-of object))))

(define (assert-equal expected got)
  (if (not (equal? expected got))
      (throw 'test-failed-exception
             (with-output-to-string
               (lambda ()
                 (display "assert-equal: expected:\n")
                 (pretty-print expected)
                 (display "\ngot: \n")
                 (pretty-print got))))))

(define (assert-true got)
  (if (not got)
      (throw 'test-failed-exception
             (with-output-to-string
               (lambda ()
                 (display "assert-true: ")
                 (display " got: ")
                 (write got))))))

(define (assert-numeric-= expected got precision)
  (if (> (abs (- expected got)) precision)
      (throw 'test-failed-exception
             (with-output-to-string
               (lambda ()
                 (display "assert-true: ")
                 (display " got: ")
                 (write got))))))


(define-macro (assert-exception expression)
  `(catch #t
          (lambda ()
            ,expression
            (throw
             'test-failed-exception
             (format #f "assert-exception: no exception on ~S"
                     ',expression)))
          (lambda (key . args)
            (case key
              ((test-failed-exception) (apply throw key args))
              (else #t)))))


;;;----------------------------------------------------------------
(define-class <test-result> ()
  (tests-run-count #:init-value 0 #:accessor tests-run)
  (tests-failed-count #:init-value 0 #:accessor tests-failed)
  (tests-log-messages #:init-value '() #:accessor tests-log)
  (test-failure-messages #:init-value '() #:accessor failure-messages))

(define-method (test-started (self <test-result>) (description <string>))
  (set! (tests-log self)
        (append (tests-log self) `(,description)))
  (set! (tests-run self)
        (+ 1 (tests-run self))))

(define-method (test-failed (self <test-result>) (description <string>))
  (set! (failure-messages self)
        (append (failure-messages self) `(,description)))
  (set! (tests-failed self)
        (+ 1 (tests-failed self))))

(define-method (summary (self <test-result>))
  (format #f "~S run, ~S failed" (tests-run self) (tests-failed self)))



;;;----------------------------------------------------------------
(define-class <test-case> ()
  (name-value #:init-value "" #:accessor name #:init-keyword #:name))

(define-method (name-if-set (self <test-case>))
  (if (string-null? (name self))
      (symbol->string (class-name (class-of self)))
      (name self)))

(define-method (set-up-test (self <test-case>)))

(define-method (tear-down-test (self <test-case>)))

(define-method (run (self <test-case>) (result <test-result>))
  (display (string-append "Running test case: " (name-if-set self) "\n")
           (current-error-port))
  (catch #t
         (lambda ()
           (set-up-test self)
           (test-started result (name-if-set self))
           (catch #t
                  (lambda ()
                    (catch 'test-failed-exception
                           (lambda ()
                             ((lookup-method self (name-if-set self)) self))
                           (lambda (exception description)
                             (test-failed result
                                          (with-output-to-string
                                            (lambda ()
                                              (display (name-if-set self))
                                              (display " failed: ")
                                              (display description)))))))
                  (lambda throw-args
                    (test-failed result
                                 (with-output-to-string
                                   (lambda ()
                                     (display (name-if-set self))
                                     (display ": exception in test: ")
                                     (write throw-args))))))
           (tear-down-test self))
         (lambda throw-args
           (test-failed result
                        (with-output-to-string
                          (lambda ()
                            (display (name-if-set self))
                            (display ": exception in set up: ")
                            (write throw-args)))))))


;;;----------------------------------------------------------------
(define-class <test-suite> ()
  (tests-value #:init-value '() #:accessor tests #:init-keyword #:tests)
  (suite-name #:init-value "unknown" #:accessor name #:init-keyword #:name))


(define-method (test-case-suite (self <test-case>))
  (make <test-suite>
    #:name (string-append (name-if-set self) "-suite")
    #:tests (map
             (lambda (method-name)
               (make (class-of self) #:name method-name))
             (filter (lambda (method-name)
                       (and (>= (string-length method-name) 4)
                            (string=? "test" (substring method-name 0 4))
                            (not (slot-accessor? self method-name))))
                     (map (lambda (method)
                            (symbol->string 
                             (generic-function-name 
                              (method-generic-function method))))
                          (class-direct-methods (class-of self)))))))

(define-method (add (self <test-suite>) (test <test-case>))
  (set! (tests self)
        (cons (test-case-suite test)
              (tests self))))

(define-method (add (self <test-suite>) (suite <test-suite>))
  (set! (tests self)
        (cons suite (tests self))))

(define-method (run (self <test-suite>) (result <test-result>))
  (display (string-append
            "\nRunning test suite: " 
            (name self)
            " "
            (make-string (max (- 50 (string-length (name self))) 0) #\-)
            "\n")
           (current-error-port))
  (for-each
   (lambda (test)
     (run test result))
   (reverse! (tests self))))

;; returns the results of running all the subclasses of <test-case>
(define (run-all-defined-test-cases)
  (let ((my-suite (make <test-suite> #:name "main-suite"))
        (result (make <test-result>)))

    ;; add in an instance of each subclass of <test-case>
    (for-each (lambda (cl) (add my-suite (make cl)))
              (class-subclasses <test-case>))

    (run my-suite result)
    result))

(define (exit-with-summary result)
  (for-each
   (lambda (fm)
     (display fm (current-error-port))(newline (current-error-port)))
   (failure-messages result))

  (newline (current-error-port))
  (display (summary result) (current-error-port))(newline (current-error-port))
  (exit (if (> (tests-failed result) 0)
            1
            0)))
            
;;; arch-tag: 5411e756-a264-40a1-a7bb-28b55f339029
