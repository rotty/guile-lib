;;; Original mail for this package....jmax@toad.net
;;;
;;;Well, it may not be the most finished/polished thing around, but for 
;;;whatever it's worth, you're welcome to use this. It's a fairly literal 
;;;translation of the unit test code in Kent Beck's "Test-Driven Development" 
;;;book (the book's code is in Java). Done for my own edification as I was 
;;;reading the book, it also served as a nice way to get my feet wet in 
;;;goops.
;;;
;;;goops-unit.scm is the actual framework; goops-unit-test.scm is a set of 
;;;unit tests for it written using it.
;;;
;;;Hope this is of some use; feedback is welcome.
;;;
;;;-John
;;;
;;;***************************************************************************
;;;goops-unit.scm:
;;;***************************************************************************
;;;
(define-module (unit-test)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
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
            exit-with-summary))


 ;; Utility method for finding an object's method given its name. The
 ;; equivalent probably already exists somewhere in the MOP, but the doc
 ;; is a little sketchy.
(define-method (lookup-method (object <object>) (name <string>))
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (method)
        (if (string=? name
                      (symbol->string (generic-function-name 
                                       (method-generic-function method))))
            (return (method-generic-function method))
            #f))
      (class-direct-methods (class-of object)))
     (throw 'no-such-method-exception
            (string-append name
                           ": no such method in class "
                           (symbol->string (class-name (class-of object))))))))


;; Utility method for finding out whether a method is a slot-accessor
;; method for a particular class.
(define-method (slot-accessor? (object <object>) (method-name <string>))
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (slot)
        (if (or
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
                                             (slot-definition-accessor slot))))))
            (return #t)))
      (class-slots (class-of object)))
     (return #f))))



(define (assert-equal expected got)
  (if (not (equal? expected got))
      (throw 'test-failed-exception
             (with-output-to-string
               (lambda ()
                 (display "assert-equal: expected: ")
                 (write expected)
                 (display " got: ")
                 (write got))))))

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
