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

(use-modules (unit-test)
             (string completion)
             (srfi srfi-11) ;; let-values
             (oop goops))

(define-class <test-basic-completion> (<test-case>)
  (cmpl #:accessor completer))

(define-method (set-up-test (self <test-basic-completion>))
  (let ((c (make <string-completer>)))
    ;; the three ways to add a string to the completer
    (add-strings! c "wince hello wine help hellfire apricot wonder you your yourself")
    (add-strings! c '("build" "barn"))
    (add-strings! c "canticle")
    (set! (completer self) c)))
  
(define-method (test-all-completions (self <test-basic-completion>))
  (assert-equal '("apricot" "barn" "build" "canticle" "hellfire" "hello" 
                  "help" "wince" "wine" "wonder" "you" "your" "yourself")
                (all-completions (completer self) ""))
  (assert-equal '("wince" "wine" "wonder")
                (all-completions (completer self) "w"))
  (assert-equal '("hellfire" "hello" "help")
                (all-completions (completer self) "hel")))

(define-method (test-too-long-dropped (self <test-basic-completion>))
  (assert-equal '()
                (all-completions (completer self) "helpi"))
  (assert-equal '("yourself")
                (all-completions (completer self) "yourse")))

(define-method (test-complete (self <test-basic-completion>))
  ;; this is the NOT-found case...
  (let-values (((comp subs exact? unique?) (complete (completer self) "rope")))
              (assert-equal '() comp)
              (assert-equal #f exact?)
              (assert-equal #f unique?)
              (assert-equal "rope" subs))

  ;; the not-exact-or-unique case...
  (let-values (((comp subs exact? unique?) (complete (completer self) "hel")))
              (assert-equal #f exact?)
              (assert-equal #f unique?)
              (assert-equal "hel" subs))

  ;; more cases...
  (let-values (((comp subs exact? unique?) (complete (completer self) "your")))
              (assert-equal #t exact?)
              (assert-equal #f unique?)
              (assert-equal "your" subs))
  (let-values (((comp subs exact? unique?) (complete (completer self) "yo")))
              (assert-equal #t exact?)
              (assert-equal #f unique?)
              (assert-equal "you" subs))
  (let-values (((comp subs exact? unique?) (complete (completer self) "a")))
              (assert-equal #t exact?)
              (assert-equal #t unique?)
              (assert-equal "apricot" subs)))



(define-class <test-ci-completion> (<test-case>)
  (cmpl #:accessor completer))

(define-method (set-up-test (self <test-ci-completion>))
  (let ((c (make <string-completer> #:case-sensitive? #f)))
    ;; the three ways to add a string to the completer
    (add-strings! c "wince HeLlo wine help hellfire ApriCOT wONder yOu yOUr yourself")
    (add-strings! c '("build" "barn"))
    (add-strings! c "canticle")
    (set! (completer self) c)))

 
(define-method (test-all-completions (self <test-ci-completion>))
  (assert-equal '("ApriCOT" "barn" "build" "canticle" "hellfire" "HeLlo" 
                  "help" "wince" "wine" "wONder" "yOu" "yOUr" "yourself")
                (all-completions (completer self) ""))
  (assert-equal '("wince" "wine" "wONder")
                (all-completions (completer self) "w"))
  (assert-equal '("hellfire" "HeLlo" "help")
                (all-completions (completer self) "Hel")))

(define-method (test-too-long-dropped (self <test-ci-completion>))
  (assert-equal '()
                (all-completions (completer self) "HelpI"))
  (assert-equal '("yourself")
                (all-completions (completer self) "YourSe")))

(define-method (test-complete (self <test-ci-completion>))
  (let-values (((comp subs exact? unique?) (complete (completer self) "your")))
              (assert-equal #t exact?)
              (assert-equal #f unique?)
              (assert-equal "yOUr" subs))
  (let-values (((comp subs exact? unique?) (complete (completer self) "yo")))
              (assert-equal #t exact?)
              (assert-equal #f unique?)
              (assert-equal "yOu" subs))
  (let-values (((comp subs exact? unique?) (complete (completer self) "a")))
              (assert-equal #t exact?)
              (assert-equal #t unique?)
              (assert-equal "ApriCOT" subs)))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 4c1f1370-7c4c-4a06-9975-19f3df8842a7
