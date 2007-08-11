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
(use-modules (string wrap)
             (srfi srfi-13)
             (unit-test)
             (oop goops))


;; **********************************************************************
;; Test of the object itself...
;; **********************************************************************
(define-class <test-string-wrapper> (<test-case>)
  (str  #:accessor test-string))

(define-method (set-up-test (tw <test-string-wrapper>))
  (set! (test-string tw)
"
The last language environment specified with 
`set-language-environment'.   This variable should be 
set only with M-x customize, which is equivalent
to using the function `set-language-environment'.
"))

(define-method (test-util-equivalent-to-class (tw <test-string-wrapper>))
  (assert-equal (fill-string (test-string tw))
                (fill-string (make <text-wrapper>) (test-string tw)))
  (assert-equal (fill-string (test-string tw) #:line-width 20)
                (fill-string (make <text-wrapper> #:line-width 20) (test-string tw)))
  (assert-equal (fill-string (test-string tw) #:initial-indent " * " #:tab-width 3)
                (fill-string (make <text-wrapper> #:initial-indent " * " #:tab-width 3) (test-string tw))))
                
(define-method (test-fill-equivalent-to-joined-lines (tw <test-string-wrapper>))
  (assert-equal (fill-string (test-string tw))
                (string-join (string->wrapped-lines (test-string tw)) "\n" 'infix)))

(define-method (test-no-collapse-ws (tw <test-string-wrapper>))
  (assert-equal (fill-string (test-string tw) #:collapse-whitespace? #f)
"The last language environment specified with  `set-language-environment'.   This
variable should be  set only with M-x customize, which is equivalent to using
the function `set-language-environment'."))

(define-method (test-no-word-break (tw <test-string-wrapper>))
  (assert-equal "thisisalongword
blah
blah"
                (fill-string "thisisalongword blah blah"
                             #:line-width 8
                             #:break-long-words? #f)))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 03aff8ef-c04c-4fdc-99fa-be36a5faaebf
