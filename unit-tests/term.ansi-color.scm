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
(use-modules (term ansi-color)
             (unit-test)
             (oop goops))


;; **********************************************************************
;; Test for color function
;; **********************************************************************
(define-class <test-color> (<test-case>))

(define-method (test-accepted-attributes  (self <test-color>))
  (for-each
   (lambda (x) (assert-true (not (string-null? (color x)))))
   '(RESET CLEAR BOLD DARK UNDERLINE UNDERSCORE BLINK CONCEALED 
           BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE
           ON-BLACK ON-RED ON-GREEN ON-YELLOW ON-BLUE ON-MAGENTA 
           ON-CYAN ON-WHITE)))
           
(define-method (test-clear-reset-equivalence (self <test-color>))
  (assert-equal (color 'RESET) (color 'CLEAR)))

(define-method (test-underline-underscore-equivalence (self <test-color>))
  (assert-equal (color 'UNDERLINE) (color 'UNDERSCORE)))


;; **********************************************************************
;; tests for colorize-string
;; **********************************************************************
(define-class <test-colorize-string> (<test-case>))

(define-method (test-ends-in-reset  (self <test-colorize-string>))
  (let ((rset (color 'RESET))
        (cs   (colorize-string "HI THERE" 'RED 'ON-BLUE)))
    (assert-equal rset
                  (substring cs (- (string-length cs) (string-length rset))))))

(define-method (test-skips-bad-attrs (self <test-colorize-string>))
  (let ((orig "Hello"))
    (assert-equal (string-append orig (color 'RESET)) 
                  (colorize-string orig 'AOEU 'MUOET 'AOCUOE))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: fa4d8d5a-2bf2-4c80-a79d-5bf6103ad537
