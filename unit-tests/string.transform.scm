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
(use-modules (string transform)
             (unit-test)
             (oop goops))


;; **********************************************************************
;; Test for expand-tabs
;; **********************************************************************
(define-class <test-expansion> (<test-case>))

(define-method (test-beginning-expansion  (self <test-expansion>))
  (assert-equal "        Hello"
                (expand-tabs "\tHello"))
  (assert-equal "                Hello"
                (expand-tabs "\t\tHello")))

(define-method (test-ending-expansion (self <test-expansion>))
  (assert-equal "Hello        "
                (expand-tabs "Hello\t"))
  (assert-equal "Hello                "
                (expand-tabs "Hello\t\t")))

(define-method (test-middle-expansion (self <test-expansion>))
  (assert-equal "Hello        there"
                (expand-tabs "Hello\tthere"))
  (assert-equal "Hello                there"
                (expand-tabs "Hello\t\tthere")))

(define-method (test-alternate-tab-size (self <test-expansion>))
  (assert-equal "Hello   there"   (expand-tabs "Hello\tthere" 3))
  (assert-equal "Hello    there"  (expand-tabs "Hello\tthere" 4))
  (assert-equal "Hello     there" (expand-tabs "Hello\tthere" 5)))
  
;; **********************************************************************
;; tests for escape-special-chars
;; **********************************************************************
(define-class <test-escape> (<test-case>))

(define-method (test-single-escape-char (self <test-escape>))
  (assert-equal "HeElElo"
                (escape-special-chars "Hello" #\l #\E)))

(define-method (test-multiple-escape-chars (self <test-escape>))
  (assert-equal "HEeElElo"
                (escape-special-chars "Hello" "el" #\E)))


;; **********************************************************************
;; tests for collapsing-multiple-chars
;; **********************************************************************
(define-class <test-collapse> (<test-case>)
  (test-string #:accessor test-string))

(define-method (set-up-test (tc <test-collapse>))
  (set! (test-string tc) "H e  l   l    o     t      h       e        r         e"))

(define-method (test-basic-collapse (tc <test-collapse>))
  (assert-equal "H e l l o t h e r e"
                (collapse-repeated-chars (test-string tc))))

(define-method (test-choose-other-char (tc <test-collapse>))
  (assert-equal "H-e-l-l-o-t-h-e-r-e"
                (collapse-repeated-chars (transform-string (test-string tc) #\space #\-) 
                                         #\-)))

(define-method (test-choose-maximum-repeats (tc <test-collapse>))
  (assert-equal "H e  l  l  o  t  h  e  r  e"
                (collapse-repeated-chars (test-string tc) #\space 2))
  (assert-equal "H e  l   l   o   t   h   e   r   e"
                (collapse-repeated-chars (test-string tc) #\space 3)))
  
(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: c18844b1-690c-4257-879a-4cadae11f0ec
