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
             (graph topological-sort)
             (oop goops))

(define-class <test-tsort> (<test-case>)
  (dag #:accessor dag))

(define-method (set-up-test (self <test-tsort>))
  (set! (dag self)
        (list '(shirt tie belt)
              '(tie belt)
              '(pants tie belt shoes shirt)
              '(belt)
              '(socks shoes pants)
              '(shoes))))

(define-method (test-tsortq (self <test-tsort>))
  (assert-equal '(socks pants shoes shirt tie belt)
                (topological-sortq (dag self))))


(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 1a7a4cb6-bd1d-4f12-a7a8-0d2bc0a92dad
