;;; ----------------------------------------------------------------------
;;;    unit test
;;;    Copyright (C) 2004 Richard Todd
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

(use-modules (md5)
             (unit-test)
             (oop goops))

(define-class <test-md5> (<test-case>)
  (test-string #:getter test-string 
               #:init-value "The quick brown fox.")
  ;; this answer generated with /usr/bin/md5 for comparison purposes...
  (test-answer #:getter test-answer 
               #:init-value  "2e87284d245c2aae1c74fa4c50a74c77"))


(define-method (test-default-port (self <test-md5>))
  (assert-equal (test-answer self) 
                (with-input-from-string (test-string self) 
                  (lambda () (md5)))))

(define-method (test-given-port (self <test-md5>))
  (assert-equal (test-answer self) 
                (md5 (open-input-string (test-string self)))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 0D9E8711-F9E7-11D8-AE52-000A95CD5044


