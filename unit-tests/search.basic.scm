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

(use-modules (search basic)
             (unit-test)
             (oop goops))

(define-class <test-dfs> (<test-case>))

(define-method (test-number-search (self <test-dfs>))
  (let ((finish?  (lambda (numlst) (= (car numlst) 25)))
        (expander (lambda (numlst) (if (> (car numlst) 50)
                                    '()
                                    (list (cons (* (car numlst) 2) 
                                                numlst)
                                          (cons (- (car numlst) 1)
                                                numlst))))))
  (assert-equal '(25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 20 10)
                (depth-first-search (list 10) finish? expander))
  (assert-equal '(25 26 13 14 7 8 9 10)
                (breadth-first-search (list 10) finish? expander))))

(define-class <test-binary-search> (<test-case>)
  (lst-1-to-100 #:accessor lst-1-to-100))

(define-method (set-up-test (self <test-binary-search>))
  (set! (lst-1-to-100 self)
        (let loop ((ans '())
                   (num 100))
          (if (< num 0)
              ans
              (loop (cons num ans) (- num 1))))))
 
(define-method (test-binary-edges  (self <test-binary-search>))
  ;; no entries...
  (assert-equal #f
                (binary-search-sorted-vector #() 3))

  ;; single entry...
  (assert-equal 0
                (binary-search-sorted-vector #(33) 33))
  (assert-equal #f
                (binary-search-sorted-vector #(33) 35))
  (assert-equal #f
                (binary-search-sorted-vector #(33) 32))

  ;; last entry
  (assert-equal 2
                (binary-search-sorted-vector #(33 35 45) 45))

  ;; middle entry
  (assert-equal 1
                (binary-search-sorted-vector #(33 35 45) 35))

  ;; first entry
  (assert-equal 0
                (binary-search-sorted-vector #(33 35 45) 33)))

(define-method (test-binary-results (self <test-binary-search>))
  ;; test single case
  (assert-equal 5
                (binary-search-sorted-vector (list->vector (lst-1-to-100 self)) 5))

  ;; test across an entire vector
  (assert-equal (lst-1-to-100 self)
                (map (lambda (n) (binary-search-sorted-vector (list->vector (lst-1-to-100 self)) n))
                     (lst-1-to-100 self))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 45377a62-02e9-4708-a8c5-34d6c6f8b606
