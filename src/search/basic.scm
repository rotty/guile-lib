;;; ----------------------------------------------------------------------
;;;    basic search functions...
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

(define-module (search basic)
  #:export (depth-first-search
            breadth-first-search
            binary-search-sorted-vector)
  #:use-module (ice-9 optargs))

(define (depth-first-search init done? expander)
  (let loop ((working (list init)))
    (if (done? (car working))
        (car working)
        (loop (append (expander (car working))
                      (cdr working))))))

(define (breadth-first-search init done? expander)
  (let loop ((working (list init)))
    (if (done? (car working))
        (car working)
        (loop (append (cdr working)
                      (expander (car working)))))))

(define (alpha-beta-search init 
                           evaluator 
                           evaluator-favors-current-player? 
                           expander 
                           depth)
  (let loop ((current init)
             (evaluator-favors evaluator-favors-current-player?)
             (d       depth))
    (if (= d 0)
        (evaluator c)
        (apply (if evaluator-favors
                   max
                   min)
               (map (lambda (c) (loop c (not evaluator-favors) (- d 1)))
                    (expander c))))))

(define* (binary-search-sorted-vector vec target #:optional (cmp -) (default #f))
  (let loop ((low-index 0)
             (high-index (vector-length vec)))

    ;; if the low index crosses the high index, then we didn't find the entry
    (if (>= low-index high-index)
        default

        (let* ((middle-index (quotient (+ low-index high-index) 2)) 
               (comparison (cmp target (vector-ref vec middle-index))))
          
          (cond 
           ;; target was less than the current...
           ((< comparison 0)
            (loop low-index middle-index))
           
           ;; target was greater than the current...
           ((> comparison 0)
            (loop (+ middle-index 1) high-index))
           
           ;; we must have found it!
           (else middle-index))))))
;;; arch-tag: 1ecf547d-ffdd-4cae-b37f-215a825aa293
