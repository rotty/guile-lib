;; (search basic) -- basic search functions
;; Copyright (C) 2003  Richard Todd

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


;;; Commentary:
; This module has the classic search functions in it.
;;; Code:

(define-module (search basic)
  #:export (depth-first-search
            breadth-first-search
            binary-search-sorted-vector)
  #:use-module (ice-9 optargs))

(define (depth-first-search init done? expander)
"Performs a depth-first search from initial state @var{init}.  It will
return the first state it sees for which predicate @var{done?}
returns @code{#t}.  It will use function @var{expander} to get a list
of all states reacheable from a given state.

@var{init} can take any form the user wishes.  This function treats it
as opaque data to pass to @var{done?} and @var{expander}.

@var{done?} takes one argument, of the same type as @var{init}, and 
returns either @code{#t} or @code{#f}.

@var{expander} takes one argument, of the same type as @var{init}, and
returns a list of states that can be reached from there."
  (let loop ((working (list init)))
    (if (done? (car working))
        (car working)
        (loop (append (expander (car working))
                      (cdr working))))))

(define (breadth-first-search init done? expander)
"Performs a breadth-first search from initial state @var{init}.  It will
return the first state it sees for which predicate @var{done?}
returns @code{#t}.  It will use function @var{expander} to get a list
of all states reacheable from a given state.

@var{init} can take any form the user wishes.  This function treats it
as opaque data to pass to @var{done?} and @var{expander}.

@var{done?} takes one argument, of the same type as @var{init}, and 
returns either @code{#t} or @code{#f}.

@var{expander} takes one argument, of the same type as @var{init}, and
returns a list of states that can be reached from there."
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
"
"
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
"Searches a sorted vector @var{vec} for item @var{target}.  A binary search
is employed which should find an item in O(log n) time if it is present.
If @var{target} is found, the index into @var{vec} is returned.

As part of the search, the function @var{cmp} is applied to
determine whether a vector item is less than, greater than, or equal
to the @var{target}.  If @var{target} cannot be found in the vector,
then @var{default} is returned.

@var{cmp} defaults to @code{-}, which gives a correct comparison for
vectors of numbers.  @var{default} will be @code{#f} if another value
is not given.

@lisp
 (binary-search-sorted-vector #(10 20 30) 20) @result{} 1
@end lisp"
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
