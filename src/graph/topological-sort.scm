;;; ----------------------------------------------------------------------
;;;    topological-sort  tsort functions....
;;;    < <FIXME (C) notice should say what?? >>
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
;;; **********************************************************************
;;; ORIGINAL CODE (C) notice....
;;; "tsort.scm" Topological sort
;;; Copyright (C) 1995 Mikael Djurfeldt
;;; This code is in the public domain.

;;; The algorithm is inspired by Cormen, Leiserson and Rivest (1990)
;;; "Introduction to Algorithms", chapter 23
;;; **********************************************************************
(define-module (graph topological-sort)
  #:export (topological-sort
            topological-sortq
            topological-sortv)
  #:use-module (math primes))

(define (topological-sort-helper dag insert lookup)
  (if (null? dag)
      '()
      (let* ((adj-table (make-hash-table
			 (car (primes> (length dag) 1))))
	     (sorted '()))
	(letrec ((visit
		  (lambda (u adj-list)
		    ;; Color vertex u
		    (insert adj-table u 'colored)
		    ;; Visit uncolored vertices which u connects to
		    (for-each (lambda (v)
				(let ((val (lookup adj-table v)))
				  (if (not (eq? val 'colored))
				      (visit v (or val '())))))
			      adj-list)
		    ;; Since all vertices downstream u are visited
		    ;; by now, we can safely put u on the output list
		    (set! sorted (cons u sorted)))))
	  ;; Hash adjacency lists
	  (for-each (lambda (def)
		      (insert adj-table (car def) (cdr def)))
		    (cdr dag))
	  ;; Visit vertices
	  (visit (caar dag) (cdar dag))
	  (for-each (lambda (def)
		      (let ((val (lookup adj-table (car def))))
			(if (not (eq? val 'colored))
			    (visit (car def) (cdr def)))))
		    (cdr dag)))
	sorted)))

(define (topological-sort dag)
  (topological-sort-helper dag hash-set! hash-ref))
(define (topological-sortq dag)
  (topological-sort-helper dag hashq-set! hashq-ref))
(define (topological-sortv dag)
  (topological-sort-helper dag hashv-set! hashv-ref))

;;; arch-tag: 9ef30b53-688a-43fc-b208-df78d5b38c74
