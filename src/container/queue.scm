;;;; q.scm --- Queues
;;;;
;;;; 	Copyright (C) 1995, 2001 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;

;;; Commentary:

;;; Q: Based on the interface to
;;;
;;; "queue.scm"  Queues/Stacks for Scheme
;;;  Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.

;;; {Q}
;;;
;;; A list is just a bunch of cons pairs that follows some constrains,
;;; right?  Association lists are the same.  Hash tables are just
;;; vectors and association lists.  You can print them, read them,
;;; write them as constants, pun them off as other data structures
;;; etc. This is good.  This is lisp.  These structures are fast and
;;; compact and easy to manipulate arbitrarily because of their
;;; simple, regular structure and non-disjointedness (associations
;;; being lists and so forth).
;;;
;;; So I figured, queues should be the same -- just a "subtype" of cons-pair
;;; structures in general.
;;;
;;; A queue is a cons pair:
;;;		( <the-q> . <last-pair> )
;;;
;;; <the-q> is a list of things in the q.  New elements go at the end
;;; of that list.
;;;
;;; <last-pair> is #f if the q is empty, and otherwise is the last
;;; pair of <the-q>.
;;;
;;; q's print nicely, but alas, they do not read well because the
;;; eq?-ness of <last-pair> and (last-pair <the-q>) is lost by read.
;;;
;;; All the functions that aren't explicitly defined to return
;;; something else (a queue element; a boolean value) return the queue
;;; object itself.

;;; Code:

(define-module (container queue)
  #:export (make-queue list->queue queue->list queue? queue-empty?  
            queue-front queue-rear queue-length
            remove-from-queue! push-queue! enqueue! pop-queue! dequeue!)
  #:use-module (srfi srfi-1))

;;; sync-q!
;;;   The procedure
;;;
;;;		(sync-q! q)
;;;
;;;   recomputes and resets the <last-pair> component of a queue.
;;;
(define (sync-queue! q)
  (set-cdr! q (if (pair? (car q)) 
                  (last-pair (car q))
		  #f))
  q)

;;; make-q
;;;  return a new q.
;;;
(define (make-queue) (cons '() #f))


;;; return a queue from the given list...
(define (list->queue lst)
  (cons lst (last-pair lst)))

;;; convert a queue to the equivalent list...
(define (queue->list q)
  (car q))

;;; q? obj
;;;   Return true if obj is a Q.
;;;   An object is a queue if it is equal? to '(() . #f)
;;;   or it is a pair P with (list? (car P))
;;;                      and (eq? (cdr P) (last-pair (car P))).
;;;
(define (queue? obj)
  (and (pair? obj)
       (if (pair? (car obj))
	   (eq? (cdr obj) (last-pair (car obj)))
	   (and (null? (car obj))
		(not (cdr obj))))))

;;; q-empty? obj
;;;
(define (queue-empty? obj) (null? (car obj)))

;;; q-empty-check q
;;;  Throw a q-empty exception if Q is empty.
(define (queue-empty-check q) (if (queue-empty? q) (throw 'q-empty q)))

;;; q-front q
;;;  Return the first element of Q.
(define (queue-front q) (queue-empty-check q) (caar q))

;;; q-rear q
;;;  Return the last element of Q.
(define (queue-rear q) (queue-empty-check q) (cadr q))

;;; q-remove! q obj
;;;  Remove all occurences of obj from Q.
(define (remove-from-queue! q should-remove?)
  (set-car! q (remove! should-remove? (car q)))
  (sync-queue! q))

;;; q-push! q obj
;;;  Add obj to the front of Q
(define (push-queue! q obj)
  (let ((h (cons obj (car q))))
    (set-car! q h)
    (or (cdr q) (set-cdr! q h)))
  q)

;;; enq! q obj
;;;  Add obj to the rear of Q
(define (enqueue! q obj)
  (let ((h (cons obj '())))
    (if (null? (car q))
	(set-car! q h)
	(set-cdr! (cdr q) h))
    (set-cdr! q h))
  q)

;;; q-pop! q
;;;  Take the front of Q and return it.
(define (pop-queue! q)
  (queue-empty-check q)
  (let ((it (caar q))
	(next (cdar q)))
    (if (null? next)
	(set-cdr! q #f))
    (set-car! q next)
    it))

;;; deq! q
;;;  Take the front of Q and return it.
(define dequeue! pop-queue!)

;;; q-length q
;;;  Return the number of enqueued elements.
;;;
(define (queue-length q) (length (car q)))

;;; arch-tag: ab9e0382-deaf-4f94-8225-cf86882eb2b5
;;; queue.scm ends here
