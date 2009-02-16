;; (container async-queue) -- a thread-safe asynchronous queue
;; Copyright (C) 2007  Andreas Rottmann <a dot rottmann at gmx dot at>

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
;;
;;A asynchronous queue can be used to safely send messages from one
;;thread to another.
;;
;;; Code:

(define-module (container async-queue)
  #:export (make-async-queue async-enqueue! async-dequeue!)
  #:use-module (ice-9 q)
  #:use-module (ice-9 threads)
  #:use-module (oop goops))

(define-class <async-queue> ()
  (queue #:init-form (make-queue) #:getter queue)
  (condv #:init-form (make-condition-variable) #:getter condv)
  (mutex #:init-form (make-mutex) #:getter mutex)
  (waiting-threads #:init-value 0 #:accessor waiting-threads))

(define (make-async-queue)
  "Create a new asynchronous queue."
  (make <async-queue>))

(define (async-enqueue! q elt)
  "Enqueue @var{elt} into @var{q}."
  (with-mutex (mutex q)
    (enq! (queue q) elt)
    (if (> (waiting-threads q) 0)
        (signal-condition-variable (condv q)))))

(define (async-dequeue! q)
  "Dequeue a single element from @var{q}. If the queue is empty, the
calling thread is blocked until an element is enqueued by another
thread."
  (with-mutex (mutex q)
    (cond ((queue-empty? (queue q))
           (set! (waiting-threads q) (+ (waiting-threads q) 1))
           (let loop ()
             (cond ((queue-empty? (queue q))
                    (wait-condition-variable (condv q) (mutex q))
                    (loop))))
           (set! (waiting-threads q) (- (waiting-threads q) 1))))
    (deq! (queue q))))
