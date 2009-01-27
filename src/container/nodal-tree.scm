;; (container nodal-tree) -- a tree data structure
;; Copyright (C) 2003,2004  Andy Wingo <wingo at pobox dot com>

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
;;A nodal tree is a tree composed of nodes, each of which may have
;;children. Nodes are represented as alists. The only alist entry that
;;is specified is @code{children}, which must hold a list of child
;;nodes. Other entries are intentionally left unspecified, so as to
;;allow for extensibility.
;;
;;; Code:

(define-module (container nodal-tree)
  #:export (nodal-tree? make-node
            node-ref node-set! node-children))

;; Returns pairs, not lists
(define (group-pairs l)
  (let lp ((in l) (out '()))
    (cond
     ((null? in) (reverse! out))
     (else (lp (list-cdr-ref in 2) (acons (car in) (cadr in) out))))))

(define (make-node . attributes)
  (or (even? (length attributes)) (error "invalid node atrributes"))
  (cons 'nodal-tree
        (let ((body (group-pairs attributes)))
          (if (assq 'children body)
              body
              (acons 'children '() body)))))

(define (node-set! node name val)
  (set-cdr! node (assq-set! (cdr node) name val)))

(define (node-ref node name)
  (assq-ref (cdr node) name))

(define (node-children node)
  (or (node-ref node 'children) '()))

(define (nodal-tree? x)
  "Predicate to determine if @var{x} is a nodal tree. Not particularly
efficient: intended for debugging purposes."
  (and (list? x)
       (eq? (car x) 'nodal-tree)
       (and-map pair? x)
       (and-map nodal-tree? (node-children x))))
