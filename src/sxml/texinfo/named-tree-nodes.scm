;; guile-lib
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;Doc me! Especially why I'm implemented with GOOPS and not lists.
;;        
;;; Code:

(define-module (sxml texinfo named-tree-nodes)
  #:use-module (ice-9 optargs)
  #:use-module (sxml simple)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:export (stexi->named-tree-nodes <named-tree-node>))

(define-class-with-docs <named-tree-node> ()
  "A class for chunking a document into hierarchical sections."
  (name
   #:init-keyword #:name)
  (value
   #:init-keyword #:value
   #:init-value #f)
  (parent
   #:init-keyword #:parent
   #:init-value #f)
  (children
   #:init-keyword #:children
   #:init-value '()))

(define numbered-list
  '(#f chapter section subsection subsubsection))
(define unnumbered-list
  '(top unnumbered unnumberedsec unnumberedsubsec unnumberedsubsubsec))
(define appendix-list
  '(#f appendix appendixsec appendixsubsec appendixsubsubsec))
(define (name->depth name max-depth)
  (let ((depth (or (list-index numbered-list name)
                   ;;(list-index unnumbered-list name)
                   (list-index appendix-list name))))
    (if (and depth (> depth max-depth))
        #f
        depth)))

(define (node? elt)
  (and (pair? elt) (eq? (car elt) 'node)))
(define (chunking-section? elt max-depth)
  (and (pair? elt) (name->depth (car elt) max-depth)))

(define (append-child! parent kid)
  (or (not parent)
      (slot-set! parent 'children
                 (append! (slot-ref parent 'children)
                          (list kid)))))

(define (find-parent node)
  (or (and=> (slot-ref node 'parent) find-parent) node))

(define* (stexi->named-tree-nodes stexi max-depth
                                  #:optional (<node> <named-tree-node>))
  "Break @var{stexi} into a tree of @code{<named-tree-node>}s. Only
break until sectioning identifiers of depth @var{max-depth}.

You may pass @code{#:<node> @var{<node>}} at the end of the arguments to
specify a subclass of @code{<named-tree-node>} to use."

  (define (make-node parent tree-title)
    (let ((node (make <node>
                  #:parent parent #:name (sxml->string tree-title))))
      (append-child! parent node)
      node))

  (or (eq? (car stexi) 'texinfo) (error "Invalid stexi"))

  (let lp ((in stexi)
           (val '())
           (node (make-node #f (cadr stexi)))
           (parent #f)
           (depth 0))
    (cond
     ((null? in)
      (slot-set! node 'value (reverse! val))
      (find-parent node))
     ((and (node? (car in)) (pair? in) (pair? (cdr in))
           (chunking-section? (cadr in) max-depth))
      (slot-set! node 'value (reverse! val))
      (let ((new-depth (name->depth (caadr in) max-depth)))
        (let new-parent ((parent node) (diff (- new-depth depth)))
          (cond
             ((not parent) (error "invalid stexi"))
             ((positive? diff)
              (or (eq? diff 1)
                  (error "can only descend by one depth level at a time"
                         (car in) (cadr in)))
              (lp (cons* 'texinfo `(% (title ,(sxml->string (cadr in))))
                         (cddr in))
                  '() (make-node parent (cadr in)) parent new-depth))
             (else
              (new-parent (slot-ref parent 'parent) (1+ diff)))))))
     (else
      (lp (cdr in) (cons (car in) val) node parent depth)))))

;;; arch-tag: aff19153-493d-4755-ba6f-22cc7fb43c60
