;; guile-lib
;; Copyright (C) 2007 Andy Wingo <wingo at pobox dot com>

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
;; Unit tests for (sxml fold).
;;
;;; Code:

(use-modules (oop goops)
             (unit-test)
             (sxml fold))

(define-macro (@@ mod var)
  `(module-ref (resolve-module ',mod) ',var))

(define atom? (@@ (sxml fold) atom?))
(define (id x) x)
(define-macro (accept expr)
  `(call-with-values
       (lambda () ,expr)
     (lambda values values)))

(define-class <test-sxml-fold> (<test-case>))

(define-method (test-fold (self <test-sxml-fold>))
  (define test-doc
    '(presentation
      (@ (width 1024)
         (height 768)
         (title-style "font-family:Georgia")
         (title-height 72)
         (title-baseline-y 96)
         (title-x 48)
         (text-height 64)
         (text-style "font-family:Georgia")
         (text-upper-left-x 96)
         (text-upper-left-y 216))
      (slide
       (@ (title "Declarative interface"))
       (p "The declarative interface"
          "lets you be more concise"
          "when making the slides."))
      (slide
       (@ (title "Still cumbersome"))
       (p "Parentheses are still"
          "cumbersome."))))

  (assert-true (atom? 'foo))
  (assert-true (atom? '()))
  (assert-true (not (atom? '(1 2 3))))

  (assert-equal (foldt id id test-doc) test-doc)

  (assert-equal (fold cons '() test-doc)
                (reverse test-doc))

  (assert-equal (foldts (lambda (seed tree) '())
                        (lambda (seed kid-seed tree)
                          (cons (reverse kid-seed) seed))
                        (lambda (seed tree)
                          (cons tree seed))
                        '()
                        test-doc)
                (cons test-doc '()))

  (assert-equal (foldts* (lambda (seed tree) (values '() tree))
                         (lambda (seed kid-seed tree)
                           (cons (reverse kid-seed) seed))
                         (lambda (seed tree)
                           (cons tree seed))
                         '()
                         test-doc)
                (cons test-doc '()))

  (assert-equal (fold-values cons test-doc '())
                (fold cons '() test-doc))

  (assert-equal (foldts*-values
                 (lambda (tree seed) (values tree '()))
                 (lambda (tree seed kid-seed)
                   (cons (reverse kid-seed) seed))
                 (lambda (tree seed)
                   (cons tree seed))
                 test-doc
                 '())
                (foldts* (lambda (seed tree) (values '() tree))
                         (lambda (seed kid-seed tree)
                           (cons (reverse kid-seed) seed))
                         (lambda (seed tree)
                           (cons tree seed))
                         '()
                         test-doc))

  (let () 
    (define (replace pred val list)
      (reverse
       (fold
        (lambda (x xs)
          (cons (if (pred x) val x) xs))
        '()
        list)))

    (define (car-eq? x what)
      (and (pair? x) (eq? (car x) what)))

    ;; avoid entering <slide>
    (assert-equal (foldts*-values
                   (lambda (tree seed)
                     (values (if (car-eq? tree 'slide) '() tree) '()))
                   (lambda (tree seed kid-seed)
                     (cons (reverse kid-seed) seed))
                   (lambda (tree seed)
                     (cons tree seed))
                   test-doc
                   '())
                  (cons
                   (replace (lambda (x) (car-eq? x 'slide))
                            '()
                            test-doc)
                   '())))

  (let ()
    (define (all-elts tree)
      (reverse!
       (foldts*-values
        (lambda (tree seed)
          (values tree seed))
        (lambda (tree seed kid-seed)
          kid-seed)
        (lambda (tree seed)
          (cons tree seed))
        tree
        '())))

    (define (len tree)
      (foldts*-values
       (lambda (tree seed)
         (values tree seed))
       (lambda (tree seed kid-seed)
         kid-seed)
       (lambda (tree seed)
         (1+ seed))
       tree
       0))

    (assert-equal (length (all-elts test-doc))
                  (len test-doc))))

(define-method (test-fold-layout (self <test-sxml-fold>))
  (define test-doc
    '(presentation
      (@ (width 1024)
         (height 768)
         (title-style "font-family:Georgia")
         (title-height 72)
         (title-baseline-y 96)
         (title-x 48)
         (text-height 64)
         (text-style "font-family:Georgia")
         (text-upper-left-x 96)
         (text-upper-left-y 216))
      (slide
       (@ (title "Declarative interface"))
       (p "The declarative interface"
          "lets you be more concise"
          "when making the slides."))
      (slide
       (@ (title "Still cumbersome"))
       (p "Parentheses are still"
          "cumbersome."))))

  (define (identity-layout tree)
    (fold-layout
     tree
     `((*default*
        . ,(lambda (tag params old-layout layout kids)
             (values layout
                     (if (null? (car params))
                         (cons tag kids)
                         (cons* tag (cons '@ (car params)) kids)))))
       (*text*
        . ,(lambda (text params layout)
             (values layout text))))
     '()
     (cons 0 0)
     '()))

  (assert-equal (accept (identity-layout test-doc))
                (list test-doc (cons 0 0))))

(exit-with-summary (run-all-defined-test-cases))
