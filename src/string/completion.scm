;;; ----------------------------------------------------------------------
;;;    completion -- string completion utility
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
(define-module (string completion)
   #:export (<string-completer>
             case-sensitive-completion?
             add-strings!
             all-completions
             complete
             )
   #:use-module (search basic)
   #:use-module (srfi srfi-13)
   #:use-module (oop goops))

(define-class <string-completer> ()
  (strings #:init-value #() #:accessor completer-strings #:init-keyword #:strings)
  (case-sensitive? #:init-value #t #:getter case-sensitive-completion? #:init-keyword #:case-sensitive?))

(define-method (initialize (sc <string-completer>)  initargs)
  (next-method)
  (if (not (vector? (completer-strings sc)))
      (add-strings! sc (completer-strings sc)))
  sc)


(define-method (add-strings! (sc <string-completer>) strings)
  (let ((comparison-func (if (case-sensitive-completion? sc)
                             string<?
                             string-ci<?)))
    (cond ((string? strings)
           (set! (completer-strings sc)
                 (list->vector (sort (append (vector->list (completer-strings sc))
                                             (string-split strings #\space))
                                     comparison-func))))

          ((list? strings)
           (set! (completer-strings sc)
                 (list->vector (sort (append (vector->list (completer-strings sc))
                                             strings)
                                     comparison-func))))

          (else (throw 'bad-type "expecting string or list")))
    #t))


(define (case-sensitive-partial-compare partial str2)
  (let ((len1 (string-length partial))
        (len2 (string-length str2)))
    (cond ((= len2 len1)  (cond ((string<? partial str2) -1)
                                ((string>? partial str2) 1)
                                (else 0)))
          ;; If partial is longer, then either it is less than the other string,
          ;; or we will call it > (even if the length they share is =)
          ((> len1 len2)  (if (string<? partial str2) 
                              -1
                              1))
          (else           (let ((newstr2 (substring str2 0 len1)))
                            (cond ((string<? partial newstr2) -1)
                                  ((string>? partial newstr2) 1)
                                  (else 0)))))))

(define (case-insensitive-partial-compare partial str2)
  (let ((len1 (string-length partial))
        (len2 (string-length str2)))
    (cond ((= len2 len1)  (cond ((string-ci<? partial str2) -1)
                                ((string-ci>? partial str2) 1)
                                (else 0)))
          ;; If partial is longer, then either it is less than the other string,
          ;; or we will call it > (even if the length they share is =)
          ((> len1 len2)  (if (string-ci<? partial str2) 
                              -1
                              1))
          (else           (let ((newstr2 (substring str2 0 len1)))
                            (cond ((string-ci<? partial newstr2) -1)
                                  ((string-ci>? partial newstr2) 1)
                                  (else 0)))))))


(define (all-completions completer str)
  (if (string-null? str)
      (vector->list (completer-strings completer))
      (let* ((vec (completer-strings completer))
             (compare (if (case-sensitive-completion? completer)
                          case-sensitive-partial-compare
                          case-insensitive-partial-compare))
             (found (binary-search-sorted-vector vec
                                                 str 
                                                 compare)))                   
        (if (not found)
            '()
            (let loop ((index (- found 1))
                       (ans (cons (vector-ref vec found)
                                  (let innerloop ((index (+ found 1))
                                                  (ans '()))
                                    (if (= index (vector-length vec))
                                        (reverse! ans)
                                        (let ((cur (vector-ref vec index)))
                                          (if (= (compare str cur) 0)
                                              (innerloop (+ index 1) (cons cur ans))
                                              (reverse! ans))))))))
              (if (< index 0)
                  ans
                  (let ((cur (vector-ref vec index)))
                    ;; is this string less than our partial target?
                    (if (= (compare str cur) 0)
                        (loop (- index 1) (cons cur ans))
                        ans))))))))

(define-method (complete (sc <string-completer>) str)
  (let ((longest-substring str)
        (exact-match? #f)
        (unique? #f)
        (completions (all-completions sc str))
        (s-p-l (if (case-sensitive-completion? sc)
                   string-prefix-length 
                   string-prefix-length-ci))
        (s=?   (if (case-sensitive-completion? sc)
                   string=?
                   string-ci=?)))
    (if (not (null? completions))
        (begin
          (set! longest-substring (car completions))
          (set! unique? (null? (cdr completions)))
          (for-each   (lambda (s)
                        (set! longest-substring 
                              (substring longest-substring 0 (s-p-l longest-substring s))))
                      completions)
          (set! exact-match? (s=? (car completions) longest-substring))))
    (values completions longest-substring exact-match? unique?)))
;;; arch-tag: cb29925e-b24e-4a69-a1f3-9595db6b7bbf
