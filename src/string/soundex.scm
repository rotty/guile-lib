;;; ----------------------------------------------------------------------
;;;    soundex -- the original soundex algorithm
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
;;; this is adapted from public domain code in SLIB...original code's notice is:
;;;
;;;"soundex.scm" Original SOUNDEX algorithm.
;;;From jjb@isye.gatech.edu Mon May  2 22:29:43 1994
;;;
;;; This code is in the public domain.
;;; ----------------------------------------------------------------------

; Taken from Knuth, Vol. 3 "Sorting and searching", pp 391--2
(define-module (string soundex)
  #:export (soundex)
  #:use-module (srfi srfi-1))

(define soundex
  (let* ((letters-to-omit
          (list #\A #\E #\H #\I #\O #\U #\W #\Y))
         (codes
          (list (list #\B #\1)
                (list #\F #\1)
                (list #\P #\1)
                (list #\V #\1)
                ;;
                (list #\C #\2)
                (list #\G #\2)
                (list #\J #\2)
                (list #\K #\2)
                (list #\Q #\2)
                (list #\S #\2)
                (list #\X #\2)
                (list #\Z #\2)
                ;;
                (list #\D #\3)
                (list #\T #\3)
                ;;
                (list #\L #\4)
                ;;
                (list #\M #\5)
                (list #\N #\5)
                ;;
                (list #\R #\6)))
         (xform
          (lambda (c)
            (let ((code (assv c codes)))
              (if code
                  (cadr code)
                  c)))))
    (lambda (name)
      (let ((char-list
             (map char-upcase
                  (remove (lambda (c)
                            (not (char-alphabetic? c)))
                             (string->list name)))))
        (if (null? char-list)
            name
            (let* ( ;; Replace letters except first with codes:
                   (n1 (cons (car char-list) (map xform char-list)))
                   ;; If 2 or more letter with same code are adjacent
                   ;; in the original name, omit all but the first:
                   (n2 (let loop ((chars n1))
                         (cond ((null? (cdr chars))
                                chars)
                               (else
                                (if (char=? (xform (car chars))
                                            (cadr chars))
                                    (loop (cdr chars))
                                    (cons (car chars) (loop (cdr chars))))))))
                   ;; Omit vowels and similar letters, except first:
                   (n3 (cons (car char-list)
                             (remove
                              (lambda (c)
                                (memv c letters-to-omit))
                              (cdr n2)))))
              ;;
              ;; pad with 0's or drop rightmost digits until of form "annn":
              (let loop ((rev-chars (reverse n3)))
                (let ((len (length rev-chars)))
                  (cond ((= 4 len)
                         (list->string (reverse rev-chars)))
                        ((> 4 len)
                         (loop (cons #\0 rev-chars)))
                        ((< 4 len)
                         (loop (cdr rev-chars))))))))))))


;;; arch-tag: 978c72d5-40bd-4e76-9af0-a74222a77b65
