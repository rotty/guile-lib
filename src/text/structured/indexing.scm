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
;;Given a piece of structured text, return an index of a specified
;;variety.
;;
;;; Code:

(define-module (text structured indexing)
  :use-module (text structured)
  :use-module (scheme documentation)
  :use-module (srfi srfi-13)
  :export (stext-extract-index))

;; Pairs: the command and the argument index for the name
(define defines
  '((deftp . 1)
    (defcv . 2)
    (defivar . 1)
    (deftypeivar . 2)
    (defop . 2)
    (deftypeop . 3)
    (defmethod . 1)
    (deftypemethod . 2)
    (defopt . 0)
    (defvr . 1)
    (defvar . 0)
    (deftypevr . 2)
    (deftypevar . 1)
    (deffn . 1)
    (deftypefn . 2)
    (defmac . 0)
    (defspec . 0)
    (defun . 0)
    (deftypefun . 1)))

(define (def-arg-ref def n)
  (define (split-args)
    (let loop ((args def) (ret '()))
      (cond
       ((null? args)
        (reverse ret))
       ((and (string? (car args))
             (string-index (car args) #\space))
        (loop (append (string-split (car args) #\space) (cdr args)) ret))
       (else
        (loop (cdr args) (cons (car args) ret))))))
  (list-ref (split-args) (1+ n)))

(define (stext-extract-index stext manual-name kind)
  "Given a piece of structured text @var{stext}, index all of the
entries of type @var{kind}. @var{kind} can be one of the predefined
texinfo indices (@code{concept}, @code{variable}, @code{function},
@code{key}, @code{program}, @code{type}) or one of the special symbols
@code{auto} or @code{all}. @code{auto} will scan the stext for a
@code{(printindex)} statement, and @code{all} will generate an index
from all entries, regardless of type.

The returned index is a list of pairs, the @sc{car} of which is the
entry (a string) and the @sc{cdr} of which is a node name (a string)."
  (let loop ((stext stext) (entries '()))
    (cond
     ((null? stext)
      entries)
     ((list? (car stext))
      (cond
       ((and (not (null? (cdr stext)))
             (eq? (caar stext) 'anchor)
             (memq (caadr stext)
                   '(cindex findex vindex kindex pindex tindex)))
        ;; We know an anchor precedes index entries.
        (loop (cddr stext)
              (cons (list (cadadr stext) manual-name (cadar stext))
                    entries)))
       ((let loop ((sym (caar stext)) (defines defines))
          (cond
           ((null? defines) #f)
           ((eq? sym (caar defines)) (cdar defines)) ;; return the index
           (else (loop sym (cdr defines)))))
        =>
        (lambda (index)
          (loop (cddr stext)
                (cons (list (def-arg-ref (car stext) index) manual-name (cadadr stext))
                      entries))))
       (else
        ;; Recurse...
        (loop (cdr stext)
              (loop (car stext) entries)))))
     (else
      (loop (cdr stext) entries)))))

;;; arch-tag: 16b34d7b-c5df-4f96-b90a-56d00cb164e6
