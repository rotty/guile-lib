;; (string completion) -- a tab completion library
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


#!
;;; Commentary:
This module provides a facility that can be used to implement features such as
TAB-completion in programs.  A class @code{<string-completer>} tracks all the 
potential complete strings.  Here is an example usage.

@lisp
(use-modules (string completion)
             (oop goops)
             (srfi srfi-11))      ;; for the (let-values)

(define c (make <string-completer>))
(add-strings! c "you your yourself yourselves")

(let-values (((completions expansion exact? unique?) (complete c "yours")))
            (display completions)(newline)
            (display expansion) (newline)
            (display exact?)(newline)
            (display unique?)(newline))

==> ("yourself" "yourselves")
    "yoursel"
    #f
    #f
@end lisp

There are several more options for usage, which are detailed in the class and
method documentation.
;;; Code:
!#

(define-module (string completion)
   #:export (<string-completer>
             case-sensitive-completion?
             add-strings!
             all-completions
             complete
             )
   #:use-module (search basic)
   #:use-module (srfi srfi-13)
   #:use-module (scheme documentation)
   #:use-module (oop goops))

(define-generic-with-docs case-sensitive-completion?
"@code{case-sensitive-completion? completer}.  Returns @code{#t} if the 
completer is case-sensitive, and @code{#f} otherwise.")

(define-class-with-docs <string-completer> ()
"This is the class that knows what the possible expansions are, and 
can determine the completions of given partial strings.  The following
are the recognized keywords on the call to @code{make}:

@table @code
@item #:strings
This gives the completer an initial set of strings.  It is optional, and
the @code{add-strings!} method can add strings to the completer later,
whether these initial strings were given or not.  The strings that 
follow this keyword can take any form that the @code{add-strings!} 
method can take (see below).

@item #:case-sensitive?
This is a boolean that directs the completer to do its comparisons in 
a case sensiteve way or not.  The default value is @code{#t}, for
case-sensitive behavior.
@end table"
  (strings #:init-value #() #:accessor completer-strings #:init-keyword #:strings)
  (case-sensitive? #:init-value #t #:getter case-sensitive-completion? #:init-keyword #:case-sensitive?))

(define-method (initialize (sc <string-completer>)  initargs)
  (next-method)
  (if (not (vector? (completer-strings sc)))
      (add-strings! sc (completer-strings sc)))
  sc)

(define-generic-with-docs add-strings!
"@code{add-strings! completer strings}.  Adds the given strings
to the set of possible comletions known to @var{completer}.
@var{strings} can either be a list of strings, or a single string
of words separated by spaces.  The order of the words given is
not important.")

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
"Returns a list of all possible completions for the given string
@var{str}.  The returned list will be in alphabetical order.

Note that users wanting to customize the completion algorithm 
can subclass @code{<string-completer>} and override this method."
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

(define-generic-with-docs complete 
"@code{complete completer str}.  Accepts a string, @var{str}, and
returns four values via a @code{values} call.  These are:
@table @var
@item completions
This is the same list that would be returned from a call to
@code{all-completions}.

@item expansion
This is the longest string that would have returned identical results.
In other words, this is what most programs replace your string with
when you press TAB once.  This value will be equal to @var{str} if
there were no known completionss.

@example
 (\"wonders\" \"wonderment\" \"wondering\") 
completed against \"won\" yields an expansion 
of \"wonder\"
@end example

@item exact?
This will be @code{#t} if the returned @var{expansion} is an exact
match of one of the possible completions.

@item unique?
This will be #t if there is only one possible completion. Note that
when @var{unique?}  is @code{#t}, then @var{exact?} will also be
@code{#t}.
@end table")

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
