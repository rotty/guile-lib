;;; ----------------------------------------------------------------------
;;;    wrap -- text filling and wrapping 
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
(define-module (string wrap)
     #:export  (<text-wrapper>
                fill-string
                string->wrapped-lines)
     #:use-module (srfi srfi-13)
     #:use-module (srfi srfi-14)
     #:use-module (string transform)
     #:use-module (oop goops))

(define-class <text-wrapper> ()
  (width #:init-value 80 #:getter line-width #:init-keyword #:line-width)
  (expand-tabs #:init-value #t #:getter expand-tabs? #:init-keyword #:expand-tabs?)
  (tab-width   #:init-value 8  #:getter tab-width #:init-keyword #:tab-width)
  (collapse-whitespace #:init-value #t #:getter collapse-whitespace? #:init-keyword #:collapse-whitespace?)
  (subsequent-indent #:init-value "" #:getter subsequent-indent #:init-keyword #:subsequent-indent)
  (initial-indent #:init-value "" #:getter initial-indent #:init-keyword #:initial-indent)
  (break-long-words? #:init-value #t #:getter break-long-words? #:init-keyword #:break-long-words?))

(define-method (fill-string str . keywds)
  (string-join (apply string->wrapped-lines (cons str keywds))
               "\n"
               'infix))

(define-method (fill-string (tw <text-wrapper>) str)
  (string-join (string->wrapped-lines tw str)
               "\n"
               'infix))

(define-method (string->wrapped-lines str . keywds)
  (string->wrapped-lines (apply make (cons <text-wrapper> keywds)) str))

;; split a text string into segments that have the form...
;;  <ws non-ws>  <ws non-ws> etc..
(define (split-by-single-words str)
  (let ((non-wschars (char-set-complement char-set:whitespace)))
    (let loop ((ans '())
               (index 0))
      (let ((next-non-ws (string-index str non-wschars index)))
        (if next-non-ws
          ;; found non-ws...look for ws following...
          (let ((next-ws (string-index str char-set:whitespace next-non-ws)))
            (if next-ws
                ;; found the ws following...
                (loop (cons (substring str index next-ws) ans)
                      next-ws)
                ;; did not find ws...must be the end...
                (reverse (cons (substring str index) ans))))
          ;; did not find non-ws... only ws at end of the string...
          (reverse ans))))))

(define-method (string->wrapped-lines (tw <text-wrapper>) str)
  ;; this is where the real work begins...

  ;; replace newlines with spaces
  (set! str (transform-string str (lambda (c) (char=? c #\nl)) #\space))

  ;; expand tabs if they wanted us to...
  (if (expand-tabs? tw)
      (set! str (expand-tabs str (tab-width tw))))

  ;; collapse whitespace if they wanted us to...
  (if (collapse-whitespace? tw)
      (set! str (collapse-repeated-chars str)))
  
  ;; drop any whitespace from the front...
  (set! str  (string-trim str))

  ;; now start breaking the text into lines...
  (let loop ((ans '())
             (words (split-by-single-words str))
             (line (initial-indent tw))
             (count 0))
    (if (null? words)
        ;; out of words? ...done!
        (reverse (if (> count 0)
                     (cons line ans)
                     ans))
        
        ;; not out of words...keep going...
        (let ((length-left (- (line-width tw)
                              (string-length line)))
              (next-word (if (= count 0)
                             (string-trim (car words))
                             (car words))))
          (cond 
           ;; does the next entry fit?
           ((<= (string-length next-word)
                length-left)
            (loop ans
                  (cdr words)
                  (string-append line next-word)
                  (+ count 1)))

           ;; ok, it didn't fit...is there already at least one word on the line?
           ((> count 0)
            ;; try to use it for the next line, then...
            (loop (cons line ans)
                  words
                  (subsequent-indent tw)
                  0))
           
           ;; ok, it didn't fit...and it's the first word. 
           ;; were we told to break up long words?
           ((break-long-words? tw)
            ;; break the like at the limit, since the user wants us to...
            (loop (cons (string-append line (substring next-word 0 length-left))
                        ans)
                  (cons (substring next-word length-left)
                        (cdr words))
                  (subsequent-indent tw)
                  0))

           ;; well, then is it the first word and we *shouldn't* break long words, then...
           (else
            (loop (cons (string-append line next-line)
                        ans)
                  (cdr words)
                  (subsequent-indent tw)
                  0)))))))
;;; arch-tag: 61c4a6f9-0c13-4575-95dc-a7e647812216
