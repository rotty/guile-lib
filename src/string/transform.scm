;;; ----------------------------------------------------------------------
;;;    transform-- text manipulation beyond what's in core guile 
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
(define-module (string transform)
     #:export  (escape-special-chars
                transform-string
                expand-tabs
                center-string
                left-justify-string
                right-justify-string
                collapse-repeated-chars)
     #:use-module (ice-9 optargs)
     #:use-module (srfi srfi-13))

(define* (transform-string str match? replace #:optional (start #f) (end #f))
  ;;  I had implemented this with string-fold, but it was
  ;; slower...
  (let* ((os (open-output-string))
         (matcher (cond ((char? match?)
                         (lambda (c) (char=? match? c)))
                        ((procedure? match?)
                         match?)
                        ((string? match?)
                         (lambda (c) (string-index match? c)))
                        ((boolean? match?)
                         (lambda (c) match?))
                        (else (throw 'bad-type "expected #t, char, string, or procedure"))))
         (replacer (if (procedure? replace)
                       (lambda (c) (display (replace c) os))
                       (lambda (c) (display replace os)))))

    ;; put the first part in, un-transformed if they asked for it...
    (if (and start (<= start (string-length str)))
        (display (substring str 0 start) os))

    ;; process the portion they want processed....
    (string-for-each
     (lambda (c)
       (if (matcher c)
           ;; we have a match! replace the char as directed...
           (replacer c)

           ;; not a match, just insert the character itself...
           (write-char c os)))
     str
     (or start 0)
     (or end (string-length str)))

    ;; if there was any at the end, tack it on...
    (if (and end (< end (string-length str)))
        (display (substring str end) os))

    (get-output-string os)))

(define* (expand-tabs str #:optional (tab-size 8))
  (transform-string str 
                    #\tab
                    (make-string tab-size #\space)))

(define (escape-special-chars str special-chars escape-char)
  (transform-string str
                    (if (char? special-chars)
                        ;; if they gave us a char, use char=?
                        (lambda (c) (char=? c special-chars))

                        ;; if they gave us a string, see if our character is in it
                        (lambda (c) (string-index special-chars c)))

                    ;; replace matches with the character preceded by the escape character
                    (lambda (c) (string escape-char c))))

(define* (center-string str #:optional (width 80) (chr #\space) (rchr #f))
  (let* ((len (string-length str))
         (lpad (make-string (max (quotient (- width len) 2) 0) chr))
         ;; right-char == char unless it has been provided by the user
         (right-chr (or rchr chr))
         (rpad (if (char=? right-chr chr)
                   lpad
                   (make-string (max (quotient (- width len) 2) 0) right-chr))))
    (if (>= len width)
        str
        (string-append lpad str rpad (if (odd? (- width len)) (string right-chr) "")))))
     
(define* (left-justify-string str #:optional (width 80) (chr #\space))
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append str pad))))

(define* (right-justify-string str #:optional (width 80) (chr #\space))
  (let* ((len (string-length str))
         (pad (make-string (max (- width len) 0) chr)))
    (if (>= len width)
        str
        (string-append pad str))))

 (define* (collapse-repeated-chars str #:optional (chr #\space) (num 1))
   ;; define repeat-locator as a stateful match? function which remembers
   ;; the last character it had seen.
   (let ((repeat-locator
          ;; initialize prev-chr to something other than what we're seeking...
          (let ((prev-chr (if (char=? chr #\space) #\A #\space))
                (match-count 0))
            (lambda (c)
              (if (and (char=? c prev-chr)
                       (char=? prev-chr chr))
                  ;; found enough duplicates if the match-count is high enough
                  (begin
                    (set! match-count (+ 1 match-count))
                    (>= match-count num))

                  ;; did not find a duplicate
                  (begin (set! match-count 0) 
                         (set! prev-chr c) 
                         #f))))))

     ;; transform the string with our stateful matcher...
     ;; deleting matches...
     (transform-string str repeat-locator "")))

;;; arch-tag: 71550291-cf61-4ddd-bb50-2386b4d38756
