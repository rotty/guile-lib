;;; ----------------------------------------------------------------------
;;;    unit test
;;;    Copyright (C) 2004 Richard Todd
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

(use-modules (text parse-lalr)
             (unit-test)
             (oop goops))

(define-class <test-parse-lalr> (<test-case>))

(define-method (test-calc-parser (self <test-parse-lalr>))
  (letrec ((calc-parser 
            (lalr-parser
             ; terminal symbols
             (ID + - * /)
             ; productions
             (e (e + t)   : (+ $1 $3)
                (e - t)   : (- $1 $3)
                (t)       : $1)
             (t (t * f)   : (* $1 $3)
                (t / f)   : (/ $1 $3)
                (f)       : $1)
             (f (ID)      : $1)))

           ; as if a lexer had lexed "2 + 3 * 6 - 20 / 4"
           (input-tokens '((ID . 2) (+ . #f) (ID . 3)
                           (* . #f) (ID . 6) (- . #f) 
                           (ID . 20) (/ . #f) (ID . 4)))

           ; text lexer returns our pre-lexed tokens, and '*eoi* after that
           (lexer (lambda ()
                    (if (not (null? input-tokens))
                        (let ((ans (car input-tokens)))
                          (set! input-tokens (cdr input-tokens))
                          ans)
                        '*eoi*)))

           ; our error processor throws...
           (errorp (lambda args (throw 'parse-error args))))

    ; our input token should lead us to compute 15...
    (assert-equal 15 (calc-parser lexer errorp))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 14E498F8-F574-11D8-8C78-000A95B4C7DC

