;;; ----------------------------------------------------------------------
;;;    ansi-color -- color output using ANSI escape sequences
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
(define-module (term ansi-color)
     #:export  (color
                colorize-string)
     #:use-module (srfi srfi-1)   ; for 'remove'
     #:use-module (srfi srfi-13)) ; for 'string-join' 

(define ansi-color-tables
  (let ((table (make-hash-table 23)))
    (hashq-set! table 'CLEAR "0")
    (hashq-set! table 'RESET "0")
    (hashq-set! table 'BOLD  "1")
    (hashq-set! table 'DARK  "2")
    (hashq-set! table 'UNDERLINE "4")
    (hashq-set! table 'UNDERSCORE "4")
    (hashq-set! table 'BLINK "5")
    (hashq-set! table 'REVERSE "6")
    (hashq-set! table 'CONCEALED "8")
    (hashq-set! table 'BLACK "30")
    (hashq-set! table 'RED "31")
    (hashq-set! table 'GREEN "32")
    (hashq-set! table 'YELLOW "33")
    (hashq-set! table 'BLUE "34")
    (hashq-set! table 'MAGENTA "35")
    (hashq-set! table 'CYAN "36")
    (hashq-set! table 'WHITE "37")
    (hashq-set! table 'ON-BLACK "40")
    (hashq-set! table 'ON-RED "41")
    (hashq-set! table 'ON-GREEN "42")
    (hashq-set! table 'ON-YELLOW "43")
    (hashq-set! table 'ON-BLUE "44")
    (hashq-set! table 'ON-MAGENTA "45")
    (hashq-set! table 'ON-CYAN "46")
    (hashq-set! table 'ON-WHITE "47")
    table))

(define (color . lst)
  (let ((color-list 
         (remove not 
                 (map (lambda (color) (hashq-ref ansi-color-tables color))
                      lst))))
    (if (null? color-list)
        ""
        (string-append 
         (string #\esc #\[)
         (string-join color-list ";" 'infix)
         "m"))))
  
(define (colorize-string str . color-list)
  (string-append
   (apply color color-list)
   str
   (color 'RESET)))

;;; arch-tag: e8dd6a14-490c-417e-a7fe-983939293db1
