;; (sxml unicode) -- rendering unicode to byte strings
;; Copyright (C) 2008  Andy Wingo <wingo at pobox dot com>

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

;;; Commentary:
;;
;; Routines for encoding unicode codepoints into byte strings.
;;
;;; Code:

(define-module (sxml unicode)
  #:export (unichar->utf-8))

(define (unichar->utf-8 u)
  (define (byte header mask shift)
    (integer->char (logior header (logand mask (ash u shift)))))
  (cond
   ((< u #x000000) (error "bad unicode code point" u))
   ((< u #x000080) (string (integer->char u)))
   ((< u #x000800) (string (byte #b11000000 #b11111 -6)
                           (byte #b10000000 #b111111 0)))
   ((< u #x00d800) (string (byte #b11100000 #b1111 -12)
                           (byte #b10000000 #b111111 -6)
                           (byte #b10000000 #b111111 0)))
   ((< u #x00e000) (error "bad unicode code point" u))
   ((< u #x010000) (string (byte #b11100000 #b1111 -12)
                           (byte #b10000000 #b111111 -6)
                           (byte #b10000000 #b111111 0)))
   ((< u #x110000) (string (byte #b11110000 #b111 -18)
                           (byte #b10000000 #b111111 -12)
                           (byte #b10000000 #b111111 -6)
                           (byte #b10000000 #b111111 0)))
   (else (error "bad unicode code point" u))))

