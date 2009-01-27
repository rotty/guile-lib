;; (debugging time) -- a timing macro
;; Copyright (C) 2004  Andy Wingo <wingo at pobox dot com>

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
;;@c it's really texinfo
;; Defines a macro to time execution of a body of expressions. Each
;; element is timed individually.
;;
;;; Code:

(define-module (debugging time)
  #:use-module (scheme documentation)
  #:export (time))

(define-macro-with-docs (time expr . others)
  "syntax: @code{(time @var{expr1} @var{expr2}...)}

Times the execution of a list of expressions, in milliseconds. The
resolution is limited to guile's @code{internal-time-units-per-second}.
Disregards the expressions' return value(s) (FIXME)."
  (let ((x (gensym)))
    `(let ((,x (get-internal-run-time)))
       ,expr
       (format #t "~A ms\n" (* 1000 (/ (- (get-internal-run-time) ,x)
                                       internal-time-units-per-second)))
       ,@(if (null? others) '() `((time ,@others))))))

;;; arch-tag: ff9cb210-9d1b-4ad4-aa5d-27a23edc91f2
