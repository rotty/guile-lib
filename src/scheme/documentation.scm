;; (scheme documentation) -- self-documentation helper macros
;; Copyright (C) 2003,2004  Andy Wingo <wingo at pobox dot com>

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
;;@c texinfo commentary
;;Defines some macros to help in documenting macros, variables, generic
;;functions, and classes.
;;
;;; Code:

(define-module (scheme documentation)
  #:export (define-macro-with-docs define-with-docs
            define-generic-with-docs define-class-with-docs))

(define-macro (define-macro-with-docs name-and-args docs . body)
  `(begin
     (define-macro ,name-and-args ,@body)
     (set-object-property! ,(car name-and-args) 'documentation ,docs)
     (if #f #f)))
(set-object-property! define-macro-with-docs 'documentation
  "Define a macro with documentation.")

(define-macro-with-docs (define-with-docs sym docs val)
  "Define a variable with documentation."
  `(begin
     (define ,sym ,val)
     (set-object-property! ,sym 'documentation ,docs)
     *unspecified*))

(define-macro-with-docs (define-generic-with-docs name documentation)
  "Define a generic function with documentation."
  `(define-with-docs ,name ,documentation
     (make-generic ',name)))

(define-macro-with-docs (define-class-with-docs name supers docs . slots)
  "Define a class with documentation."
  `(begin
     (define-class ,name ,supers ,@slots)
     (set-object-property! ,name 'documentation ,docs)
     (if #f #f)))

;;; arch-tag: f5297a2f-bb0a-4d42-8b3b-eb712199d9a0
