;; Soundscrape
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
;;Defines some macros to help in documenting variables, generic
;;functions, and classes.
;;
;;; Code:

(define-module (scheme documentation)
  :export (define-with-docs define-generic-with-docs define-class-with-docs))

(define-macro (define-with-docs sym docs val)
  `(begin
     (define ,sym ,val)
     (set-object-property! ,sym 'documentation ,docs)
     *unspecified*))
(set-object-property! define-with-docs 'documentation
  "Define a variable with documentation.")

(define-macro (define-generic-with-docs name documentation)
  `(define-with-docs ,name ,documentation
     (make-generic ',name)))
(set-object-property! define-generic-with-docs 'documentation
  "Define a generic function with documentation.")

(define-macro (define-class-with-docs name supers docs . slots)
  `(begin
     (define-class ,name ,supers ,@slots)
     (set-object-property! ,name 'documentation ,docs)
     (if #f #f)))
(set-object-property! define-class-with-docs 'documentation
  "Define a class with documentation.")

;;; arch-tag: f5297a2f-bb0a-4d42-8b3b-eb712199d9a0
