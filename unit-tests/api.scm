;;; ----------------------------------------------------------------------
;;;    unit test
;;;    Copyright (C) 2007 Andy Wingo
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
(use-modules (unit-test)
             (apicheck)
             (oop goops))

(define-class <test-api> (<test-case>))

(load (getenv "DOC_GUILE_LIBRARY_SCM"))

(define-method (test-api (self <test-api>))
  (apicheck-validate
   (call-with-input-file (getenv "API_FILE") read)
   (map car *modules*)))

(exit-with-summary (run-all-defined-test-cases))
