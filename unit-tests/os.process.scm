;;; ----------------------------------------------------------------------
;;;    unit test
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
(use-modules (oop goops)
             (unit-test)
             (os process))

(define-class <test-proc> (<test-case>))

(define-method (test-exit-code (self <test-proc>))
  ;; several ways to run a program and get its exit code.
  (assert-equal 21
                (status:exit-val (run+ (tail-call-program "guile" "-c" "(exit 21)"))))
  (assert-equal 22
                (status:exit-val (run "guile" "-c" "(exit 22)")))
  (assert-equal 23
                (status:exit-val 
                 (cdr 
                  (waitpid 
                   (run-concurrently "guile" "-c" "(exit 23)")))))
  (assert-equal 24
                (status:exit-val 
                 (cdr 
                  (waitpid 
                   (run-concurrently+ 
                    (tail-call-program "guile" "-c" "(exit 24)")))))))


(define-method (test-pipeline (self <test-proc>))
  (assert-equal 25
                (status:exit-val 
                 (run+ (tail-call-pipeline ("echo" "25") 
                                           ("guile" "-c" "(use-modules (ice-9 rdelim)) (exit (string->number (read-line)))"))))))

(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: c77c3aab-d791-4838-a36a-f234daf63331
