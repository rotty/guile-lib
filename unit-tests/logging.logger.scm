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

(use-modules (unit-test guileUnit)
             (logging logger)
             (logging port-log)
             (oop goops))

(define-class <test-logging> (<test-case>))

(define-method (test-log-to-one-port (self <test-logging>))
  (let* ((strport (open-output-string))
         (lgr     (make <logger> #:handlers (list (make <port-log> #:port strport)))))
    (open-log! lgr)
    (log-msg lgr 'CRITICAL "Hello!")
    (assert-equal "(CRITICAL): Hello!\n"
                  ;; skip over the time/date, since that will vary!
                  (substring (get-output-string strport) 20))))

(define-method  (test-log-to-default-logger (self <test-logging>))
  (let* ((strport (open-output-string))
         (lgr     (make <logger> #:handlers (list (make <port-log> #:port strport)))))
    (open-log! lgr)
    (set-default-logger! lgr)
    (log-msg 'CRITICAL "Hello!")
    (set-default-logger! #f)
    (assert-equal "(CRITICAL): Hello!\n"
                  ;; skip over the time/date, since that will vary!
                  (substring (get-output-string strport) 20))))

(define-method  (test-log-to-registered-logger (self <test-logging>))
  (let* ((strport (open-output-string))
         (lgr     (make <logger> #:handlers (list (make <port-log> #:port strport)))))
    (register-logger! "main" lgr)
    (log-msg (lookup-logger "main") 'CRITICAL "Hello!")
    (assert-equal "(CRITICAL): Hello!\n"
                  ;; skip over the time/date, since that will vary!
                  (substring (get-output-string strport) 20))))
  
(exit-with-summary (run-all-defined-test-cases))

;;; arch-tag: 551ce47d-fa94-4f0e-be29-b9287f574b41
