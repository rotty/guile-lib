;;; ----------------------------------------------------------------------
;;;    port-log  -- a log that writes to a given port
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

(define-module (logging port-log)
  #:use-module (oop goops)
  #:use-module (logging logger)
  #:export (<port-log>))

(define-class <port-log> (<log-handler>)
  (port  #:init-value #f   #:accessor port #:init-keyword #:port))

(define-method (emit-log (self <port-log>) str)
  (if (port self)
      (display str (port self))))

(define-method (flush-log (self <port-log>))
  (if (port self) 
      (force-output (port self))))

(define-method (close-log! (self <port-log>))
  (if (port self)
      (close-port (port self)))
  (set! (port self) #f))


;;; arch-tag: 41c3aa9e-3a3f-41a0-be3c-a28995989634
