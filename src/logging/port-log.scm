;; (logging port-log) -- a log that writes to a given port
;; Copyright (C) 2003  Richard Todd

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

#!
;;; Commentary:
@cindex logs, through ports
@cindex ports, for logging

This module defines a log handler that writes to an arbitrary port of
the user's choice.  Uses of this handler could include:
@itemize @bullet
@item  
Sending logs across a socket to a network log collector.
@item
Sending logs to the screen
@item
Sending logs to a file
@item
Collecting logs in memory in a string port for later use
@end itemize
;;; Code:
!#
(define-module (logging port-log)
  #:use-module (oop goops)
  #:use-module (logging logger)
  #:use-module (scheme documentation)
  #:export (<port-log>))

(define-class-with-docs <port-log> (<log-handler>)
"This is a log handler which writes logs to a user-provided port.

Keywords recognized by @code{<port-log>} on creation are:
@table @code
@item #:port
This is the port to which the log handler will write.

@item #:formatter
Allows the user to provide a function to use as the log formatter for
this handler.  @xref{logging logger <log-handler>}, for details.
@end table

Example of creating a @code{<port-log>}:
@lisp
 (make <port-log> #:port (current-error-port))
@end lisp"
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
