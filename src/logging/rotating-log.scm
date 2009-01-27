;; (logging rotating-log) -- a log that writes to rotating files
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
@cindex logs, rotating

This module defines a log handler for text logs that rotate when they
get to be a user-defined size.  This is similar to the behavior of
many UNIX standard log files.  @xref{logging logger}, for more
information in general on log handlers.

;;; Code:
!#

(define-module (logging rotating-log)
  #:use-module (oop goops)
  #:use-module (logging logger)
  #:use-module (scheme documentation)
  #:export (<rotating-log>))

(define-class-with-docs <rotating-log> (<log-handler>)
"This is a log handler which writes text logs that rotate when they reach
a configurable size limit.

Keywords recognized by @code{<rotating-log>} on creation are:
@table @code
@item #:num-files
This is the number of log files you want the logger to use.  Default is 4.

@item #:size-limit
This is the size, in bytes, a log file must get before the logs get
rotated.  Default is 1MB (104876 bytes).

@item #:file-name
This is the base of the log file name.   Default is ``logfile''.  Numbers will
be appended to the file name representing the log number.  The newest log
file is always ``@var{NAME}.1''.

@item #:formatter
Allows the user to provide a function to use as the log formatter for
this handler.  @xref{logging logger <log-handler>}, for details.
@end table

Example of creating a @code{<rotating-log>}:
@lisp
 (make <rotating-log>
      #:num-files 3
      #:size-limit 1024
      #:file-name \"test-log-file\"))
@end lisp"
  (num-files #:init-value 4 #:getter num-files #:init-keyword #:num-files)
  (size-limit #:init-value 1048576 #:getter size-limit #:init-keyword #:size-limit)
  (file-name  #:init-value "logfile" #:getter file-name #:init-keyword #:file-name)
  (port       #:init-value #f        #:accessor port)
  (fpos       #:accessor fpos))

(define-method (log-file-name (self <rotating-log>) num)
  (string-append (file-name self) "." (number->string num)))

(define-method (open-log! (self <rotating-log>))
  (set! (port self) (open-file (log-file-name self 1) "a"))
  (set! (fpos self) (ftell (port self))))

(define-method (close-log! (self <rotating-log>))
  (if (port self)
      (close-port (port self)))
  (set! (port self) #f))

(define-method (rotate-log (self <rotating-log>))
  ;; close the existing log...
  (close-log! self)

  ;; loop through the files, renaming .2 to .3, .1 to .2, etc...
  (let loop ((num (- (num-files self) 1)))
    (if (<= num 0)
        #t
        (begin
          (if (access? (log-file-name self num) F_OK)
              (rename-file (log-file-name self num)
                           (log-file-name self (+ num 1))))
          (loop (- num 1)))))

  ;; now open up a new xx.1 file...
  (open-log! self))

(define-method (emit-log (self <rotating-log>) str)
  (if (port self)
      (begin 
        (display str (port self))
        (set! (fpos self) (+ (string-length str)
                             (fpos self)))
        (if (> (fpos self)
               (size-limit self))
            (rotate-log self)))))

(define-method (flush-log (self <rotating-log>))
  (if (port self) 
      (force-output (port self))))

;;; arch-tag: 2e4d28d0-2c71-4ea5-b46d-572e1aa94a22
