;;; ----------------------------------------------------------------------
;;;    rotating-log  -- a log that writes to a certain length, then 
;;;                      moves the files to xxx.2, xxx.3, etc.
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

(define-module (logging rotating-log)
  #:use-module (oop goops)
  #:use-module (logging logger)
  #:export (<rotating-log>))

(define-class <rotating-log> (<log-handler>)
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
