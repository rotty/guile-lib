;;; ----------------------------------------------------------------------
;;;    logger.scm -- write methods to log files...
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

(define-module (logging logger)
  #:export (
            ;; handler exports...
            <log-handler>
            emit-log
            accept-log

            ;; logger exports...
            <logger>
            add-handler!
            log-msg

            ;; module-level methods...
            set-default-logger!
            register-logger!
            lookup-logger

            ;; these work on loggers and handlers...
            enable-log-level!
            disable-log-level!
            flush-log
            open-log!
            close-log!
            )
  #:use-module (oop goops))

;;; ----------------------------------------------------------------------
(define default-logger #f)
(define all-loggers (make-hash-table 7))

(define (set-default-logger! lgr)
  (cond ((string? lgr)
         (set! default-logger (hash-ref all-loggers lgr)))
        ((is-a? lgr <logger>) (set! default-logger lgr))
        ((not lgr) (set! default-logger #f))
        (else (throw 'bad-type "expected a string, #f, or a <logger>"))))

(define (register-logger! str lgr)
  (if (not (string? str))
      (throw 'bad-type "Expected a string for the log registration"))      
  (hash-set! all-loggers str lgr))

(define (lookup-logger str)
  (if (not (string? str))
      (throw 'bad-type "Expected a string for the logger lookup"))      
  (hash-ref all-loggers str))

(define-class <logger> ()
  (levels #:init-form (make-hash-table 17) #:getter levels)
  (log-handlers  #:init-value '() #:accessor handlers #:init-keyword #:handlers))

(define (log-helper lgr level objs)
  ;; the level must be enabled in the logger to proceed...
  (if (level-enabled? lgr level)
      (let ((cur-time (current-time)))
        (for-each (lambda (str)                    
                    (if (not (string-null? str))

                        ;; pass the string to each log handler for lgr
                        (for-each (lambda (handler)
                                    (accept-log handler level cur-time str))
                                  (handlers lgr))))

                  ;; split the string at newlines into different log statements
                  (string-split 
                   (with-output-to-string (lambda () (for-each (lambda (o) (display o)) objs)))
                   #\nl)))))

(define-method (log-msg (lvl <symbol>) . objs)
  (if default-logger
      (log-helper default-logger lvl objs)))

(define-method (log-msg (lgr <logger>) lvl . objs)
  (log-helper lgr lvl objs))

;; if called with no args, pass to the default logger...
(define-method (flush-log)
  (if default-logger
      (flush-log default-logger)))

;; if called on a logger, pass the call to all the handlers...
(define-method (flush-log (lgr <logger>))
  (for-each (lambda (handler)
              (flush-log handler))
            (handlers lgr)))

(define-method (flush-log!)
  (if default-logger
      (flush-log! default-logger)))

(define-method (open-log! (lgr <logger>))
  (for-each (lambda (handler)
              (open-log! handler))
            (handlers lgr)))

(define-method (open-log!)
  (if default-logger
      (open-log! default-logger)))

(define-method (close-log! (lgr <logger>))
  (for-each (lambda (handler)
              (close-log! handler))
            (handlers lgr)))

(define-method (close-log!)
  (if default-logger
      (close-log! default-logger)))

;; the default formatter makes a log statement like:
;; 2003/12/29 14:53:02 (CRITICAL): The servers are melting!
(define (default-log-formatter lvl time str)
  (with-output-to-string
    (lambda ()
      (display (strftime "%F %H:%M:%S" (localtime time)))
      (display " (")
      (display (symbol->string lvl))
      (display "): ")
      (display str)
      (newline))))

(define-class <log-handler> ()
  (formatter #:init-value default-log-formatter #:getter log-formatter #:init-keyword #:formatter)
  (levels #:init-form (make-hash-table 17) #:getter levels))

(define-method (add-handler! (lgr <logger>) (handler <log-handler>))
  (set! (handlers lgr)
        (cons handler (handlers lgr))))

;; This can be overridden by log handlers if this default behaviour
;; is not desired..
(define-method (accept-log (self <log-handler>) level time str)
  (if (level-enabled? self level)
      (emit-log self ((log-formatter self) level time str))))

;; This should be overridden by all log handlers to actually 
;; write out a string.
(define-generic emit-log)

;; provide do-nothing open for handlers that don't care about it
(define-method (open-log! (lh <log-handler>))
  #t)
;; provide do-nothing close for handlers that don't care about it
(define-method (close-log! (lh <log-handler>))
  #t)
;; provide do-nothing flush for handlers that don't care about it
(define-method (flush-log (lh <log-handler>))
  #t)

;; ----------------------------------------------------------------------
;; These functions work on both <logger> and <log-handler>.
;; I could make them methods, but the contents would just be duplicated
;; Making them methods would allow people to make subclasses that altered
;; the log level behavior, I guess...
;; ----------------------------------------------------------------------
(define (enable-log-level! lgr lvl)
  (hashq-set! (levels lgr) lvl #t))

(define (disable-log-level! lgr lvl)
  (hashq-set! (levels lgr) lvl #f))
  
(define (level-enabled? lgr lvl)
  ;; defaults to #t so that if you misspell the log level you get your log
  (hashq-ref (levels lgr) lvl #t))

;;; arch-tag: b90591f5-553e-4967-8f6e-83ab9a727a35
