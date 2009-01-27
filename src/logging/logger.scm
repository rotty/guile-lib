;; (logging logger) -- write methods to log files
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
@cindex logging
@cindex loggers, relationship with handlers
@cindex handlers, relationship with loggers
@cindex log levels

This is a logging subsystem similar to the one in the python standard
library.  There are two main concepts to understand when working with 
the logging modules.  These are loggers and log handlers.

@table @asis
@item Loggers
Loggers are the front end interfaces for program logging.
They can be registered by name so that no part of a program
needs to be concerned with passing around loggers.  In 
addition, a default logger can be designated so that, for
most applications, the program does not need to be concerned
with logger instances at all beyond the initial setup.

Log messages all flow through a logger.  Messages carry with them a
level (for example: 'WARNING, 'ERROR, 'CRITICAL), and loggers can
filter out messages on a level basis at runtime.  This way, the amount
of logging can be turned up during development and bug investigation,
but turned back down on stable releases.

Loggers depend on Log Handlers to actually get text to the log's
destination (for example, a disk file).  A single Logger can send
messages through multiple Log Handlers, effectively multicasting logs
to multiple destinations.

@item Log Handlers
Log Handlers actually route text to a destination.  One or more handlers
must be attached to a logger for any text to actually appear in a log.

Handlers apply a configurable transformation to the text so that it is
formatted properly for the destination (for instance: syslogs, or a
text file).  Like the loggers, they can filter out messages based on
log levels.  By using filters on both the Logger and the Handlers,
precise controls can be put on which log messages go where, even
within a single logger.
@end table

@section Example use of logger

Here is an example program that sets up a logger with two handlers.  One
handler sends the log messages to a text log that rotates its logs.  The
other handler sends logs to standard error, and has its levels set so that
INFO and WARN-level logs don't get through.

@lisp
(use-modules (logging logger)
             (logging rotating-log)
             (logging port-log)
             (scheme documentation)
             (oop goops))

;; ----------------------------------------------------------------------
;; Support functions
;; ----------------------------------------------------------------------
(define (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 3
                     #:size-limit 1024
                     #:file-name "test-log-file"))
        (err       (make <port-log> #:port (current-error-port))))

    ;; don't want to see warnings or info on the screen!!
    (disable-log-level! err 'WARN)
    (disable-log-level! err 'INFO)
    
    ;; add the handlers to our logger
    (add-handler! lgr rotating)
    (add-handler! lgr err)
    
    ;; make this the application's default logger
    (set-default-logger! lgr)
    (open-log! lgr)))


(define (shutdown-logging)
  (flush-log)   ;; since no args, it uses the default
  (close-log!)  ;; since no args, it uses the default
  (set-default-logger! #f))

;; ----------------------------------------------------------------------
;; Main code
;; ----------------------------------------------------------------------
(setup-logging)

;; Due to log levels, this will get to file, 
;; but not to stderr
(log-msg 'WARN "This is a warning.")

;; This will get to file AND stderr
(log-msg 'CRITICAL "ERROR message!!!")

(shutdown-logging)

@end lisp
;;; Code:
!#

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
  #:use-module (oop goops)
  #:use-module (scheme documentation))

;;; ----------------------------------------------------------------------
(define default-logger #f)
(define all-loggers (make-hash-table 7))

(define (set-default-logger! lgr)
"Sets the given logger, @var{lgr}, as the default for logging methods where
a logger is not given.  @var{lgr} can be an instance of @code{<logger>},
a string that has been registered via @code{register-logger!}, or @code{#f}
to remove the default logger.

With this mechanism, most applications will never need to worry about
logger registration or lookup.

@lisp
;; example 1
 (set-default-logger! \"main\")  ;; look up \"main\" logger and make it the default

;; example 2
 (define lgr (make  <logger>))
 (add-handler! lgr 
              (make <port-handler>
                    #:port (current-error-port)))
 (set-default-logger! lgr)
 (log-msg 'CRITICAL \"This is a message to the default logger!!!\")
 (log-msg lgr 'CRITICAL \"This is a message to a specific logger!!!\")
@end lisp"
  (cond ((string? lgr)
         (set! default-logger (hash-ref all-loggers lgr)))
        ((is-a? lgr <logger>) (set! default-logger lgr))
        ((not lgr) (set! default-logger #f))
        (else (throw 'bad-type "expected a string, #f, or a <logger>"))))

(define (register-logger! str lgr)
"Makes @var{lgr} accessible from other parts of the program by a name
given in @var{str}.  @var{str} should be a string, and @var{lgr}
should be an instance of class @code{<logger>}.
@lisp 
 (define main-log  (make <logger>))
 (define corba-log (make <logger>))
 (register-logger! \"main\" main-log)
 (register-logger! \"corba\" corba-log)

;; in a completely different part of the program....
 (log-msg (lookup-logger \"corba\") 'WARNING \"This is a corba warning.\")
@end lisp"
  (if (not (string? str))
      (throw 'bad-type "Expected a string for the log registration"))      
  (hash-set! all-loggers str lgr))

(define (lookup-logger str)
 "Looks up an instance of class @code{<logger>} by the name given
in @var{str}.  The string should have already been registered via
a call to @code{register-logger!}." 
 (if (not (string? str))
      (throw 'bad-type "Expected a string for the logger lookup"))      
  (hash-ref all-loggers str))

(define-class-with-docs <logger> ()
"This is the class that aggregates and manages log handlers.  It also
maintains the global information about which levels of log messages 
are enabled, and which have been suppressed.  Keyword arguments accepted
on creation are:

@table @code
@item #:handlers
This optional parameter must be a list of objects derived from @code{<log-handler>}.
Handlers can always be added later via @code{add-handler!} calls.  
@end table"
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

(define-generic-with-docs log-msg
"@code{log-msg [lgr] lvl arg1 arg2 ...}.  Send a log message
made up of the @code{display}'ed representation of the given
arguments.  The log is generated at level @var{lvl}, which should
be a symbol.  If the @var{lvl} is disabled, the log message is
not generated.  Generated log messages are sent through each of
@var{lgr}'s handlers.

If the @var{lgr} parameter is omitted, then the default logger
is used, if one is set.

As the args are @code{display}'ed, a large string is built up.  Then,
the string is split at newlines and sent through the log handlers as
independent log messages.  The reason for this behavior is to make 
output nicer for log handlers that prepend information like pid and
timestamps to log statements.

@lisp
;; logging to default logger, level of WARN
 (log-msg 'WARN \"Warning! \" x \" is bigger than \" y \"!!!\")

;; looking up a logger and logging to it
 (let ((l (lookup-logger \"main\")))
     (log-msg l 'CRITICAL \"FAILURE TO COMMUNICATE!\")
     (log-msg l 'CRITICAL \"ABORTING NOW\"))
@end lisp")

(define-method (log-msg (lvl <symbol>) . objs)
  (if default-logger
      (log-helper default-logger lvl objs)))

(define-method (log-msg (lgr <logger>) lvl . objs)
  (log-helper lgr lvl objs))

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

(define-class-with-docs <log-handler> ()
"This is the base class for all of the log handlers, and encompasses
the basic functionality that all handlers are expected to have.
Keyword arguments recognized by the @code{<log-handler>} at creation
time are:

@table @code
@item #:formatter
This optional parameter must be a function that takes three arguments:
the log level, the time (as from @code{current-time}), and the log string
itself.  The function must return a string representing the formatted log.

Here is an example invokation of the default formatter, and what it's
output looks like:
@lisp
 (default-log-formatter 'CRITICAL 
                       (current-time) 
                       \"The servers are melting!\")
==> \"2003/12/29 14:53:02 (CRITICAL): The servers are melting!\"
@end lisp
@end table"
  (formatter #:init-value default-log-formatter #:getter log-formatter #:init-keyword #:formatter)
  (levels #:init-form (make-hash-table 17) #:getter levels))

(define-generic-with-docs add-handler! 
  "@code{add-handler! lgr handler}.  Adds @var{handler} to @var{lgr}'s list of handlers.  All subsequent
logs will be sent through the new handler, as well as any previously
registered handlers.")

(define-method (add-handler! (lgr <logger>) (handler <log-handler>))
  (set! (handlers lgr)
        (cons handler (handlers lgr))))

(define-generic-with-docs accept-log
"@code{accept-log handler lvl time str}.  If @var{lvl} is
enabled for @var{handler}, then @var{str} will be formatted and
sent to the log via the @code{emit-log} method.  Formatting is
done via the formatting function given at @var{handler}'s
creation time, or by the default if none was given.

This method should not normally need to be overridden by subclasses.
This method should not normally be called by users of the logging 
system.  It is only exported so that writers of log handlers can
override this behavior.")

;; This can be overridden by log handlers if this default behaviour
;; is not desired..
(define-method (accept-log (self <log-handler>) level time str)
  (if (level-enabled? self level)
      (emit-log self ((log-formatter self) level time str))))

;; This should be overridden by all log handlers to actually 
;; write out a string.
(define-generic-with-docs emit-log
"@code{emit-log handler str}.  This method should be implemented
for all the handlers.  This sends a string to their output media.
All level checking and formatting has already been done by
@code{accept-log}.")

(define-generic-with-docs open-log!
"@code{open-log! handler}.  Tells the @code{handler} to open its log.  Handlers for which
an open operation doesn't make sense can choose not to implement this method.
The default implementation just returns @code{#t}.")
;; provide do-nothing open for handlers that don't care about it
(define-method (open-log! (lh <log-handler>))
  #t)

(define-generic-with-docs close-log!
"@code{open-log! handler}.  Tells the @code{handler} to close its
log.  Handlers for which a close operation doesn't make sense can
choose not to implement this method.  The default implementation
just returns @code{#t}.")
;; provide do-nothing close for handlers that don't care about it
(define-method (close-log! (lh <log-handler>))
  #t)

(define-generic-with-docs flush-log
"@code{flush-log handler}.  Tells the @code{handler} to output
any log statements it may have buffered up.  Handlers for which a
flush operation doesn't make sense can choose not to implement
this method.  The default implementation just returns
@code{#t}.")
;; provide do-nothing flush for handlers that don't care about it
(define-method (flush-log (lh <log-handler>))
  #t)

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

;; ----------------------------------------------------------------------
;; These functions work on both <logger> and <log-handler>.
;; I could make them methods, but the contents would just be duplicated
;; Making them methods would allow people to make subclasses that altered
;; the log level behavior, I guess...
;; ----------------------------------------------------------------------
(define (enable-log-level! lgr lvl)
"Enables a specific logging level given by the symbol @var{lvl},
such that messages at that level will be sent to the log
handlers.  @var{lgr} can be of type @code{<logger>} or
@code{<log-handler>}.

Note that any levels that are neither enabled or disabled are treated
as enabled by the logging system.  This is so that misspelt level
names do not cause a logging blackout."
  (hashq-set! (levels lgr) lvl #t))

(define (disable-log-level! lgr lvl)
"Disables a specific logging level, such that messages at that
level will not be sent to the log handlers.  @var{lgr} can be of
type @code{<logger>} or @code{<log-handler>}.

Note that any levels that are neither enabled or disabled are treated
as enabled by the logging system.  This is so that misspelt level
names do not cause a logging blackout."
  (hashq-set! (levels lgr) lvl #f))
  
(define (level-enabled? lgr lvl)
  ;; defaults to #t so that if you misspell the log level you get your log
  (hashq-ref (levels lgr) lvl #t))

;;; arch-tag: b90591f5-553e-4967-8f6e-83ab9a727a35
