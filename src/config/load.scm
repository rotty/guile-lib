;;; ----------------------------------------------------------------------
;;;    load.scm -- code to load configuration files
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
;;;    You should have received a copy of the GNU General Public
;;;    License along with this program; if not, write to the Free
;;;    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;;;    MA 02111-1307 USA
;;; ----------------------------------------------------------------------
;;;   Copyright (C) 2003-2004 Andreas Rottmann
;;; ----------------------------------------------------------------------
(define-module (config load)
  #:use-module (oop goops)
  #:use-module (ice-9 safe)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  
  #:export (<configuration>
            load-config!
            
            &config-error 
            config-error-arguments))

(define-class <configuration> ())

(define-condition-type &config-error &error
  config-error?
  (key config-error-key)
  (arguments config-error-arguments))

(define-method (write (self &config-error) port)
  (display "#<" port)
  (display (class-name (class-of self)) port)
  (display #\space port)
  (display (config-error-key self) port)
  (display #\space port)
  (display (config-error-arguments self) port)
  (display ">" port))

(define-method (cfg-include (cfg <configuration>) (file-name <string>) env)
  (let ((old-dir (getcwd)))
    (dynamic-wind
        (lambda () (chdir (dirname file-name)))
        (lambda ()
          (do-load-config cfg (open-input-file (basename file-name)) env))
        (lambda () (chdir old-dir)))))

(define (do-load-config cfg port env)
    ;; Read one expression a time.
    (let lp ((expr (read port)))
      ;; End of file? -> Return.
      (if (eof-object? expr)
	  #t
	  (catch #t
	    (lambda ()
	      ;; Evaluate the expression in the safe environment.
	      (eval expr env)
	      ;; ... and read the next expression if no error occured.
	      (lp (read port)))

	    ;; Handle exceptions.  This procedure will be called when
	    ;; an error occurs while evaluating the expression.  If a
	    ;; condition was signalled, it is re-raised, otherwise a
	    ;; &config-error condition is raised with the original
	    ;; exception key and arguments.
	    ;;
	    (lambda (key . args)
              (if (and (= (length args) 1) (condition? (car args)))
                  (raise (car args))
                  (raise (condition (&config-error (key key)
                                                   (arguments args))))))))))

(define (bind-first proc . bind-args)
  (lambda args
    (apply proc (append bind-args args))))

(define (bind-last proc . bind-args)
  (lambda args
    (apply proc (append args bind-args))))

(define-method (load-config! (cfg <configuration>)
                             (commands <list>)
                             (file-name <string>))
  (let ((config-env (safe-environment 5)))
    
    (for-each (lambda (entry)
                (module-define! config-env
                                (first entry) (bind-first (second entry) cfg)))
              (cons `(include ,(bind-last cfg-include config-env)) commands))
    
    (cfg-include cfg file-name config-env)))


;;; arch-tag: 09ffbf0f-b7e1-4280-a21f-178226487634
