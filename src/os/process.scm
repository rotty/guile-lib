;; (os process): process chains
;; Copyright (C) 1997, 2000, 2001  Free Software Foundation, Inc.
;; Written by Gary Houston <ghouston@arglist.com>, originally as "goosh.scm".

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
@cindex Goosh module
@cindex process, Operating System
@cindex process chain
@cindex pipeline, process

This is a library for execution of other programs from Guile.  It
also allows communication using pipes (or a pseudo terminal device, but
that's not currently implemented).  This code originates in the 
@code{(goosh)} modules, which itself was part of goonix in one of
Guile's past lives.

The following will hold when starting programs:

@enumerate
@item If the name of the program does not contain a @code{/} then the
directories listed in the current @code{PATH} environment variable are
searched to locate the program.
@item Unlike for the corresponding primitive exec procedures, e.g.,
@code{execlp}, the name of the program can not be set independently of
the path to execute: the zeroth and first members of the argument
vector are combined into one.
@end enumerate

All symbols exported with the prefix @code{os:process:} are there in support 
of macros that use them.  They should be ignored by users of this module.
;;; Code:
!#

(define-module (os process))

(export tail-call-program run run-concurrently run-with-pipe)
(export-syntax run-concurrently+ run+ tail-call-pipeline+
	       tail-call-pipeline)

;; these are exported because they appear in code generated by
;; macros.
(export os:process:pipe-make-redir-commands os:process:pipe-make-commands
	os:process:setup-redirected-port os:process:new-comm-pipes)
(export-syntax os:process:pipe-fork-child)

;; setup file descriptors 0, 1, 2 from the current Scheme ports, if
;; possible.  if some of these ports can not be used, open new
;; descriptors on /dev/null.

(define (stdports->stdio)

  ;; select the three file descriptors to be used as
  ;; standard descriptors 0, 1, 2 for the new process.

  (let* ((ensure-fdes (lambda (port mode)
			(or (false-if-exception (fileno port))
			    (open-fdes *null-device* mode))))

	 (input-fdes (ensure-fdes (current-input-port) O_RDONLY))
	 (output-fdes (ensure-fdes (current-output-port) O_WRONLY))
	 (error-fdes (ensure-fdes (current-error-port) O_WRONLY)))

    ;; copy the three selected descriptors to the standard
    ;; descriptors 0, 1, 2.  note that it's possible that
    ;; any of output-fdes, input-fdes and error-fdes are equal.

    (cond ((not (= input-fdes 0))
	   (if (= output-fdes 0)
	       (set! output-fdes (dup->fdes 0)))
	   (if (= error-fdes 0)
	       (set! error-fdes (dup->fdes 0)))
	   (dup2 input-fdes 0)))

    (cond ((not (= output-fdes 1))
	   (if (= error-fdes 1)
	       (set! error-fdes (dup->fdes 1)))
	   (dup2 output-fdes 1)))

    (dup2 error-fdes 2)))

(define (tail-call-program prog . args)
"Replace the current process image by executing @var{prog} with the
supplied list of arguments, @var{args}.

This procedure will reset the signal handlers and attempt to set up file
descriptors as follows:

@enumerate
@item File descriptor 0 is set from (current-input-port).
@item File descriptor 1 is set from (current-output-port).
@item File descriptor 2 is set from (current-error-port).
@end enumerate

If a port can not be used (e.g., because it's closed or it's a string
port) then the file descriptor is opened on the file specified by
@code{*null-device*} instead.

Note that this procedure does not close any ports or flush output
buffers.  Successfully executing @var{prog} will prevent the normal
flushing of buffers that occurs when Guile terminates.  Doing otherwise
would be incorrect after forking a child process, since the buffers
would be flushed in both parent and child.

Examples:
@example
 (tail-call-program \"cat\" \"/etc/passwd\")
@end example
@example
 (with-input-from-file \"/etc/passwd\"
  (lambda ()
    (tail-call-program \"cat\")))
@end example"
  (set-batch-mode?! #t)
  (stdports->stdio)
  (apply execlp (cons prog (cons prog args))))

;;; create a pipe with the writing end unbuffered.  the reading end doesn't
;;; matter, making it unbuffered would just slow things down.
(define (unbuffered-pipe)
  (let ((result (pipe)))
    (setvbuf (cdr result) _IONBF)
    result))

(defmacro-public run-concurrently+ (proc . connections)
  (let ((pid (gensym))
	(ports (gensym)))
    `(let ((,pid (primitive-fork))
	   (,ports (list)))
       (cond ((= ,pid 0)
	      ;; child
	      (set-batch-mode?! #t)
	      ,@(os:process:pipe-make-redir-commands connections ports)
	      ,proc
	      (primitive-exit 1))
	     (else
	      ,pid)))))

(set-object-property! run-concurrently+ 'documentation
"Evaluate an expression in a new background process.  If no connection
terms are specified, then all ports except @code{current-input-port},
@code{current-output-port} and  @code{current-error-port} will be
closed in the new process.  The file descriptors
underlying these ports will not be changed.

The value returned in the parent is the pid of the new process.

When the process terminates its exit status can be collected
using the @code{waitpid} procedure.

Keywords can be specified before the connection list:

@code{#:slave} causes the new process to be put into a new session.
If @code{current-input-port} (after redirections) is a tty it will
be assigned as the controlling terminal.  This option is used when
controlling a process via a pty.

@code{#:no-auto-close} prevents the usual closing of ports which
occurs by default.

@code{#:foreground} makes the new process the foreground job of the
controlling terminal, if the current process is using job control.
 (not currently implemented).
The default is to place it into the background

The optional connection list can take several forms:

@code{(port)} usually specifies that a given port not be closed.
However if @code{#:no-auto-close} is present it specifies instead
a port which should be closed.

@code{(port 0)}
specifies that a port be moved to a given file descriptor
 (e.g., 0) in the new process.  The order of the two components
is not significant,
but one must be a number and the other must evaluate to a port.
If the file descriptor is one of the standard set @code{(0, 1, 2)}
then the corresponding standard port (e.g., @code{current-input-port})
will be set to
the specified port.

Example:
@example
 (let ((p (open-input-file \"/etc/passwd\")))
   (run-concurrently+ (tail-call-program \"cat\") (p 0)))
@end example")

;;; generate the code needed to set up redirections for a child process.
(define (os:process:pipe-make-redir-commands connections portvar)
  (let next-conn ((conns connections)
		  (insert (list))         ;; result
		  (slave #f)
		  (no-auto-close #f))
    (cond ((null? conns)
	   (cond (slave
		  (next-conn conns
			     (append insert
				     (list
				      ;; make a new session, drop old ctty.
				      '(setsid)
				      ;; get a new ctty if possible.
				      '(cond ((isatty? (current-input-port))
					      ;; opening the tty should make
					      ;; it the ctty, now we are the
					      ;; session leader.
					      (let ((name
						     (ttyname 
						      (current-input-port)))
						    (mode 
							(port-mode
							 (current-input-port))))
						(close-port
						 (current-input-port))
						(set-current-input-port
						 (open-file name mode))))
					     ;; try this too -- required
					     ;; under BSD?.
					     ;(%set-ctty (current-input-port))
					     )))
			     #f
			     no-auto-close))
		 (no-auto-close
		  (append insert
			  (list
			   `(map (lambda (p)
				   (false-if-exception
				    (close-fdes (fileno p))))
				 ,portvar))))
		 (else
		  (append insert
			  (list
			   `(let loop ((pts (append
					     (list
					      (current-input-port)
					      (current-output-port)
					      (current-error-port))
					     ,portvar)) ; keep open.
				       (fds (list))) ; fdes keep open.
			      (if (null? pts)
				  (port-for-each (lambda (p)
						   (let ((f
							  (false-if-exception
							   (fileno p))))
						     (if (and f
							      (not
							       (memv f fds)))
							 (false-if-exception
							  (close-fdes f))))))
				  (loop (cdr pts)
					(let ((fd (false-if-exception 
						   (fileno (car pts)))))
					  (if fd
					      (cons fd fds)
					      fds))))))))))
	  (else
	   (let* ((c (car conns)))
	     (cond ((eq? c #:slave)
		    (next-conn (cdr conns)
			       insert
			       #t
			       no-auto-close))
		   ((eq? c #:no-auto-close)
		    (next-conn (cdr conns)
			       insert
			       slave
			       #t))
		   ((eq? c #:foreground) ; would be processed earlier.
		    (next-conn (cdr conns)
			       insert
			       slave
			       no-auto-close))
		   ((= (length c) 1)
		    (next-conn
		     (cdr conns)
		     (cons
		      `(set! ,portvar (cons ,(car c) ,portvar))
		      insert)
		     slave
		     no-auto-close))
		   (else
		    (let* ((reversed (number? (cadr c)))
			   (in (if reversed
				   (cadr c)
				   (car c)))
			   (out (if reversed
				    (car c)
				    (cadr c))))
		      (next-conn (cdr conns)
				 (append
				  (os:process:pipe-make-commands
				   in out portvar)
				  insert)
				 slave
				 no-auto-close)))))))))


;;; returns the commands for redirecting a single port in the child.
(define (os:process:pipe-make-commands fdes port portvar)
  (if (= fdes 0)
      `((let ((newport (os:process:setup-redirected-port ,port ,fdes)))
	  (set-current-input-port newport)))
      (if (= fdes 1)
	  `((let ((newport (os:process:setup-redirected-port ,port ,fdes)))
	      (set-current-output-port newport)))
	  (if (= fdes 2)
	      `((let ((newport (os:process:setup-redirected-port ,port ,fdes)))
		  (set-current-error-port newport)))
	      `((let ((newport (os:process:setup-redirected-port ,port ,fdes)))
		  (set! ,portvar (cons newport ,portvar))))))))

;;; safely redirect a port to a file descriptor.  it must usually be
;;; duplicated, in case it's redirected more than once.
(define (os:process:setup-redirected-port port fdes)
  (if (= (fileno port) fdes)
      port
      (let ((newport (duplicate-port port (port-mode port))))
	(primitive-move->fdes newport fdes)
	newport)))

(defmacro run+ (expr . connections)
  `(cdr (waitpid (run-concurrently+ ,expr #:foreground ,@connections))))

(set-object-property! run+ 'documentation
"Evaluate an expression in a new foreground process and wait for its
completion.  If no connection terms are specified, then all ports except
@code{current-input-port}, @code{current-output-port} and
@code{current-error-port} will be closed in the new process.
The file descriptors underlying these ports will not be changed.

The value returned is the exit status from the new process as returned
by the @code{waitpid} procedure.

The @var{keywords} and @var{connections} arguments are optional: see
@code{run-concurrently+}, which is documented below.
The @code{#:foreground} keyword is implied.

@example
 (run+ (begin (write (+ 2 2)) (newline) (quit 0)))
@end example
@example
 (run+ (tail-call-program \"cat\" \"/etc/passwd\"))
@end example")

(define (run prog . args)
"Execute @var{prog} in a new foreground process
and wait for its completion.  The value returned is the exit status 
of the new process as returned by the @code{waitpid} procedure.

Example:
@example
 (run \"cat\" \"/etc/passwd\")
@end example"
  (run+ (apply tail-call-program prog args)))

(define (run-concurrently . args)
"Start a program running in a new background process.  The value returned
is the pid of the new process.

When the process terminates its exit status can be collected
using the @code{waitpid} procedure.

Example:
@example
 (run-concurrently \"cat\" \"/etc/passwd\")
@end example"
  (run-concurrently+ (apply tail-call-program args)))

(define (run-with-pipe mode prog . args)
"Start @var{prog} running in a new background process.
The value returned is a pair: the CAR is the pid of the new process
and the CDR is either a port or a pair of ports (with the CAR containing
the input port and the CDR the output port).  The port(s) can
be used to read from the standard output of the process
and/or write to its standard input, depending on the @var{mode}
setting.  The value of @var{mode} should be one of \"r\", \"w\" or \"r+\".

When the process terminates its exit status can be collected using the
@code{waitpid} procedure.

Example:
@example
 (use-modules (ice-9 rdelim)) ; needed by read-line
 (define catport (cdr (run-with-pipe \"r\" \"cat\" \"/etc/passwd\")))
 (read-line catport)
@end example"
  (cond ((string=? mode OPEN_READ)
	 (let* ((upipe (unbuffered-pipe))
		(pid (run-concurrently+ (apply tail-call-program prog args)
					(1 (cdr upipe)))))
	   (close-port (cdr upipe))
	   (cons pid (car upipe))))
	((string=? mode OPEN_WRITE)
	 (let* ((upipe (unbuffered-pipe))
		(pid (run-concurrently+ (apply tail-call-program prog args)
					(0 (car upipe)))))
	   (close-port (car upipe))
	   (cons pid (cdr upipe))))
	((string=? mode OPEN_BOTH)
	 (let* ((upipe-r (unbuffered-pipe))
		(upipe-w (unbuffered-pipe))
		(pid (run-concurrently+ (apply tail-call-program prog args)
					(0 (car upipe-w))
					(1 (cdr upipe-r)))))
	   (close-port (car upipe-w))
	   (close-port (cdr upipe-r))
	   (cons pid (cons (car upipe-r) (cdr upipe-w)))))
	(else
	 (error "bad mode string: " mode))))
	
(defmacro tail-call-pipeline+ args
  (let* ((pipes (gensym))
	 (split-comps (pipe-split-components args))
	 (expressions (car split-comps))
	 (connections (cdr split-comps))
	 (pids (gensym)))
    `(let ((,pipes (cons (list) (list)))
	   (,pids (list)))
       ,@(let loop ((rem-exps expressions)
		    (rem-conns connections)
		    (insert (list)))
	   (cond ((null? rem-exps)
		  insert)
		 (else
		  (loop (cdr rem-exps)
			(cdr rem-conns)
			(append
			 insert
			 `(;; update the pipes used by this child.
			   (set! ,pipes (os:process:new-comm-pipes
					 ,pipes
					 ',(cadr rem-conns)))
			   ;; start one child process.
			   (set! ,pids (cons
					(os:process:pipe-fork-child ,(car rem-exps)
							 ,(car rem-conns)
							 ,(cadr rem-conns)
							 ,pipes)
					,pids))
			   ;; close used pipes in the parent.
			   (map (lambda (pipe-list)
				  (map close-port pipe-list))
				(car ,pipes))))))))
       ;; wait for all the processes to terminate and quit with the
       ;; exit status from the one at the tail of the pipe.
       ;; could save memory by exec'ing a tiny program to do the waiting.
       (set-batch-mode?! #t)
       (let next-pid ((waiting-for (length ,pids))
		      (result 0))
	 (cond ((> waiting-for 0)
		(let* ((report (waitpid WAIT_ANY))
		       (pid (car report))
		       ;; if normal termination return the exit status,
		       ;; otherwise 128 + the signal number.
		       (status (let ((exit-val (status:exit-val (cdr report)))
				     (term-sig (status:term-sig (cdr report))))
				 (or exit-val (+ term-sig 128)))))
		  (cond ((member pid ,pids)
			 ;; the pid list is reversed.
			 (if (= pid (car ,pids))
			     (next-pid (- waiting-for 1) status)
			     (next-pid (- waiting-for 1) result)))
			(else
			 (next-pid waiting-for result)))))
	       (else
		(primitive-exit result)))))))

(set-object-property! tail-call-pipeline+ 'documentation
"Replace the current process image with a pipeline of connected processes.

Each process is specified by an expression and each pair of processes
has a connection list with pairs of file descriptors.  E.g.,
@code{((1 0) (2 0))} specifies that file descriptors 1 and 2 are to be
connected to file descriptor 0.  This may also be written
as @code{((1 2 0))}.

The expressions in the pipeline are run in new background processes.
The foreground process waits for them all to terminate.  The exit
status is derived from the status of the process at the tail of the
pipeline: its exit status if it terminates normally, otherwise 128
plus the number of the signal that caused it to terminate.

The signal handlers will be reset and file descriptors set up as for
@code{tail-call-program}.  Like @code{tail-call-program} it does not
close open ports or flush buffers.

Example:
@example
 (tail-call-pipeline+ (tail-call-program \"ls\" \"/etc\") ((1 0))
                      (tail-call-program \"grep\" \"passwd\"))
@end example")

;;; create pipes for communication: RHS connection list for a process.
;;; the previous set of pipes gets recycled to the LHS.
(define (os:process:new-comm-pipes old-pipes out-conns)
  (cons (cdr old-pipes)
	(map (lambda (conn)
	       (let ((rw-pair (unbuffered-pipe)))
		 (let next-dup ((new-pipes (list (cdr rw-pair) (car rw-pair)))
				(count (- (length conn) 2)))
		   (if (= count 0)
		       (reverse new-pipes)
		       (next-dup (cons (duplicate-port (car new-pipes) "w0")
				       new-pipes)
				 (- count 1))))))
	     out-conns)))

;;; fork a single child process, given redirections and pipes.
(defmacro os:process:pipe-fork-child (expr in-conns out-conns pipes)
  `(run-concurrently+
    ,expr #:no-auto-close
    ,@(append (let iloop ((count (- (length in-conns) 1))
			  (redirs (list)))
		(if (< count 0)
		    redirs
		    (iloop (- count 1)
			   (append
			    (let ((this-conn (list-ref in-conns count)))
			      ;; may be several ports to close (dups).
			      (let next-line ((dcount
					       (- (length this-conn) 2))
					      (lines (list)))
				(if (< dcount 0)
				    (append
				     lines
				     ;; redirect (port fdes).
				     `(((car (list-ref (car ,pipes) ,count))
					,(car (reverse this-conn))))
				     redirs)
				    (next-line
				     (- dcount 1)
				     (cons
				      ;; close the other pipe ends.
				      `((list-ref (list-ref (car ,pipes)
							    ,count)
						  ,(+ dcount 1)))
				      lines)))))))))
	      (let oloop ((count (- (length out-conns) 1))
			  (redirs (list)))
		(if (< count 0)
		    redirs
		    (oloop (- count 1)
			   ;; may need several redirections (dups).
			   (let ((this-conn (list-ref out-conns count)))
			     (let next-line ((dcount
					      (- (length this-conn) 2))
					     (lines (list)))
			       (if (< dcount 0)
				   (append lines
					   ;; close the other pipe ends.
					   `(((car
					       (list-ref (cdr ,pipes)
							 ,count))))
					   redirs)
				   (next-line
				    (- dcount 1)
				    (cons
				     ;; redirect (port fdes).
				     `((list-ref
					(list-ref (cdr ,pipes) ,count)
					,(+ dcount 1))
				       ,(list-ref this-conn dcount))
				     lines)))))))))))

;;; split a pipe into a process list and a connection list.
(define (pipe-split-components ppe)
  (let loop ((remaining ppe)
	     (do-expr? #t)  ; track alternating process / connection.
	     (exprs (list))
	     (connections (list)))
    (cond ((null? remaining)
	   (cons (reverse exprs)
		 ;; the null lists represent input and output from the pipe
		 ;; ends.
		 (cons (list) (reverse (cons (list) connections)))))
	  (do-expr? (loop (cdr remaining)
			  #f
			  (cons (car remaining) exprs)
			  connections))
	  (else (loop (cdr remaining)
		      #t
		      exprs
		      (cons (remove-dup-connections! (car remaining))
			    connections))))))

;;; convert connection spec like ((1 0)(2 0)) into ((1 2 0)).
;;; returns the mutated connection spec.
(define (remove-dup-connections! connections)
  (let ((r-connections (map reverse connections)))
    (let next-left ((left r-connections))
      (if (or (null? left) (null? (cdr left)))
	  (map reverse r-connections)
	  (let next-right ((right-1 left))
	    (let ((right (cdr right-1)))
	      (if (null? right)
		  (next-left (cdr left))
		  (cond ((= (caar left) (caar right))
			 (set-car! left (append (car left) (cdar right)))
			 (set-cdr! right-1 (cdr right))
			 (next-right right-1))
			(else
			 (next-right (cdr right-1)))))))))))

(defmacro tail-call-pipeline args
  `(tail-call-pipeline+
    ,@(let next-arg ((rem args)
		     (result (list)))
	(cond ((null? rem)
	       (reverse result))
	      (else
	       (next-arg (cdr rem)
			 (let ((temp (cons `(tail-call-program
					     ,@(car rem))
					   result)))
			   (if (null? (cdr rem))
			       temp
			       (cons '((1 0)) temp)))))))))

(set-object-property! tail-call-pipeline 'documentation
"Replace the current process image with a pipeline of connected processes.

The expressions in the pipeline are run in new background processes.
The foreground process waits for them all to terminate.  The exit
status is derived from the status of the process at the tail of the
pipeline: its exit status if it terminates normally, otherwise 128
plus the number of the signal that caused it to terminate.

The signal handlers will be reset and file descriptors set up as for
@code{tail-call-program}.  Like @code{tail-call-program} it does not
close open ports or flush buffers.

Example:
@example
 (tail-call-pipeline (\"ls\" \"/etc\") (\"grep\" \"passwd\"))
@end example")

; try debugging a macro through a fork some day...
;(false-if-exception (delete-file "/tmp/goosh-debug"))
;(define-public (debug arg)
;  (let ((p (open-file "/tmp/goosh-debug" "a")))
;    (write arg p)
;    (newline p)
;    (close-port p)))

;;; arch-tag: 74b1df36-abe4-4b5e-b40d-025ec64a9f8a
