;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001 Rob Browning <rlb at defaultvalue dot org>

;; What's the license on this?

;;; Commentary:
;;
;; ...
;;
;;; Code:

;; When you add new features, please also add tests to ./tests/ if you
;; have time, and then add the new files to ./run-tests.  Also, if
;; anyone's bored, there are a lot of existing API bits that don't
;; have tests yet.

;; TODO
;;
;; Check why tail calls aren't apply-frame counted (or are they).
;; Calculate leaf + parents.
;; make sure frames we're skipping in PROF handler are OK.

(define-module (debugging statprof)
  #:export (statprof-active?
            statprof-start
            statprof-stop
            statprof-reset

            statprof-accumulated-time
            statprof-sample-count
            statprof-fold-call-data
            statprof-proc-call-data
            statprof-call-data-name
            statprof-call-data-calls
            statprof-call-data-cum-samples
            statprof-call-data-self-samples
            statprof-call-data->stats
           
            statprof-stats-proc-name
            statprof-stats-%-time-in-proc
            statprof-stats-cum-secs-in-proc
            statprof-stats-self-secs-in-proc
            statprof-stats-calls
            statprof-stats-self-secs-per-call
            statprof-stats-cum-secs-per-call

            statprof-display

            with-statprof))

;;(use-modules (ice-9 format))
(use-modules (ice-9 slib))
(require 'stdio)

;; This profiler tracks two numbers for every function called while
;; it's active.  It tracks the total number of calls, and the number
;; of times the function was active when the sampler fired.
;;
;; Globally the profiler tracks the total time elapsed and the number
;; of times the sampler was fired.
;;
;; Right now, this profiler is not per-thread and is not thread safe.

(define accumulated-time #f)            ; total so far.
(define last-start-time #f)             ; start-time when timer is active.
(define sample-count #f)                ; total count of sampler calls.
(define sampling-frequency #f)          ; in (seconds . microseconds)
(define remaining-prof-time #f)         ; time remaining when prof suspended.
(define profile-level 0)                ; for user start/stop nesting.
(define %count-calls? #t)               ; whether to catch apply-frame.
(define gc-time-taken 0)                ; gc time between statprof-start and
                                        ; statprof-stop.

;; procedure-data will be a hash where the key is the function object
;; itself and the value is the data. The data will be a vector like
;; this: #(name call-count cum-sample-count self-sample-count)
(define procedure-data #f)

;; If you change the call-data data structure, you need to also change
;; sample-uncount-frame.
(define (make-call-data name call-count cum-sample-count self-sample-count)
  (vector (or name (error "internal error (we don't count anonymous procs)"))
          call-count cum-sample-count self-sample-count))
(define (call-data-name cd) (vector-ref cd 0))
(define (call-data-call-count cd) (vector-ref cd 1))
(define (call-data-cum-sample-count cd) (vector-ref cd 2))
(define (call-data-self-sample-count cd) (vector-ref cd 3))

(define (set-call-data-name! cd name)
  (vector-set! cd 0 name))
(define (inc-call-data-call-count! cd)
  (vector-set! cd 1 (1+ (vector-ref cd 1))))
(define (inc-call-data-cum-sample-count! cd)
  (vector-set! cd 2 (1+ (vector-ref cd 2))))
(define (inc-call-data-self-sample-count! cd)
  (vector-set! cd 3 (1+ (vector-ref cd 3))))

(define-macro (accumulate-time stop-time)
  `(set! accumulated-time (+ accumulated-time (- ,stop-time last-start-time))))

(define (get-call-data proc)
  (or (hashq-ref procedure-data proc)
      (let ((call-data (make-call-data (procedure-name proc) 0 0 0)))
        (hashq-set! procedure-data proc call-data)
        call-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIGPROF handler

(define (sample-stack-procs stack)
  (let ((stacklen (stack-length stack))
        (hit-count-call? #f)
        ;; we need to skip the 5 profile frames: %deliver-signals,
        ;; profile-signal-handler, if, let*, and make-stack.
        (profiling-frames 5))

    (if (< profiling-frames stacklen)
        (begin
          ;; We've got at least one non-profiling frame
          (set! sample-count (+ sample-count 1))
          ;; Now accumulate stats for the whole stack.
          (let loop ((frame (stack-ref stack profiling-frames))
                     (procs-seen (make-hash-table 13))
                     (self #f))
            (cond
             ((not frame)
              (hash-fold
               (lambda (proc val accum)
                 (inc-call-data-cum-sample-count!
                  (get-call-data proc)))
               #f
               procs-seen)
              (and=> (and=> self get-call-data)
                     inc-call-data-self-sample-count!))
             ((frame-procedure frame)
              => (lambda (proc)
                   (cond
                    ((eq? proc count-call)
                     ;; We're not supposed to be sampling count-call and
                     ;; its sub-functions, so loop again with a clean
                     ;; slate.
                     (set! hit-count-call? #t)
                     (loop (frame-previous frame) (make-hash-table 13) #f))
                    ((procedure-name proc)
                     (hashq-set! procs-seen proc #t)
                     (loop (frame-previous frame)
                           procs-seen
                           (or self proc)))
                    (else
                     (loop (frame-previous frame) procs-seen self)))))
             (else
              (loop (frame-previous frame) procs-seen self))))))

    hit-count-call?))

(define inside-profiler? #f)

(define (profile-signal-handler sig)
  (set! inside-profiler? #t)

  ;; FIXME: with-statprof should be able to set an outer frame for the
  ;; stack cut
  (if (positive? profile-level)
      (let* ((stop-time (get-internal-run-time))
             (stack (make-stack #t))
             (inside-apply-trap? (sample-stack-procs stack)))

        (if (not inside-apply-trap?)
            (begin
              ;; disabling here is just a little more efficient, but
              ;; not necessary given inside-profiler?.  We can't just
              ;; disable unconditionally at the top of this function
              ;; and eliminate inside-profiler? because it seems to
              ;; confuse guile wrt re-enabling the trap when
              ;; count-call finishes.
              (if %count-calls? (trap-disable 'apply-frame))
              (accumulate-time stop-time)))
        
        (setitimer ITIMER_PROF
                   0 0
                   (car sampling-frequency)
                   (cdr sampling-frequency))
        
        (if (not inside-apply-trap?)
            (begin
              (set! last-start-time (get-internal-run-time))
              (if %count-calls? (trap-enable 'apply-frame))))))

  (set! inside-profiler? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Count total calls.

(define (count-call trap-name continuation tail)
  (if (not inside-profiler?)
      (begin
        (accumulate-time (get-internal-run-time))

        (and=> (frame-procedure (last-stack-frame continuation))
               (lambda (proc)
                 (if (procedure-name proc)
                     (inc-call-data-call-count!
                      (get-call-data proc)))))
        
        (set! last-start-time (get-internal-run-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (statprof-active?) (positive? profile-level))

;; Do not call this from statprof internal functions -- user only.
(define (statprof-start)
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (+ profile-level 1))
  (if (= profile-level 1)
      (let* ((rpt remaining-prof-time)
             (use-rpt? (and rpt
                            (or (positive? (car rpt))
                                (positive? (cdr rpt))))))
        (set! remaining-prof-time #f)
        (set! last-start-time (get-internal-run-time))
        (set! gc-time-taken
              (cdr (assq 'gc-time-taken (gc-stats))))
        (if use-rpt?
            (setitimer ITIMER_PROF 0 0 (car rpt) (cdr rpt))
            (setitimer ITIMER_PROF
                       0 0
                       (car sampling-frequency)
                       (cdr sampling-frequency)))
        (trap-enable 'apply-frame)
        #t)))
  
;; Do not call this from statprof internal functions -- user only.
(define (statprof-stop)
  ;; After some head-scratching, I don't *think* I need to mask/unmask
  ;; signals here, but if I'm wrong, please let me know.
  (set! profile-level (- profile-level 1))
  (if (zero? profile-level)
      (begin
        (set! gc-time-taken
              (- (cdr (assq 'gc-time-taken (gc-stats))) gc-time-taken))
        (trap-disable 'apply-frame)
        ;; I believe that we need to do this before getting the time
        ;; (unless we want to make things even more complicated).
        (set! remaining-prof-time (setitimer ITIMER_PROF 0 0 0 0))
        (accumulate-time (get-internal-run-time))
        (set! last-start-time #f))))

(define (statprof-reset sample-seconds sample-microseconds count-calls?)
  (if (positive? profile-level)
      (error "Can't reset profiler while profiler is running."))
  (set! %count-calls? count-calls?)
  (set! accumulated-time 0)
  (set! last-start-time #f)
  (set! sample-count 0)
  (set! sampling-frequency (cons sample-seconds sample-microseconds))
  (set! remaining-prof-time #f)
  (set! procedure-data (make-hash-table 131))
  (if %count-calls?
      (begin
        (trap-set! apply-frame-handler count-call)
        (trap-enable 'traps)))
  (debug-enable 'debug)
  (sigaction SIGPROF profile-signal-handler)
  #t)

(define (statprof-fold-called proc init)
  ;; proc should take two args (call-data prior-result).  Note that a
  ;; given proc-name may appear multiple times, but if it does, it
  ;; represents different functions with the same name.
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  (hash-fold
   (lambda (key value prior-result)
     (proc value prior-result))
   init
   procedure-data))

(define (statprof-proc-call-data proc)
  (if (positive? profile-level)
      (error "Can't call statprof-fold-called while profiler is running."))

  ;; func should take one arg, call-data.  Note that a
  ;; given proc-name may appear multiple times, but if it does, it
  ;; represents different functions with the same name.
  (hashq-ref procedure-data proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stats

(define (statprof-call-data->stats call-data)
  ;; returns (vector proc-name
  ;;                 %-time-in-proc
  ;;                 cum-seconds-in-proc
  ;;                 self-seconds-in-proc
  ;;                 num-calls
  ;;                 self-secs-per-call
  ;;                 total-secs-per-call)

  (let* ((proc-name (call-data-name call-data))
         (self-samples (call-data-self-sample-count call-data))
         (cum-samples (call-data-cum-sample-count call-data))
         (all-samples (statprof-sample-count))
         (secs-per-sample (/ (statprof-accumulated-time)
                             (statprof-sample-count)))
         (num-calls (and %count-calls? (statprof-call-data-calls call-data))))

    (vector proc-name
            (* (/ self-samples all-samples) 100)
            (* cum-samples secs-per-sample)
            (* self-samples secs-per-sample)
            num-calls
            (and num-calls ;; maybe we only sampled in children
                 (if (zero? self-samples) 0.0
                     (/ (* self-samples secs-per-sample) num-calls)))
            (and num-calls ;; cum-samples must be positive
                 (/ (* cum-samples secs-per-sample) num-calls)))))

(define (statprof-stats-proc-name stats) (vector-ref stats 0))
(define (statprof-stats-%-time-in-proc stats) (vector-ref stats 1))
(define (statprof-stats-cum-secs-in-proc stats) (vector-ref stats 2))
(define (statprof-stats-self-secs-in-proc stats) (vector-ref stats 3))
(define (statprof-stats-calls stats) (vector-ref stats 4))
(define (statprof-stats-self-secs-per-call stats) (vector-ref stats 5))
(define (statprof-stats-cum-secs-per-call stats) (vector-ref stats 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stats-sorter x y)
  (let ((diff (- (statprof-stats-self-secs-in-proc x)
                 (statprof-stats-self-secs-in-proc y))))
    (positive?
     (if (= diff 0)
         (- (statprof-stats-cum-secs-in-proc x)
            (statprof-stats-cum-secs-in-proc y))
         diff))))

(define (statprof-display . port)
  
  (if (null? port) (set! port (current-output-port)))
  
  (cond
   ((zero? (statprof-sample-count))
    (format port "No samples recorded.\n"))
   (else
    (let* ((stats-list (statprof-fold-called
                        (lambda (data prior-value)
                          (cons (statprof-call-data->stats data)
                                prior-value))
                        '()))
           (sorted-stats (sort stats-list stats-sorter)))

      (define (display-stats-line stats)
        (if %count-calls?
            (fprintf port "%6.2f %9.2f %9.2f %8lu %8.2f %8.2f  "
                     (statprof-stats-%-time-in-proc stats)
                     (statprof-stats-cum-secs-in-proc stats)
                     (statprof-stats-self-secs-in-proc stats)
                     (statprof-stats-calls stats)
                     (* 1000 (statprof-stats-self-secs-per-call stats))
                     (* 1000 (statprof-stats-cum-secs-per-call stats)))
            (fprintf port "%6.2f %9.2f %9.2f  "
                     (statprof-stats-%-time-in-proc stats)
                     (statprof-stats-cum-secs-in-proc stats)
                     (statprof-stats-self-secs-in-proc stats)))
        (display (statprof-stats-proc-name stats) port)
        (newline port))
    
      (if %count-calls?
          (begin
            (fprintf port "%5.5s %10.10s   %7.7s %8.8s %8.8s %8.8s  %-8.8s\n"
                     "%  " "cumulative" "self" "" "self" "total" "")
            (fprintf port "%5.5s  %9.9s  %8.8s %8.8s %8.8s %8.8s  %-8.8s\n"
                     "time" "seconds" "seconds" "calls" "ms/call" "ms/call" "name"))
          (begin
            (fprintf port "%5.5s %10.10s   %7.7s  %-8.8s\n"
                     "%  " "cumulative" "self" "")
            (fprintf port "%5.5s  %9.9s  %8.8s  %-8.8s\n"
                     "time" "seconds" "seconds" "name")))

      (for-each display-stats-line sorted-stats)

      (display "---\n" port)
      (simple-format #t "Sample count: ~A\n" (statprof-sample-count))
      (simple-format #t "Total time: ~A seconds (~A seconds in GC)\n"
                     (statprof-accumulated-time)
                     (/ gc-time-taken internal-time-units-per-second))))))

(define (statprof-display-anomolies)
  (statprof-fold-called
   (lambda (data prior-value)
     (if (and %count-calls?
              (zero? (call-data-call-count data))
              (positive? (call-data-sample-count data)))
         (simple-format #t
                        "==[~A ~A ~A]\n"
                        (call-data-name data)
                        (call-data-call-count data)
                        (call-data-sample-count data))))
   #f)
  (simple-format #t "Total time: ~A\n" (statprof-accumulated-time))
  (simple-format #t "Sample count: ~A\n" (statprof-sample-count)))

(export statprof-display-anomolies)

(define (statprof-accumulated-time)
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  (/ accumulated-time internal-time-units-per-second))

(define (statprof-sample-count)
  (if (positive? profile-level)
      (error "Can't get accumulated time while profiler is running."))
  sample-count)

(define statprof-call-data-name call-data-name)
(define statprof-call-data-calls call-data-call-count)
(define statprof-call-data-cum-samples call-data-cum-sample-count)
(define statprof-call-data-self-samples call-data-self-sample-count)

;; Profiles the expressions in its body.
;;
;; Keyword arguments:
;;   #:loop
;;      execute the body LOOP number of times, or #f for no looping
;;      default: #f
;;   #:hz
;;      sampling rate
;;      default: 20
;;   #:count-calls?
;;      whether to instrument each function call (expensive)
;;      default: #f
(define-macro (with-statprof . args)
  (define (kw-arg-ref kw args def)
    (cond
     ((null? args) (error "Invalid macro body"))
     ((keyword? (car args))
      (if (eq? (car args) kw)
          (cadr args)
          (kw-arg-ref kw (cddr args) def)))
     ((eq? kw #f def) ;; asking for the body
      args)
     (else def))) ;; kw not found
  (let ((loop (kw-arg-ref #:loop args #f))
        (hz (kw-arg-ref #:hz args 20))
        (count-calls? (kw-arg-ref #:count-calls? args #f))
        (body (kw-arg-ref #f args #f)))
    `(begin
       (,statprof-reset ,(inexact->exact (floor (/ 1 hz)))
                        ,(inexact->exact (* 1e6 (- (/ 1 hz)
                                                   (floor (/ 1 hz)))))
                        ,count-calls?)
       (,dynamic-wind
           ,statprof-start
           (lambda ()
             ,(if loop
                  (let ((lp (gensym "statprof ")) (x (gensym)))
                    `(let ,lp ((,x ,loop))
                          (if (,not (,zero? ,x))
                              (begin ,@body (,lp (,1- ,x))))))
                  `(begin ,@body)))
           ,statprof-stop)
       (,statprof-display)
       (,(lambda () (set! procedure-data #f))))))

;;; arch-tag: 83969178-b576-4c52-a31c-6a9c2be85d10
