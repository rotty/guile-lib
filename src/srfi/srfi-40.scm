(define-module (srfi srfi-40)
  #:use-module (ice-9 syncase)
  #:use-module (ice-9 rdelim)
  #:use-module (oop goops)

  #:export
  (<stream> stream-null make-stream stream-cons stream list->stream
   vector->stream string->stream stream-of stream-from stream-from-to
   stream-repeat stream-iterate
   
   stream? stream-null? stream-pair?
   
   stream-car stream-cdr stream-car+cdr stream-last stream-butlast
   stream-caar stream-ref
   stream-take stream-take-until stream-take-while
   stream-drop stream-drop-until stream-drop-while
   stream-split stream-split-until stream-split-while
   
   stream-define stream-map stream-for-each stream-filter stream-remove
   stream-partition
   stream-fold-left stream-fold-left-one
   stream-fold-right stream-fold-right-one
   stream-scan-left stream-scan-left-one 
   stream-scan-right stream-scan-right-one
   
   port->stream port->char-stream port->line-stream port->word-stream
   stream-display stream-display-lines stream-write
   
   stream->list stream->vector stream->string stream-append stream-concat
   stream-reverse stream-zip stream-unzip stream-alternate  stream-interleave
   stream-merge stream-sort stream-uniq

   ;; helpers
   stream-of-tail
   ))

;;; STREAM -- library of syntax and functions to manipulate streams

;;; A stream is a new data type, disjoint from all other data types, that
;;; contains a promise that, when forced, is either nil (a single object
;;; distinguishable from all other objects) or consists of an object (the
;;; stream element) followed by a stream.  Each stream element is evaluated
;;; exactly once, when it is first retrieved (not when it is created); once
;;; evaluated its value is saved to be returned by subsequent retrievals
;;; without being evaluated again.

;; STREAM-TYPE -- type of streams
;; MAKE-STREAM obj -- convert object to type of streams
;; STREAM-PROMISE stream -- extract promise from stream
;; STREAM? object -- #t if object is a stream, #f otherwise
(define-class <stream> ()
  (promise #:getter stream-promise #:init-keyword #:promise))

(define (make-stream promise)
  (make <stream> #:promise promise))

(define (stream? obj)
  (is-a? obj <stream>))

;; STREAM-NULL -- the distinguished null stream
(define stream-null (make-stream (delay '())))

;; STREAM-NULL? object -- #t if object is the nil stream, #f otherwise
(define (stream-null? obj)
    (and (stream? obj)
         (null? (force (stream-promise obj)))))

;; STREAM-CONS object stream -- the primitive stream constructor
(define-syntax stream-cons
    (syntax-rules ()
        ((stream-cons obj strm)
            (make-stream (delay (cons obj strm))))))

;; STREAM-PAIR? object -- #t if object is a non-null stream, #f otherwise
(define (stream-pair? obj)
    (and (stream? obj)
         (not (null? (force (stream-promise obj))))))

;; STREAM-CAR stream -- first element of stream
(define (stream-car strm)
    (if (not (stream? strm))
        (error "attempt to take car of non-stream"))
    (if (stream-null? strm)
        (error "attempt to take car of null stream"))
    (car (force (stream-promise strm))))

;; STREAM-CDR stream -- stream containing all elements in stream except first
(define (stream-cdr strm)
    (if (not (stream? strm))
        (error "attempt to take cdr of non-stream"))
    (if (stream-null? strm)
        (error "attempt to take cdr of null stream"))
    (cdr (force (stream-promise strm))))

;; STREAM-DEFINE (name args ...) body ... -- define stream-valued function
;; STREAM-DEFINE (name args ... . rest) body ... -- define stream-valued func
(define-syntax stream-define
    (syntax-rules ()
        ((stream-define name "next" (args ...) (arg . rest) body0 body1 ...)
            (stream-define name "next" (args ... arg) rest body0 body1 ...))
        ((stream-define name "next" (args ...) () body0 body1 ...)
            (stream-define name "done" (args ...) body0 body1 ...))
        ((stream-define name "next" (args ...) rest body0 body1 ...)
            (stream-define name "rest" (args ...) rest body0 body1 ...))
        ((stream-define name "done" (args ...) body0 body1 ...)
            (define (name args ...)
                (make-stream (delay (force (stream-promise (begin body0 body1 ...)))))))
        ((stream-define name "rest" (args ...) rest body0 body1 ...)
            (define (name args ... . rest)
                (make-stream (delay (force (stream-promise (begin body0 body1 ...)))))))
        ((stream-define (name . rest) body0 body1 ...)
            (stream-define name "next" () rest body0 body1 ...))
        ((stream-define (name args ...) body0 body1 ...)
            (stream-define name "next" () (args ...) body0 body1 ...))))

;; LIST->STREAM list -- new stream containing elements of list
(stream-define (list->stream lst)
    (if (not (list? lst))
        (error "attempt to convert non-list to stream"))
    (let loop ((lst lst))
        (if (null? lst)
            stream-null
            (stream-cons (car lst) (loop (cdr lst))))))

;; VECTOR->STREAM vector -- new stream containing elements of vector
(stream-define (vector->stream vec)
    (if (not (vector? vec))
        (error "attempt to convert non-vector to stream"))
    (list->stream (vector->list vec)))

;; STRING->STREAM string -- new stream containing characters of string
(stream-define (string->stream str)
    (if (not (string? str))
        (error "attempt to convert non-string to stream"))
    (list->stream (string->list str)))

;; STREAM object ... -- new stream whose elements are object ...
(define (stream . objs) (list->stream objs))

;; STREAM-INTERLEAVE stream -- interleave stream of streams "diagonally"
(stream-define (stream-interleave-helper s1 s2)
    (if (stream-null? s1)
        s2
        (stream-cons (stream-car s1)
                     (stream-interleave-helper s2 (stream-cdr s1)))))
(stream-define (stream-interleave strm)
    (cond ((stream-null? strm) stream-null)
          ((stream-null? (stream-car strm))
              (stream-interleave (stream-cdr strm)))
          (else (stream-cons
                    (stream-caar strm)
                    (stream-interleave-helper
                        (stream-interleave (stream-cdr strm))
                        (stream-cdar strm))))))

;; STREAM-OF expr qualifier ... -- stream comprehension syntax for infinite streams
(define-syntax stream-of
    (syntax-rules ()
        ((stream-of expr ...)
            (let* ((signal (list 'signal))
                   (strm (stream-of-tail signal expr ...)))
                (stream-remove (lambda (x) (eq? x signal)) strm)))))
(define-syntax stream-of-tail
    (syntax-rules (in list vector string port is)
        ((stream-of-tail signal expr (var in generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f generator))))
        ((stream-of-tail signal expr (var in list generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f (list->stream generator)))))
        ((stream-of-tail signal expr (var in vector generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f (vector->stream generator)))))
        ((stream-of-tail signal expr (var in string generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f (string->stream generator)))))
        ((stream-of-tail signal expr (var in port reader generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f (port->stream reader generator)))))
        ((stream-of-tail signal expr (var in port generator) clause ...)
            (let ((f (lambda (var) (stream-of-tail signal expr clause ...))))
                (stream-interleave (stream-map f (port->char-stream generator)))))
        ((stream-of-tail signal expr (var is declaration) clause ...)
            (let ((var declaration)) (stream-of-tail signal expr clause ...)))
        ((stream-of-tail signal expr pred? clause ...)
            (if pred?
                (stream-of-tail signal expr clause ...)
                (stream signal)))
        ((stream-of-tail signal expr) (stream expr))))

;; STREAM-FROM start [step] -- new stream of numbers from start [step=1]
(stream-define (stream-from start . step)
    (if (not (number? start))
        (error "non-numeric start"))
    (let ((delta (if (pair? step) (car step) 1)))
        (if (not (number? delta))
            (error "non-numeric step"))
        (stream-cons start (stream-from (+ start delta) delta))))

;; STREAM-FROM-TO start stop [step] -- new stream from start to stop [step=1]
(stream-define (stream-from-to start stop . step)
    (if (not (number? start))
        (error "non-numeric start"))
    (if (not (number? stop))
        (error "non-numeric stop"))
    ;(let ((delta (if (pair? step) (car step) 1)))
    (let ((delta (cond ((pair? step) (car step)) ((< start stop) 1) (else -1))))
        (if (not (number? delta))
            (error "non-numeric step"))
        (if ((if (negative? delta) < >) start stop)
            stream-null
            (stream-cons start (stream-from-to (+ start delta) stop delta)))))

;; STREAM-REPEAT object ... -- infinite stream of object ...
(stream-define (stream-repeat . objs)
    (if (null? objs)
        stream-null
        (stream-cons (car objs)
                     (apply stream-repeat (append (cdr objs)
                                                  (list (car objs)))))))

;; STREAM-ITERATE func obj -- (stream obj (func obj) (func (func obj)) ...)
(stream-define (stream-iterate func obj)
    (if (procedure? func)
        (stream-cons obj (stream-iterate func (func obj)))
        (error "non-functional object passed to iterate")))

;; STREAM-CAR+CDR stream -- (values (stream-car stream) (stream-cdr stream))
(define (stream-car+cdr strm)
    (if (stream-pair? strm)
        (values (stream-car strm) (stream-cdr strm))
        (error "argument to car+cdr must be non-null")))

;; STREAM-LAST stream -- last item in stream, error if empty
(define (stream-last strm)
    (cond ((stream-null? strm)
              (error "can't take last item in null stream"))
          ((stream-null? (stream-cdr strm)) (stream-car strm))
          (else (stream-last (stream-cdr strm)))))

;; STREAM-BUTLAST stream -- stream containing all items in stream except last
(stream-define (stream-butlast strm)
    (cond ((stream-null? strm)
              (error "attempt to take butlast of null stream"))
          ((stream-null? (stream-cdr strm)) stream-null)
          (else (stream-cons (stream-car strm)
                             (stream-butlast (stream-cdr strm))))))

;; STREAM-CAAR to STREAM-CDDDDR -- compositions of stream-car and stream-cdr
(define (stream-caar   strm) (stream-car (stream-car   strm)))
(define (stream-cdar   strm) (stream-cdr (stream-car   strm)))
(define (stream-cadr   strm) (stream-car (stream-cdr   strm)))
(define (stream-cddr   strm) (stream-cdr (stream-cdr   strm)))
(define (stream-caaar  strm) (stream-car (stream-caar  strm)))
(define (stream-cdaar  strm) (stream-cdr (stream-caar  strm)))
(define (stream-cadar  strm) (stream-car (stream-cdar  strm)))
(define (stream-cddar  strm) (stream-cdr (stream-cdar  strm)))
(define (stream-caadr  strm) (stream-car (stream-cadr  strm)))
(define (stream-cdadr  strm) (stream-cdr (stream-cadr  strm)))
(define (stream-caddr  strm) (stream-car (stream-cddr  strm)))
(define (stream-cdddr  strm) (stream-cdr (stream-cddr  strm)))
(define (stream-caaaar strm) (stream-car (stream-caaar strm)))
(define (stream-cdaaar strm) (stream-cdr (stream-caaar strm)))
(define (stream-cadaar strm) (stream-car (stream-cdaar strm)))
(define (stream-cddaar strm) (stream-cdr (stream-cdaar strm)))
(define (stream-caadar strm) (stream-car (stream-cadar strm)))
(define (stream-cdadar strm) (stream-cdr (stream-cadar strm)))
(define (stream-caddar strm) (stream-car (stream-cddar strm)))
(define (stream-cdddar strm) (stream-cdr (stream-cddar strm)))
(define (stream-caaadr strm) (stream-car (stream-caadr strm)))
(define (stream-cdaadr strm) (stream-cdr (stream-caddr strm)))
(define (stream-cadadr strm) (stream-car (stream-cdadr strm)))
(define (stream-cddadr strm) (stream-cdr (stream-cdadr strm)))
(define (stream-caaddr strm) (stream-car (stream-caddr strm)))
(define (stream-cdaddr strm) (stream-cdr (stream-caddr strm)))
(define (stream-cadddr strm) (stream-car (stream-cdddr strm)))
(define (stream-cddddr strm) (stream-cdr (stream-cdddr strm)))

;; STREAM-REF stream n -- nth item in stream, counting from zero
(define (stream-ref strm n)
    (if (not (stream? strm))
        (error "attempt to refer to non-stream"))
    (if (not (integer? n))
        (error "attempt to refer to non-integral stream position"))
    (if (< n 0)
        (error "stream-ref out of bounds"))
    (let loop ((strm strm) (n n))
        (cond ((stream-null? strm) (error "stream-ref out of bounds"))
              ((zero? n) (stream-car strm))
              (else (loop (stream-cdr strm) (- n 1))))))

;; STREAM-FIRST to STREAM-TENTH -- synonym for (stream-ref stream (- nth 1))
(define (stream-first   stream) (stream-ref stream 0))
(define (stream-second  stream) (stream-ref stream 1))
(define (stream-third   stream) (stream-ref stream 2))
(define (stream-fourth  stream) (stream-ref stream 3))
(define (stream-fifth   stream) (stream-ref stream 4))
(define (stream-sixth   stream) (stream-ref stream 5))
(define (stream-seventh stream) (stream-ref stream 6))
(define (stream-eighth  stream) (stream-ref stream 7))
(define (stream-ninth   stream) (stream-ref stream 8))
(define (stream-tenth   stream) (stream-ref stream 9))

;; STREAM-TAKE n stream -- new stream of up to the first n items in stream
(stream-define (stream-take n strm)
    (if (not (integer? n))
        (error "attempt to take non-integral number of elements"))
    (if (< n 0)
        (error "attempt to take negative number of elements"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (if (or (stream-null? strm) (zero? n))
        stream-null
        (stream-cons (stream-car strm)
                     (stream-take (- n 1) (stream-cdr strm)))))

;; STREAM-TAKE-WHILE pred? stream -- stream of stream prefix where pred? is #t
(stream-define (stream-take-while pred? strm)
    (if (not (procedure? pred?))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (let take-while ((s strm) (r stream-null))
        (if (or (stream-null? s) (not (pred? (stream-car s))))
            (stream-reverse r)
            (take-while (stream-cdr s) (stream-cons (stream-car s) r)))))

;; STREAM-TAKE-UNTIL pred? stream -- stream of stream prefix where pred? is #f
(stream-define (stream-take-until pred? strm)
    (if (not (procedure? pred?))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (let take-until ((s strm) (r stream-null))
        (if (or (stream-null? s) (pred? (stream-car s)))
            (stream-reverse r)
            (take-until (stream-cdr s) (stream-cons (stream-car s) r)))))

;; STREAM-DROP n stream -- nth cdr of stream
(stream-define (stream-drop n strm)
    (if (not (integer? n))
        (error "attempt to take non-integral number of elements"))
    (if (< n 0)
        (error "attempt to take negative number of elements"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (if (or (stream-null? strm) (<= n 0))
        strm
        (stream-drop (- n 1) (stream-cdr strm))))

;; STREAM-DROP-WHILE pred? stream -- stream less prefix satisfying pred?
(stream-define (stream-drop-while pred? strm)
    (if (not (procedure? pred?))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (if (or (stream-null? strm) (not (pred? (stream-car strm))))
        strm
        (stream-drop-while pred? (stream-cdr strm))))

;; STREAM-DROP-UNTIL pred? stream -- stream less prefix not satisfying pred?
(stream-define (stream-drop-until pred? strm)
    (if (not (procedure? pred?))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (if (or (stream-null? strm) (pred? (stream-car strm)))
        strm
        (stream-drop-until pred? (stream-cdr strm))))

;; STREAM-SPLIT n stream -- (values (take ...) (drop ...))
(define (stream-split n strm)
    (if (not (integer? n))
        (error "attempt to take non-integral number of elements"))
    (if (< n 0)
        (error "attempt to take negative number of elements"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (let split ((s strm) (n n) (r '()))
        (if (or (stream-null? s) (<= n 0))
            (values (list->stream (reverse r)) s)
            (split (stream-cdr s) (- n 1) (cons (stream-car s) r)))))

;; STREAM-SPLIT-WHILE pred? stream -- values (take-while ...) (drop-while ...)
(define (stream-split-while pred? strm)
    (if (not (procedure? strm))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (let split-while ((s strm) (r '()))
        (if (or (stream-null? s) (not (pred? (stream-car s))))
            (values (list->stream (reverse r)) s)
            (split-while (stream-cdr s) (cons (stream-car s) r)))))

;; STREAM-SPLIT-UNTIL pred? stream -- values (take-until ...) (drop-until ...)
(define (stream-split-until pred? strm)
    (if (not (procedure? strm))
        (error "non-functional predicate"))
    (if (not (stream? strm))
        (error "attempt to take elements from non-stream"))
    (let split-until ((s strm) (r '()))
        (if (or (stream-null? s) (pred? (stream-car s)))
            (values (list->stream (reverse r)) s)
            (split-until (stream-cdr s) (cons (stream-car s) r)))))

;; STREAM-MAP func stream ... -- stream produced by applying func to streams
(stream-define (stream-map func . strms)
    (if (not (all stream? strms))
        (error "stream-map applied to non-stream"))
    (if (or (null? strms) (any stream-null? strms))
        stream-null
        (stream-cons (apply func (map stream-car strms))
                     (apply stream-map (cons func (map stream-cdr strms))))))

;; STREAM-FOR-EACH proc stream ... -- apply proc elementwise on stream ...
(define (stream-for-each proc . strms)
    (if (not (all stream? strms))
        (error "for-each applied to non-stream"))
    (let loop ((strms strms))
        (if (not (any stream-null? strms))
            (begin (apply proc (map stream-car strms))
                   (loop (map stream-cdr strms))))))

;; STREAM-FILTER pred? stream -- new stream including only items passing pred?
(define (stream-filter pred? strm)
  (stream-filter-aux pred? strm #f))
(define (stream-filter-aux pred? strm prom)
  (make-stream
    (delay
      (stream-filter-aux-1 pred? (force (stream-promise strm)) prom))))
(define (stream-filter-aux-1 pred? stuff prom)
  (cond ((null? stuff) '())
        ((pred? (car stuff))
          (cons (car stuff)
                (let ((prom (list #f)))
                  (set-car! prom
                            (stream-filter-aux-2 pred? (cdr stuff) prom))
                  (stream-filter-aux-3 prom))))
        (else
          (let ((more (stream-filter-aux-2 pred? (cdr stuff) prom)))
            (if prom (set-car! prom more))
            (more)))))
(define (stream-filter-aux-2 pred? strm prom)
  (lambda ()
    (stream-filter-aux-1 pred? (force (stream-promise strm)) prom)))
(define (stream-filter-aux-3 prom)
  (make-stream
    (delay
      ((car prom)))))

;; STREAM-REMOVE pred? stream -- stream containing only items with pred? #f
(stream-define (stream-remove pred? strm)
    (stream-filter (lambda (x) (not (pred? x))) strm))

;; STREAM-PARTITION pred? stream -- (values (pred? => #t) (pred? => #f))
(define (stream-partition pred? strm)
    (if (stream-null? strm)
        (values stream-null stream-null)
        (call-with-values
            (lambda () (stream-partition pred? (stream-cdr strm)))
            (lambda (pass fail)
                (let ((obj (stream-car strm)))
                    (if (pred? obj)
                        (values (stream-cons obj pass) fail)
                        (values pass (stream-cons obj fail))))))))

;; STREAM-FOLD-LEFT func base stream ... -- apply func to base and items ...
(define (stream-fold-left func base . strms)
    (cond ((not (procedure? func)) (error "non-functional argument"))
          ((not (all stream? strms)) (error "non-stream argument"))
          ((null? (cdr strms))
              (let loop ((base base) (strm (car strms)))
                  (if (stream-null? strm)
                      base
                      (loop (func base (stream-car strm)) (stream-cdr strm)))))
          (else (let loop ((base base) (strms strms))
                    (if (any stream-null? strms)
                        base
                        (loop (apply func (cons base (map stream-car strms)))
                              (map stream-cdr strms)))))))

;; STREAM-FOLD-LEFT-ONE func stream ... -- apply func pairwise to stream items
(define (stream-fold-left-one func . strms)
    (cond ((null? strms)
              (error "no stream supplied to stream-fold-left-one"))
          ((any stream-null? strms)
              (error "can't apply stream-fold-left-one to null stream"))
          ((null? (cdr strms))
              (let ((strm (car strms)))
                  (stream-fold-left func (stream-car strm) (stream-cdr strm))))
          (else (apply stream-fold-left func
                                        (apply func (map stream-car strms))
                                        (map stream-cdr strms)))))

;; STREAM-FOLD-RIGHT func base stream ... -- apply func pairwise to base and items
(define (stream-fold-right func base . strms)
    (cond ((not (procedure? func)) (error "non-functional argument"))
          ((not (all stream? strms)) (error "non-stream argument"))
          ((null? (cdr strms))
              (let loop ((base base) (strm (car strms)))
                  (if (stream-null? strm)
                      base
                      (loop (func base (stream-last strm)) (stream-butlast strm)))))
          (else (let loop ((base base) (strms strms))
                    (if (any stream-null? strms)
                        base
                        (loop (apply func (cons base (map stream-last strms)))
                              (map stream-butlast strms)))))))

;; STREAM-FOLD-RIGHT-ONE func base stream ... -- apply func pairwise to items
(define (stream-fold-right-one func . strms)
    (cond ((null? strms)
              (error "no stream supplied to stream-fold-right-one"))
          ((any stream-null? strms)
              (error "can't apply stream-fold-right-one to null stream"))
          ((null? (cdr strms))
              (let ((strm (car strms)))
                  (stream-fold-right func (stream-last strm) (stream-butlast strm))))
          (else (apply stream-fold-right func
                                         (apply func (map stream-last strms))
                                         (map stream-butlast strms)))))

;; STREAM-SCAN-LEFT func base stream ... -- stream of partial reductions
(stream-define (stream-scan-left func base . strms)
    (if (not (procedure? func))
        (error "non-functional argument"))
    (if (not (all stream? strms))
        (error "non-stream argument"))
    (stream-cons base
                 (if (any stream-null? strms)
                     stream-null
                     (apply stream-scan-left
                            func
                            (apply func base (map stream-car strms))
                            (map stream-cdr strms)))))

;; STREAM-SCAN-LEFT-ONE func stream ... -- partial reductions, from first
(stream-define (stream-scan-left-one func . strms)
    (if (any stream-null? strms)
        (error "can't apply stream-scan-left-one to null stream")
        (apply stream-scan-left
               func
               (if (pair? (cdr strms))
                   (apply func (map stream-car strms))
                   (stream-car (car strms)))
               (map stream-cdr strms))))

;; STREAM-SCAN-RIGHT func base stream ... -- stream of partial reductions
(stream-define (stream-scan-right func base . strms)
    (if (not (procedure? func))
        (error "non-functional argument"))
    (if (not (all stream? strms))
        (error "non-stream argument"))
    (if (any stream-null? strms)
        (stream base)
        (let ((bases
                 (apply stream-scan-right func base (map stream-cdr strms))))
            (stream-cons (apply func (stream-car bases) (map stream-car strms))
                         bases))))
 
;; STREAM-SCAN-RIGHT-ONE func stream ... -- partial reductions, from first
(stream-define (stream-scan-right-one func . strms)
    (if (not (procedure? func))
        (error "non-functional argument"))
    (if (not (all stream? strms))
        (error "non-stream argument"))
    (if (any stream-null? strms)
        (error "can't apply stream-scan-right-one to null stream"))
    (if (any stream-null? (map stream-cdr strms))
        (stream (if (null? (cdr strms))
                    (stream-car (car strms))
                    (apply func (map stream-car strms))))
        (let ((bases
                 (apply stream-scan-right-one func (map stream-cdr strms))))
            (stream-cons (apply func (stream-car bases) (map stream-car strms))
                         bases))))

;; PORT->STREAM reader [port] -- stream of objects returned by reader from port
(stream-define (port->stream reader . port)
    (let* ((p (if (null? port) (current-input-port) (car port)))
           (obj (reader p)))
        (if (eof-object? obj)
            stream-null
            (stream-cons obj (port->stream reader p)))))

;; PORT->CHAR-STREAM [port] -- stream of characters on [current input] port
(stream-define (port->char-stream . port)
    (let ((p (if (null? port) (current-input-port) (car port))))
        (port->stream read-char p)))

;; PORT->WORD-STREAM [port] -- stream of words on [current input] port
(stream-define (port->word-stream . port)
    (letrec
        ((read-word (lambda (port)
            (let loop ((c (read-char port)) (word '()))
                (cond ((eof-object? c)
                          (if (null? word) c (list->string (reverse word))))
                      ((char-alphabetic? c)
                          (loop (read-char port) (cons c word)))
                      ((null? word) (loop (read-char port) word))
                      (else (list->string (reverse word))))))))
        (let ((p (if (null? port) (current-input-port) (car port))))
            (port->stream read-word p))))

;; PORT->LINE-STREAM [port] -- stream of lines on [current input] port
(stream-define (port->line-stream . port)
    (letrec
        ((read-line (lambda (port)
            (let loop ((c (read-char port)) (line '()))
                (cond ((eof-object? c)
                          (if (null? line) c (list->string (reverse line))))
                      ((char=? #\newline c) (list->string (reverse line)))
                      (else (loop (read-char port) (cons c line))))))))
        (let ((p (if (null? port) (current-input-port) (car port))))
            (port->stream read-line p))))

;; STREAM-DISPLAY stream [port] -- display stream on [current output] port
(define (stream-display strm . port)
    (let ((p (if (null? port) (current-output-port) (car port))))
        (stream-for-each (lambda (x) (display x p)) strm)))

;; STREAM-DISPLAY-LINES stream [port] -- display stream with newlines
(define (stream-display-lines strm . port)
    (let ((p (if (null? port) (current-output-port) (car port))))
        (stream-for-each (lambda (x) (display x p) (newline p)) strm)))

;; STREAM-WRITE stream [port] -- write stream on [current output] port
(define (stream-write strm . port)
    (let ((p (if (null? port) (current-output-port) (car port))))
        (stream-for-each (lambda (x) (write x p)) s)))

;; STREAM->LIST stream [n] -- new list containing [first n] elements of stream
(define (stream->list strm . n)
    (if (not (stream? strm))
        (error "attempt to convert non-stream to list"))
    (let ((m (if (pair? n) (car n) -1)))
        (if (not (integer? m))
            (error "non-integral stream->list length")
            (let loop ((strm strm) (m m))
                (if (or (zero? m) (stream-null? strm))
                    '()
                    (cons (stream-car strm)
                          (loop (stream-cdr strm) (- m 1))))))))

;; STREAM->VECTOR stream [n] -- vector containing [first n] elements of stream
(define (stream->vector strm . n)
    (if (not (stream? strm))
        (error "attempt to convert non-stream to vector"))
    (if (pair? n)
        (list->vector (stream->list strm (car n)))
        (list->vector (stream->list strm))))

;; STREAM->STRING stream [n] -- string with [first n] characters of stream
(define (stream->string strm . n)
    (if (not (stream? strm))
        (error "attempt to convert non-stream to string"))
    (if (pair? n)
        (list->string (stream->list strm (car n)))
        (list->string (stream->list strm))))

;; STREAM-APPEND stream ... -- append one or more streams end to end
(stream-define (stream-append . strms)
    (if (not (all stream? strms))
        (error "attempt to append non-stream"))
    (let outer-loop ((s stream-null) (strms strms))
        (if (null? strms)
            s
            (let inner-loop ((s s))
                (if (stream-null? s)
                    (outer-loop (car strms) (cdr strms))
                    (stream-cons (stream-car s)
                                 (inner-loop (stream-cdr s))))))))

;; STREAM-CONCAT stream -- append a stream of streams into a single stream
(define (stream-concat strm)
    (apply stream-append (stream->list strm)))

;; STREAM-REVERSE stream -- new stream with elements of stream in reverse order
(stream-define (stream-reverse strm)
    (if (not (stream? strm))
        (error "attempt to reverse non-stream"))
    (let reverse ((s strm) (r stream-null))
        (if (stream-null? s)
            r
            (reverse (stream-cdr s) (stream-cons (stream-car s) r)))))

;; STREAM-ZIP stream ... -- convert multiple streams to stream of lists
(stream-define (stream-zip . strms)
    (if (null? strms)
        (error "stream-zip applied to null arguments")
        (if (any stream-null? strms)
            stream-null
            (stream-cons
                (apply list (map stream-car strms))
                (apply stream-zip (map stream-cdr strms))))))

;; STREAM-UNZIP stream -- convert stream of lists to multiple streams
(define (stream-unzip strm)
    (if (or (null? strm)
            (stream-null? (stream-car strm))
            (null? (stream-car strm)))
        (error "stream-unzip applied to null arguments")
        (apply values
            (let unzip ((strm strm))
                (if (null? (stream-car strm))
                    '()
                    (cons (stream-map car strm)
                          (unzip (stream-map cdr strm))))))))

;; STREAM-MERGE lt? stream ... -- stably merge multiple streams by less-than
(stream-define (stream-merge lt? . strms)
    (cond ((null? strms) stream-null)
          ((null? (cdr strms)) (car strms))
          ((any stream-null? strms)
              (apply stream-merge (cons lt?
                  (let loop ((strms strms) (result '()))
                      (cond ((null? strms) (reverse result))
                            ((stream-null? (car strms)) (loop (cdr strms) result))
                            (else (loop (cdr strms) (cons (car strms) result))))))))
          (else (let loop ((unexamined (cdr strms))
                           (min (stream-car (car strms)))
                           (minstrm (car strms))
                           (beforemin '())
                           (aftermin '()))
                     (cond ((null? unexamined) ; end of list
                               (stream-cons min
                                   (apply stream-merge
                                       (cons lt?
                                           (append beforemin
                                                   (list (stream-cdr minstrm))
                                                   aftermin)))))
                           ((lt? (stream-car (car unexamined)) min) ; new minimum
                               (loop (cdr unexamined)
                                     (stream-car (car unexamined))
                                     (car unexamined)
                                     (append beforemin (list minstrm) aftermin)
                                     '()))
                           (else ; same minimum
                               (loop (cdr unexamined)
                                     min
                                     minstrm
                                     beforemin
                                     (append aftermin (list (car unexamined))))))))))

;; STREAM-SORT lt? stream -- newly-allocated stream ordered by lt?
;  smooth applicative merge sort due to Richard O'Keefe via Larry Paulson
(define (stream-merge-pairs lt? list-of-runs k)
    (if (or (null? (cdr list-of-runs)) (odd? k))
        list-of-runs
        (stream-merge-pairs lt?
            (cons (stream-merge lt?
                      (car list-of-runs)
                      (cadr list-of-runs))
                  (cddr list-of-runs))
            (/ k 2))))
(define (stream-next-run lt? run strm)
    (if (or (stream-null? strm)
            (lt? (stream-car strm) (stream-car run)))
       (list (stream-reverse run) strm)
       (stream-next-run lt? (stream-cons (stream-car strm) run)
                            (stream-cdr strm))))
(stream-define (stream-sorting lt? strm list-of-runs k)
    (if (stream-null? strm)
        (car (stream-merge-pairs lt? list-of-runs 0))
        (let* ((lsts
                  (stream-next-run lt? (stream (stream-car strm)) (stream-cdr strm)))
               (run (car lsts))
               (tail (cadr lsts)))
            (stream-sorting lt?
               tail
               (stream-merge-pairs lt? (cons run list-of-runs) (+ k 1))
               (+ k 1)))))
(stream-define (stream-sort lt? strm)
    (if (stream-null? strm)
        strm
        (stream-sorting lt? strm '() 0)))

;; STREAM-UNIQ eql? stream -- new stream with adjacent duplicates removed
(stream-define (stream-uniq-aux eql? strm omit)
  (if (stream-null? strm)
      stream-null
      (let ((first (stream-car strm)))
	(if (eql? omit (stream-car strm))
	    (stream-uniq-aux eql? (stream-cdr strm) omit)
	    (stream-cons first
			 (stream-uniq-aux eql? (stream-cdr strm) first))))))
(stream-define (stream-uniq eql? strm)
    (if (not (procedure? eql?))
        (error "non-functional argument to uniq"))
    (if (not (stream? strm))
        (error "non-stream argument to uniq"))
    (if (stream-null? strm)
	stream-null
	(let ((first (stream-car strm)))
	  (stream-cons first
		       (stream-uniq-aux eql? strm first)))))

;; STREAM-ALTERNATE stream ... -- stream of items from streams in round-robin
(stream-define (stream-alternate . strms)
    (cond ((null? strms) stream-null)
          ((stream-null? (car strms)) (apply stream-alternate (cdr strms)))
          (else (stream-cons
                    (stream-car (car strms))
                    (apply stream-alternate
                        (append (cdr strms)
                                (list (stream-cdr (car strms)))))))))

;; ALL pred? list -- #f if any (pred? list-item) is #f, or last pred?
(define (all pred? lst)
    (cond ((null? lst) #t)
          ((null? (cdr lst)) (pred? (car lst)))
          (else (and (pred? (car lst)) (all pred? (cdr lst))))))

;; ANY pred? list -- first non-#f (pred? list-item), else #f
(define (any pred? lst)
    (cond ((null? lst) #f)
          ((null? (cdr lst)) (pred? (car lst)))
          (else (or (pred? (car lst)) (any pred? (cdr lst))))))



;;; arch-tag: 5ccf4797-93cb-4981-a9a6-655253b216ca
