(define-module (srfi srfi-35)
  #:use-module (oop goops)
  #:use-module (oop goops util)
  #:use-module (srfi srfi-1)

  ;; (oop goops util) and (srfi srfi-1) both define any, every
  ;; #:duplicates last ; inhibits the warning, but Guile 1.7 only

  #:export (make-condition-type
            condition-type? condition-has-type?
            
            &condition
            make-condition make-compound-condition
            condition? extract-condition
            condition-ref
            
            &message
            message-condition? condition-message
            
            &serious
            serious-condition?

            &error
            error?
            
            ;; Not part of the SRFI
            &compound-condition
            %make-compound-condition-helper
            handle-condition)
  #:export-syntax (define-condition-type condition))

(define-class &condition-meta (<class>))
  
(define-class &condition ()
  (%name #:accessor condition-type-name)
  #:metaclass &condition-meta)

(define (condition-type? thing)
  (is-a? thing &condition-meta))

(define (condition-type-all-fields type)
  (fold-right (lambda (slot lst)
                (let ((name (car slot)))
                  (if (eq? name '%name)
                      lst
                      (cons name lst))))
              '()
              (class-slots type)))

(define (make-condition-type name supertype fields)
  (if (not (symbol? name))
      (error "make-condition-type: name is not a symbol"
             name))
  (if (not (condition-type? supertype))
      (error "make-condition-type: supertype is not a condition type"
             supertype))
  (if (not
       (null? (lset-intersection eq?
                                 (condition-type-all-fields supertype)
                                 fields)))
      (error "make-condition-type: duplicate field name" ))
  
  (make-class (list supertype) (map list fields) #:name name))

(define-macro (define-condition-type ?name ?supertype ?predicate . ?field-acc)
  `(begin
     (define ,?name
       (make-condition-type ',?name
                            ,?supertype
                            (map car ',?field-acc)))
     (define (,?predicate thing)
        (and (condition? thing)
             (condition-has-type? thing ,?name)))
     ,@(map
        (lambda (f-a)
          `(define (,(cadr f-a) condition)
             (condition-ref (extract-condition condition ,?name)
                            ',(car f-a))))
        ?field-acc)))

;; Stolen from oop/goops.scm
(define (list2set l)	       
  (let loop ((l l)
	     (res '()))
    (cond		       
     ((null? l) res)
     ((memq (car l) res) (loop (cdr l) res))
     (else (loop (cdr l) (cons (car l) res))))))

;; This should be in goops.scm, really
(define (class-supers c)
  (letrec ((allsubs (lambda (c)
		      (cons c (mapappend allsubs
					 (class-direct-supers c))))))
    (list2set (cdr (allsubs c)))))

(define (condition-subtype? subtype supertype)
  (or (equal? subtype supertype)
      (memq supertype (class-supers subtype))))

(define (condition-type-field-supertype condition-type field)
  (let loop ((condition-type condition-type))
    (cond ((not condition-type) #f)
          ((memq field (condition-type-fields condition-type))
           condition-type)
          (else
           (loop (condition-type-supertype condition-type))))))

(define (condition? thing)
  (is-a? thing &condition))

(define (make-condition type . field-plist)
  (let ((alist (let loop ((plist field-plist))
                 (if (null? plist)
                            '()
                     (cons (cons (car plist)
                                 (cadr plist))
                           (loop (cddr plist)))))))
    (if (not (lset<= eq?
                     (map car alist)
                     (condition-type-all-fields type)))
        (error "condition fields don't match condition type"
               (condition-type-all-fields type) (map car alist)))
    (let ((condition (make type)))
      (for-each (lambda (pr)
                  (slot-set! condition (car pr) (cdr pr)))
               alist)
      condition)))

(define-method (condition-has-type? condition type)
  (if (memq type (condition-types condition))
      #t #f))

(define condition-ref slot-ref)

(define (type-field-alist-ref type-field-alist field)
  (let loop ((alist type-field-alist))
    (cond ((null? alist) #f)
          ((assq field (cdr (car alist)))
           => identity)
          (else
           (loop (cdr alist))))))

(define-class &compound-condition (&condition)
  (%components #:init-keyword #:components))

(define (make-compound-condition condition-1 . conditions)
  (if (null? conditions)
      condition-1
      (make &compound-condition
        #:components (cons condition-1 conditions))))

(define-method (extract-condition (condition &condition)
                                  (type &condition-meta))
    (if (not (condition-subtype? (class-of condition) type))
        (error "extract-condition: invalid condition type"
                      condition type))
    condition)

(define-method (extract-condition (condition &compound-condition)
                                  (type &condition-meta))
  (any (lambda (component)
         (if (condition-has-type? component type)
             (extract-condition component type)
             #f))
       (slot-ref condition '%components)))

(define (%make-compound-condition-helper type-field-alist)
  (apply
   make-compound-condition
   (map
    (lambda (form)
      (apply make-condition
             (cons
              (car form)
              ;; Fold to plist
              (fold (lambda (entry lst)
                      (cons (car entry) (cons (cadr entry) lst)))
                    '()
                    (cdr form)))))
    ;; Extend entries
    (map (lambda (entry)
           (cons (car entry)
                 (fold (lambda (field rest)
                         (let ((pr (or (assq field (cdr entry))
                                       (type-field-alist-ref type-field-alist
                                                             field))))
                           (if pr
                               (cons pr rest)
                               rest)))
                       '() (condition-type-all-fields (car entry)))))
        type-field-alist))))

(define-macro (condition . forms)
  ;; forms: ((type1 (field1 value1) ...) ...)
  (list
   '%make-compound-condition-helper
   (list
    'quasiquote
    (map
     (lambda (form)
       ;;(format #t "form ~S\n" form)
       (cons
        (list 'unquote (car form))
        (map (lambda (entry)
               ;;(format #t "entry ~S\n" entry)
               (list (car entry) (list 'unquote (cadr entry))))
             (cdr form))))
       forms))))
  
(define-method (condition-types (condition &condition))
  (let ((own-class (class-of condition)))
    (cons own-class (class-direct-supers own-class))))

(define-method (condition-types (condition &compound-condition))
  (mapappend condition-types (slot-ref condition '%components)))

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)

(define-method (handle-condition (c &condition))
  (error "unhandled condition" c))

(define-method (handle-condition (c &message))
  (error "unhandled message condition" (condition-message c)))

(define-method (handle-condition (c &serious))
  (error "unhandled serious condition" c))

;;; arch-tag: 1145fba2-0008-4c99-8304-a53cdcea50f9
