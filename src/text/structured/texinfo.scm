(define-module (text structured texinfo)
  :use-module (text structured plain-text) ;; for table-o-contents
  :use-module (text structured)
  :export (texinfo->stext))

(define (call-with-file-and-dir filename proc)
  (define current-dir (getcwd))
  (dynamic-wind
      (lambda () (chdir (dirname filename)))
      (lambda ()
        (with-input-from-file (basename filename) proc))
      (lambda () (chdir current-dir))))

(define (graft! list shoot)
  (let ((CDR (cdr list)))
    (set-car! list (car shoot))
    (if (not (null? (cdr shoot)))
        (begin
          (set-cdr! list (cdr shoot))
          (set-cdr! (last-pair shoot) CDR)))))

(define (delete-head! list)
  (if (null? (cdr list))
      (error "You can't delete the head of the last pair in a list"))
  (set-car! list (cadr list))
  (set-cdr! list (cddr list)))

(define make-anchor-name
  (let ((count -1))
    (lambda ()
      (set! count (1+ count))
      (string-append "indexed-anchor-" (number->string count)))))

(define (texinfo->stext . args)
  "Converts one texinfo file into one big stext. With no arguments,
parses data from the current input port. Otherwise it reads from the
file name passed as the first argument."
  (letrec ((state '()) ;; an alist
           (postprocess
            (lambda (tokens)
              ;; Deal with strange syntax: macros, aliases,
              ;; definfoenclose, raisesections, lowersections, set/value,
              ;; insertcopying, contents, bye
              (let loop ((input tokens))
                (cond
                 ((null? input)
                  tokens)
                 ((list? (car input))
                  (cond
                   ((and (not (null? (cdr input)))
                         (memq (caar input) '(copying set)))
                    ;; We need a pair in the cdr to use delete-head!.
                    (case (caar input)
                      ((copying)
                       (set! state (assoc-set! state 'copying (list-copy (cdar input))))
                       (delete-head! input)
                       (loop input))
                      ((set)
                       ;; This is kindof a hack, but hey.
                       (let* ((split (string-split (cadar input) #\space))
                              (variable (car split))
                              (value (if (null? (cddar input))
                                         (cadr split)
                                         (caddar input))))
                         (set! state (assoc-set! state variable value)))
                       (delete-head! input)
                       (loop input))
                      (else (error))))
                   (else
                    (case (caar input)
                      ((value)
                       (set-car! input (or (assoc-ref state (cadar input))
                                           (error "No such value" (cadar input) state)))
                       (loop input))
                      ((insertcopying)
                       (graft! input (or (assoc-ref state 'copying)
                                         (error "No copying available")))
                       (loop input))
                      ((contents)
                       (graft! input (scan-contents input))
                       (loop input))
                      ((deftp defcv defivar deftypeivar defop deftypeop defmethod
                        deftypemethod defopt defvr defvar deftypevr deftypevar
                        deffn deftypefn defmac defspec defun deftypefun
                        cindex findex vindex kindex pindex tindex)
                       (graft! input (list (list 'anchor (make-anchor-name))
                                           (car input)))
                       (loop (cddr input)))
                      ((bye)
                       (set-car! input "")
                       (set-cdr! input '())
                       (loop (cdr input)))
                      (else
                       (postprocess (car input))
                       (loop (cdr input)))))))
                 (else
                  (loop (cdr input))))))))

    (let ((tokens (if (null? args)
                      (tokenize)
                      (call-with-file-and-dir (car args) tokenize))))
      (postprocess tokens))))

(define (cons-to-top-of-stack item stack)
  (append (list (cons item (car stack))) (cdr stack)))
(define (deep-reverse l)
  (if (list? l)
      (reverse (map deep-reverse l))
      l))

(define (scan-contents tokens)
  (define (pop-n stack n)
    (if (zero? n)
        stack
        (pop-n
         (cons (cons (car stack) (cadr stack)) (cddr stack))
         (1- n))))
  (define (push stack)
    (cons '(enumerate) stack))
  (define (node? tok)
    (and (list? tok) (eq? (car tok) 'node)))
  (define (chunking-section? tok)
    (and (list? tok) (name->depth (car tok) 4)))
  
  (let loop ((input tokens) (level 0) (output '((enumerate))))
    (define (loop-contents-entry entry new-level new-input)
      (loop new-input new-level
            (cons-to-top-of-stack
             entry
             (cons-to-top-of-stack
              '(item)
              (cond
               ((= new-level level) output)
               ((< new-level level) (pop-n output
                                           (- level new-level)))
               ((= new-level (1+ level)) (push output))
               (else
                (error
                 "Section heading jumps too much: " input)))))))
    (cond
     ((null? input)
      (cons '(unnumberedsec "Table of Contents")
            ;; This cdar is a hack, I'm really lost :/
            (cdar (deep-reverse (pop-n output level)))))
     ((and (node? (car input))
           (not (null? (cdr input))) ;; newline
           (not (null? (cddr input))) ;; section
           (chunking-section? (caddr input)))
      (loop-contents-entry (reverse (list 'ref (car (cdar input))))
                           (name->depth (caaddr input) 4)
                           (cdddr input)))
     ((chunking-section? (car input))
      (loop-contents-entry (stext->plain-text-title (cons 'text (cdar input)))
                           (name->depth (caar input) 4)
                           (cdr input)))
     (else
      (loop (cdr input) level output)))))

(define texinfo-environments
  '(cartouche copying defcv deffn defivar defmac defmethod defop defopt defspec
    deftp deftypefn deftypefun deftypevar deftypevr defun defvar defvr
    description display enumerate example flushleft flushright format ftable
    group ifclear ifset ifhtml ifinfo ifnothtml ifnotinfo ifnottex iftex
    ignore itemize lisp macro multitable quotation smalldisplay smallexample
    smallformat smalllisp table tex titlepage verbatim vtable menu detailmenu))

(define (tokenize)
  (define (preprocess l)
    ;; Deals with include
    (let loop ((input l))
      (cond
       ((or (null? input) (null? (cdr input)))
        l)
       ((list? (car input))
        (case (caar input)
          ((include)
           (let ((included (call-with-file-and-dir (cadar input) tokenize))
                 (CDR (cdr input)))
             (set-car! input (car included))
             (if (not (null? (cdr included)))
                 (begin
                   (set-cdr! input (cdr included))
                   (set-cdr! (last-pair included) CDR)))
             ;; Although calling texinfo->stext has already
             (loop input)))
          (else
           (preprocess (car input))
           (loop (cdr input)))))
       (else
        (loop (cdr input))))))
  (define (filter-environments l)
    (let loop ((stack '(())) (input l))
      (if (null? input)
          (if (not (null? (cdr stack)))
              (error "Unfinished begin" stack (car (last-pair (car stack))))
              (reverse (car stack)))
          (if (and (list? (car input)) (memq (caar input) texinfo-environments))
              (loop (cons (if (null? (cdar input)) ;; push
                              (car input)
                              (list (car input)))
                          stack)
                    (cdr input))
              (if (and (list? (car input)) (eq? (caar input) 'end))
                  (cond
                   ((null? (cdr stack))
                    (error "Invalid end: " (car input)))
                   ((not (eq? (string->symbol (cadar input))
                              (let ((head (car (last-pair (car stack)))))
                                (if (list? head) (car head) head))))
                    (error "Mismatching begin and end: "
                           (cadar input) (car (last-pair (car stack)))))
                   (else
                    (loop (cons-to-top-of-stack (reverse (car stack)) (cdr stack))
                          (cdr input))))
                  (loop (cons-to-top-of-stack (car input) stack) (cdr input)))))))
  (let lp ((tokens '()) (tok (read-token)))
    (if (not tok)
        (let ((ret (preprocess (filter-environments (deep-reverse tokens)))))
          ;; Ignore anything before settitle, if there is one
          (let take-before-settitle ((stext ret))
            (if (null? stext)
                (if (or (null? ret) (string? (car ret)))
                    (cons 'text ret)
                    ret)
                (if (and (list? (car stext)) (eq? (caar stext) 'settitle))
                    (begin
                      (set-car! (car stext) 'stext)
                      stext)
                    (take-before-settitle (cdr stext))))))
        (lp (cons tok tokens) (read-token)))))

(define (read-token)
  (let ((c (read-char)))
    (cond
     ((eof-object? c)
      #f)
     ((char=? c #\newline)
      (string c))
     ((char=? c #\@)
      (if (not (char-alphabetic? (peek-char)))
          (string (read-char))
          (let ((sym (apply symbol
                            (let lp ((chars '()))
                              (let ((c (read-char)))
                                (if (not (and (char? c) (char-alphabetic? c)))
                                    (begin
                                      (unread-char c)
                                      (reverse chars))
                                    (lp (cons c chars))))))))
            ;; We have the command name, we just need to see about the
            ;; arguments
            (let ((next-char (peek-char)))
              (cond
               ((char=? next-char #\{)
                (read-char)
                ;; We have inline arguments
                (let lp ((args (list sym)))
                  (if (eq? (peek-char) #\})
                      (begin
                        (read-char)
                        args)
                      (lp (cons (or (read-token)
                                    (error "Unexpected end of file while searching for close bracket"))
                                args)))))
               ((char=? next-char #\newline)
                ;; No arguments
                ;; We don't eat the newline, either
                (list sym))
               ((char=? next-char #\space)
                ;; Look for arguments until newline
                (read-char)
                (let lp ((args (list sym)))
                  (if (eq? (peek-char) #\newline)
                      args
                      (lp (cons (or (read-token)
                                    (error "Unexpected end of file while searching for newline"))
                                args)))))
               (else
                (errorf "Bad character after @-command ~A: ~A" sym (peek-char))))))))
     ((char=? c #\{)
      (let lp ((args '(text)))
        (if (eq? (peek-char) #\})
            (begin
              (read-char)
              args)
            (lp (cons (or (read-token)
                          (error "Unexpected end of file while searching for close bracket"))
                      args)))))
     ((char=? c #\})
      (error "Bad closing bracket"))
     (else
      (apply string
             (let lp ((chars (list c)) (c (read-char)))
               (if (eof-object? c)
                   (reverse chars)
                   (case c
                     ((#\@ #\{ #\} #\newline)
                      (unread-char c)
                      (reverse chars))
                     (else
                      (lp (cons c chars) (read-char)))))))))))

;;; arch-tag: 7711fbcf-7308-475f-9da4-7e8f654653cd
