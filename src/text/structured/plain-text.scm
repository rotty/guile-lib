(define-module (text structured plain-text)
  :use-module (text structured)
  :use-module (oop goops)
  :use-module (srfi srfi-13)
  :export (stext->plain-text stext->plain-text-title))

(define (default-stext->plain-text-func type tokens loop)
  (let ((name (if (list? type) (car type) type))
        (ret '()))
    ;; If the token is of the (<identifier> <element>+) variety, we loop
    ;; on it, too.
    (if (list? type) (set! ret (loop type)))
    (let inside-loop ((tokens tokens) (ret ret) (broken? #t))
      (if (null? tokens)
          ret
          (let ((token (car tokens)))
            (cond
             ((ignored? token)
              (inside-loop (cdr tokens) ret broken?))
             ((equal? token "\n")
              (cond
               (broken?
                ;; do nothing if it's already broken...
                (inside-loop (cdr tokens) ret #t))
               ((null? (cdr tokens))
                ;; ...or if we have a trailing newline
                (inside-loop (cdr tokens) ret #t))
               ((equal? (cadr tokens) "\n")
                ;; condense multiple newlines into one paragraph break
                (inside-loop
                 (let find-tail ((tail (cddr tokens)))
                   (if (or (null? tail) (not (equal? (car tail) "\n")))
                       tail
                       (find-tail (cdr tail)))) ;; nasty!
                 (cons "\n" ret)
                 #t))
               (else
                ;; replace single newline with nothing
                (inside-loop (cdr tokens) ret #f))))
             (else
              (if (and (not (inline? token)) (not broken?))
                  (set! ret (cons "\n" ret)))
              (if (string? token)
                  (set! ret (cons token ret))
                  (set! ret (append! (loop token) ret)))
              (if (and (not (inline? token)) (not (null? (cdr tokens))))
                  (set! ret (cons "\n" ret)))
              (inside-loop (cdr tokens) ret (not (inline? token))))))))))

(define (example-stext->plain-text-func type tokens loop)
  (define (deep-apply proc l)
    (apply
     proc
     (let loop ((walk l))
       (cond
        ((null? walk)
         l)
        ((list? (car walk))
         (set-car! walk (deep-apply proc (car walk)))
         (loop (cdr walk)))
        (else
         (loop (cdr walk)))))))
  (let ((name (if (list? type) (car type) type)))
    (list
     (deep-apply
      string-append
      (reverse!
       (let inside-loop ((tokens tokens) (ret '()))
         (if (null? tokens)
             ret
             (let ((token (car tokens)))
               (if (string? token)
                   (if (equal? token "\n")
                       (inside-loop (cdr tokens) (cons "    " (cons token ret)))
                       (inside-loop (cdr tokens) (cons token ret)))
                   (inside-loop (cdr tokens) (append! (loop token) ret)))))))))))

(define (top-stext->plain-text-func type tokens loop)
  tokens)

(define stext->plain-text-func-alist
  `((copyright . ,(lambda args
                    (list (string #\302 #\251))))
    (example . ,example-stext->plain-text-func)
    (smallexample . ,example-stext->plain-text-func)
    (stext . ,top-stext->plain-text-func)
    ))

(define ignore-list
  '(page setfilename setchapternewpage iftex ifhtml ifxml sp vskip
    node menu ignore titlepage syncodeindex))
(define (ignored? token)
  (and (list? token) (memq (car token) ignore-list)))
(for-each
 (lambda (sym)
   (set! stext->plain-text-func-alist (acons sym (lambda args '())
                                             stext->plain-text-func-alist)))
 ignore-list)

(define (get-stext->plain-text-func symbol)
  (or (assq-ref stext->plain-text-func-alist symbol)
      default-stext->plain-text-func))

(define (stext->plain-text stext)
  "Format @var{stext} into a plain-text string."
  ;; We remove spaces between standalone elements, thus preventing extra
  ;; lines to be thrown into the output. Because comments throw off the
  ;; newline code, we remove them too.
  (define (filter l)
    (define (loop-ignoring loop input standalone?)
      (if (null? (cdr input))
          (set! input '())
          (begin
            (set-car! input (cadr input))
            (set-cdr! input (cddr input))))
      (loop input standalone?))
    (let loop ((input l) (standalone? #t))
      (if (null? input)
          l
          (if (and standalone?
                   (equal? (car input) " "))
              (loop-ignoring loop input standalone?)
              (if (list? (car input))
                  (if (memq (caar input) '(c comment))
                      (loop-ignoring loop input standalone?)
                      (begin
                        (filter (cdar input))
                        (loop (cdr input) (not (inline? (car input))))))
                  (loop (cdr input) (not (inline? (car input)))))))))
  (define (string-trim str)
    (let ((len (string-length str)))
      (if (and (> len 0) (char=? (string-ref str (1- len)) #\newline))
          (substring str 0 (1- len))
          str)))
  (define (fill strings)
    (let loop ((strings (apply append (map
                                       (lambda (str)
                                         (string-split str #\space))
                                       strings)))
               (filled '())
               (tmp '())
               (level 0))
      (if (null? strings)
          (string-join (reverse! (cons (string-join (reverse tmp)) filled))
                       "\n" 'suffix)
          (let ((len (string-length (car strings))))
            (cond
             ((= len 0)
              (loop (cdr strings) filled tmp level))
             ((> (+ len level) 70)
              (loop (cdr strings)
                    (cons (string-join (reverse tmp)) filled)
                    (list (car strings))
                    len))
             (else
              (loop (cdr strings)
                    filled
                    (cons (car strings) tmp)
                    (+ len level))))))))
  (let ((strings '()))
    (set!
     strings
     (reverse!
      (let loop ((text (filter stext)))
        ((get-stext->plain-text-func (car text)) (car text) (cdr text) loop))))
    (string-trim
     (apply
      string-append
      (let loop ((strings strings) (ret '()))
        (cond
         ((null? strings)
          (reverse! ret))
         ((string-index (car strings) #\newline)
          ;; If it contains a newline, it's not meant to be filled
          (loop (cdr strings) (cons (car strings) ret)))
         (else
          ;; We need to fill the string.
          (let ((filled #f) (rest #f))
            (let find-tail ((fill-strings (list (car strings)))
                            (tail (cdr strings)))
              (if (or (null? tail) (string-index (car tail) #\newline))
                  (begin
                    (set! filled (fill (reverse fill-strings)))
                    (set! rest tail))
                  (find-tail (cons (car tail) fill-strings) (cdr tail))))
            (loop rest (cons filled ret))))))))))

(define (stext->plain-text-title stext)
  "Take @var{stext} and format it into plain-text as a title."
  (stext->plain-text (cons '(stext) (cdr stext))))

;;; arch-tag: 8813e93f-0bf9-4771-ad02-3385fe877e43
