;; guile-lib
;; Copyright (C) 2003,2004 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;Doc me!
;;        
;;; Code:

(define-module (text structured plain-text)
  :use-module (text structured)
  :use-module (oop goops)
  :use-module (srfi srfi-13)
  :use-module (srfi srfi-1)
  :export (stext->plain-text stext->plain-text-title))

(define (fill strings indent)
  (define (join-punctuation l)
    (let lp ((in l) (out '()))
      (cond
       ((null? in)
        (reverse! out))
       ((null? (cdr in))
        (lp (cdr in) (cons (car in) out)))
       ((and (= (string-length (car in)) 1)
             (string-index "`\#([{" (string-ref (car in) 0)))
        (lp (cons (string-append (car in) (cadr in)) (cddr in)) out))
       ((and (= (string-length (cadr in)) 1)
             (string-index "\"'!,-.:;?" (string-ref (cadr in) 0)))
        (lp (cons (string-append (car in) (cadr in)) (cddr in)) out))
       (else
        (lp (cdr in) (cons (car in) out))))))
  (define (make-indent)
    (if (zero? indent) '() (list (list->string (make-list indent #\space)))))
  (let loop ((strings (join-punctuation
                       (filter
                        (lambda (str) (not (zero? (string-length str))))
                        (apply append (map
                                       (lambda (str)
                                         (string-split str #\space))
                                       strings)))))
             (filled '())
             (tmp (make-indent))
             (level indent))
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
                  (cons (car strings) (make-indent))
                  (+ len indent)))
           (else
            (loop (cdr strings)
                  filled
                  (cons (car strings) tmp)
                  (+ len level))))))))

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

(define (table-stext->plain-text-func type tokens loop)
  (let ((item-type (if (list? type) (caadr type) 'text)))
    (let inside-loop ((in tokens) (out '()) (broken? #t))
      (cond
       ((null? in)
        ;; We prefill the output so it looks nice.
        (let lp ((in out) (out '()) (tmp '()))
          (if (null? in)
              (reverse (if (null? tmp) out (cons (fill tmp 5) out)))
              (let ((str (car in)))
                (if (string-index str #\newline) ;; means it's an item
                    (lp (cdr in) (cons str (if (null? tmp) out (cons (fill tmp 5) out))) '())
                    (lp (cdr in) out (cons str tmp)))))))
       (else
        (let ((token (car in)))
          (cond
           ((ignored? token)
            (inside-loop (cdr in) out broken?))
           ((equal? token "\n")
            (cond
             (broken?
              ;; do nothing if it's already broken...
              (inside-loop (cdr in) out #t))
             ((null? (cdr in))
              ;; ...or if we have a trailing newline
              (inside-loop (cdr in) out #t))
             ((equal? (cadr in) "\n")
              ;; condense multiple newlines into one paragraph break
              (inside-loop
               (let find-tail ((tail (cddr in)))
                 (if (or (null? tail) (not (equal? (car tail) "\n")))
                     tail
                     (find-tail (cdr tail)))) ;; nasty!
               (cons "\n" out)
               #t))
             (else
              ;; replace single newline with nothing
              (inside-loop (cdr in) out #f))))
           (else
            (cond
             ((string? token)
              (set! out (cons token out)))
             ((eq? (car token) 'item)
              (if (not broken?)
                  (set! out (cons "\n" out)))
              (set! out (cons (fill (reverse (loop (cons item-type (cdr token)))) 0) out)))
             (else
              (if (and (not (inline? token)) (not broken?))
                  (set! out (cons "\n" out)))
              (set! out (append! (loop token) out))
              (if (and (not (inline? token)) (not (null? (cdr in))))
                  (set! out (cons "\n" out)))))
            (inside-loop (cdr in) out (not (inline? token)))))))))))

(define (code-stext->plain-text-func type tokens loop)
  (let inside-loop ((in tokens) (out '()))
      (if (null? in)
          (append! (cons "'" out) (list "`")) ;; it's reversed
          (let ((token (car in)))
            (inside-loop (cdr in)
                         (cond ((ignored? token) out)
                               ((string? token) (cons token out))
                               (else (append! (loop token) out))))))))

(define (var-stext->plain-text-func type tokens loop)
  (list (apply string-append (map string-upcase tokens))))

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

;(define (top-stext->plain-text-func type tokens loop)
;  tokens)

(define stext->plain-text-func-alist
  `((copyright . ,(lambda args
                    (list (string #\302 #\251))))
    (example . ,example-stext->plain-text-func)
    (smallexample . ,example-stext->plain-text-func)
;    (stext . ,top-stext->plain-text-func)
    (var . ,var-stext->plain-text-func)
    (code . ,code-stext->plain-text-func)
    (table . ,table-stext->plain-text-func)
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
  (or (assq-ref stext->plain-text-func-alist (if (list? symbol) (car symbol) symbol))
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
                    (set! filled (fill (reverse fill-strings) 0))
                    (set! rest tail))
                  (find-tail (cons (car tail) fill-strings) (cdr tail))))
            (loop rest (cons filled ret))))))))))

(define (stext->plain-text-title stext)
  "Take @var{stext} and format it into plain-text as a title."
  (stext->plain-text (cons '(stext) (cdr stext))))

;;; arch-tag: 8813e93f-0bf9-4771-ad02-3385fe877e43
