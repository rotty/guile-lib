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

(define-module (text structured gtk-text-buffer)
  :use-module (text structured)
  :use-module (scheme documentation)
  :use-module (gnome gtk)
  :use-module (gnome gtk gdk-event)
  :use-module (gnome gobject)
  :use-module (gnome gtk gw-pango)
  :use-module (gnome gnome gw-gnome)
  :export (stext->gtk-text-buffer
           stext-buffer-xref-activated-hook))

;; The two arguments are the node name and the manual name or #f
(define-with-docs stext-buffer-xref-activated-hook
  "A hook run when the user activates a cross-reference. The two
arguments to the functions are the name of the node and the name of the
manual."
  (make-hook 2))

(define tag-table (make <gtk-text-tag-table>))
(define tag-prop-alist
  ;; List the non-inline styles first so inlines get priority
  '((center . (:justification center))
    (example . (:font "Monospace" :wrap-mode none
                      :pixels-below-lines 0 :pixels-above-lines 0 :left-margin 20))
    (smallexample . (:font "Monospace" :wrap-mode none :scale 0.9
                     :pixels-below-lines 0 :pixels-above-lines 0 :left-margin 20))
    (cartouche . (:pixels-below-lines 6 :pixels-above-lines 6 :pixels-inside-wrap 0
                  :left-margin 25 :right-margin 25 :background "grey"
                  :justification center))
    (title . (:pixels-above-lines 6 :pixels-below-lines 12 :scale 1.4399999999 :weight 700))
    (subtitle . (:pixels-above-lines 0 :pixels-below-lines 12 :scale 1.2 :weight 700))
    (author . (:pixels-above-lines 0 :pixels-below-lines 12 :scale 1.2 :weight 700))
    ;; Assuming chapters and sections fall at the beginning of a buffer.
    ;; Not too good of an assumption. Better would be to detect if we're
    ;; at the start of a buffer, then adjust the tag accordingly.
    (chapter . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (section . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (appendix . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (appendixsec . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (unnumbered . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))
    (unnumberedsec . (:pixels-above-lines 6 :pixels-below-lines 18 :scale 1.4399999999 :weight 700))

    (subsection . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (subsubsection . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (appendixsubsec . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (appendixsubsubsec . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (unnumberedsubsec . (:pixels-above-lines 12 :pixels-below-lines 6 :scale 1.2 :weight 700))
    (unnumberedsubsubsec . (:pixels-above-lines 6 :pixels-below-lines 6 :weight 700))
    (table . (:left-margin 50 :right-margin 25))
    (ftable . (:left-margin 50 :right-margin 25))
    (vtable . (:left-margin 50 :right-margin 25))
    (deftp . (:left-margin 50 :right-margin 50))
    (defcv . (:left-margin 50 :right-margin 50))
    (defivar . (:left-margin 50 :right-margin 50))
    (deftypeivar . (:left-margin 50 :right-margin 50))
    (defop . (:left-margin 50 :right-margin 50))
    (deftypeop . (:left-margin 50 :right-margin 50))
    (defmethod . (:left-margin 50 :right-margin 50))
    (deftypemethod . (:left-margin 50 :right-margin 50))
    (defopt . (:left-margin 50 :right-margin 50))
    (defvr . (:left-margin 50 :right-margin 50))
    (defvar . (:left-margin 50 :right-margin 50))
    (deftypevr . (:left-margin 50 :right-margin 50))
    (deftypevar . (:left-margin 50 :right-margin 50))
    (deffn . (:left-margin 50 :right-margin 50))
    (deftypefn . (:left-margin 50 :right-margin 50))
    (defspec . (:left-margin 50 :right-margin 50))
    (defmac . (:left-margin 50 :right-margin 50))
    (defun . (:left-margin 50 :right-margin 50))
    (deftypefun . (:left-margin 50 :right-margin 50))
    (quotation . (:left-margin 50 :right-margin 50 :scale 0.9))

    (bold . (:weight 700))
    (item . (:left-margin 25 :right-margin 25 :pixels-below-lines 0))
    (itemx . (:left-margin 25 :right-margin 25 :pixels-below-lines 0))
    (sample . (:font "Monospace"))
    (samp . (:font "Monospace"))
    (code . (:font "Monospace"))
    (kbd . (:font "Monospace" :style oblique))
    (key . (:font "Monospace" :variant small-caps))
    (var . (:font "Monospace" :style italic))
    (env . (:font "Monospace"))
    (file . (:font "Monospace"))
    (command . (:font "Monospace"))
    (option . (:font "Monospace"))
    (dfn . (:style italic :weight 700))
    (cite . (:style italic))
    (acro . (:scale 0.8333333333))
    (url . (:font "Monospace"))
    (email . (:font "Monospace"))
    (emph . (:style italic))
    (strong . (:weight 700))
    (sc . (:variant small-caps))
    ))
(for-each ;; Make the tags now so the priorities are correct
 (lambda (pair)
   (let ((tag (apply make <gtk-text-tag> :name (symbol->string (car pair)) (cdr pair))))
     (add tag-table tag)))
 tag-prop-alist)

(define (get-tag name)
  (lookup tag-table (symbol->string name)))

;; The only really tricky thing here is when to insert line breaks.
(define (default-stext->buffer-func buffer type tokens loop)
  (let* ((right (get-mark buffer "insert"))
         (left (create-mark buffer #f (get-iter-at-mark buffer right) #t))
         (name (if (list? type) (car type) type))
         (tag (get-tag name)))
    ;; If the token is of the (<identifier> <element>+) variety, we loop
    ;; on it, too.
    (if (list? type)
        (begin
          (loop type)
          (insert buffer (get-iter-at-mark buffer right) "\n")))
    (let inside-loop ((tokens tokens) (broken? #t))
      (if (not (null? tokens))
          (let ((token (car tokens)))
            (cond
             ((ignored? token)
              (inside-loop (cdr tokens) broken?))
             ((equal? token "\n")
              (cond
               (broken?
                ;; do nothing if it's already broken...
                (inside-loop (cdr tokens) #t))
               ((null? (cdr tokens))
                ;; ...or if we have a trailing newline
                (inside-loop (cdr tokens) #t))
               ((equal? (cadr tokens) "\n")
                ;; condense multiple newlines into one paragraph break
                (insert buffer (get-iter-at-mark buffer right) "\n")
                (inside-loop
                 (let find-tail ((tail (cddr tokens)))
                   (if (or (null? tail) (not (equal? (car tail) "\n")))
                       tail
                       (find-tail (cdr tail)))) ;; nasty!
                 #t))
               (else
                ;; replace single newline with space
                (insert buffer (get-iter-at-mark buffer right) " ")
                (inside-loop (cdr tokens) #f))))
             (else
              (if (and (not (inline? token)) (not (invisible? token)) (not broken?))
                  (insert buffer (get-iter-at-mark buffer right) "\n"))
              (if (string? token)
                  (insert buffer (get-iter-at-mark buffer right) token)
                  (loop token))
              (if (and (not (inline? token)) (not (invisible? token))
                       (not (null? (cdr tokens))))
                  (insert buffer (get-iter-at-mark buffer right) "\n"))
              (inside-loop (cdr tokens) (cond
                                         ((invisible? token) broken?)
                                         ((inline? token) #f)
                                         (else #t))))))))
    (if tag
        (apply-tag buffer tag (get-iter-at-mark buffer left)
                   (get-iter-at-mark buffer right)))
    (delete-mark buffer left)))

(define (example-stext->buffer-func buffer type tokens loop)
  (let* ((right (get-mark buffer "insert"))
         (left (create-mark buffer #f (get-iter-at-mark buffer right) #t))
         (name (if (list? type) (car type) type))
         (tag (or (get-tag name) (error "Unknown tag:" name))))
    (let inside-loop ((tokens tokens))
      (if (not (null? tokens))
          (let ((token (car tokens)))
            (if (string? token)
                (insert buffer (get-iter-at-mark buffer right) token)
                (loop token))
            (inside-loop (cdr tokens)))))
    (apply-tag buffer tag
               (get-iter-at-mark buffer left) (get-iter-at-mark buffer right))
    (delete-mark buffer left)))

(define (top-stext->buffer-func buffer type tokens loop)
  (let* ((right (get-mark buffer "insert"))
         (left (create-mark buffer #f (get-iter-at-mark buffer right) #t)))
    (for-each
     (lambda (token)
       (if (string? token)
           (insert buffer (get-iter-at-mark buffer right) token)
           (loop token)))
     (cdr type))
    (insert buffer (get-iter-at-mark buffer right) "\n")
    (apply-tag buffer (get-tag 'titlefont)
               (get-iter-at-mark buffer left) (get-iter-at-mark buffer right))
    (delete-mark buffer left))
  (default-stext->buffer-func buffer (car type) tokens loop))

(define-class <stext-ref-tag> (<gtk-text-tag>)
  (type :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                      :flags (read write)))
  (node-name :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                           :flags (read write)))
  (info-ref-name :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                               :flags (read write)))
  (section-name :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                              :flags (read write)))
  (info-file :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                           :flags (read write)))
  (manual :param-spec `(,<gparam-boxed> :boxed-type ,gtype:gboxed-scm
                                        :flags (read write))))

(define-method (initialize (obj <stext-ref-tag>) initargs)
  (next-method)
  (set obj 'foreground "blue")
  (set obj 'underline 'single)
  (connect obj 'event
           (lambda (tag object event iter)
             (if (eq? (gdk-event:type event) 'button-press)
                 (run-hook stext-buffer-xref-activated-hook
                           (slot-ref obj 'node-name)
                           (slot-ref obj 'manual)))
             #f)))

(define-class <stext-uref-tag> (<gtk-text-tag>)
  (uri :param-spec `(,<gparam-string> :flags (read write))))

(define-method (initialize (obj <stext-uref-tag>) initargs)
  (next-method)
  (set obj 'foreground "blue")
  (set obj 'underline 'single)
  ;; Unfortunately, we can't make the cursor go to a hand when the mouse
  ;; is over a tag, because the tag has no window. The proper way to do
  ;; this is to put a GtkLabel in the text flow. The future!
  (connect obj 'event
           (lambda (tag object event iter)
             (if (eq? (gdk-event:type event) 'button-press)
                 (begin
                   (format #t "\nshowing ~A in new window\n" (slot-ref obj 'uri))
                   (gnome-url-show (slot-ref obj 'uri) #f)))
             #f)))

(define (ref-stext->buffer-func buffer type tokens loop)
  (let* ((right (get-mark buffer "insert"))
         (left #f)
         (node-name (or (argument-ref tokens 0) (error "No node name:" type tokens)))
         (info-ref-name (or (argument-ref tokens 1) node-name))
         (section-name (or (argument-ref tokens 2) node-name))
         (info-file (argument-ref tokens 3))
         (manual (argument-ref tokens 4))
         (tag (make <stext-ref-tag>
                :type type :node-name node-name :info-ref-name info-ref-name
                :section-name section-name :info-file info-file :manual manual)))
    (add tag-table tag)
    (case type
      ((xref)
       (insert buffer (get-iter-at-mark buffer right)
               "See the section on "))
      ((pxref)
       (insert buffer (get-iter-at-mark buffer right)
               "see the section on ")))
    (set! left (create-mark buffer #f (get-iter-at-mark buffer right) #t))
    (insert buffer (get-iter-at-mark buffer right)
            section-name)
    (apply-tag buffer tag
               (get-iter-at-mark buffer left) (get-iter-at-mark buffer right))
    (delete-mark buffer left)))

(define (uref-stext->buffer-func buffer type tokens loop)
  (let* ((right (get-mark buffer "insert"))
         (left #f)
         (uri (or (argument-ref tokens 0) (error "No URI:" type tokens)))
         (title (or (argument-ref tokens 1) uri))
         (tag (make <stext-uref-tag> :uri uri)))
    (add tag-table tag)
    (set! left (create-mark buffer #f (get-iter-at-mark buffer right) #t))
    (insert buffer (get-iter-at-mark buffer right)
            title)
    (apply-tag buffer tag
               (get-iter-at-mark buffer left) (get-iter-at-mark buffer right))
    (delete-mark buffer left)))

(define (node-stext->buffer-func buffer type tokens loop)
  (let* ((name (or (argument-ref tokens 0) (error "No node name:" type tokens))))
    (create-mark buffer (string-append "node-" name)
                 (get-end-iter buffer) #t)))

;; For nesting enums and itemizes
(define nested-depth (make-fluid))
(fluid-set! nested-depth 10)

(define (nested-stext->buffer-func buffer type tokens loop)
  ;; Preprocess tokens into a list of %items. Each %item can be
  ;; processed by the default function, then it gets a tag applied to
  ;; set its indentation. Shallow depth tags are added to the table
  ;; before deep ones so that priorities work out.
  (define (preprocess)
    (let inner-loop ((out '())
                     (%item #f)
                     (in tokens))
      (cond
       ((null? in)
        (reverse (if %item
                     (cons (reverse %item) out)
                     out)))
       ((list? (car in))
        (if (eq? (caar in) 'item)
            (inner-loop (if %item
                            (cons (reverse %item) out)
                            out)
                        (reverse (cons '%item (cdar in)))
                        (cdr in))
            (inner-loop out
                        (cons (car in)
                              %item)
                        (cdr in))))
       (else
        (inner-loop out
                    ;; Same as above
                    (if %item (cons (car in) %item) %item)
                    (cdr in))))))
  (define tag-alist '())
  (define (get-tag depth)
    (or (assq-ref tag-alist depth)
        (begin
          (let ((tag (make <gtk-text-tag>
                       :name #f
                       :left-margin depth)))
            (add tag-table tag)
            (set! tag-alist (acons depth tag tag-alist))
            tag))))
  (with-fluids ((nested-depth (+ 15 (fluid-ref nested-depth))))
    (let* ((tag (get-tag (fluid-ref nested-depth)))
           (right (get-mark buffer "insert"))
           (left (create-mark buffer #f (get-iter-at-mark buffer right) #t)))
      (for-each
       (lambda (%item)
         ;; FIXME: Insert a bullet, minus, number, or letter here
         (default-stext->buffer-func buffer (car %item) (cdr %item) loop)
         (insert buffer (get-iter-at-mark buffer right)
                 "\n"))
       (preprocess))
      (apply-tag buffer tag
                 (get-iter-at-mark buffer left) (get-iter-at-mark buffer right)))))

(define (def-stext->buffer-func buffer type tokens loop)
  (define (list-join l infix)
    (let loop ((ret '()) (l l))
      (cond
       ((null? l)
        (reverse ret))
       ((null? (cdr l))
        (loop (cons (car l) ret) (cdr l)))
       (else
        (loop (cons* infix (car l) ret) (cdr l))))))
  (define (split-args)
    (let loop ((args tokens) (ret '()))
      (cond
       ((null? args)
        (reverse ret))
       ((and (string? (car args))
             (string-index (car args) #\space))
        (loop (append (string-split (car args) #\space) (cdr args)) ret))
       (else
        (loop (cdr args) (cons (car args) ret))))))
  ;; We only deal with the case where the head is just a symbol. This
  ;; whole infrastructure is becoming hackish.
  (cond
   ((list? type)
    ;; Make sure a list or a table in a definition is displayed
    ;; properly.
    (with-fluids ((nested-depth 50))
      (default-stext->buffer-func buffer type tokens loop)))
   (else
    (let* ((right (get-mark buffer "insert"))
           (left (create-mark buffer #f (get-iter-at-mark buffer right) #t))
           (args (split-args))
           (def-line-tag (or (get-tag 'def-line)
                             (let ((tabs (pango-tab-array-new 1 #t)))
                               (pango-tab-array-set-tab tabs 0 'left 325)
                               ((lambda (x)
                                  (add tag-table x)
                                  x)
                                (make <gtk-text-tag>
                                  :name "def-line"
                                  :tabs tabs
                                  :left-margin 25 :right-margin 25
                                  :pixels-below-lines 0 :pixels-above-lines 6))))))
      (case type
        ((deftp)
         (loop `(code (bold ,(list-ref args 1))))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\tab #\[))
         (loop `(text ,(list-ref args 0)))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\])))
        ((defun)
         (loop `(code (bold ,(list-ref args 0))))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\space))
         (loop (cons 'var (list-join (cdr args) " ")))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\tab))
         (loop '(text "[Function]")))
        ((defspec)
         (loop `(code (bold ,(list-ref args 0))))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\space))
         (loop (cons 'text (list-join (cdr args) " ")))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\tab))
         (loop '(text "[Special Form]")))
        ((defvar)
         (loop `(code (bold ,(list-ref args 0))))
         (insert buffer (get-iter-at-mark buffer right)
                 (string #\tab))
         (loop '(text "[Variable]")))
        (else
         (loop (cons 'text (list-join args " ")))))
;     (defcv)
;     (defivar)
;     (deftypeivar)
;     (defop)
;     (deftypeop)
;     (defmethod)
;     (deftypemethod)
;     (defopt)
;     (defvr)
;     (deftypevr)
;     (deftypevar)
;     (deffn)
;     (deftypefn)
;     (defmac)
;     (deftypefun)))
        (apply-tag buffer def-line-tag
                   (get-iter-at-mark buffer left)
                   (get-iter-at-mark buffer right))
        (delete-mark buffer left)))))

(define stext->buffer-func-alist
  `((copyright . ,(lambda (buffer . args)
                    (insert buffer (get-end-iter buffer) (string #\302 #\251))))
    (results . ,(lambda (buffer . args)
                  (insert buffer (get-end-iter buffer) (string #\342 #\207 #\222))))
    (example . ,example-stext->buffer-func)
    (smallexample . ,example-stext->buffer-func)
    (stext . ,top-stext->buffer-func)
    (xref . ,ref-stext->buffer-func)
    (ref . ,ref-stext->buffer-func)
    (pxref . ,ref-stext->buffer-func)
    (uref . ,uref-stext->buffer-func)
    (node . ,node-stext->buffer-func)
    (anchor . ,node-stext->buffer-func)
    ;(table . Fix me :)
    (itemize . ,nested-stext->buffer-func)
    (enumerate . ,nested-stext->buffer-func)
    (deftp . ,def-stext->buffer-func)
    (defcv . ,def-stext->buffer-func)
    (defivar . ,def-stext->buffer-func)
    (deftypeivar . ,def-stext->buffer-func)
    (defop . ,def-stext->buffer-func)
    (deftypeop . ,def-stext->buffer-func)
    (defmethod . ,def-stext->buffer-func)
    (deftypemethod . ,def-stext->buffer-func)
    (defopt . ,def-stext->buffer-func)
    (defvr . ,def-stext->buffer-func)
    (defvar . ,def-stext->buffer-func)
    (deftypevr . ,def-stext->buffer-func)
    (deftypevar . ,def-stext->buffer-func)
    (deffn . ,def-stext->buffer-func)
    (deftypefn . ,def-stext->buffer-func)
    (defmac . ,def-stext->buffer-func)
    (defspec . ,def-stext->buffer-func)
    (defun . ,def-stext->buffer-func)
    (deftypefun . ,def-stext->buffer-func)))

(define ignore-list
  '(page setfilename setchapternewpage iftex ifhtml ifplaintext ifxml sp vskip
    menu ignore syncodeindex))
(define (ignored? token)
  (and (list? token) (memq (car token) ignore-list)))
(for-each
 (lambda (sym)
   (set! stext->buffer-func-alist (acons sym noop stext->buffer-func-alist)))
 ignore-list)

(define invisible-list
  ;; Implicitly contains the ignore-list
  '(node anchor))
(define (invisible? token)
  (and (list? token)
       (or (memq (car token) ignore-list)
           (memq (car token) invisible-list))))

(define (get-stext->buffer-func head)
  (or (and (symbol? head) (assq-ref stext->buffer-func-alist head))
      (and (list? head) (get-stext->buffer-func (car head)))
      default-stext->buffer-func))

(define (stext->gtk-text-buffer stext)
  "Format @var{stext} into a @code{<gtk-text-buffer>}."
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
  ;; stexts start with their name, but we ignore it, counting on a
  ;; chapter or something to appear at the top of the text
  (let ((buffer (make <gtk-text-buffer> :tag-table tag-table)))
    (let loop ((text (cons 'text (cdr (filter stext)))))
      ((get-stext->buffer-func (car text)) buffer (car text) (cdr text) loop))
    buffer))

;;; arch-tag: 8d9cb62d-229e-4859-9954-cc9cd99f95ff
