(define-module (text structured named-tree-nodes)
  :use-module (text structured)
  :use-module (text structured plain-text)
  :use-module (scheme documentation)
  :use-module (oop goops)
  :export (stext->named-tree-nodes <named-tree-node>))

(define-class-with-docs <named-tree-node> ()
  "A class for chunking a document into hierarchical sections. Probably
belongs in a general tree library."
  (name
   #:init-keyword #:name)
  (value
   #:init-keyword #:value
   #:init-value #f)
  (parent
   #:init-keyword #:parent
   #:init-value #f)
  (children
   #:init-keyword #:children
   #:init-value #f))

(define (stext->named-tree-nodes stext max-depth . args)
  "Break @var{stext} into a tree of @code{<named-tree-node>}s. Only break
until sectioning identifiers of depth @var{max-depth}.

You may pass @code{#:class @var{class}} at the end of the arguments to
specify a subclass of @code{<named-tree-node>} to use."

  (let ((<node> (or (kw-arg-ref args #:class) <named-tree-node>))
        (node? (lambda (tok)
                 (and (list? tok) (eq? (car tok) 'node))))
        (chunking-section? (lambda (tok)
                             (and (list? tok) (name->depth (car tok) max-depth)))))

    (define (stext->help-nodes-helper stext parent depth)
      (let ((node (make <node>
                    #:parent parent
                    #:name (stext->plain-text-title (cons 'text (cdar stext))))))
        (if parent
            (slot-set! parent 'children
                       (cond
                        ((slot-ref parent 'children)
                         => (lambda (siblings) (append siblings (list node))))
                        (else (list node)))))
        (let loop ((current (list (cadr stext) (car stext)))
                   (input (cddr stext))) ;; skip the stext and the first
					 ;; node to avoid re-detection
          (if (null? input)
              (begin
                (slot-set! node 'value (reverse current))
                ;; return the node for the top of the file
                (let find-parent ((node node))
                  (let ((parent (slot-ref node 'parent)))
                    (if parent
                        (find-parent parent)
                        node))))
              (if (and (node? (car input))
                       (not (null? (cdr input)))
                       ;; HACK to allow for an intervening newline
                       (not (null? (cddr input)))
                       (chunking-section? (caddr input)))
                  (let ((new-parent node)
                        (new-depth (name->depth (caaddr input) max-depth)))
                    (do ((i (- depth new-depth) (1- i)))
                        ((< i 0) #f)
                      (set! new-parent (slot-ref new-parent 'parent)))
                    (slot-set! node 'value (reverse current))
                    (stext->help-nodes-helper
                     (cons* (cons 'stext (cdaddr input)) input)
                     new-parent new-depth))
                  (loop (cons (car input) current) (cdr input)))))))

    (stext->help-nodes-helper stext #f 0)))

;;; arch-tag: 67d202cd-1c94-429c-bc2a-3637580c3445
