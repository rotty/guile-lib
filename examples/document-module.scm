#! /usr/bin/guile -s
!#

(read-set! keywords 'prefix)

(use-modules (srfi srfi-13)
             (srfi srfi-1)
             (sxml simple)
             (texinfo reflection)
             (texinfo html)
             (texinfo plain-text))

(define format 'text)
(define module-name #f)

(define (usage)
  (format #t "Usage: document-module.scm {--html,--texinfo,--text} MODULE-NAME\n")
  (exit 1))

(case (length (program-arguments))
  ((2)
   (set! module-name
         (call-with-input-string (cadr (program-arguments)) read)))
  ((3)
   (let ((sformat (cadr (program-arguments))))
     (cond
      ((string=? sformat "--html") (set! format 'html))
      ((string=? sformat "--texinfo") (set! format 'texinfo))
      ((string=? sformat "--text") (set! format 'text))
      (else (usage)))
     (set! module-name
           (call-with-input-string (caddr (program-arguments)) read))))
  (else (usage)))

(define module-docs (module-stexi-documentation module-name))

(case format
  ((text)
   (display (stexi->plain-text module-docs)))
  ((html)
   (sxml->xml (stexi->shtml module-docs)))
  ((texinfo)
   (error "not yet implemented")))

;;; arch-tag: 0529d121-eeb0-4f8c-8046-12f2020763ab
