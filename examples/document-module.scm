#! /usr/bin/guile -s
!#

;; document-module.scm -- document a set of scheme modules as HTML
;; Copyright (C) 2004,2009  Andy Wingo <wingo at pobox dot com>

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

(read-set! keywords 'prefix)

(use-modules (srfi srfi-13)
             (srfi srfi-1)
             (sxml simple)
             (texinfo reflection)
             (texinfo html)
             (texinfo serialize)
             (texinfo plain-text))

(define output-format 'text)
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
      ((string=? sformat "--html") (set! output-format 'html))
      ((string=? sformat "--texinfo") (set! output-format 'texinfo))
      ((string=? sformat "--text") (set! output-format 'text))
      (else (usage)))
     (set! module-name
           (call-with-input-string (caddr (program-arguments)) read))))
  (else (usage)))

(define module-docs (module-stexi-documentation module-name))

(case output-format
  ((text)
   (display (stexi->plain-text module-docs)))
  ((html)
   (sxml->xml (stexi->shtml module-docs)))
  ((texinfo)
   (display (stexi->texi module-docs))))

;;; arch-tag: 0529d121-eeb0-4f8c-8046-12f2020763ab
