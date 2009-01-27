#!/bin/sh
# -*- scheme -*-
exec guile --debug -s $0 "$@"
!#

;; make-html.scm -- document a set of scheme modules as HTML
;; Copyright (C) 2006,2007,2009  Andy Wingo <wingo at pobox dot com>

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

(use-modules (texinfo)
             (texinfo reflection)
             (texinfo html)
             (sxml simple)
             (sxml transform)
             ((srfi srfi-13) :select (string-join)))

(define (makedirs path)
  (let loop ((path ".") (components (string-split path #\/)))
    (if (not (null? components))
        (let ((sub-path (string-append path "/" (car components))))
          (if (or (not (file-exists? sub-path))
                  (not (file-is-directory? sub-path)))
              (mkdir sub-path))
          (loop sub-path (cdr components))))))

(define (wrap-html title root-path scm-url body)
  `(html (@ (xmlns "http://www.w3.org/1999/xhtml"))
    (head
     (title ,title)
     (meta (@ (name "Generator")
              (content "The Guile SXML Toolkit")))
     (style (@ (type "text/css") (media "screen"))
       "@import url("
       ,(string-append root-path "base.css")
       ");"))
    (body
     (div (@ (id "body"))
          (h1 (@ (id "heading"))
              (a (@ (href ,root-path)) ,*name*))
          (div (@ (id "text"))
               (h2 (@ (class "centered")) ,title)
               ,@body)
          (div (@ (id "footer"))
               "powered by sxml")))))

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"))

(define (module->str scm)
  (call-with-output-string (lambda (p) (display scm p))))
(define (module->ustr scm)
  (string-append (string-join (map symbol->string scm) ".") "/"))
(define (extra-entry->ustr str)
  (string-append (string-join (string-split str #\space) ".") "/"))
(define (script->ustr str)
  (string-append str "/"))

(define (make-html-index)
  (with-output-to-file "html/index.html"
    (lambda ()
      (display xhtml-doctype)
      (sxml->xml
       (pre-post-order
        (stexi->shtml
         `(texinfo
           (% (title "unused"))
           ,@(cdr
              (package-stexi-standard-copying
               *name* *version* *updated* *years* *copyright-holder*
               *permissions*))
           (table
            (% (formatter (bold)))
            ,@(map
               (lambda (module description)
                 `(entry
                   (% (heading
                       (uref (% (url ,(module->ustr module))
                                (title ,(module->str module))))))
                   ,@description))
               (map car *modules*) (map cdr *modules*))
            ,@(map
               (lambda (script description)
                 `(entry
                   (% (heading
                       (uref (% (url ,(script->ustr script))
                                (title ,script)))))
                   ,@description))
               (map basename (map car *scripts*)) (map cdr *scripts*))
            ,@(map
               (lambda (args)
                 (apply
                  (lambda (filename label . description)
                    `(entry
                      (% (heading
                          (uref (% (url ,(extra-entry->ustr label))
                                   (title ,label)))))
                      ,@description))
                  args))
               *extra-html-entry-files*))))
        `((html . ,(lambda (tag attrs head body)
                     (wrap-html
                      *name*
                      *html-relative-root-path*
                      "index.scm"
                      (cdr body)))) ;; cdr past the 'body tag
          (*text* . ,(lambda (tag text) text))
          (*default* . ,(lambda args args))))))))

(define (append-map proc l)
  (let lp ((in l))
    (if (null? in)
        '()
        (append (proc (car in)) (lp (cdr in))))))
(define (string-split* s . chars)
  (let lp ((chars (cdr chars)) (out (string-split s (car chars))))
    (if (null? chars)
        out
        (append-map
         (lambda (x)
           (lp (cdr chars) (string-split x (car chars))))
         out))))
(define (negate pred)
  (lambda (x) (not (pred x))))

(define (resolve-ref node manual)
  (and (or (not manual) (string=? manual *name*))
       (let* ((split (filter (negate string-null?)
                             (string-split* node #\space #\newline)))
              (symbols (map string->symbol split))
              (last (car (last-pair symbols)))
              (except-last (reverse (cdr (reverse symbols)))))
         (cond
          ((member symbols (map car *modules*))
           (string-append "../" (module->ustr symbols)))
          ((member except-last (map car *modules*))
           (string-append "../" (module->ustr except-last)
                          "#" (string-join split "-")))
          ((member node (map car *scripts*))
           (string-append "../" (script->ustr node)))
          ((member node (map cadr *extra-html-entry-files*))
           (string-append "../" (extra-entry->ustr node)))
          (else
           (warn "dangling reference" split)
           #f)))))
(add-ref-resolver! resolve-ref)
      
(define (make-html-module-pages)
  (for-each
   (lambda (module)
     (let* ((ustr (string-append "./html/" (module->ustr module)))
            (port (begin
                    (makedirs ustr)
                    (open-output-file (string-append ustr "index.html")))))
       (display xhtml-doctype port)
       (sxml->xml
        (pre-post-order
         (stexi->shtml (module-stexi-documentation module))
         `((html . ,(lambda (tag attrs head body)
                      (wrap-html
                       (module->str module)
                       (string-append "../" *html-relative-root-path*)
                       "../index.scm"
                       (cdr body)))) ;; cdr past the 'body tag
           (*text* . ,(lambda (tag text) text))
           (*default* . ,(lambda args args))))
        port)))
   (map car *modules*)))

(define (make-html-script-pages)
  (for-each
   (lambda (scriptpath)
     (let* ((script (basename scriptpath))
            (ustr (string-append "./html/" (script->ustr script)))
            (port (begin
                    (makedirs ustr)
                    (open-output-file (string-append ustr "index.html")))))
       (display xhtml-doctype port)
       (sxml->xml
        (pre-post-order
         (stexi->shtml (script-stexi-documentation scriptpath))
         `((html . ,(lambda (tag attrs head body)
                      (wrap-html
                       script
                       (string-append "../" *html-relative-root-path*)
                       "../index.scm"
                       (cdr body)))) ;; cdr past the 'body tag
           (*text* . ,(lambda (tag text) text))
           (*default* . ,(lambda args args))))
        port)))
   (map car *scripts*)))

(define (make-html-extra-pages)
  (for-each
   (lambda (filename label)
     (let* ((ustr (string-append "./html/" (extra-entry->ustr label)))
            (port (begin
                    (makedirs ustr)
                    (open-output-file (string-append ustr "index.html")))))
       (display xhtml-doctype port)
       (sxml->xml
        (pre-post-order
         (stexi->shtml (call-with-input-file filename texi-fragment->stexi))
         `((html . ,(lambda (tag attrs head body)
                      (wrap-html
                       label
                       (string-append "../" *html-relative-root-path*)
                       "../index.scm"
                       (cdr body)))) ;; cdr past the 'body tag
           (*text* . ,(lambda (tag text) text))
           (*default* . ,(lambda args args))))
        port)))
   (map car *extra-html-entry-files*)
   (map cadr *extra-html-entry-files*)))

(define (main config-scm)
  (load config-scm)
  (makedirs "./html")
  (make-html-index)
  (make-html-module-pages)
  (make-html-script-pages)
  (make-html-extra-pages))

(apply main (cdr (command-line)))
