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
;;Structured text is essentially texinfo expressed with s-expressions.
;;Its syntax is:
;;
;;@table @code
;;@item <structured-text>
;; --> (<header> <element>+)
;;@item <header>
;; --> ('stext <title>)
;;@item <title>
;; --> <string>
;;@item <element>
;; --> <string> | (<identifier> <element>+)
;;@item <identifier>
;; --> <identifier-name> | (<identifier-name> <element>+)
;;@item <identifier-name>
;; --> <symbol>
;;@end table
;;
;;The set of valid @code{<identifier-name>}s is the same as the set of
;;@code{@@}-commands in texinfo. If an @code{<identifier>} is a list,
;;the @code{cdr} of the identifier are considered the arguments to the
;;identifier. Note however that "environments" in texinfo are expressed
;;in structured text as a single @code{<element>}, so
;;
;;@example
;;@@quotation
;;quotation foo
;;@@end quotation
;;@end example
;;
;;would translate to
;;
;;@example
;;(quotation "quotation foo")
;;@end example
;;
;;Some renderers treat whitespace as significant (for example,
;;@code{example}), while others will treat it specially: for example,
;;many formats break paragraphs at two or more line breaks, while single
;;line breaks are the same as spaces. For that reason, linebreaks should
;;exist as elements on their own.
;;
;;Explain 'text here.
;;        
;;; Code:

(define-module (text structured)
  :use-module (scheme documentation)
  :use-module (oop goops)
  :export (inline-list inline?
           argument-ref
           name->depth))

(define-with-docs inline-list
  "A list of all stext @code{<identifier-name>}s that exist inline with
other text."
  '(bold copyright sample samp code kbd key var env file command option
    dfn cite acro url email emph strong sample sc set value ref xref
    pxref uref text node anchor))
(define (inline? token)
  "A predicate to see if @var{token} is inline or not."
  (or (string? token) (memq (car token) inline-list)))

(define (argument-ref args n)
  "Access the @var{n}th comma-delimited member of @var{args}, or #f if
not present."
  (let lp ((args (string-split (apply string-append args) #\,))
           (n n))
    (cond
     ((null? args) #f)
     ((zero? n) (if (equal? (car args) "") #f (car args)))
     (else (lp (cdr args) (1- n))))))

(define numbered-list
  '(#f chapter section subsection subsubsection))
(define unnumbered-list
  '(top unnumbered unnumberedsec unnumberedsubsec unnumberedsubsubsec))
(define appendix-list
  '(#f appendix appendixsec appendixsubsec appendixsubsubsec))
(define (name->depth name max-depth)
  "If an <identifier-name> @var{name} is actually a sectioning
identifier, return the depth of the identifier, starting at 0 for
@code{top}. Otherwise return #f."
  (let ((depth (or (list-index numbered-list name)
                   ;;(list-index unnumbered-list name)
                   (list-index appendix-list name))))
    (if (and depth (> depth max-depth))
        #f
        depth)))

;;; arch-tag: 36129bb0-5c2e-47d2-96d4-eab867b73c3b
