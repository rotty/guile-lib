;; About the package
(define *name* "Guile Library")
(define *description* "Common modules for Guile Scheme")
(define *version* "0.1.3")
(define *updated* "12 November 2006")
(define *authors*
  '(("Andy Wingo" . "wingo at pobox.com")
    ("Richard Todd" . "richardt at vzavenue.net")))

;; Copying the documentation
(define *copyright-holder* "Andy Wingo, Richard Todd")
(define *years* '(2003 2004 2005 2006))
(define *permissions*
  "Permission is granted to copy, distribute and/or
modify this document under the terms of the GNU Free
Documentation License, Version 1.2 or any later version published
y the Free Software Foundation; with no Invariant Sections, no
Front-Cover Texts, and no Back-Cover Texts. A copy of the license
is included in the section entitled \"GNU Free Documentation
License\".")

;; Texinfo info
(define *texinfo-basename* "guile-library")
(define *texinfo-category* "The Algorithmic Language Scheme")
(define *extra-texinfo-menu-entries*
  '(("Copying This Manual")
    ("Concept Index")
    ("Function Index")))
(define *texinfo-epilogue*
  `((node (% (name "Copying This Manual")))
    (appendix "Copying This Manual")
    "This manual is covered under the GNU Free Documentation "
    "License. A copy of the FDL is provided here."
    (menu
     "* GNU Free Documentation License::  License for copying this manual")
    (include "fdl.texi")
    (node (% (name "Concept Index")))
    (unnumbered "Concept Index")
    (printindex (% (type "cp")))
    (node (% (name "Function Index")))
    (unnumbered "Function Index")
    (printindex (% (type "fn")))))

;; HTML foo
(define *html-relative-root-path* "../")

;; The modules to document
(define *modules*
  '(((container nodal-tree)
     "A tree consisting of nodes with attributes")
    ((container delay-tree)
     "A nodal tree with lazily evaluated fields")
    ((debugging time)
     ;; FIXME: also in ice-9, this needs to go.
     "A simple macro to time the execution of an expression")
    ((scheme documentation)
     "Macros to define different kinds of variables with documentation")
    ((texinfo)
     "Parse texinfo files or fragments into " (code "stexi") ", a "
     "scheme representation")
    ((texinfo html)
     "Transform " (code "stexi") " into HTML")
    ((texinfo indexing)
     "Extract an index from a piece of " (code "stexi"))
    ((texinfo nodal-tree)
     "Chunk a " (code "stexi") " document into pieces")
    ((texinfo plain-text)
     "Render " (code "stexi") " as plain text")
    ((texinfo serialize)
     "Render " (code "stexi") " as texinfo")
    ((texinfo reflection)
     "Enable texinfo across Guile's help system")
    ;; link to literate programming article
    ((sxml simple)
     "Convenient XML parsing and serializing")
    ;; do something about "http://neilvandyke.org/htmlprag/"
    ((htmlprag)
     "Neil Van Dyke's permissive (\"pragmatic\") HTML parser")
    ;; "http://ssax.sourceforge.net/"
    ((sxml ssax)
     "Functional-style XML parsing for Scheme")
    ((sxml xpath)
     "XPath for SXML")
    ((sxml transform)
     "A higher-order SXML transformation operator, "
     (code "pre-post-order"))
    ((sxml apply-templates)
     "A more XSLT-like approach to SXML transformations")
    ((sxml ssax input-parse)
     "The SSAX tokenizer, optimized for Guile")
    ((debugging assert)
     "Helpful assert macro")
    ((statprof)
     "Statistical profiler")
    ((io string)
     "SLIB's IO routines dealing with strings")
    ((scheme session)
     "A more featureful " (code "(ice-9 session)"))))

(define *module-sources*
  '(((sxml ssax) . "http://ssax.sourceforge.net/")
    ((sxml xpath) . "http://ssax.sourceforge.net/")
    ((sxml transform) . "http://ssax.sourceforge.net/")
    ((sxml apply-templates) . "http://ssax.sourceforge.net/")
    ((sxml ssax input-parse) . "http://ssax.sourceforge.net/")
    ((htmlprag) . "http://neilvandyke.org/htmlprag/")))

;; arch-tag: e493ad42-ad58-451c-a2d6-b17ba6c1d1d0
