;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001,2002 Oleg Kiselyov <oleg at pobox dot com>

;; This file is based on SSAX's SSAX.scm.

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
;; Texinfo processing in scheme.
;;
;; This module parses texinfo into SXML. TeX does a great job, and is
;; quite extensible, so it doesn't make sense to compete with it in
;; print output. Although makeinfo works well for info, its output in
;; other formats is not very customizable, and the program is not
;; extensible as a whole. This module aims to provide an extensible
;; framework for texinfo processing that integrates texinfo into the
;; constellation of SXML processing tools.
;;
;; Consider the following texinfo fragment:
;;
;; @deffn Primitive set-car! pair value
;; This function...
;; @end deffn
;;
;; Logically, the category (Primitive), name (set-car!), and arguments
;; (pair value) are ``attributes'' of the deffn, with the description as
;; the content. However, texinfo allows for @-commands within the
;; arguments to an environment, like @deffn, which means that texinfo
;; ``attributes'' are PCDATA. XML attributes, on the other hand, are
;; CDATA. For this reason, ``attributes'' of texinfo @-commands are
;; called ``arguments'', and are grouped under the special element, `%'.
;;
;; Because `%' is not a valid NCName, stexinfo is a superset of SXML. In
;; the interests of interoperability, this module provides a conversion
;; function to replace the `%' with `texinfo-arguments'.
;;
;;; Code:

;; Comparison to xml output of texinfo (which is rather undocumented):
;;  Doesn't conform to texinfo dtd
;;  No DTD at all, in fact :-/
;;  Actually outputs valid xml, after transforming %
;;  Slower (although with caching the SXML that problem can go away)
;;  Doesn't parse menus (although menus are shite)
;;  Args go in a dedicated element, FBOFW
;;  Definitions are handled a lot better
;;  Does parse comments
;;  Outputs only significant line breaks (a biggie!)
;;  Examples contain a single para, fwiw
;;  Nodes are treated as anchors, rather than content organizers (a biggie)
;;    (more book-like, less info-like)

;; TODO
;; Update the comments
;; Integration: HTML, chunkers, help, indexing, plain text, toc

(define-module (sxml texinfo)
  #:use-module (sxml transform)
  #:use-module (text lex-simple)
  #:use-module (io string)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)
  #:export (texi->stexi stexi->sxml
            call-with-file-and-dir))

(define (parser-error port message . rest)
  (apply error port message rest))

(define (call-with-file-and-dir filename proc)
  (let ((current-dir (getcwd)))
    (dynamic-wind
        (lambda () (chdir (dirname filename)))
        (lambda ()
          (call-with-input-file (basename filename) proc))
        (lambda () (chdir current-dir)))))

; Like let* but allowing for multiple-value bindings
(define-macro (let*-values bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply
       (lambda (vars initializer)
	 (let ((cont 
		(cons 'let*-values
		      (cons (cdr bindings) body))))
	   (cond
	    ((not (pair? vars))		; regular let case, a single var
	     `(let ((,vars ,initializer)) ,cont))
	    ((null? (cdr vars))		; single var, see the prev case
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(call-with-values (lambda () ,initializer)
	      (lambda ,vars ,cont))))))
       (car bindings))))

;========================================================================
;				Data Types

; TAG-KIND
;	A symbol 'EMPTY, 'START, 'LINE, 'END

; TAG-NAME
;	A a single symbol.

; ELEM-CONTENT-MODEL
;	A symbol:
;	ENVIRON	  - The tag is an environment tag, expect `@end foo'.
;	EMPTY-COMMAND - no content, and no @end is coming
;	EOL-ARGS  - Arguments := CDATA to end of line
;	EOL-TEXT  - Arguments end at end of line, but are parsed
;       INLINE-ARGS - Attribute is inline
;       INLINE-TEXT - Attribute is inline

; STR-HANDLER
;	A procedure of three arguments: STRING1 STRING2 SEED
;	returning a new SEED
;	The procedure is supposed to handle a chunk of character data
;	STRING1 followed by a chunk of character data STRING2.
;	STRING2 is a short string, often "\n" and even ""

; INCLUDES
;	A stack of strings indicating which files are being included.
;       An attempt to include a file in this list will result in an
;       error: violation of nonrecursion.

; TOKEN -- a record

; This record represents a markup, which is either a stop or an end tag.
;
;	kind -- a TAG-KIND
;	head -- a TAG-NAME. Only 'START For xml-tokens of kinds 'COMMENT and
;		'CDSECT, the head is #f
;
; For example,
;	<P>     => kind='START, head='P
;	</P>    => kind='END, head='P
;	@dots{} => kind='EMPTY-EL, head='BR
; 
; Character references are not represented by xml-tokens as these references
; are transparently resolved into the corresponding characters.

(define (make-token kind head) (cons kind head))
(define token? pair?)
(define token-kind car)
(define token-head cdr)

;; The % is for arguments
(define (space-significant? command)
  (memq command
        '(example smallexample verbatim lisp smalllisp menu %)))

;; Like a DTD for texinfo
(define (command-spec command)
  (or (assq command texinfo-command-specs)
      (parser-error #f "Unknown command" command)))

(define (inline-content? content)
  (or (eq? content 'INLINE-TEXT) (eq? content 'INLINE-ARGS)))

(define texinfo-command-specs
  ;; name content-model args
  ;; args are there only for
  ;;   content-model := INLINE-ARGS, EOL-ARGS, ENVIRON, TABLE-ENVIRON
  ;; although ENTRY has args, just for the sake of the dtd
  ;;
  ;; args has the same format as the formals for a lambda
  ;; #f for args means no args (expect EOL or {}),
  ;; #t means put args as leaves of element
  '(;; Special commands
    (include            #f) ;; this is a low-level token
    (para               PARAGRAPH)
    (item               ITEM)
    (entry              ENTRY . heading)
    (noindent           EMPTY-COMMAND)

    ;; Inline text commands
    (bold               INLINE-TEXT)
    (sample             INLINE-TEXT)
    (samp               INLINE-TEXT)
    (code               INLINE-TEXT)
    (kbd                INLINE-TEXT)
    (key                INLINE-TEXT)
    (var                INLINE-TEXT)
    (env                INLINE-TEXT)
    (file               INLINE-TEXT)
    (command            INLINE-TEXT)
    (option             INLINE-TEXT)
    (dfn                INLINE-TEXT)
    (cite               INLINE-TEXT)
    (acro               INLINE-TEXT)
    (url                INLINE-TEXT)
    (email              INLINE-TEXT)
    (emph               INLINE-TEXT)
    (strong             INLINE-TEXT)
    (sample             INLINE-TEXT)
    (sc                 INLINE-TEXT)
    (titlefont          INLINE-TEXT)
    (*braces*           INLINE-TEXT) ;; FIXME: make me irrelevant
    (asis               INLINE-TEXT)

    ;; Inline args commands
    (value              INLINE-ARGS . (key))
    (ref                INLINE-ARGS . (node #:opt name section info-file manual))
    (xref               INLINE-ARGS . (node #:opt name section info-file manual))
    (pxref              INLINE-ARGS . (node #:opt name section info-file manual))
    (uref               INLINE-ARGS . (url #:opt title replacement))
    (anchor             INLINE-ARGS . (name))
    (dots               INLINE-ARGS . ())
    (result             INLINE-ARGS . ())
    (bullet             INLINE-ARGS . ())
    (copyright          INLINE-ARGS . ())

    ;; EOL args elements
    (node               EOL-ARGS . (this #:opt next previous up))
    (c                  EOL-ARGS . all)
    (comment            EOL-ARGS . all)
    (setchapternewpage  EOL-ARGS . all)
    (sp                 EOL-ARGS . all)
    (page               EOL-ARGS . ())
    (vskip              EOL-ARGS . all)
    (syncodeindex       EOL-ARGS . all)
    (contents           EOL-ARGS . ())
    (insertcopying      EOL-ARGS . ())

    ;; EOL text commands
    (*ENVIRON-ARGS*     EOL-TEXT)
    (itemx              EOL-TEXT)
    (deffnx             EOL-TEXT) ;; FIXME: x commands for all def*
    (set                EOL-TEXT)
    (center             EOL-TEXT)
    (title              EOL-TEXT)
    (subtitle           EOL-TEXT)
    (author             EOL-TEXT)
    (chapter            EOL-TEXT)
    (section            EOL-TEXT)
    (appendix           EOL-TEXT)
    (appendixsec        EOL-TEXT)
    (unnumbered         EOL-TEXT)
    (unnumberedsec      EOL-TEXT)
    (subsection         EOL-TEXT)
    (subsubsection      EOL-TEXT)
    (appendixsubsec     EOL-TEXT)
    (appendixsubsubsec  EOL-TEXT)
    (unnumberedsubsec   EOL-TEXT)
    (unnumberedsubsubsec EOL-TEXT)

    ;; Environment commands (those that need @end)
    (texinfo            ENVIRON . title)
    (ignore             ENVIRON . ())
    (ifinfo             ENVIRON . ())
    (iftex              ENVIRON . ())
    (ifhtml             ENVIRON . ())
    (ifxml              ENVIRON . ())
    (ifplaintext        ENVIRON . ())
    (titlepage          ENVIRON . ())
    (menu               ENVIRON . ())
    (copying            ENVIRON . ())
    (example            ENVIRON . ())
    (smallexample       ENVIRON . ())
    (display            ENVIRON . ())
    (smalldisplay       ENVIRON . ())
    (verbatim           ENVIRON . ())
    (format             ENVIRON . ())
    (smallformat        ENVIRON . ())
    (lisp               ENVIRON . ())
    (smalllisp          ENVIRON . ())
    (cartouche          ENVIRON . ())
    (quotation          ENVIRON . ())
    (deftp              ENVIRON . (category name . attributes))
    (defcv              ENVIRON . (category class name))
    (defivar            ENVIRON . (class name))
    (deftypeivar        ENVIRON . (class data-type name))
    (defop              ENVIRON . (category class name . arguments))
    (deftypeop          ENVIRON . (category class data-type name . arguments))
    (defmethod          ENVIRON . (class name . arguments))
    (deftypemethod      ENVIRON . (class data-type name . arguments))
    (defopt             ENVIRON . (name))
    (defvr              ENVIRON . (category name))
    (defvar             ENVIRON . (name))
    (deftypevr          ENVIRON . (category data-type name))
    (deftypevar         ENVIRON . (data-type name))
    (deffn              ENVIRON . (category name . arguments))
    (deftypefn          ENVIRON . (category data-type name . arguments))
    (defspec            ENVIRON . (name arguments))
    (defmac             ENVIRON . (name arguments))
    (defun              ENVIRON . (name arguments))
    (deftypefun         ENVIRON . (data-type name . arguments))

    (table              TABLE-ENVIRON . (formatter))
    (itemize            TABLE-ENVIRON . (formatter))
    (enumerate          TABLE-ENVIRON . (start))
    (ftable             TABLE-ENVIRON . (formatter))
    (vtable             TABLE-ENVIRON . (formatter))))


;-------------------------
; Utilities

;; Test if a string is made of only whitespace
;; An empty string is considered made of whitespace as well
(define (string-whitespace? str)
  (or (string-null? str)
      (string-every char-whitespace? str)))

;; allows eof
(define read-eof-breaks '(*eof* #\return #\newline))
(define (read-eof-line port)
  (if (eof-object? (peek-char port))
      (peek-char port)
      (let* ((line (next-token '() read-eof-breaks
                               "reading a line" port))
             (c (read-char port)))	; must be either \n or \r or EOF
        (if (and (eq? c #\return) (eq? (peek-char port) #\newline))
            (read-char port))		; skip \n that follows \r
        line)))

(define ascii->char integer->char)

;========================================================================
;		Lower-level parsers and scanners
;
; They deal with primitive lexical units (Names, whitespaces, tags)
; and with pieces of more generic productions. Most of these parsers
; must be called in appropriate context. For example, complete-start-tag
; must be called only when the start-tag has been detected and its GI
; has been read.

(define (skip-whitespace port)
  (skip-while '(#\space #\tab #\return #\newline) port))

(define (skip-horizontal-whitespace port)
  (skip-while '(#\space #\tab) port))

; command ::= Letter+
; Read a command starting from the current position in the PORT and
; return it as a symbol.
(define (read-command port)
  (let ((first-char (peek-char port)))
    (or (char-alphabetic? first-char)
        (parser-error port "Nonalphabetic @-command char: '" first-char "'")))
  (string->symbol
    (next-token-of
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          (else #f)))
      port)))

; procedure:	read-command-token PORT
; This procedure starts parsing of a command token. The current position
; in the stream must be #\@. This procedure scans enough of the input stream
; to figure out what kind of a command token it is seeing. The procedure returns
; a token structure describing the token. Note, generally reading
; of the current command is not finished! In particular, no arguments of
; the start-tag token are scanned.
;; FIXME: doc return values

(define (read-command-token port)
  (assert-curr-char '(#\@) "start of the command" port)
  (let ((peeked (peek-char port)))
    (cond
     ((memq peeked '(#\! #\. #\? #\@ #\\ #\{ #\}))
      ;; @-commands that escape characters
      (make-token 'STRING (string (read-char port))))
     ((char-alphabetic? peeked)
      (let ((name (read-command port)))
        (case name
          ((end)
           ;; got an ending tag
           (let ((command (string-trim-both
                           (read-eof-line port))))
             (or (and (not (string-null? command))
                      (string-every char-alphabetic? command))
                 (parser-error port "malformed @end" command))
             (make-token 'END (string->symbol command))))
          ((bye)
           ;; the end of the top
           (make-token 'END 'texinfo))
          ((item)
           (make-token 'ITEM 'item))
          ((include)
           (make-token 'INCLUDE #f))
          (else
           (make-token 'START name))))))))

; procedure+: 	read-verbatim-body PORT STR-HANDLER SEED
;
; This procedure must be called after we have read a string
; "@verbatim\n" that begins a verbatim section. The current position
; must be the first position of the verbatim body. This function reads
; _lines_ of the verbatim body and passes them to a STR-HANDLER, a
; character data consumer.
;
; The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
; The first STRING1 argument to STR-HANDLER never contains a newline.
; The second STRING2 argument often will. On the first invocation of the
; STR-HANDLER, the seed is the one passed to read-verbatim-body
; as the third argument. The result of this first invocation will be
; passed as the seed argument to the second invocation of the line
; consumer, and so on. The result of the last invocation of the
; STR-HANDLER is returned by the read-verbatim-body. Note a
; similarity to the fundamental 'fold' iterator.
;
; Within a verbatim section all characters are taken at their face
; value. It ends with "\n@end verbatim(\r)?\n".

;; Must be called right after the newline after @verbatim.
(define (read-verbatim-body port str-handler seed)
  (let loop ((seed seed))
    (let ((fragment (next-token '() '(#\newline)
                                "reading verbatim" port)))
      ;; We're reading the char after the 'fragment', which is
      ;; #\newline.
      (read-char port)
      (if (string=? fragment "@end verbatim")
          seed
          (loop (str-handler fragment "\n" seed))))))

; procedure+:	read-arguments PORT
;
; This procedure reads and parses a production ArgumentList.
; ArgumentList ::= S* Argument (S* , S* Argument)* S*
; Argument ::= ([^@{},])*
;
; Arguments are the things in braces, i.e @ref{my node} has one
; argument, "my node". Most commands taking braces actually don't have
; arguments, they process text. For example, in
; @emph{@strong{emphasized}}, the emph takes text, because the tree
; traversal continues into the braces.
;
; Any whitespace within Argument is replaced with a single space.
; Whitespace around an Argument is trimmed.
;
; The procedure returns a list of arguments. Afterwards the current
; character will be after the final #\}.

(define (read-arguments port stop-char)
  (define (split str)
    (read-char port) ;; eat the delimiter
    (let ((ret (map (lambda (x) (if (string-null? x) #f x))
                    (map string-trim-both (string-split str #\,)))))
      (if (and (pair? ret) (eq? (car ret) #f) (null? (cdr ret)))
          '()
          ret)))
  (split (next-token '() (list stop-char)
                     "arguments of @-command" port)))

; procedure+:	complete-start-command COMMAND PORT
;
; This procedure is to complete parsing of an @-command. The procedure
; must be called after the command token has been read. COMMAND is a
; TAG-NAME.
;
; This procedure returns several values:
;  COMMAND: a symbol.
;  ARGUMENTS: command's arguments, as an alist.
;  ELEM-CONTENT-MODEL

; On exit, the current position in PORT will depends on the ELEM-CONTENT-MODEL.
; For INLINE-TEXT, it will be one character after the #\{.
; For INLINE-ARGS, it will be the first character after the #\}.
; For EOL-TEXT, it will be the first non-whitespace character after the command.
; For EOL-ARGS, it will be the first character on the next line.
; ENVIRON is like EOL-TEXT.

(define (arguments->attlist port args arg-names)
  (let loop ((in args) (names arg-names) (opt? #f) (out '()))
    (cond
     ((symbol? names) ;; a rest arg
      (reverse (if (null? in) out (acons names in out))))
     ((and (not (null? names)) (eq? (car names) #:opt))
      (loop in (cdr names) #t out))
     ((null? in)
      (if (or (null? names) opt?)
          (reverse out)
          (parser-error port "@-command expected more arguments:" 
                        args arg-names names)))
     ((null? names)
      (parser-error port "@-command didn't expect more arguments:" in))
     ((not (car in))
      (or (and opt? (loop (cdr in) (cdr names) opt? out))
          (parser-error "@-command missing required argument"
                        (car names))))
     (else
      (loop (cdr in) (cdr names) opt?
            (cons (list (car names) (car in)) out))))))

(define (parse-table-args command port)
  (let* ((line (string-trim-both (read-text-line port)))
         (length (string-length line)))
    (define (get-formatter)
      (or (and (not (zero? length))
               (eq? (string-ref line 0) #\@)
               (let ((f (string->symbol (substring line 1))))
                 (or (inline-content? (cadr (command-spec f)))
                     (parser-error
                      port "@item formatter must be INLINE" f))
                 f))
          (parser-error "Invalid @item formatter" line)))
    (case command
      ((enumerate)
       (if (zero? length)
           '()
           `((start
              ,(if (or (and (eq? length 1)
                            (char-alphabetic? (string-ref line 0)))
                       (string-every char-numeric? line))
                   line
                   (parser-error
                    port "Invalid enumerate start" line))))))
      ((itemize)
       `((bullet
          ,(or (and (eq? length 1)
                    (not (char-whitespace? (string-ref line 0)))
                    line)
               (list (get-formatter))
               line))))
      (else ;; tables of various varieties
       `((formatter (,(get-formatter))))))))

(define (complete-start-command command port)
  (define (get-arguments type arg-names stop-char)
    (arguments->attlist port (read-arguments port stop-char) arg-names))

  (let* ((spec (command-spec command))
         (type (cadr spec))
         (arg-names (cddr spec)))
    (case type
      ((INLINE-TEXT)
       (assert-curr-char '(#\{) "Inline element lacks {" port)
       (values command '() type))
      ((INLINE-ARGS)
       (assert-curr-char '(#\{) "Inline element lacks {" port)
       (values command (get-arguments type arg-names #\}) type))
      ((EOL-ARGS)
       (values command (get-arguments type arg-names #\newline) type))
      ((ENVIRON ENTRY)
       (skip-horizontal-whitespace port)
       (values command (parse-environment-args command port) type))
      ((TABLE-ENVIRON)
       (skip-horizontal-whitespace port)
       (values command (parse-table-args command port) type))
      ((EOL-TEXT)
       (skip-horizontal-whitespace port)
       (values command '() type))
      ((PARAGRAPH EMPTY-COMMAND ITEM)
       (values command '() type))
      (else ;; INCLUDE shouldn't get here
       (parser-error port "can't happen")))))

;-----------------------------------------------------------------------------
;			Higher-level parsers and scanners
;
; They parse productions corresponding to the whole (document) entity
; or its higher-level pieces (prolog, root element, etc).


;; Only reads @settitle, leaves it to the command parser to finish
;; reading the title.
(define (take-until-settitle port)
  (or (find-string-from-port? "\n@settitle " port)
      (parser-error port "No \\n@settitle  found"))
  (skip-horizontal-whitespace port)
  (and (eq? (peek-char port) #\newline)
       (parser-error port "You have a @settitle, but no title")))

; procedure+:	read-char-data PORT EXPECT-EOF? STR-HANDLER SEED
;
; This procedure is to read the CharData of a texinfo document.
;
; text ::= (CharData | Command | Environment)*
;
; The procedure reads CharData and stops at @-commands (or
; environments). It also stops at an open or close brace.
;
; port
;	a PORT to read
; expect-eof?
;	a boolean indicating if EOF is normal, i.e., the character
;	data may be terminated by the EOF. EOF is normal
;	while processing the main document.
; str-handler
;	a STR-HANDLER
; seed
;	an argument passed to the first invocation of STR-HANDLER.
;
; The procedure returns two results: SEED and TOKEN.
; The SEED is the result of the last invocation of STR-HANDLER, or the
; original seed if STR-HANDLER was never called.
;
; TOKEN can be either an eof-object (this can happen only if expect-eof?
; was #t), or a texinfo token denoting the start or end of a tag.
;
; @verbatim sections are expanded inline and never returned. Comments
; are silently disregarded.

;; read-char-data port expect-eof? preserve-ws? str-handler seed
(define read-char-data
  (let* ((end-chars-eof '(*eof* #\{ #\} #\@ #\newline)))
    (define (handle str-handler str1 str2 seed)
      (if (and (string-null? str1) (string-null? str2))
          seed
          (str-handler str1 str2 seed)))

    (lambda (port expect-eof? preserve-ws? str-handler seed)
      (let ((end-chars ((if expect-eof? identity cdr) end-chars-eof)))
        (let loop ((seed seed))
          (let* ((fragment (next-token '() end-chars "reading char data" port))
                 (term-char (peek-char port))) ; one of end-chars
            (cond
             ((eof-object? term-char) ; only if expect-eof?
              (values (handle str-handler fragment "" seed) term-char))
             ((memq term-char '(#\@ #\{ #\}))
              (values (handle str-handler fragment "" seed)
                      (case term-char
                        ((#\@) (read-command-token port))
                        ((#\{) (make-token 'START '*braces*))
                        ((#\}) (read-char port) (make-token 'END #f)))))
             ((eq? term-char #\newline)
              ;; Always significant, unless directly before an end token.
              (let ((c (peek-next-char port)))
                (cond
                 ((eof-object? c)
                  (or expect-eof?
                      (parser-error port "EOF while reading char data"))
                  (values (handle str-handler fragment "" seed) c))
                 ((eq? c #\@)
                  (let* ((token (read-command-token port))
                         (end? (eq? (token-kind token) 'END)))
                    (values
                     (handle str-handler fragment (if end? "" " ") seed)
                     token)))
                 ((and (not preserve-ws?) (eq? c #\newline))
                  ;; paragraph-separator ::= #\newline #\newline+
                  (skip-while '(#\newline) port)
                  (skip-horizontal-whitespace port)
                  (values (handle str-handler fragment "" seed)
                          (make-token 'PARA 'para)))
                 (else
                  (loop (handle str-handler fragment "\n" seed)))))))))))))

; procedure+:	assert-token TOKEN KIND NAME
; Make sure that TOKEN is of anticipated KIND and has anticipated NAME
(define (assert-token token kind name)
  (or (and (token? token)
           (eq? kind (token-kind token))
           (equal? name (token-head token)))
      (parser-error #f "Expecting @end for " command ", got " token)))

;========================================================================
;		Highest-level parsers: Texinfo to SXML

; These parsers are a set of syntactic forms to instantiate a SSAX
; parser. The user tells what to do with the parsed character and
; element data. These latter handlers determine if the parsing follows a
; SAX or a DOM model.

; syntax: make-command-parser fdown fup str-handler

; Create a parser to parse and process one element, including its
; character content or children elements. The parser is typically
; applied to the root element of a document.

; fdown
;	procedure COMMAND ARGUMENTS EXPECTED-CONTENT SEED
;
;	This procedure is to generate the seed to be passed to handlers
;	that process the content of the element. This is the function
;	identified as 'fdown' in the denotational semantics of the XML
;	parser given in the title comments to this file.
;
; fup
;	procedure COMMAND ARGUMENTS PARENT-SEED SEED
;
;	This procedure is called when parsing of COMMAND is finished.
;	The SEED is the result from the last content parser (or from
;	fdown if the element has the empty content). PARENT-SEED is the
;	same seed as was passed to fdown. The procedure is to generate a
;	seed that will be the result of the element parser. This is the
;	function identified as 'fup' in the denotational semantics of
;	the XML parser given in the title comments to this file.
;
; str-handler
;	A STR-HANDLER
;

; The generated parser is a
;	procedure COMMAND PORT SEED
;
; The procedure must be called *after* the command token has been read.

(define (read-include-file-name port)
  (let ((x (string-trim-both (read-eof-line port))))
    (if (string-null? x)
        (error "no file listed")
        x))) ;; fixme: should expand @value{} references

(define (make-command-parser fdown fup str-handler)
  (lambda (command port seed)
    (let visit ((command command) (port port) (sig-ws? #f) (parent-seed seed))
      (let*-values (((command arguments expected-content)
                     (complete-start-command command port)))
        (let* ((seed (fdown command arguments expected-content parent-seed))
               (eof-closes? (or (memq command '(texinfo para))
                                (eq? expected-content 'EOL-TEXT)))
               (sig-ws? (or sig-ws? (space-significant? command)))
               (up (lambda (s) (fup command arguments parent-seed s)))
               (new-para (lambda (s) (fdown 'para '() 'PARAGRAPH s)))
               (make-end-para (lambda (p) (lambda (s) (fup 'para '() p s)))))
          
          (define (port-for-content)
            (if (eq? expected-content 'EOL-TEXT)
                (call-with-input-string (read-text-line port) identity)
                port))

          (cond
           ((memq expected-content '(EMPTY-COMMAND INLINE-ARGS EOL-ARGS))
            ;; empty or finished by complete-start-command
            (up seed))
           (else
            (let loop ((port (port-for-content))
                       (expect-eof? eof-closes?)
                       (end-para identity)
                       (need-break? (and (not sig-ws?)
                                         (memq expected-content
                                               '(ENVIRON TABLE-ENVIRON
                                                 ENTRY ITEM))))
                       (seed seed))
              (cond
               ((and need-break? (or sig-ws? (skip-whitespace port))
                     (not (memq (peek-char port) '(#\@ #\})))
                     (not (eof-object? (peek-char port))))
                ;; Even if we have an @, it might be inline -- check
                ;; that later
                (let ((seed (end-para seed)))
                  (loop port expect-eof? (make-end-para seed) #f
                        (new-para seed))))
               (else
                (let*-values (((seed token)
                               (read-char-data
                                port expect-eof? sig-ws? str-handler seed)))
                  (cond
                   ((eof-object? token)
                    (case expect-eof? 
                      ((include #f) (end-para seed))
                      (else (up (end-para seed)))))
                   (else
                    (case (token-kind token)
                      ((STRING)
                       ;; this is only @-commands that escape
                       ;; characters: @}, @@, @{ -- new para if need-break
                       (let ((seed ((if need-break? end-para identity) seed)))
                         (loop port expect-eof?
                               (if need-break? (make-end-para seed) end-para) #f
                               (str-handler (token-head token) ""
                                            ((if need-break? new-para identity)
                                             seed)))))
                      ((END)
                       ;; The end will only have a name if it's for an
                       ;; environment
                       (cond
                        ((memq command '(item entry))
                         (let ((spec (command-spec (token-head token))))
                           (or (eq? (cadr spec) 'TABLE-ENVIRON)
                               (parser-error
                                port "@item not ended by @end table/enumerate/itemize"
                                token))))
                        ((eq? expected-content 'ENVIRON)
                         (assert-token token 'END command)))
                       (up (end-para seed)))
                      ((ITEM)
                       (cond
                        ((memq command '(enumerate itemize))
                         (up (visit 'item port sig-ws? (end-para seed))))
                        ((eq? expected-content 'TABLE-ENVIRON)
                         (up (visit 'entry port sig-ws? (end-para seed))))
                        ((memq command '(item entry))
                         (visit command port sig-ws? (up (end-para seed))))
                        (else
                         (parser-error
                          port "@item must be within a table environment"
                          command))))
                      ((PARA)
                       ;; examine valid paragraphs?
                       (loop port expect-eof? end-para (not sig-ws?) seed))
                      ((INCLUDE)
                       ;; Recurse for include files
                       (let ((seed (call-with-file-and-dir
                                    (read-include-file-name port)
                                    (lambda (port)
                                      (loop port 'include end-para
                                            need-break? seed)))))
                         (loop port expect-eof? end-para need-break? seed)))
                      ((START)          ; Start of an @-command
                       (let* ((head (token-head token))
                              (type (cadr (command-spec head)))
                              (inline? (inline-content? type))
                              (seed ((if (and inline? (not need-break?))
                                         identity end-para) seed))
                              (end-para (if inline?
                                            (if need-break? (make-end-para seed)
                                                end-para)
                                            identity))
                              (new-para (if (and inline? need-break?)
                                            new-para identity)))
                         (loop port expect-eof? end-para (not inline?)
                               (visit head port sig-ws? (new-para seed)))))
                      (else
                       (parser-error port "Unknown token type" token))))))))))))))))

(define parse-environment-args
  (let ((parser (make-command-parser
                 (lambda args           ; fdown
                   '())
                 (lambda (command arguments parent-seed seed) ; fup
                   (acons
                    command
                    (if (null? arguments) seed (acons '% arguments seed))
                    parent-seed))
                 (lambda (string1 string2 seed) ; str-handler
                   (if (string-null? string2) 
                       (cons string1 seed)
                       (cons* string2 string1 seed))))))
    ;; duplicate arguments->attlist to avoid unnecessary splitting
    (lambda (command port)
      (let ((args (reverse! (cdar (parser '*ENVIRON-ARGS* port '()))))
            (arg-names (cddr (command-spec command))))
        (cond
         ((not arg-names)
          (if (null? args) '()
              (parser-error port "@-command doesn't take args" command)))
         ((eq? arg-names #t)
          (list (cons 'arguments args)))
         (else
          (let loop ((args args) (arg-names arg-names) (out '()))
            (cond
             ((null? arg-names)
              (if (null? args) (reverse! out)
                  (parser-error port "@-command didn't expect more args"
                                command args)))
             ((symbol? arg-names)
              (reverse! (acons arg-names args out)))
             ((null? args)
              (parser-error port "@-command expects more args"
                            command arg-names))
             ((and (string? (car args)) (string-index (car args) #\space))
              => (lambda (i)
                   (let ((rest (substring/shared (car args) (1+ i))))
                     (if (zero? i)
                         (loop (cons rest (cdr args)) arg-names out)
                         (loop (cons rest (cdr args)) (cdr arg-names)
                               (cons (list (car arg-names)
                                           (substring (car args) 0 i))
                                     out))))))
             (else
              (loop (cdr args) (cdr arg-names)
                    (if (and (pair? (car args)) (eq? (caar args) '*braces*))
                        (acons (car arg-names) (cdar args) out)
                        (cons (list (car arg-names) (car args)) out))))))))))))
   
; syntax: make-parser user-handler-tag user-handler-proc ...
;
; Create a texinfo parser as an instance of this parsing framework. It
; will be a SAX, a DOM, or a specialized parser depending on the
; handlers that the user supplies.

; user-handler-tag is a symbol that identifies a procedural expression
; that follows the tag. Given below are tags and signatures of the
; corresponding procedures. Not all tags have to be specified. If some
; are omitted, reasonable defaults will apply.

; tag: FDOWN
; handler-procedure: see make-elem-parser, my-new-level-seed

; tag: FUP
; handler-procedure: see make-elem-parser, my-finish-element

; tag: STR-HANDLER
; handler-procedure: see make-elem-parser, my-char-data-handler

; The generated parser is a
;	procedure PORT SEED

(define (make-parser . user-handlers)
  ;; An assoc list of user-handler-tag and default handlers
  (define all-handlers
    `((FDOWN . REQD)                    ; required
      (FUP . REQD)                      ; required
      (STR-HANDLER . REQD)))            ; required

  ;; Delete an association with the tag from alist
  ;; exit to cont, passing the tag, tag-association, and the list
  ;; of remaining associations.
  ;; It's an error if the association with the tag does not exist
  ;; in alist
  (define (delete-assoc alist tag cont)
    (let loop ((alist alist) (scanned '()))
      (cond
       ((null? alist) (error "Unknown user-handler-tag: " tag))
       ((eq? tag (caar alist))
        (cont tag (cdar alist) (append scanned (cdr alist))))
       (else (loop (cdr alist) (cons (car alist) scanned))))))

  ;; create an assoc list of tags and handlers
  ;; based on the defaults and on the given handlers
  (define (merge-handlers declared-handlers given-handlers)
    (cond
     ((null? given-handlers)		; the arguments are exhausted...
      (cond
       ((null? declared-handlers) '())
       ((not (eq? 'REQD (cdar declared-handlers))) ; default value was given:
        (cons (car declared-handlers)	; use it
              (merge-handlers (cdr declared-handlers) given-handlers)))
       (else (error "The handler for the tag " (caar declared-handlers)
                    " must be specified"))))
     ((null? (cdr given-handlers))
      (error "Odd number of arguments to make-parser"))
     (else
      (delete-assoc declared-handlers (car given-handlers)
                    (lambda (tag value alist)
                      (cons (cons tag (cadr given-handlers))
                            (merge-handlers alist (cddr given-handlers))))))))

  (let ((user-handlers (merge-handlers all-handlers user-handlers)))

    (define (get-handler tag)
      (cond
       ((assq tag user-handlers) => cdr)
       (else (error "unknown tag: " tag))))

    ;; this is the parser
    (lambda (port seed)
      
      ;; A procedure command port seed
      (define command-parser
        (make-command-parser (get-handler 'FDOWN)
                             (get-handler 'FUP)
                             (get-handler 'STR-HANDLER)))

      (take-until-settitle port)
      (command-parser 'texinfo port seed))))


;========================================================================
;		Highest-level parsers: Texinfo to SXML
;

; procedure: texi->stexi PORT NAMESPACE-PREFIX-ASSIG
;
; This is an instance of a SSAX parser above that returns an SXML
; representation of the texinfo document ready to be read at PORT.
;
; The procedure returns an SXML tree. The port points to the
; first character after the @bye, or to the end of the file.

(define (texi->stexi port)
  (define (reverse-collect-str-drop-ws fragments)
    ;; given the list of fragments (some of which are text strings)
    ;; reverse the list and concatenate adjacent text strings We also
    ;; drop "unsignificant" whitespace, that is, whitespace in front,
    ;; behind and between elements. The whitespace that is included in
    ;; character data is not affected.
    (cond 
     ((null? fragments) '())		; a shortcut
     ((and (string? (car fragments))	; another shortcut
           (null? (cdr fragments))	; remove trailing ws
           (string-whitespace? (car fragments))) '())
     (else
      (let loop ((fragments fragments) (result '()) (strs '())
                 (all-whitespace? #t))
        (cond
         ((null? fragments)
          (if all-whitespace? result	; remove leading ws
              (cons (apply string-append strs) result)))
         ((string? (car fragments))
          (loop (cdr fragments) result (cons (car fragments) strs)
                (and all-whitespace?
                     (string-whitespace? (car fragments)))))
         (else
          (loop (cdr fragments)
                (cons
                 (car fragments)
                 (if all-whitespace? result
                     (cons (apply string-append strs) result)))
                '() #t)))))))

    (let ((result
	   (reverse
	    ((make-parser
              'FDOWN
              (lambda (command arguments expected-content seed)
                ;;(pk 'down command)
                ;;(pk 'fdown command arguments expected-content seed)
                '())
   
              'FUP
              (lambda (command arguments parent-seed seed)
                ;;(pk 'up command)
                ;;(pk 'fup command arguments parent-seed seed)
                (let ((seed (reverse-collect-str-drop-ws seed)))
                    (acons
                     command
                     (if (null? arguments)
                         seed
                         (acons '% arguments seed))
                     parent-seed)))

              'STR-HANDLER
              (lambda (string1 string2 seed)
                ;;(pk 'str-handler string1 string2 seed)
                (if (string-null? string2)
                    (cons string1 seed)
                    (cons* string2 string1 seed))))
             port '()))))
      (postprocess (car result))))

(define (trim-whitespace str trim-left? trim-right?)
  (let* ((left-space? (and (not trim-left?)
                           (string-prefix? " " str)))
         (right-space? (and (not trim-right?)
                            (string-suffix? " " str)))
         (tail (append! (string-tokenize str)
                        (if right-space? '("") '()))))
    (string-join (if left-space? (cons "" tail) tail))))

(define (postprocess tree)
  (define (loop in out state first? sig-ws?)
    (cond
     ((null? in)
      (values (reverse! out) state))
     ((string? (car in))
      (loop (cdr in)
            (cons (if sig-ws? (car in)
                      (trim-whitespace (car in) first? (null? (cdr in))))
                  out)
            state #f sig-ws?))
     ((pair? (car in))
      (case (caar in)
        ((set)
         (if (null? (cdar in)) (error "@set missing arguments" seed))
         (if (string? (cadar in))
             (let ((i (string-index (cadar in) #\space)))
               (if i 
                   (loop (cdr in) out
                         (acons (substring (cadar in) 0 i)
                                (cons (substring (cadar in) (1+ i)) (cddar in))
                                state)
                         #f sig-ws?)
                   (loop (cdr in) out (acons (cadar in) (cddar in) state)
                         #f sig-ws?)))
             (error "expected a constant to define for @set" in)))
        ((value)
         (loop (fold-right cons (cdr in)
                           (or (cdr (assoc (cadr (assq 'key (cdadar in))) state))
                               (error "unknown value" (cdadar in) state)))
               out
               state #f sig-ws?))
        ((copying)
         (loop (cdr in) out (cons (car in) state) #f sig-ws?))
        ((insertcopying)
         (loop (fold-right cons (cdr in)
                           (or (cdr (assoc 'copying state))
                               (error "copying isn't set yet")))
               out
               state #f sig-ws?))
        (else
         (let*-values (((kid-out state)
                        (loop (car in) '() state #t
                              (or sig-ws? (space-significant? (caar in))))))
           (loop (cdr in) (cons kid-out out) state #f sig-ws?)))))
     (else ; a symbol
      (loop (cdr in) (cons (car in) out) state #t sig-ws?))))

  (call-with-values
      (lambda () (loop tree '() '() #t #f))
    (lambda (out state) out)))

;; Replace % with texinfo-arguments.
(define (stexi->sxml tree)
  (pre-post-order
   tree
   `((% . ,(lambda (x . t) (cons 'texinfo-arguments t)))
     (*text* . ,(lambda (x t) t))
     (*default* . ,(lambda (x . t) (cons x t))))))

;;; arch-tag: 73890afa-597c-4264-ae70-46fe7756ffb5
;;; texinfo.scm ends here
