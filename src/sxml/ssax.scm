;; guile-lib
;; Copyright (C) 2004 Andy Wingo <wingo at pobox dot com>
;; Copyright (C) 2001,2002 Oleg Kiselyov <oleg at pobox dot com>

;; This file is based on SSAX's SSAX.scm, and is in the public domain.

;;; Commentary:
;;
;; Functional XML parsing framework: SAX/DOM and SXML parsers with
;; support for XML Namespaces and validation. See the guile-lib info
;; documentation for more information.
;;
;;; Code:

(define-module (sxml ssax)
  #:use-module (text lex-simple)
  #:use-module (io string)
  #:use-module (srfi srfi-1)
  #:export (make-xml-token xml-token? xml-token-kind xml-token-head

            make-empty-attlist attlist-add attlist-null?
            attlist-remove-top attlist->alist attlist-fold

            SSAX:warn SSAX:S-chars SSAX:skip-S
            SSAX:ncname-starting-char? SSAX:read-NCName SSAX:read-QName
            SSAX:Prefix-XML SSAX:largest-unres-name
            SSAX:read-markup-token SSAX:skip-pi
            SSAX:read-pi-body-as-string SSAX:read-pi-body-as-name-values
            SSAX:skip-internal-dtd SSAX:read-CDATA-body
            SSAX:read-char-ref SSAX:predefined-parsed-entities
            SSAX:handle-parsed-entity SSAX:read-attributes
            SSAX:resolve-name SSAX:uri-string->symbol
            SSAX:complete-start-tag SSAX:read-external-ID SSAX:scan-Misc
            SSAX:read-char-data SSAX:assert-token SSAX:make-pi-parser
            SSAX:make-elem-parser SSAX:make-parser SSAX:XML->SXML))


(define (parser-error port message . rest)
  (apply throw 'parser-error port message rest))

;; Upstream version:
; $Id: SSAX.scm,v 1.2 2001/09/05 20:14:00 oleg Exp $

;========================================================================
;				Data Types

; TAG-KIND
;	a symbol 'START, 'END, 'PI, 'DECL, 'COMMENT, 'CDSECT
;		or 'ENTITY-REF that identifies a markup token

; UNRES-NAME
;	a name (called GI in the XML Recommendation) as given in an xml
;	document for a markup token: start-tag, PI target, attribute name.
;	If a GI is an NCName, UNRES-NAME is this NCName converted into
;	a Scheme symbol. If a GI is a QName, UNRES-NAME is a pair of
;	symbols: (PREFIX . LOCALPART)

; RES-NAME
;	An expanded name, a resolved version of an UNRES-NAME.
;	For an element or an attribute name with a non-empty namespace URI,
;	RES-NAME is a pair of symbols, (URI-SYMB . LOCALPART).
;	Otherwise, it's a single symbol.

; ELEM-CONTENT-MODEL
;	A symbol:
;	ANY	  - anything goes, expect an END tag.
;	EMPTY-TAG - no content, and no END-tag is coming
;	EMPTY	  - no content, expect the END-tag as the next token
;	PCDATA    - expect character data only, and no children elements
;	MIXED
;	ELEM-CONTENT

; URI-SYMB
;	A symbol representing a namespace URI -- or other symbol chosen
;	by the user to represent URI. In the former case,
;	URI-SYMB is created by %-quoting of bad URI characters and
;	converting the resulting string into a symbol.

; NAMESPACES
;	A list representing namespaces in effect. An element of the list
;	has one of the following forms:
;	(PREFIX URI-SYMB . URI-SYMB) or
;	(PREFIX USER-PREFIX . URI-SYMB)
;		USER-PREFIX is a symbol chosen by the user
;		to represent the URI.
;	(#f USER-PREFIX . URI-SYMB)
;		Specification of the user-chosen prefix and a URI-SYMBOL.
;	(*DEFAULT* USER-PREFIX . URI-SYMB)
;		Declaration of the default namespace
;	(*DEFAULT* #f . #f)
;		Un-declaration of the default namespace. This notation
;		represents overriding of the previous declaration
;	A NAMESPACES list may contain several elements for the same PREFIX.
;	The one closest to the beginning of the list takes effect.

; ATTLIST
;	An ordered collection of (NAME . VALUE) pairs, where NAME is
;	a RES-NAME or an UNRES-NAME. The collection is an ADT

; STR-HANDLER
;	A procedure of three arguments: STRING1 STRING2 SEED
;	returning a new SEED
;	The procedure is supposed to handle a chunk of character data
;	STRING1 followed by a chunk of character data STRING2.
;	STRING2 is a short string, often "\n" and even ""

; ENTITIES
;	An assoc list of pairs:
;	   (named-entity-name . named-entity-body)
;	where named-entity-name is a symbol under which the entity was
;	declared, named-entity-body is either a string, or
;	(for an external entity) a thunk that will return an
;	input port (from which the entity can be read).
;	named-entity-body may also be #f. This is an indication that a
;	named-entity-name is currently being expanded. A reference to
;	this named-entity-name will be an error: violation of the
;	WFC nonrecursion.

; XML-TOKEN -- a record

; This record represents a markup, which is, according to the XML
; Recommendation, "takes the form of start-tags, end-tags, empty-element tags,
; entity references, character references, comments, CDATA section delimiters,
; document type declarations, and processing instructions."
;
;	kind -- a TAG-KIND
;	head -- an UNRES-NAME. For xml-tokens of kinds 'COMMENT and
;		'CDSECT, the head is #f
;
; For example,
;	<P>  => kind='START, head='P
;	</P> => kind='END, head='P
;	<BR/> => kind='EMPTY-EL, head='BR
;	<!DOCTYPE OMF ...> => kind='DECL, head='DOCTYPE
;	<?xml version="1.0"?> => kind='PI, head='xml
;	&my-ent; => kind = 'ENTITY-REF, head='my-ent
; 
; Character references are not represented by xml-tokens as these references
; are transparently resolved into the corresponding characters.

(define (make-xml-token kind head) (cons kind head))
(define xml-token? pair?)
(define xml-token-kind car)
(define xml-token-head cdr)

; XML-DECL -- a record

; The following is Gambit-specific, see below for a portable declaration
;(define-structure xml-decl elems entities notations)

; The record represents a datatype of an XML document: the list of
; declared elements and their attributes, declared notations, list of
; replacement strings or loading procedures for parsed general
; entities, etc. Normally an xml-decl record is created from a DTD or
; an XML Schema, although it can be created and filled in in many other
; ways (e.g., loaded from a file).
;
; elems: an (assoc) list of decl-elem or #f. The latter instructs
;	the parser to do no validation of elements and attributes.
;
; decl-elem: declaration of one element:
;	(elem-name elem-content decl-attrs)
;	elem-name is an UNRES-NAME for the element.
;	elem-content is an ELEM-CONTENT-MODEL.
;	decl-attrs is an ATTLIST, of (ATTR-NAME . VALUE) associations
; !!!This element can declare a user procedure to handle parsing of an
; element (e.g., to do a custom validation, or to build a hash of
; IDs as they're encountered).
;
; decl-attr: an element of an ATTLIST, declaration of one attribute
;	(attr-name content-type use-type default-value)
;	attr-name is an UNRES-NAME for the declared attribute
;	content-type is a symbol: CDATA, NMTOKEN, NMTOKENS, ...
;		or a list of strings for the enumerated type.
;	use-type is a symbol: REQUIRED, IMPLIED, FIXED
;	default-value is a string for the default value, or #f if not given.
;
;

; see a function make-empty-xml-decl to make a XML declaration entry
; suitable for a non-validating parsing.


;-------------------------
; Utilities

;   SSAX:warn PORT MESSAGE SPECIALISING-MSG*
; to notify the user about warnings that are NOT errors but still
; may alert the user.
; Result is unspecified.
; We need to define the function to allow the self-tests to run.
; Normally the definition of SSAX:warn is to be provided by the user.

(define (SSAX:warn port msg . other-msg)
  (newline) (display "SSAX-Warning: ")
  (display msg) (for-each display other-msg) (newline))

; Test if a string is made of only whitespace
; An empty string is considered made of whitespace as well
(define (string-whitespace? str)
  (let ((len (string-length str)))
    (cond
     ((zero? len) #t)
     ((= 1 len) (char-whitespace? (string-ref str 0)))
     ((= 2 len) (and (char-whitespace? (string-ref str 0))
		     (char-whitespace? (string-ref str 1))))
     (else
      (let loop ((i 0))
	(or (>= i len)
	    (and (char-whitespace? (string-ref str i))
		 (loop (+ 1 i)))))))))

;; if condition is true, execute stmts in turn and return the result of
;; the last statement otherwise, return #f
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))

;; Execute a sequence of forms and return the result of the _first_ one.
;; Like PROG1 in Lisp. Typically used to evaluate one or more forms with
;; side effects and return a value that must be computed before some or
;; all of the side effects happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

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

(define ascii->char integer->char)

;========================================================================
;		Lower-level parsers and scanners
;
; They deal with primitive lexical units (Names, whitespaces, tags)
; and with pieces of more generic productions. Most of these parsers
; must be called in appropriate context. For example, SSAX:complete-start-tag
; must be called only when the start-tag has been detected and its GI
; has been read.

;------------------------------------------------------------------------
;			Low-level parsing code

; Skip the S (whitespace) production as defined by
; [3] S ::= (#x20 | #x9 | #xD | #xA)
; The procedure returns the first not-whitespace character it
; encounters while scanning the PORT. This character is left
; on the input stream.

(define SSAX:S-chars '(#\space #\tab #\return #\newline))

(define (SSAX:skip-S port)
  (skip-while SSAX:S-chars port))


; Read a Name lexem and return it as string
; [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
;                  | CombiningChar | Extender
; [5] Name ::= (Letter | '_' | ':') (NameChar)*
;
; This code supports the XML Namespace Recommendation REC-xml-names,
; which modifies the above productions as follows:
;
; [4] NCNameChar ::= Letter | Digit | '.' | '-' | '_'
;                       | CombiningChar | Extender
; [5] NCName ::= (Letter | '_') (NCNameChar)*
; As the Rec-xml-names says,
; "An XML document conforms to this specification if all other tokens
; [other than element types and attribute names] in the document which
; are required, for XML conformance, to match the XML production for
; Name, match this specification's production for NCName."
; Element types and attribute names must match the production QName,
; defined below.

; Check to see if a-char may start a NCName
(define (SSAX:ncname-starting-char? a-char)
  (and (char? a-char)
    (or
      (char-alphabetic? a-char)
      (char=? #\_ a-char))))


; Read a NCName starting from the current position in the PORT and
; return it as a symbol.
(define (SSAX:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (SSAX:ncname-starting-char? first-char)
      (parser-error port "XMLNS [4] for '" first-char "'")))
  (string->symbol
    (next-token-of
      (lambda (c)
        (cond
          ((eof-object? c) #f)
          ((char-alphabetic? c) c)
          ((string-index "0123456789.-_" c) c)
          (else #f)))
      port)))

; Read a (namespace-) Qualified Name, QName, from the current
; position in the PORT.
; From REC-xml-names:
;	[6] QName ::= (Prefix ':')? LocalPart
;	[7] Prefix ::= NCName
;	[8] LocalPart ::= NCName
; Return: an UNRES-NAME
(define (SSAX:read-QName port)
  (let ((prefix-or-localpart (SSAX:read-NCName port)))
    (case (peek-char port)
      ((#\:)			; prefix was given after all
       (read-char port)		; consume the colon
       (cons prefix-or-localpart (SSAX:read-NCName port)))
      (else prefix-or-localpart) ; Prefix was omitted
      )))

; The prefix of the pre-defined XML namespace
(define SSAX:Prefix-XML (string->symbol "xml"))

; Compare one RES-NAME or an UNRES-NAME with the other.
; Return a symbol '<, '>, or '= depending on the result of
; the comparison.
; Names without PREFIX are always smaller than those with the PREFIX.
(define name-compare
  (letrec ((symbol-compare
	    (lambda (symb1 symb2)
	      (cond 
	       ((eq? symb1 symb2) '=)
	       ((string<? (symbol->string symb1) (symbol->string symb2))
		'<)
	       (else '>)))))
    (lambda (name1 name2)
      (cond
       ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2)
			    '<))
       ((symbol? name2) '>)
       ((eq? name2 SSAX:largest-unres-name) '<)
       ((eq? name1 SSAX:largest-unres-name) '>)
       ((eq? (car name1) (car name2))	; prefixes the same
	(symbol-compare (cdr name1) (cdr name2)))
       (else (symbol-compare (car name1) (car name2)))))))

; An UNRES-NAME that is postulated to be larger than anything that can occur in
; a well-formed XML document.
; name-compare enforces this postulate.
(define SSAX:largest-unres-name (cons (string->symbol "#UNRES")
				      (string->symbol "#UNRES")))


; procedure:	SSAX:read-markup-token PORT
; This procedure starts parsing of a markup token. The current position
; in the stream must be #\<. This procedure scans enough of the input stream
; to figure out what kind of a markup token it is seeing. The procedure returns
; an xml-token structure describing the token. Note, generally reading
; of the current markup is not finished! In particular, no attributes of
; the start-tag token are scanned.
;
; Here's a detailed break out of the return values and the position in the PORT
; when that particular value is returned:
;	PI-token:	only PI-target is read.
;			To finish the Processing Instruction and disregard it,
;			call SSAX:skip-pi. SSAX:read-attributes may be useful
;			as well (for PIs whose content is attribute-value
;			pairs)
;	END-token:	The end tag is read completely; the current position
;			is right after the terminating #\> character.	
;	COMMENT		is read and skipped completely. The current position
;			is right after "-->" that terminates the comment.
;	CDSECT		The current position is right after "<!CDATA["
;			Use SSAX:read-CDATA-body to read the rest.
;	DECL		We have read the keyword (the one that follows "<!")
;			identifying this declaration markup. The current
;			position is after the keyword (usually a
;			whitespace character)
;
;	START-token	We have read the keyword (GI) of this start tag.
;			No attributes are scanned yet. We don't know if this
;			tag has an empty content either.
;			Use SSAX:complete-start-tag to finish parsing of
;			the token.

(define SSAX:read-markup-token ; procedure SSAX:read-markup-token port
 (let ()
  		; we have read "<!-". Skip through the rest of the comment
		; Return the 'COMMENT token as an indication we saw a comment
		; and skipped it.
  (define (skip-comment port)
    (assert-curr-char '(#\-) "XML [15], second dash" port)
    (if (not (find-string-from-port? "-->" port))
      (parser-error port "XML [15], no -->"))
    (make-xml-token 'COMMENT #f))

  		; we have read "<![" that must begin a CDATA section
  (define (read-CDATA port)
    (or (string=? "CDATA[" (read-chars->string 6 port))
        (parser-error port "malformed CDATA"))
    (make-xml-token 'CDSECT #f))

  (lambda (port)
    (assert-curr-char '(#\<) "start of the token" port)
    (case (peek-char port)
      ((#\/) (read-char port)
       (begin0 (make-xml-token 'END (SSAX:read-QName port))
	       (SSAX:skip-S port)
	       (assert-curr-char '(#\>) "XML [42]" port)))
      ((#\?) (read-char port) (make-xml-token 'PI (SSAX:read-NCName port)))
      ((#\!)
       (case (peek-next-char port)
	 ((#\-) (read-char port) (skip-comment port))
	 ((#\[) (read-char port) (read-CDATA port))
	 (else (make-xml-token 'DECL (SSAX:read-NCName port)))))
      (else (make-xml-token 'START (SSAX:read-QName port)))))
))


; The current position is inside a PI. Skip till the rest of the PI
(define (SSAX:skip-pi port)      
  (if (not (find-string-from-port? "?>" port))
    (parser-error port "Failed to find ?> terminating the PI")))


; The current position is right after reading the PITarget. We read the
; body of PI and return is as a string. The port will point to the
; character right sfter '?>' combination that terminates PI.
; [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'

(define (SSAX:read-pi-body-as-string port)
  (SSAX:skip-S port)		; skip WS after the PI target name
  (apply string-append
    (let loop ()
      (let ((pi-fragment
	     (next-token '() '(#\?) "reading PI content" port)))
	(if (eq? #\> (peek-next-char port))
	    (begin
	      (read-char port)
	      (cons pi-fragment '()))
	    (cons* pi-fragment "?" (loop)))))))

;(define (SSAX:read-pi-body-as-name-values port)

; The current pos in the port is inside an internal DTD subset
; (e.g., after reading #\[ that begins an internal DTD subset)
; Skip until the "]>" combination that terminates this DTD
(define (SSAX:skip-internal-dtd port)      
  (if (not (find-string-from-port? "]>" port))
    (parser-error port
		  "Failed to find ]> terminating the internal DTD subset")))


; procedure+: 	SSAX:read-CDATA-body PORT STR-HANDLER SEED
;
; This procedure must be called after we have read a string "<![CDATA["
; that begins a CDATA section. The current position must be the first
; position of the CDATA body. This function reads _lines_ of the CDATA
; body and passes them to a STR-HANDLER, a character data consumer.
;
; The str-handler is a STR-HANDLER, a procedure STRING1 STRING2 SEED.
; The first STRING1 argument to STR-HANDLER never contains a newline.
; The second STRING2 argument often will. On the first invocation of
; the STR-HANDLER, the seed is the one passed to SSAX:read-CDATA-body
; as the third argument. The result of this first invocation will be
; passed as the seed argument to the second invocation of the line
; consumer, and so on. The result of the last invocation of the
; STR-HANDLER is returned by the SSAX:read-CDATA-body.  Note a
; similarity to the fundamental 'fold' iterator.
;
; Within a CDATA section all characters are taken at their face value,
; with only three exceptions:
;	CR, LF, and CRLF are treated as line delimiters, and passed
;	as a single #\newline to the STR-HANDLER
;	"]]>" combination is the end of the CDATA section.
;	&gt; is treated as an embedded #\> character
; Note, &lt; and &amp; are not specially recognized (and are not expanded)!

(define SSAX:read-CDATA-body 
  (let ((nl-str (string #\newline)))

    (lambda (port str-handler seed)
      (let loop ((seed seed))
	(let ((fragment (next-token '() '(#\return #\newline #\] #\&)
				    "reading CDATA" port)))
			; that is, we're reading the char after the 'fragment'
     (case (read-char port)	
       ((#\newline) (loop (str-handler fragment nl-str seed)))
       ((#\return)		; if the next char is #\newline, skip it
         (if (eqv? (peek-char port) #\newline) (read-char port))
         (loop (str-handler fragment nl-str seed)))
       ((#\])
	(if (not (eqv? (peek-char port) #\]))
	    (loop (str-handler fragment "]" seed))
	    (let check-after-second-braket
		((seed (if (string-null? fragment) seed
			   (str-handler fragment "" seed))))
	      (case (peek-next-char port)	; after the second bracket
		((#\>) (read-char port)	seed)	; we have read "]]>"
		((#\]) (check-after-second-braket
			(str-handler "]" "" seed)))
		(else (loop (str-handler "]]" "" seed)))))))
       ((#\&)		; Note that #\& within CDATA may stand for itself
	(let ((ent-ref 	; it does not have to start an entity ref
               (next-token-of (lambda (c) 
		 (and (not (eof-object? c)) (char-alphabetic? c) c)) port)))
	  (cond		; "&gt;" is to be replaced with #\>
	   ((and (string=? "gt" ent-ref) (eq? (peek-char port) #\;))
	    (read-char port)
	    (loop (str-handler fragment ">" seed)))
	   (else
	    (loop 
	     (str-handler ent-ref ""
			  (str-handler fragment "&" seed)))))))
       (else
         (parser-error port "can't happen"))))))))

; procedure+:	SSAX:read-char-ref PORT
;
; [66]  CharRef ::=  '&#' [0-9]+ ';' 
;                  | '&#x' [0-9a-fA-F]+ ';'
;
; This procedure must be called after we we have read "&#" 
; that introduces a char reference.
; The procedure reads this reference and returns the corresponding char
; The current position in PORT will be after ";" that terminates
; the char reference
; Faults detected:
;	WFC: XML-Spec.html#wf-Legalchar

(define (SSAX:read-char-ref port)
  (let* ((base
           (cond ((eq? (peek-char port) #\x) (read-char port) 16)
                 (else 10)))
         (name (next-token '() '(#\;) "XML [66]" port))
         (char-code (string->number name base)))
    (read-char port)	; read the terminating #\; char
    (if (integer? char-code) (ascii->char char-code)
      (parser-error port "[wf-Legalchar] broken for '" name "'"))))


; procedure+:	SSAX:handle-parsed-entity PORT NAME ENTITIES 
;		CONTENT-HANDLER STR-HANDLER SEED
;
; Expand and handle a parsed-entity reference
; port - a PORT
; name - the name of the parsed entity to expand, a symbol
; entities - see ENTITIES
; content-handler -- procedure PORT ENTITIES SEED
;	that is supposed to return a SEED
; str-handler - a STR-HANDLER. It is called if the entity in question
; turns out to be a pre-declared entity
;
; The result is the one returned by CONTENT-HANDLER or STR-HANDLER
; Faults detected:
;	WFC: XML-Spec.html#wf-entdeclared
;	WFC: XML-Spec.html#norecursion

(define SSAX:predefined-parsed-entities
  `(
    (,(string->symbol "amp") . "&")
    (,(string->symbol "lt") . "<")
    (,(string->symbol "gt") . ">")
    (,(string->symbol "apos") . "'")
    (,(string->symbol "quot") . "\"")))

(define (SSAX:handle-parsed-entity port name entities
				   content-handler str-handler seed)
  (cond	  ; First we check the list of the declared entities
   ((assq name entities) =>
    (lambda (decl-entity)
      (let ((ent-body (cdr decl-entity)) ; mark the list to prevent recursion
	    (new-entities (cons (cons name #f) entities)))
	(cond
	 ((string? ent-body)
	  (call-with-input-string ent-body
	     (lambda (port) (content-handler port new-entities seed))))
	 ((procedure? ent-body)
	  (let ((port (ent-body)))
	    (begin0
	     (content-handler port new-entities seed)
	     (close-input-port port))))
	 (else
	  (parser-error port "[norecursion] broken for " name))))))
    ((assq name SSAX:predefined-parsed-entities)
     => (lambda (decl-entity)
	  (str-handler (cdr decl-entity) "" seed)))
    (else (parser-error port "[wf-entdeclared] broken for " name))))



; The ATTLIST Abstract Data Type
; Currently is implemented as an assoc list sorted in the ascending
; order of NAMES.

(define (make-empty-attlist) '())

; Add a name-value pair to the existing attlist preserving the order
; Return the new list, in the sorted ascending order.
; Return #f if a pair with the same name already exists in the attlist

(define (attlist-add attlist name-value)
  (if (null? attlist) (cons name-value attlist)
      (case (name-compare (car name-value) (caar attlist))
	((=) #f)
	((<) (cons name-value attlist))
	(else (cons (car attlist) (attlist-add (cdr attlist) name-value)))
	)))

(define attlist-null? null?)

; Given an non-null attlist, return a pair of values: the top and the rest
(define (attlist-remove-top attlist)
  (values (car attlist) (cdr attlist)))

(define (attlist->alist attlist) attlist)
(define attlist-fold fold)

; procedure+:	SSAX:read-attributes PORT ENTITIES
;
; This procedure reads and parses a production Attribute*
; [41] Attribute ::= Name Eq AttValue
; [10] AttValue ::=  '"' ([^<&"] | Reference)* '"' 
;                 | "'" ([^<&'] | Reference)* "'"
; [25] Eq ::= S? '=' S?
;
;
; The procedure returns an ATTLIST, of Name (as UNRES-NAME), Value (as string)
; pairs. The current character on the PORT is a non-whitespace character
; that is not an ncname-starting character.
;
; Note the following rules to keep in mind when reading an 'AttValue'
; "Before the value of an attribute is passed to the application
; or checked for validity, the XML processor must normalize it as follows: 
; - a character reference is processed by appending the referenced
;   character to the attribute value 
; - an entity reference is processed by recursively processing the
;   replacement text of the entity [see ENTITIES]
;   [named entities amp lt gt quot apos are assumed pre-declared]
; - a whitespace character (#x20, #xD, #xA, #x9) is processed by appending #x20
;   to the normalized value, except that only a single #x20 is appended for a
;   "#xD#xA" sequence that is part of an external parsed entity or the
;   literal entity value of an internal parsed entity 
; - other characters are processed by appending them to the normalized value "
;
;
; Faults detected:
;	WFC: XML-Spec.html#CleanAttrVals
;	WFC: XML-Spec.html#uniqattspec

(define SSAX:read-attributes  ; SSAX:read-attributes port entities
 (let ()
		; Read the AttValue from the PORT up to the delimiter
		; (which can be a single or double-quote character,
		; or even a symbol *eof*)
		; 'prev-fragments' is the list of string fragments, accumulated
		; so far, in reverse order.
		; Return the list of fragments with newly read fragments
		; prepended.
  (define (read-attrib-value delimiter port entities prev-fragments)
    (let* ((new-fragments
	    (cons
	     (next-token '() (cons delimiter
				   '(#\newline #\return #\space #\tab #\< #\&))
			 "XML [10]" port)
	     prev-fragments))
	   (cterm (read-char port)))
      (if (or (eof-object? cterm) (eqv? cterm delimiter))
	  new-fragments
          (case cterm
            ((#\newline #\space #\tab)
              (read-attrib-value delimiter port entities
				 (cons " " new-fragments)))
            ((#\return)
              (if (eqv? (peek-char port) #\newline) (read-char port))
              (read-attrib-value delimiter port entities
				 (cons " " new-fragments)))
            ((#\&)
              (cond
                ((eqv? (peek-char port) #\#)
                  (read-char port)
                  (read-attrib-value delimiter port entities
		     (cons (string (SSAX:read-char-ref port)) new-fragments)))
                (else
		 (read-attrib-value delimiter port entities
		     (read-named-entity port entities new-fragments)))))
            ((#\<) (parser-error port "[CleanAttrVals] broken"))
            (else (parser-error port "Can't happen"))))))

  ;; we have read "&" that introduces a named entity reference.
  ;; read this reference and return the result of
  ;; normalizing of the corresponding string
  ;; (that is, read-attrib-value is applied to the replacement
  ;; text of the entity)
  ;; The current position will be after ";" that terminates
  ;; the entity reference
  (define (read-named-entity port entities fragments)
    (let ((name (SSAX:read-NCName port)))
      (assert-curr-char '(#\;) "XML [68]" port)
      (SSAX:handle-parsed-entity port name entities
	(lambda (port entities fragments)
	  (read-attrib-value '*eof* port entities fragments))
	(lambda (str1 str2 fragments)
	  (if (equal? "" str2) (cons str1 fragments)
	      (cons* str2 str1 fragments)))
	fragments)))

  (lambda (port entities)
    (let loop ((attr-list (make-empty-attlist)))
      (if (not (SSAX:ncname-starting-char? (SSAX:skip-S port))) attr-list
	  (let ((name (SSAX:read-QName port)))
	    (SSAX:skip-S port)
	    (assert-curr-char '(#\=) "XML [25]" port)
	    (SSAX:skip-S port)
	    (let ((delimiter 
		   (assert-curr-char '(#\' #\" ) "XML [10]" port)))
	      (loop 
	       (or (attlist-add attr-list 
		     (cons name 
			   (apply string-append
				  (reverse
				   (read-attrib-value delimiter port entities
						      '())))))
		   (parser-error port "[uniqattspec] broken for " name))))))))))

; SSAX:resolve-name PORT UNRES-NAME NAMESPACES apply-default-ns?
;
; Convert an UNRES-NAME to a RES-NAME given the appropriate NAMESPACES
; declarations.
; the last parameter apply-default-ns? determines if the default
; namespace applies (for instance, it does not for attribute names)
;
; Per REC-xml-names/#nsc-NSDeclared, "xml" prefix is considered pre-declared
; and bound to the namespace name "http://www.w3.org/XML/1998/namespace".
;
; This procedure tests for the namespace constraints:
; http://www.w3.org/TR/REC-xml-names/#nsc-NSDeclared

(define (SSAX:resolve-name port unres-name namespaces apply-default-ns?)
  (cond
   ((pair? unres-name)		; it's a QNAME
    (cons 
     (cond
     ((assq (car unres-name) namespaces) => cadr)
     ((eq? (car unres-name) SSAX:Prefix-XML) SSAX:Prefix-XML)
     (else
      (parser-error port "[nsc-NSDeclared] broken; prefix " (car unres-name))))
     (cdr unres-name)))
   (apply-default-ns?		; Do apply the default namespace, if any
    (let ((default-ns (assq '*DEFAULT* namespaces)))
      (if (and default-ns (cadr default-ns))
	  (cons (cadr default-ns) unres-name)
	  unres-name)))		; no default namespace declared
   (else unres-name)))		; no prefix, don't apply the default-ns
	   
	  
; procedure+:	SSAX:uri-string->symbol URI-STR
; Convert a URI-STR to an appropriate symbol
(define (SSAX:uri-string->symbol uri-str)
  (string->symbol uri-str))

; procedure+:	SSAX:complete-start-tag TAG PORT ELEMS ENTITIES NAMESPACES
;
; This procedure is to complete parsing of a start-tag markup. The
; procedure must be called after the start tag token has been
; read. TAG is an UNRES-NAME. ELEMS is an instance of xml-decl::elems;
; it can be #f to tell the function to do _no_ validation of elements
; and their attributes.
;
; This procedure returns several values:
;  ELEM-GI: a RES-NAME.
;  ATTRIBUTES: element's attributes, an ATTLIST of (RES-NAME . STRING)
;	pairs. The list does NOT include xmlns attributes.
;  NAMESPACES: the input list of namespaces amended with namespace
;	(re-)declarations contained within the start-tag under parsing
;  ELEM-CONTENT-MODEL

; On exit, the current position in PORT will be the first character after
; #\> that terminates the start-tag markup.
;
; Faults detected:
;	VC: XML-Spec.html#enum 
;	VC: XML-Spec.html#RequiredAttr
;	VC: XML-Spec.html#FixedAttr
;	VC: XML-Spec.html#ValueType
;	WFC: XML-Spec.html#uniqattspec (after namespaces prefixes are resolved)
;	VC: XML-Spec.html#elementvalid 
;	WFC: REC-xml-names/#dt-NSName

; Note, although XML Recommendation does not explicitly say it,
; xmlns and xmlns: attributes don't have to be declared (although they
; can be declared, to specify their default value)

; Procedure:  SSAX:complete-start-tag tag-head port elems entities namespaces
(define SSAX:complete-start-tag

 (let ((xmlns (string->symbol "xmlns"))
       (largest-dummy-decl-attr (list SSAX:largest-unres-name #f #f #f)))

  ; Scan through the attlist and validate it, against decl-attrs
  ; Return an assoc list with added fixed or implied attrs.
  ; Note that both attlist and decl-attrs are ATTLISTs, and therefore,
  ; sorted
  (define (validate-attrs port attlist decl-attrs)

    ; Check to see decl-attr is not of use type REQUIRED. Add
    ; the association with the default value, if any declared
    (define (add-default-decl decl-attr result)
      (let*-values
	 (((attr-name content-type use-type default-value)
	   (apply values decl-attr)))
	 (and (eq? use-type 'REQUIRED)
	      (parser-error port "[RequiredAttr] broken for" attr-name))
	 (if default-value
	     (cons (cons attr-name default-value) result)
	     result)))

    (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
      (if (attlist-null? attlist)
	  (attlist-fold add-default-decl result decl-attrs)
	  (let*-values
	   (((attr attr-others)
	     (attlist-remove-top attlist))
	    ((decl-attr other-decls)
	     (if (attlist-null? decl-attrs)
		 (values largest-dummy-decl-attr decl-attrs)
		 (attlist-remove-top decl-attrs)))
	    )
	   (case (name-compare (car attr) (car decl-attr))
	     ((<) 
	      (if (or (eq? xmlns (car attr))
		      (and (pair? (car attr)) (eq? xmlns (caar attr))))
		  (loop attr-others decl-attrs (cons attr result))
		  (parser-error port "[ValueType] broken for " attr)))
	     ((>) 
	      (loop attlist other-decls 
		    (add-default-decl decl-attr result)))
	     (else	; matched occurrence of an attr with its declaration
	      (let*-values
	       (((attr-name content-type use-type default-value)
		 (apply values decl-attr)))
	       ; Run some tests on the content of the attribute
	       (cond
		((eq? use-type 'FIXED)
		 (or (equal? (cdr attr) default-value)
		     (parser-error port "[FixedAttr] broken for " attr-name)))
		((eq? content-type 'CDATA) #t) ; everything goes
		((pair? content-type)
		 (or (member (cdr attr) content-type)
		     (parser-error port "[enum] broken for " attr-name "="
			    (cdr attr))))
		(else
		 (SSAX:warn port "declared content type " content-type
		       " not verified yet")))
	       (loop attr-others other-decls (cons attr result)))))
	   ))))
	    

  ; Add a new namespace declaration to namespaces.
  ; First we convert the uri-str to a uri-symbol and search namespaces for
  ; an association (_ user-prefix . uri-symbol).
  ; If found, we return the argument namespaces with an association
  ; (prefix user-prefix . uri-symbol) prepended.
  ; Otherwise, we prepend (prefix uri-symbol . uri-symbol)
  (define (add-ns port prefix uri-str namespaces)
    (and (equal? "" uri-str)
	 (parser-error port "[dt-NSName] broken for " prefix))
    (let ((uri-symbol (SSAX:uri-string->symbol uri-str)))
      (let loop ((nss namespaces))
	(cond 
	 ((null? nss)
	  (cons (cons* prefix uri-symbol uri-symbol) namespaces))
	 ((eq? uri-symbol (cddar nss))
	  (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
	 (else (loop (cdr nss)))))))
      
  ; partition attrs into proper attrs and new namespace declarations
  ; return two values: proper attrs and the updated namespace declarations
  (define (adjust-namespace-decl port attrs namespaces)
    (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
      (cond
       ((null? attrs) (values proper-attrs namespaces))
       ((eq? xmlns (caar attrs))	; re-decl of the default namespace
	(loop (cdr attrs) proper-attrs 
	      (if (equal? "" (cdar attrs))	; un-decl of the default ns
		  (cons (cons* '*DEFAULT* #f #f) namespaces)
		  (add-ns port '*DEFAULT* (cdar attrs) namespaces))))
       ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
	(loop (cdr attrs) proper-attrs
	      (add-ns port (cdaar attrs) (cdar attrs) namespaces)))
       (else
	(loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))

  ;; The body of the function
  (lambda (tag-head port elems entities namespaces)
    (let*-values 
     (((attlist) (SSAX:read-attributes port entities))
      ((empty-el-tag?)
       (begin
         (SSAX:skip-S port)
         (and
          (eqv? #\/ 
                (assert-curr-char '(#\> #\/) "XML [40], XML [44], no '>'" port))
          (assert-curr-char '(#\>) "XML [44], no '>'" port))))
      ((elem-content decl-attrs)	; see xml-decl for their type
       (if elems			; elements declared: validate!
           (cond
            ((assoc tag-head elems) =>
             (lambda (decl-elem)        ; of type xml-decl::decl-elem
               (values
                (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem))
                (caddr decl-elem))))
            (else
             (parser-error port "[elementvalid] broken, no decl for " tag-head)))
           (values                      ; non-validating parsing
            (if empty-el-tag? 'EMPTY-TAG 'ANY)
            #f)                         ; no attributes declared
           ))
      ((merged-attrs) (if decl-attrs (validate-attrs port attlist decl-attrs)
                          (attlist->alist attlist)))
      ((proper-attrs namespaces)
       (adjust-namespace-decl port merged-attrs namespaces))
      )
     ;;(cerr "proper attrs: " proper-attrs nl)
     ;; build the return value
     (values
      (SSAX:resolve-name port tag-head namespaces #t)
      (fold-right
       (lambda (name-value attlist)
         (or
          (attlist-add attlist
                       (cons (SSAX:resolve-name port (car name-value) namespaces #f)
                             (cdr name-value)))
          (parser-error port "[uniqattspec] after NS expansion broken for " 
                        name-value)))
       (make-empty-attlist)
       proper-attrs)
      namespaces
      elem-content)))))

; procedure+:	SSAX:read-external-ID PORT
;
; This procedure parses an ExternalID production:
; [75] ExternalID ::= 'SYSTEM' S SystemLiteral
;		| 'PUBLIC' S PubidLiteral S SystemLiteral
; [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'") 
; [12] PubidLiteral ::=  '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
; [13] PubidChar ::=  #x20 | #xD | #xA | [a-zA-Z0-9]
;                         | [-'()+,./:=?;!*#@$_%]
;
; This procedure is supposed to be called when an ExternalID is expected;
; that is, the current character must be either #\S or #\P that start
; correspondingly a SYSTEM or PUBLIC token. This procedure returns the
; SystemLiteral as a string. A PubidLiteral is disregarded if present.
 
(define (SSAX:read-external-ID port)
  (let ((discriminator (SSAX:read-NCName port)))
    (assert-curr-char SSAX:S-chars "space after SYSTEM or PUBLIC" port)
    (SSAX:skip-S port)
    (let ((delimiter 
          (assert-curr-char '(#\' #\" ) "XML [11], XML [12]" port)))
      (cond
        ((eq? discriminator (string->symbol "SYSTEM"))
          (begin0
            (next-token '() (list delimiter) "XML [11]" port)
            (read-char port)	; reading the closing delim
            ))
         ((eq? discriminator (string->symbol "PUBLIC"))
           (skip-until (list delimiter) port)
           (assert-curr-char SSAX:S-chars "space after PubidLiteral" port)
           (SSAX:skip-S port)
           (let* ((delimiter 
                  (assert-curr-char '(#\' #\" ) "XML [11]" port))
                  (systemid
                    (next-token '() (list delimiter) "XML [11]" port)))
                (read-char port)	; reading the closing delim
                systemid))
         (else
           (parser-error port "XML [75], " discriminator 
		  " rather than SYSTEM or PUBLIC"))))))


;-----------------------------------------------------------------------------
;			Higher-level parsers and scanners
;
; They parse productions corresponding to the whole (document) entity
; or its higher-level pieces (prolog, root element, etc).


; Scan the Misc production in the context
; [1]  document ::=  prolog element Misc*
; [22] prolog ::= XMLDecl? Misc* (doctypedec l Misc*)?
; [27] Misc ::= Comment | PI |  S
;
; The following function should be called in the prolog or epilog contexts.
; In these contexts, whitespaces are completely ignored.
; The return value from SSAX:scan-Misc is either a PI-token,
; a DECL-token, a START token, or EOF.
; Comments are ignored and not reported.

(define (SSAX:scan-Misc port)
  (let loop ((c (SSAX:skip-S port)))
    (cond
      ((eof-object? c) c)
      ((not (char=? c #\<))
        (parser-error port "XML [22], char '" c "' unexpected"))
      (else
        (let ((token (SSAX:read-markup-token port)))
          (case (xml-token-kind token)
            ((COMMENT) (loop (SSAX:skip-S port)))
            ((PI DECL START) token)
            (else 
              (parser-error port "XML [22], unexpected token of kind "
		     (xml-token-kind token)
		     ))))))))

; procedure+:	SSAX:read-char-data PORT EXPECT-EOF? STR-HANDLER SEED
;
; This procedure is to read the character content of an XML document
; or an XML element.
; [43] content ::= 
;	(element | CharData | Reference | CDSect | PI
; 	| Comment)*
; To be more precise, the procedure reads CharData, expands CDSect
; and character entities, and skips comments. The procedure stops
; at a named reference, EOF, at the beginning of a PI or a start/end tag.
;
; port
;	a PORT to read
; expect-eof?
;	a boolean indicating if EOF is normal, i.e., the character
;	data may be terminated by the EOF. EOF is normal
;	while processing a parsed entity.
; str-handler
;	a STR-HANDLER
; seed
;	an argument passed to the first invocation of STR-HANDLER.
;
; The procedure returns two results: SEED and TOKEN.
; The SEED is the result of the last invocation of STR-HANDLER, or the
; original seed if STR-HANDLER was never called.
;
; TOKEN can be either an eof-object (this can happen only if
; expect-eof? was #t), or:
;     - an xml-token describing a START tag or an END-tag;
;	For a start token, the caller has to finish reading it.
;     - an xml-token describing the beginning of a PI. It's up to an
;	application to read or skip through the rest of this PI;
;     - an xml-token describing a named entity reference.
;
; CDATA sections and character references are expanded inline and
; never returned. Comments are silently disregarded.
;
; As the XML Recommendation requires, all whitespace in character data
; must be preserved. However, a CR character (#xD) must be disregarded
; if it appears before a LF character (#xA), or replaced by a #xA character
; otherwise. See Secs. 2.10 and 2.11 of the XML Recommendation. See also
; the canonical XML Recommendation.

	; SSAX:read-char-data port expect-eof? str-handler seed
(define SSAX:read-char-data
 (let
     ((terminators-usual '(#\< #\& #\return))
      (terminators-usual-eof '(#\< *eof* #\& #\return))

      (handle-fragment
       (lambda (fragment str-handler seed)
	 (if (string-null? fragment) seed
	     (str-handler fragment "" seed)))))

   (lambda (port expect-eof? str-handler seed)

     ; Very often, the first character we encounter is #\<
     ; Therefore, we handle this case in a special, fast path
     (if (eqv? #\< (peek-char port))

         ; The fast path
	 (let ((token (SSAX:read-markup-token port)))
	   (case (xml-token-kind token)
	     ((START END)	; The most common case
	      (values seed token))
	     ((CDSECT)
	      (let ((seed (SSAX:read-CDATA-body port str-handler seed)))
		(SSAX:read-char-data port expect-eof? str-handler seed)))
	     ((COMMENT) (SSAX:read-char-data port expect-eof?
					     str-handler seed))
	     (else
	      (values seed token))))


         ; The slow path
	 (let ((char-data-terminators
		(if expect-eof? terminators-usual-eof terminators-usual)))

	   (let loop ((seed seed))
	     (let* ((fragment
		     (next-token '() char-data-terminators 
				 "reading char data" port))
		    (term-char (peek-char port)) ; one of char-data-terminators
		    )
	       (if (eof-object? term-char)
		   (values
		    (handle-fragment fragment str-handler seed)
		    term-char)
		   (case term-char
		     ((#\<)
		      (let ((token (SSAX:read-markup-token port)))
			(case (xml-token-kind token)
			  ((CDSECT)
			   (loop
			    (SSAX:read-CDATA-body port str-handler
			        (handle-fragment fragment str-handler seed))))
			  ((COMMENT)
			   (loop (handle-fragment fragment str-handler seed)))
			  (else
			   (values
			    (handle-fragment fragment str-handler seed)
			    token)))))
		     ((#\&)
		      (case (peek-next-char port)
			((#\#) (read-char port) 
			 (loop (str-handler fragment
				       (string (SSAX:read-char-ref port))
				       seed)))
			(else
			 (let ((name (SSAX:read-NCName port)))
			   (assert-curr-char '(#\;) "XML [68]" port)
			   (values
			    (handle-fragment fragment str-handler seed)
			    (make-xml-token 'ENTITY-REF name))))))
		     (else		; This must be a CR character
		      (if (eqv? (peek-next-char port) #\newline)
			  (read-char port))
		      (loop (str-handler fragment (string #\newline) seed))))
		   ))))))))



; procedure+:	SSAX:assert-token TOKEN KIND GI
; Make sure that TOKEN is of anticipated KIND and has anticipated GI
; Note GI argument may actually be a pair of two symbols, Namespace
; URI or the prefix, and of the localname.
; If the assertion fails, error-cont is evaluated by passing it
; three arguments: token kind gi. The result of error-cont is returned.
(define (SSAX:assert-token token kind gi error-cont)
  (or
    (and (xml-token? token)
      (eq? kind (xml-token-kind token))
      (equal? gi (xml-token-head token)))
    (error-cont token kind gi)))

;========================================================================
;		Highest-level parsers: XML to SXML

; These parsers are a set of syntactic forms to instantiate a SSAX parser.
; A user can instantiate the parser to do the full validation, or
; no validation, or any particular validation. The user specifies
; which PI he wants to be notified about. The user tells what to do
; with the parsed character and element data. The latter handlers
; determine if the parsing follows a SAX or a DOM model.

; SSAX:make-pi-parser my-pi-handlers
; Create a parser to parse and process one Processing Element (PI).

; my-pi-handlers
;	An assoc list of pairs (PI-TAG . PI-HANDLER)
;	where PI-TAG is an NCName symbol, the PI target, and
;	PI-HANDLER is a procedure PORT PI-TAG SEED
;	where PORT points to the first symbol after the PI target.
;	The handler should read the rest of the PI up to and including
;	the combination '?>' that terminates the PI. The handler should
;	return a new seed.
;	One of the PI-TAGs may be a symbol *DEFAULT*. The corresponding
;	handler will handle PIs that no other handler will. If the
;	*DEFAULT* PI-TAG is not specified, SSAX:make-pi-parser will make
;	one, which skips the body of the PI
;	
; The output of the SSAX:make-pi-parser is a procedure
;	PORT PI-TAG SEED
; that will parse the current PI accoding to user-specified handlers.

(define SSAX:make-pi-parser
  (lambda (pi-handlers)
    (lambda (port target seed)
      (let loop ((pi-handlers pi-handlers) (default #f))
	(cond
	 ((null? pi-handlers)
	  (if default
	      (default port target seed)
	      (begin
		(SSAX:warn port "Skipping PI: " target)
		(SSAX:skip-pi port)
		seed)))
	 ((eq? '*DEFAULT* (caar pi-handlers))
	  (loop (cdr pi-handlers) (cdar pi-handlers)))
	 ((eq? (caar pi-handlers) default)
	  ((cdar pi-handlers) port target seed))
	 (else
	  (loop (cdr pi-handlers) default)))))))
	  

; syntax: SSAX:make-elem-parser my-new-level-seed my-finish-element
;				my-char-data-handler my-pi-handlers

; Create a parser to parse and process one element, including its
; character content or children elements. The parser is typically
; applied to the root element of a document.

; my-new-level-seed
;	procedure ELEM-GI ATTRIBUTES NAMESPACES EXPECTED-CONTENT SEED
;		where ELEM-GI is a RES-NAME of the element
;		about to be processed.
;	This procedure is to generate the seed to be passed
;	to handlers that process the content of the element.
;	This is the function identified as 'fdown' in the denotational
;	semantics of the XML parser given in the title comments to this
;	file.
;
; my-finish-element
;	procedure ELEM-GI ATTRIBUTES NAMESPACES PARENT-SEED SEED
;	This procedure is called when parsing of ELEM-GI is finished.
;	The SEED is the result from the last content parser (or
;	from my-new-level-seed if the element has the empty content).
;	PARENT-SEED is the same seed as was passed to my-new-level-seed.
;	The procedure is to generate a seed that will be the result
;	of the element parser.
;	This is the function identified as 'fup' in the denotational
;	semantics of the XML parser given in the title comments to this
;	file.
;
; my-char-data-handler
;	A STR-HANDLER
;
; my-pi-handlers
;	See SSAX:make-pi-handler above
;

; The generated parser is a
;	procedure START-TAG-HEAD PORT ELEMS ENTITIES
;	NAMESPACES PRESERVE-WS? SEED
; The procedure must be called after the start tag token has been
; read. START-TAG-HEAD is an UNRES-NAME from the start-element tag.
; ELEMS is an instance of xml-decl::elems.
; See SSAX:complete-start-tag::preserve-ws?

; Faults detected:
;	VC: XML-Spec.html#elementvalid 
;	WFC: XML-Spec.html#GIMatch

(define SSAX:make-elem-parser
  (lambda (my-new-level-seed my-finish-element
			     my-char-data-handler my-pi-handlers)
  
    (lambda (start-tag-head port elems entities namespaces
			    preserve-ws? seed)

      (define xml-space-gi (cons SSAX:Prefix-XML
				 (string->symbol "space")))

      (let handle-start-tag ((start-tag-head start-tag-head)
			     (port port) (entities entities)
			     (namespaces namespaces)
			     (preserve-ws? preserve-ws?) (parent-seed seed))
	(let*-values
	    (((elem-gi attributes namespaces expected-content)
	      (SSAX:complete-start-tag start-tag-head port elems
				       entities namespaces))
	     ((seed)
	      (my-new-level-seed elem-gi attributes
				 namespaces expected-content parent-seed)))
	  (case expected-content
	    ((EMPTY-TAG)
	     (my-finish-element
	      elem-gi attributes namespaces parent-seed seed))
	    ((EMPTY)			; The end tag must immediately follow
	     (SSAX:assert-token 
	      (and (eqv? #\< (SSAX:skip-S port)) (SSAX:read-markup-token port))
	      'END  start-tag-head
	      (lambda (token exp-kind exp-head)
		(parser-error port "[elementvalid] broken for " token 
			      " while expecting "
			      exp-kind exp-head)))
	     (my-finish-element
	      elem-gi attributes namespaces parent-seed seed))
	    (else			; reading the content...
	     (let ((preserve-ws?	; inherit or set the preserve-ws? flag
		    (cond
		     ((assoc xml-space-gi attributes) =>
		      (lambda (name-value)
			(equal? "preserve" (cdr name-value))))
		     (else preserve-ws?))))
	       (let loop ((port port) (entities entities)
			  (expect-eof? #f) (seed seed))
		 (let*-values
		     (((seed term-token)
		       (SSAX:read-char-data port expect-eof?
					    my-char-data-handler seed)))
		   (if (eof-object? term-token)
		       seed
		       (case (xml-token-kind term-token)
			 ((END)
			  (SSAX:assert-token term-token 'END  start-tag-head
					     (lambda (token exp-kind exp-head)
					       (parser-error port "[GIMatch] broken for "
							     term-token " while expecting "
							     exp-kind exp-head)))
			  (my-finish-element
			   elem-gi attributes namespaces parent-seed seed))
			 ((PI)
			  (let ((seed 
				 ((SSAX:make-pi-parser my-pi-handlers)
				  port (xml-token-head term-token) seed)))
			    (loop port entities expect-eof? seed)))
			 ((ENTITY-REF)
			  (let ((seed
				 (SSAX:handle-parsed-entity
				  port (xml-token-head term-token)
				  entities
				  (lambda (port entities seed)
				    (loop port entities #t seed))
				  my-char-data-handler
				  seed))) ; keep on reading the content after ent
			    (loop port entities expect-eof? seed)))
			 ((START)	; Start of a child element
			  (if (eq? expected-content 'PCDATA)
			      (parser-error port "[elementvalid] broken for "
					    elem-gi
					    " with char content only; unexpected token "
					    term-token))
					; Do other validation of the element content
			  (let ((seed
				 (handle-start-tag
				  (xml-token-head term-token)
				  port entities namespaces
				  preserve-ws? seed)))
			    (loop port entities expect-eof? seed)))
			 (else
			  (parser-error port "XML [43] broken for "
					term-token))))))))
	    )))
      )))

; syntax: SSAX:make-parser user-handler-tag user-handler-proc ...
;
; Create an XML parser, an instance of the XML parsing framework.
; This will be a SAX, a DOM, or a specialized parser depending
; on the supplied user-handlers.

; user-handler-tag is a symbol that identifies a procedural expression
; that follows the tag. Given below are tags and signatures of the
; corresponding procedures. Not all tags have to be specified. If some
; are omitted, reasonable defaults will apply.
;

; tag: DOCTYPE
; handler-procedure: PORT DOCNAME SYSTEMID INTERNAL-SUBSET? SEED
; If internal-subset? is #t, the current position in the port
; is right after we have read #\[ that begins the internal DTD subset.
; We must finish reading of this subset before we return
; (or must call skip-internal-subset if we aren't interested in reading it).
; The port at exit must be at the first symbol after the whole
; DOCTYPE declaration.
; The handler-procedure must generate four values:
;	ELEMS ENTITIES NAMESPACES SEED
; See xml-decl::elems for ELEMS. It may be #f to switch off the validation.
; NAMESPACES will typically contain USER-PREFIXes for selected URI-SYMBs.
; The default handler-procedure skips the internal subset,
; if any, and returns (values #f '() '() seed)

; tag: UNDECL-ROOT
; handler-procedure: ELEM-GI SEED
; where ELEM-GI is an UNRES-NAME of the root element. This procedure
; is called when an XML document under parsing contains _no_ DOCTYPE
; declaration.
; The handler-procedure, as a DOCTYPE handler procedure above,
; must generate four values:
;	ELEMS ENTITIES NAMESPACES SEED
; The default handler-procedure returns (values #f '() '() seed)

; tag: DECL-ROOT
; handler-procedure: ELEM-GI SEED
; where ELEM-GI is an UNRES-NAME of the root element. This procedure
; is called when an XML document under parsing does contains the DOCTYPE
; declaration.
; The handler-procedure must generate a new SEED (and verify
; that the name of the root element matches the doctype, if the handler
; so wishes). 
; The default handler-procedure is the identity function.

; tag: NEW-LEVEL-SEED
; handler-procedure: see SSAX:make-elem-parser, my-new-level-seed

; tag: FINISH-ELEMENT
; handler-procedure: see SSAX:make-elem-parser, my-finish-element

; tag: CHAR-DATA-HANDLER
; handler-procedure: see SSAX:make-elem-parser, my-char-data-handler

; tag: PI
; handler-procedure: see SSAX:make-pi-parser
; The default value is '()
 
; The generated parser is a
;	procedure PORT SEED

; This procedure parses the document prolog and then exits to
; an element parser (created by SSAX:make-elem-parser) to handle
; the rest.
;
; [1]  document ::=  prolog element Misc*
; [22] prolog ::= XMLDecl? Misc* (doctypedec | Misc*)?
; [27] Misc ::= Comment | PI |  S
;
; [28] doctypedecl ::=  '<!DOCTYPE' S Name (S ExternalID)? S? 
;			('[' (markupdecl | PEReference | S)* ']' S?)? '>'
; [29] markupdecl ::= elementdecl | AttlistDecl
;                      | EntityDecl
;                      | NotationDecl | PI
;                      | Comment 
;

(define SSAX:make-parser
  (lambda user-handlers

    ;; An assoc list of user-handler-tag and default handlers
    (define all-handlers
      `((DOCTYPE .
		 ,(lambda (port docname systemid internal-subset? seed)
		   (when internal-subset?
		     (SSAX:warn port "Internal DTD subset is not currently handled ")
		     (SSAX:skip-internal-dtd port))
		   (SSAX:warn port "DOCTYPE DECL " docname " " 
			      systemid " found and skipped")
		   (values #f '() '() seed)
		   ))
	(UNDECL-ROOT .
		     ,(lambda (elem-gi seed) (values #f '() '() seed)))
	(DECL-ROOT .
		   ,(lambda (elem-gi seed) seed))
	(NEW-LEVEL-SEED . REQD)		; required
	(FINISH-ELEMENT . REQD)		; required
	(CHAR-DATA-HANDLER . REQD)	; required
	(PI . ())
	))

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

					; create an assoc list of tags and handlers
					; based on the defaults and on the given handlers
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
	(error "Odd number of arguments to SSAX:make-parser"))
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


      (lambda (port seed)

	 ;; We must've just scanned the DOCTYPE token 
	 ;; Handle the doctype declaration and exit to
	 ;; scan-for-significant-prolog-token-2, and eventually, to the
	 ;; element parser.
	 (define (handle-decl port token-head seed)
	   (or (eq? (string->symbol "DOCTYPE") token-head)
	       (parser-error port "XML [22], expected DOCTYPE declaration, found "
			     token-head))
	   (assert-curr-char SSAX:S-chars "XML [28], space after DOCTYPE" port)
	   (SSAX:skip-S port)
	   (let*-values 
	       (((docname) (SSAX:read-QName port))
		((systemid)
		 (and (SSAX:ncname-starting-char? (SSAX:skip-S port))
		      (SSAX:read-external-ID port)))
		((internal-subset?)
		 (begin (SSAX:skip-S port)
			(eqv? #\[ (assert-curr-char '(#\> #\[)
						    "XML [28], end-of-DOCTYPE" port))))
		((elems entities namespaces seed)
		 ((get-handler 'DOCTYPE) port docname systemid
		  internal-subset? seed))
		)
	     (scan-for-significant-prolog-token-2 port elems entities namespaces
						  seed)))


	 ;; Scan the leading PIs until we encounter either a doctype declaration
	 ;; or a start token (of the root element)
	 ;; In the latter two cases, we exit to the appropriate continuation
	 (define (scan-for-significant-prolog-token-1 port seed)
	   (let ((token (SSAX:scan-Misc port)))
	     (if (eof-object? token)
		 (parser-error port "XML [22], unexpected EOF")
		 (case (xml-token-kind token)
		   ((PI)
		    (let ((seed 
			   ((SSAX:make-pi-parser (get-handler 'PI))
			    port (xml-token-head token) seed)))
		      (scan-for-significant-prolog-token-1 port seed)))
		   ((DECL) (handle-decl port (xml-token-head token) seed))
		   ((START)
		    (let*-values
			(((elems entities namespaces seed)
			  ((get-handler 'UNDECL-ROOT) (xml-token-head token) seed)))
		      (element-parser (xml-token-head token) port elems
				      entities namespaces #f seed)))
		   (else (parser-error port "XML [22], unexpected markup "
				       token))))))


	 ;; Scan PIs after the doctype declaration, till we encounter
	 ;; the start tag of the root element. After that we exit
	 ;; to the element parser
	 (define (scan-for-significant-prolog-token-2 port elems entities
						      namespaces seed)
	   (let ((token (SSAX:scan-Misc port)))
	     (if (eof-object? token)
		 (parser-error port "XML [22], unexpected EOF")
		 (case (xml-token-kind token)
		   ((PI)
		    (let ((seed 
			   ((SSAX:make-pi-parser (get-handler 'PI))
			    port (xml-token-head token) seed)))
		      (scan-for-significant-prolog-token-2 port elems entities
							   namespaces seed)))
		   ((START)
		    (element-parser (xml-token-head token) port elems
				    entities namespaces #f
				    ((get-handler 'DECL-ROOT) (xml-token-head token) seed)))
		   (else (parser-error port "XML [22], unexpected markup "
				       token))))))


	 ;; A procedure start-tag-head port elems entities namespaces
	 ;;		 preserve-ws? seed
	 (define element-parser
	   (SSAX:make-elem-parser (get-handler 'NEW-LEVEL-SEED)
				  (get-handler 'FINISH-ELEMENT)
				  (get-handler 'CHAR-DATA-HANDLER)
				  (get-handler 'PI)))

	 ;; Get the ball rolling ...
	 (scan-for-significant-prolog-token-1 port seed)
	 ))))


;========================================================================
;		Highest-level parsers: XML to SXML
;

; procedure: SSAX:XML->SXML PORT NAMESPACE-PREFIX-ASSIG
;
; This is an instance of a SSAX parser above that returns an SXML
; representation of the XML document to be read from PORT.
; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
; that assigns USER-PREFIXes to certain namespaces identified by
; particular URI-STRINGs. It may be an empty list.
; The procedure returns an SXML tree. The port points out to the
; first character after the root element.

(define (SSAX:XML->SXML port namespace-prefix-assig)
  (letrec
      ((namespaces
	(map (lambda (el)
	       (cons* #f (car el) (SSAX:uri-string->symbol (cdr el))))
	     namespace-prefix-assig))

       (RES-NAME->SXML
	(lambda (res-name)
	  (string->symbol
	   (string-append
	    (symbol->string (car res-name))
	    ":"
	    (symbol->string (cdr res-name))))))

       ; given the list of fragments (some of which are text strings)
       ; reverse the list and concatenate adjacent text strings
       (reverse-collect-str
	(lambda (fragments)
	  (if (null? fragments) '()	; a shortcut
	      (let loop ((fragments fragments) (result '()) (strs '()))
		(cond
		 ((null? fragments)
		  (if (null? strs) result
		      (cons (apply string-append strs) result)))
		 ((string? (car fragments))
		  (loop (cdr fragments) result (cons (car fragments) strs)))
		 (else
		  (loop (cdr fragments)
			(cons
			 (car fragments)
			 (if (null? strs) result
			     (cons (apply string-append strs) result)))
			'())))))))

       ; given the list of fragments (some of which are text strings)
       ; reverse the list and concatenate adjacent text strings
       ; We also drop "unsignificant" whitespace, that is, whitespace
       ; in front, behind and between elements. The whitespace that
       ; is included in character data is not affected.
       (reverse-collect-str-drop-ws
	(lambda (fragments)
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
		      '() #t))))))))
       )
    (let ((result
	   (reverse
	    ((SSAX:make-parser
	     'NEW-LEVEL-SEED 
	     (lambda (elem-gi attributes namespaces
			      expected-content seed)
	       '())
   
	     'FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
	       (let ((seed (reverse-collect-str-drop-ws seed))
		     (attrs
		      (attlist-fold
		       (lambda (attr accum)
			 (cons (list 
				(if (symbol? (car attr)) (car attr)
				    (RES-NAME->SXML (car attr)))
				(cdr attr)) accum))
		       '() attributes)))
		 (cons
		  (cons 
		   (if (symbol? elem-gi) elem-gi
		       (RES-NAME->SXML elem-gi))
		   (if (null? attrs) seed
		       (cons (cons '@ attrs) seed)))
		  parent-seed)))

	     'CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
	       (if (string-null? string2) (cons string1 seed)
		   (cons* string2 string1 seed)))

	     'DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (SSAX:warn port
			   "Internal DTD subset is not currently handled ")
		     (SSAX:skip-internal-dtd port))
	       (SSAX:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f '() namespaces seed))

	     'UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() namespaces seed))

	     'PI
	     `((*DEFAULT* .
		,(lambda (port pi-tag seed)
		  (cons
		   (list '*PI* pi-tag (SSAX:read-pi-body-as-string port))
		   seed))))
	     )
	    port '()))))
      (cons '*TOP*
	    (if (null? namespace-prefix-assig) result
		(cons (cons '*NAMESPACES* 
			    (map (lambda (ns) (list (car ns) (cdr ns)))
				 namespace-prefix-assig))
		      result)))
)))

;;; arch-tag: c30e0855-8f4c-4e8c-ab41-ec24ec391e44
;;; ssax.scm ends here
