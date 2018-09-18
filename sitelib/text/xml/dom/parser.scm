;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dom/parser.scm - XML parser for DOM
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; reference
;; - https://www.w3.org/TR/2008/REC-xml-20081126/
;; - https://www.w3.org/TR/xml-names/ (namespace TBD)

;; This library returns DOM AST which is *not* DOM yet but intermediate
;; data structure. At some point, we will be able to convert SXML to DOM AST
;; but that's a bit far future.

;; this library is basically only internal use.
;; to construct actual DOM tree, use (text xml dom factory)
#!nounbound
(library (text xml dom parser)
    (export +xml:char-set+ +xml:name-start-char-set+ +xml:name-char-set+
	    $xml:s
	    $xml:name $xml:names $xml:qname
	    $xml:nmtoken $xml:nmtokens
	    $xml:entity-value $xml:att-value
	    $xml:system-literal $xml:pubid-literal
	    $xml:pi
	    $xml:cd-sect
	    $xml:prolog $xml:xml-decl $xml:doctype-decl
	    $xml:element-decl $xml:entity-decl $xml:attlist-decl
	    $xml:notation-decl

	    $xml:element $xml:document
	    $xml:char-data $xml:comment
	    
	    $xml:char-ref $xml:entity-ref $xml:reference $xml:pe-reference
	    $xml:entity-value

	    ;; for factory
	    qname? qname-prefix qname-namespace qname-local-part
	    )
    (import (rnrs)
	    (peg)
	    (peg chars)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))
;; alist of prefix and namespace
;; e.g. (("xsd" . "http://www.w3.org/2001/XMLSchema") ...)
(define *current-namespaces* (make-parameter '()))

;; [2] Char ::=	#x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD]
;;            | [#x10000-#x10FFFF]
(define +xml:char-set+
  (char-set-union
   (char-set #\x9)
   (char-set #\xA)
   (char-set #\xD)
   (ucs-range->char-set #x20 #xD800)
   (ucs-range->char-set #xE000 #xFFFE)
   (ucs-range->char-set #x10000 #x110000)))

;; [3] S ::= (#x20 | #x9 | #xD | #xA)+
(define $xml:s
  ($do (c* ($many ($or ($eqv? #\x20) ($eqv? #\x9) ($eqv? #\xD) ($eqv? #\xA)) 1))
       ($return (list->string c*))))

;; [4] NameStartChar ::=   ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
;;                     | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D]
;;                     | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F]
;;                     | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF]
;;                     | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
(define +xml:name-start-char-set+
  (char-set-union
   (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ:_")
   (ucs-range->char-set #xC0 #xD7)
   (ucs-range->char-set #xD8 #xF7)
   (ucs-range->char-set #xF8 #x300)
   (ucs-range->char-set #x370 #x37E)
   (ucs-range->char-set #x37F #x2000)
   (ucs-range->char-set #x200C #x200E)
   (ucs-range->char-set #x2070 #x2190)
   (ucs-range->char-set #x2C00 #x2FF0)
   (ucs-range->char-set #x3001 #xD800)
   (ucs-range->char-set #xF900 #xFDD0)
   (ucs-range->char-set #xFDF0 #xFFFE)
   (ucs-range->char-set #x10000 #xD0000)))
;; [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
;;                 | [#x0300-#x036F] | [#x203F-#x2040]
(define +xml:name-char-set+
  (char-set-union
   +xml:name-start-char-set+
   (string->char-set "-.0123456789")
   (char-set #\xB7)
   (ucs-range->char-set #x0300 #x0370)
   (ucs-range->char-set #x203F #x2041)))
;; helper
(define $in-set $char-set-contains?)

;; [5] Name   ::= NameStartChar (NameChar)*
(define $xml:name
  ($do (s ($in-set +xml:name-start-char-set+))
       (c* ($many ($in-set +xml:name-char-set+)))
       ($return (list->string (cons s c*)))))
;; [6] Names  ::= Name (#x20 Name)*
(define $xml:names
  ($do (n $xml:name)
       (n* ($many ($do (($eqv? #\x20)) (n $xml:name) ($return n))))
       ($return (cons n n*))))
;; [7] Nmtoken ::= (NameChar)+
(define $xml:nmtoken
  ($do (c* ($many ($in-set +xml:name-char-set+) 1))
       ($return (list->string c*))))
;; [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
(define $xml:nmtokens
  ($do (t $xml:nmtoken)
       (t* ($many ($do (($eqv? #\x20)) (n $xml:nmtoken) ($return n))))
       ($return (cons t t*))))

;; QName stuff
;; [4] NCName ::= Name - (Char* ':' Char*) /* An XML Name, minus the ":" */
(define $xml:ncname
  ;; we define newly for better performance...
  (let ((scs (char-set-difference +xml:name-start-char-set+ (char-set #\:)))
	(cs (char-set-difference +xml:name-char-set+ (char-set #\:))))
    ($do (s ($in-set scs))
	 (c* ($many ($in-set cs)))
	 ($return (list->string (cons s c*))))))
;; [3] DefaultAttName ::= 'xmlns'
(define $xml:default-att-name ($token "xmlns"))
;; [2] PrefixedAttName ::= 'xmlns:' NCName
(define $xml:prefixed-att-name
  ($do (($token "xmlns:"))
       (n $xml:ncname)
       ($return n)))
;; [1] NSAttName ::= PrefixedAttName
;;                 | DefaultAttName
(define $xml:nsatt-name
  ($or ($do (n $xml:prefixed-att-name) ($return `(xmlns ,n)))
       ($do $xml:default-att-name ($return '(xmlns #f)))))

;; [10] Prefix ::= NCName
;; [11] LocalPart ::= NCName
(define $xml:prefix $xml:ncname)
(define $xml:local-part $xml:ncname)
;; [8] PrefixedName ::= Prefix ':' LocalPart
(define $xml:prefixed-name
  ($do (p $xml:prefix)
       (($eqv? #\:))
       (l $xml:local-part)
       ($return (list p l))))
;; [9] UnprefixedName ::= LocalPart
(define $xml:unprefixed-name $xml:local-part)
;; [7] QName ::= PrefixedName
;;             | UnprefixedName
(define $xml:qname
  ($or ($do (p $xml:prefixed-name) ($return (apply make-qname p)))
       ($do (l $xml:unprefixed-name) ($return l))))

(define (find-namespace prefix)
  (cond ((assoc prefix (*current-namespaces*)) => cdr)
	(else #f)))
(define make-qname
  (case-lambda
   ((prefix local-part) `(qname ,(find-namespace prefix) ,prefix ,local-part))
   ((namespace prefix local-part) `(qname ,namespace ,prefix ,local-part))))
(define (qname? name) (and (pair? name) (eq? (car name) 'qname)))
(define (qname-namespace qname) (cadr qname))
(define (qname-prefix qname) (caddr qname))
(define (qname-local-part qname) (cadddr qname))

;; [66] CharRef ::= '&#' [0-9]+ ';'
;;                | '&#x' [0-9a-fA-F]+ ';'
;; Returns: (char-ref radix n)
(define ascii-digit-set (char-set-intersection char-set:ascii char-set:digit))
;; TODO should we expand reference to char according to the option?
(define $xml:char-ref
  ($or ($do (($token "&#"))
	    (c* ($many ($in-set ascii-digit-set) 1))
	    (($eqv? #\;))
	    ($return `(char-ref 10 ,(string->number (list->string c*)))))
       ($do (($token "&#x"))
	    (c* ($many ($in-set char-set:hex-digit) 1))
	    (($eqv? #\;))
	    ($return `(char-ref 16 ,(string->number (list->string c*) 16))))))

;; [68] EntityRef ::= '&' Name ';'
(define $xml:entity-ref
  ($do (($eqv? #\&))
       (n $xml:name)
       (($eqv? #\;))
       ($return `(entity-ref ,n))))

;; [67] Reference ::= EntityRef | CharRef
(define $xml:reference ($or $xml:entity-ref $xml:char-ref))

;; [69] PEReference ::= '%' Name ';'
(define $xml:pe-reference
  ($do (($eqv? #\%))
       (n $xml:name)
       (($eqv? #\;))
       ($return `(pe-ref ,n))))

;; [9]  EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
;;                    | "'" ([^%&'] | PEReference | Reference)* "'"
(define (merge-value result)
  (let-values (((out extract) (open-string-output-port)))
    (let loop ((r '()) (result result) (emit? #f))
      (if (null? result)
	  (reverse (if emit? (cons (extract) r) r))
	  (let ((e (car result)))
	    (cond ((char? e) (put-char out e) (loop r (cdr result) #t))
		  (else
		   (if emit?
		       (loop (cons e (cons (extract) r)) (cdr result) #f)
		       (loop (cons e r) (cdr result) #f)))))))))
(define $xml:entity-value
  (let ((no-dquote (char-set-complement (string->char-set "%&\"")))
	(no-squote (char-set-complement (string->char-set "%&'"))))
    ($or ($do (($eqv? #\"))
	      (r ($many ($or ($do (c ($in-set no-dquote)) ($return c))
			     $xml:pe-reference
			     $xml:reference)))
	      (($eqv? #\"))
	      ($return `(entity-value ,@(merge-value r))))
	 ($do (($eqv? #\'))
	      (r ($many ($or ($do (c ($in-set no-squote)) ($return c))
			     $xml:pe-reference
			     $xml:reference)))
	      (($eqv? #\'))
	      ($return `(entity-value ,@(merge-value r)))))))

;; [10] AttValue ::= '"' ([^<&"] | Reference)* '"'
;;                 | "'" ([^<&'] | Reference)* "'"
(define $xml:att-value
  (let ((no-dquote (char-set-complement (string->char-set "<&\"")))
	(no-squote (char-set-complement (string->char-set "<&'"))))
    ($or ($do (($eqv? #\"))
	      (r ($many ($or ($do (c ($in-set no-dquote)) ($return c))
			     $xml:reference)))
	      (($eqv? #\"))
	      ($return `(att-value ,@(merge-value r))))
	 ($do (($eqv? #\'))
	      (r ($many ($or ($do (c ($in-set no-squote)) ($return c))
			     $xml:reference)))
	      (($eqv? #\'))
	      ($return `(att-value ,@(merge-value r)))))))
;; [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
(define $xml:system-literal
  (let ((no-dquote (char-set-complement (string->char-set "\"")))
	(no-squote (char-set-complement (string->char-set "'"))))
    ($or ($do (($eqv? #\"))
	      (c* ($many ($in-set no-dquote)))
	      (($eqv? #\"))
	      ($return (list->string c*)))
	 ($do (($eqv? #\'))
	      (c* ($many ($in-set no-squote)))
	      (($eqv? #\'))
	      ($return (list->string c*))))))

;; [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
(define ascii-letter+digit
  (char-set-intersection char-set:ascii char-set:letter+digit))
(define $xml:pubid-literal
  ;; [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
  (let* ((pubid-char (char-set-union
		     (char-set #\x20 #\xD #\xA)
		     ascii-letter+digit
		     (string->char-set "-'()+,./:=?;!*#@$_%")))
	 (pubid-char-squote (char-set-difference pubid-char (char-set #\'))))
    ($or ($do (($eqv? #\"))
	      (c* ($many ($in-set pubid-char)))
	      (($eqv? #\"))
	      ($return (list->string c*)))
	 ($do (($eqv? #\'))
	      (c* ($many ($in-set pubid-char-squote)))
	      (($eqv? #\'))
	      ($return (list->string c*))))))

;; [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
(define $xml:char-data
  (let ((char-data-set (char-set-complement (char-set #\< #\&))))
    ($do (c* ($many ($in-set char-data-set)))
	 (($not ($token "]]>")))
	 ($return (if (null? c*) #f (list->string c*))
		  (if (null? c*) +parse-fail+ +parse-success+)))))

;; [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
(define $xml:comment
  ($do (($token "<!--"))
       (c* ($many ($seq ($not ($token "--")) ($in-set +xml:char-set+))))
       (($token "-->"))
       ($return `(comment ,(list->string c*)))))

;; [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
(define $xml:pi-target
  ($do (n $xml:name)
       ($return n (if (string-ci=? n "xml") +parse-expect+ +parse-success+))))
;; [16] PI   ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
(define $xml:pi
  ($do (($token "<?"))
       (n $xml:pi-target)
       (v ($optional ($do $xml:s
			  (c* ($many ($seq ($not ($token "?>"))
					   ($in-set +xml:char-set+))))
			  ($return (list->string c*)))
		     #f))
       (($token "?>"))
       ($return `(PI ,n ,v))))

;; [19] CDStart ::= '<![CDATA['
(define $xml:cd-start ($token "<![CDATA["))
;; [21] CDEnd   ::= ']]>'
(define $xml:cd-end ($token "]]>"))
;; [20] CData   ::= (Char* - (Char* ']]>' Char*))
(define $xml:c-data
  ($do (c* ($many ($seq ($not $xml:cd-end) ($in-set +xml:char-set+))))
       ($return (list->string c*))))
;; [18] CDSect  ::= CDStart CData CDEnd
(define $xml:cd-sect
  ($do $xml:cd-start
       (d $xml:c-data)
       $xml:cd-end
       ($return `(cdata ,d))))

;; [27] Misc    ::= Comment | PI | S
(define $xml:misc ($or $xml:comment $xml:pi $xml:s))

;; [25] Eq    ::= S? '=' S?
(define $xml:eq ($seq ($optional $xml:s #f) ($eqv? #\=) ($optional $xml:s #f)))
;; [26] VersionNum  ::= '1.' [0-9]+
(define $xml:version-num
  ($do (($token "1."))
       (v* ($many ($in-set ascii-digit-set)))
       ($return (string-append "1." (list->string v*)))))
;; [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
(define $xml:enc-name
  (let* ((ascii-letter (char-set-intersection char-set:ascii char-set:letter))
	 (enc-sub (char-set-union ascii-letter+digit (string->char-set "._-"))))
    ($do (f ($in-set ascii-letter))
	 (l* ($many ($in-set enc-sub)))
	 ($return (list->string (cons f l*))))))
;; [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
(define $xml:encoding-decl
  ($do $xml:s
       (($token "encoding"))
       $xml:eq
       (v ($or ($do (($eqv? #\")) (v $xml:enc-name) (($eqv? #\"))
		    ($return v))
	       ($do (($eqv? #\')) (v $xml:enc-name) (($eqv? #\'))
		    ($return v))))
       ($return `(encoding ,v))))
;; [32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
(define $xml:sd-decl
  (let ((yes/no ($or ($do (($token "yes")) ($return "yes"))
		     ($do (($token "no")) ($return "no")))))
    ($do $xml:s
	 (($token "standalone"))
	 $xml:eq
	 (v ($or ($do (($eqv? #\")) (v yes/no) (($eqv? #\")) ($return v))
		 ($do (($eqv? #\')) (v yes/no) (($eqv? #\')) ($return v))))
	 ($return `(standalone ,v)))))

;; [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
(define $xml:version-info
  ($do $xml:s
       (($token "version"))
       $xml:eq
       (v ($or ($do (($eqv? #\")) (v $xml:version-num) (($eqv? #\"))
		    ($return v))
	       ($do (($eqv? #\')) (v $xml:version-num) (($eqv? #\'))
		    ($return v))))
       ($return `(version ,v))))
;; [23] XMLDecl    ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
(define $xml:xml-decl
  ($do (($token "<?xml"))
       (vi $xml:version-info)
       (ed ($optional $xml:encoding-decl #f))
       (sd ($optional $xml:sd-decl #f))
       ($optional $xml:s #f)
       (($token "?>"))
       ($return `(xml-decl ,vi ,ed ,sd))))
;; [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? QName)* S? ')*'
;;              | '(' S? '#PCDATA' S? ')'
(define $xml:mixed
  ($or ($do (($eqv? #\()) (($optional $xml:s))
	    (($token "#PCDATA"))
	    (n* ($many ($do (($optional $xml:s)) (($eqv? #\|))
			    (($optional $xml:s))
			    (n ($or $xml:qname $xml:pe-reference))
			    ($return n))))
	    (($optional $xml:s))
	    (($token ")*"))
	    ($return `(pcdata . ,n*)))
       ($do (($eqv? #\()) (($optional $xml:s))
	    (($token "#PCDATA")) (($optional $xml:s))
	    (($token ")"))
	    ($return '(pcdata)))))
;; [47] children ::= (choice | seq) ('?' | '*' | '+')?
(define $xml:children
  (let ()
    ;; [48] cp	 ::= (QName | choice | seq) ('?' | '*' | '+')?
    (define ($xml:cp)
      ($do (n ($or $xml:qname $xml:pe-reference $xml:choice $xml:seq))
	   (c ($optional ($or ($eqv? #\?) ($eqv? #\*) ($eqv? #\+)) #f))
	   ($return (if c
			`(,(string->symbol (string c)) ,n)
			n))))
    ;; [49] choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
    (define $xml:choice
      ($do (($eqv? #\()) (($optional $xml:s))
	   (c ($xml:cp))
	   (c* ($many ($do (($optional $xml:s)) (($eqv? #\|))
			   (($optional $xml:s)) (c ($xml:cp))
			   ($return c))
		      1))
	   (($optional $xml:s))
	   (($eqv? #\)))
	   ($return `(choice ,c . ,c*))))
    ;; [50] seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
    (define $xml:seq
      ($do (($eqv? #\()) (($optional $xml:s))
	   (c ($xml:cp))
	   (c* ($many ($do (($optional $xml:s)) (($eqv? #\,))
			   (($optional $xml:s)) (c ($xml:cp))
			   ($return c))))
	   (($optional $xml:s))
	   (($eqv? #\)))
	   ($return `(seq ,c . ,c*))))

    ($do (c ($or $xml:choice $xml:seq))
	 (u ($optional ($or ($eqv? #\?) ($eqv? #\*) ($eqv? #\+)) #f))
	 ($return (if u `(,(string->symbol (string u)) ,c) c)))))

;; [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
(define $xml:content-spec
  ($or ($do (($token "EMPTY")) ($return 'empty))
       ($do (($token "ANY"))   ($return 'any))
       $xml:pe-reference
       $xml:mixed
       $xml:children))
       
;; [45] elementdecl ::= '<!ELEMENT' S QName S contentspec S? '>'
(define $xml:element-decl
  ($do (($token "<!ELEMENT")) $xml:s
       (n ($or $xml:qname $xml:pe-reference)) $xml:s
       (c $xml:content-spec) (($optional $xml:s))
       (($eqv? #\>))
       ($return `(!element ,n ,c))))

;; [83] PublicID     ::= 'PUBLIC' S PubidLiteral
(define $xml:public-id
  ($do (($token "PUBLIC")) $xml:s
       (l $xml:pubid-literal)
       ($return `(public ,l))))
;; [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
(define $xml:notation-decl
  ($do (($token "<!NOTATION")) $xml:s
       (n $xml:name) $xml:s
       (id ($or $xml:external-id $xml:public-id)) (($optional $xml:s))
       (($eqv? #\>))
       ($return `(!notation ,n ,id))))


;; [75] ExternalID ::= 'SYSTEM' S SystemLiteral
;;                   | 'PUBLIC' S PubidLiteral S SystemLiteral
(define $xml:external-id
  ($or ($do (($token "SYSTEM")) $xml:s
	    (l $xml:system-literal)
	    ($return `(system ,l)))
       ($do (($token "PUBLIC")) $xml:s
	    (p $xml:pubid-literal) $xml:s
	    (l $xml:system-literal)
	    ($return `(public ,p ,l)))))
;; [76] NDataDecl ::= S 'NDATA' S Name
(define $xml:n-data-decl
  ($do $xml:s
       (($token "NDATA")) $xml:s
       (n $xml:name)
       ($return `(ndata ,n))))

;; [74] PEDef   ::= EntityValue | ExternalID
(define $xml:pe-def ($or $xml:entity-value $xml:external-id))
;; [72] PEDecl   ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
(define $xml:pe-decl
  ($do (($token "<!ENTITY")) $xml:s
       (($eqv? #\%)) $xml:s
       (n $xml:name) $xml:s
       (d $xml:pe-def) (($optional $xml:s))
       (($eqv? #\>))
       ($return `(!entity pe ,n ,d))))
;; [73] EntityDef  ::= EntityValue | (ExternalID NDataDecl?)
(define $xml:entity-def
  ($or ($do (v $xml:entity-value) ($return (list v)))
       ($do (e $xml:external-id)
	    (n ($optional $xml:n-data-decl '()))
	    ($return (if (null? n) (list e) (list e n))))))

;; [71] GEDecl   ::= '<!ENTITY' S Name S EntityDef S? '>'
(define $xml:ge-decl
  ($do (($token "<!ENTITY")) $xml:s
       (n $xml:name) $xml:s
       (d $xml:entity-def) (($optional $xml:s))
       (($eqv? #\>))
       ($return `(!entity ge ,n ,@d))))

;; [70] EntityDecl ::= GEDecl | PEDecl
(define $xml:entity-decl ($or $xml:ge-decl $xml:pe-decl))


;; [55] StringType ::= 'CDATA'
(define $xml:string-type ($do (($token "CDATA")) ($return 'cdata)))
;; [56] TokenizedType ::= 'ID'
;; 			| 'IDREF'
;; 			| 'IDREFS'
;; 			| 'ENTITY'
;; 			| 'ENTITIES'
;; 			| 'NMTOKEN'
;; 			| 'NMTOKENS'
(define $xml:tokenized-type
  ($or ($do (($token "ID")) ($return 'id))
       ($do (($token "IDREF")) ($return 'idref))
       ($do (($token "IDREFS")) ($return 'idrefs))
       ($do (($token "ENTITY")) ($return 'entity))
       ($do (($token "ENTITIES")) ($return 'entities))
       ($do (($token "NMTOKEN")) ($return 'nmtoken))
       ($do (($token "NMTOKENS")) ($return 'nmtokens))))

;; [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
(define $xml:notation-type
  ($do (($token "NOTATION")) $xml:s
       (($eqv? #\()) (($optional $xml:s))
       (n $xml:name) 
       (n* ($many ($seq ($optional $xml:s) ($eqv? #\|) $xml:name)))
       (($optional $xml:s))
       (($eqv? #\)))
       ($return `(notation ,n ,@n*))))
;; [59] Enumeration ::=	'(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
(define $xml:enumeration
  ($do (($eqv? #\())  (($optional $xml:s))
       (t $xml:nmtoken)
       (t* ($many ($seq ($optional $xml:s) ($eqv? #\|) $xml:nmtoken)))
       (($optional $xml:s))
       (($eqv? #\)))
       ($return (cons t t*))))
;; [57] EnumeratedType ::= NotationType | Enumeration
(define $xml:enumerated-type ($or $xml:notation-type $xml:enumeration))
;; [54] AttType	 ::= StringType | TokenizedType | EnumeratedType
(define $xml:att-type
  ($or $xml:string-type $xml:tokenized-type $xml:enumerated-type))

;; [60] DefaultDecl ::=	'#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
(define $xml:default-decl
  ($or ($do (($token "#REQUIRED")) ($return 'required))
       ($do (($token "#IMPLIED")) ($return 'implied))
       ($do (($optional ($seq ($token "#FIXED") $xml:s)))
	    (v $xml:att-value)
	    ($return `(fixed ,v)))))
;; [53] AttDef ::= S Name S AttType S DefaultDecl
(define $xml:att-def
  ($do $xml:s
       (n $xml:qname) $xml:s
       (t $xml:att-type) $xml:s
       (d $xml:default-decl)
       ($return `(att-def ,n ,t, d))))
;; [52] AttlistDecl ::= '<!ATTLIST' S QName AttDef* S? '>'
(define $xml:attlist-decl
  ($do (($token "<!ATTLIST")) $xml:s
       (n $xml:qname)
       (d* ($many $xml:att-def)) (($optional $xml:s))
       (($eqv? #\>))
       ($return `(!attlist ,n ,@d*))))

;; [29] markupdecl ::= elementdecl | AttlistDecl
;;                   | EntityDecl | NotationDecl | PI | Comment
(define $xml:markup-decl
  ($or $xml:element-decl
       $xml:attlist-decl
       $xml:entity-decl
       $xml:notation-decl
       $xml:pi
       $xml:comment))
;; [28a] DeclSep ::= PEReference | S
(define $xml:decl-sep ($or $xml:pe-reference ($do $xml:s ($return #f))))
;; [28b] intSubset ::= (markupdecl | DeclSep)*
(define $xml:int-subset
  ($do (s* ($many ($or $xml:markup-decl $xml:decl-sep)))
       ($return (filter values s*))))

;; [28] doctypedecl ::= '<!DOCTYPE' S QName (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
(define $xml:doctype-decl
  ($do (($token "<!DOCTYPE"))
       $xml:s
       (n $xml:qname)
       (id ($optional ($seq $xml:s $xml:external-id) #f))
       (($optional $xml:s))
       (subst ($optional ($do (($eqv? #\[)) (s $xml:int-subset) (($eqv? #\]))
			      (($optional $xml:s))
			      ($return s))
			 '()))
       (($eqv? #\>))
       ($return `(!doctype ,n ,id (subset ,@subst)))))

;; [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
(define $xml:prolog
  ($do (decl ($optional $xml:xml-decl #f))
       (misc ($many $xml:misc))
       (doctype ($optional ($do (dec $xml:doctype-decl)
				(misc ($many $xml:misc))
				($return (cons dec (filter pair? misc))))
			   #f))
       ($return `(prolog ,decl
			 (misc ,@(filter pair? misc))
			 ,(and doctype (car doctype))
			 (misc ,@(if doctype (cdr doctype) '()))))))
;; [41] Attribute ::= NSAttName Eq AttValue
;;                  | QName Eq AttValue
(define $xml:attribute
  (let ()
    (define (add/replace-ns name uri)
      (let ((prefix (cadr name))
	    (current (*current-namespaces*)))
	;; it's a bit too ad-hoc but I'm lazy for now...
	;; this is incase of "urn:foo:&amp;bla" or so
	(let ((uri (if (null? (cdr uri)) (car uri) uri)))
	  ;; we don't remove, but first found one is the one to be used.
	  (*current-namespaces* (cons (cons prefix uri) current)))
	(cons name uri)))
    ($or ($do (n $xml:nsatt-name) $xml:eq (v $xml:att-value)
	      ($return (add/replace-ns n (cdr v))))
	 ($do (n $xml:qname) $xml:eq (v $xml:att-value)
	      ($return (cons n (cdr v)))))))

(define (fixup-qname qname)
  (cond ((and (qname? qname) (not (qname-namespace qname)))
	 (let ((namespace (find-namespace (qname-prefix qname))))
	   (make-qname namespace (qname-prefix qname)
		       (qname-local-part qname))))
	((qname? qname) qname)
	(else 
	 ;; check default namespace
	 (let ((namespace (find-namespace #f)))
	   (if namespace
	       (make-qname namespace #f qname)
	       qname)))))

;; helper
(define $xml:attributes
  (let ()
    (define (fixup-attribute a)
      (let ((name (car a)))
	(if (qname? name)
	    (let ((namespace (find-namespace (qname-prefix name))))
	      (list (make-qname namespace (qname-prefix name)
				(qname-local-part name))
		    (cadr a)))
	      a)))
    ($do (a* ($many ($seq $xml:s $xml:attribute)))
	 ($return (map fixup-attribute a*)))))
       
;; [44] EmptyElemTag ::= '<' QName (S Attribute)* S? '/>'
(define $xml:empty-elem-tag
  ($do (($eqv? #\<))
       (n $xml:qname)
       (a $xml:attributes)
       (($optional $xml:s))
       (($token "/>"))
       ($return `(element ,(fixup-qname n) (attributes ,@a)))))
;; [39] element ::= EmptyElemTag
;;                | STag content ETag
(define $xml:element
  (let ()
    ;; [40] STag ::= '<' QName (S Attribute)* S? '>'
    (define $xml:stag
      ($do (($eqv? #\<))
	   (n $xml:qname)
	   (a $xml:attributes)
	   (($optional $xml:s))
	   (($eqv? #\>))
	   ($return (cons (fixup-qname n) a))))
    ;; [42] ETag ::= '</' QName S? '>'
    (define ($xml:etag name)
	(if (qname? name)
	    (let ((prefix (qname-prefix name))
		  (local-part (qname-local-part name)))
	      (if prefix
		  ($seq ($token "</")
			($token prefix) ($eqv? #\:)
			($token local-part)
			($optional $xml:s) ($eqv? #\>))
		  ($seq ($token "</")
			($token local-part)
			($optional $xml:s) ($eqv? #\>))))
	    ($seq ($token "</")
		  ($token name)
		  ($optional $xml:s) ($eqv? #\>))))
    ;; [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)*
    (define ($xml:content)
      ($do (c ($optional $xml:char-data))
	   (e ($many ($do (v ($or $xml:element
				  $xml:reference
				  $xml:cd-sect
				  $xml:pi
				  $xml:comment))
			  (c ($optional $xml:char-data))
			  ($return (if c (list v c) (list v))))))
	   ($return (let ((t (apply append e))) (if c (cons c t) t)))))
    ($parameterize ((*current-namespaces* (*current-namespaces*)))
      ($or $xml:empty-elem-tag
	   ($do (s $xml:stag)
		(c ($xml:content))
		(e ($xml:etag (car s)))
		($return `(element ,(car s) (attributes . ,(cdr s)) . ,c)))))))

;; [1] document ::= prolog element Misc*
(define $xml:document
  ($do (prolog $xml:prolog)
       (element $xml:element)
       (misc* ($many $xml:misc))
       ($return `(document ,prolog ,element
			   ,@(filter pair? misc*)))))

)
