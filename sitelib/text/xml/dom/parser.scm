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

;; This library returns DOM AST which is *not* DOM yet but intermediate
;; data structure. At some point, we can also convert SXML to DOM AST
;; but that's a bit far future.
#!nounbound
(library (text xml dom parser)
    (export make-xml-document-parse-options
	    xml-document-parse-options?

	    *xml:parse-option*
	    ;; for testing
	    +xml:char-set+ +xml:name-start-char-set+ +xml:name-char-set+
	    $xml:s
	    $xml:name $xml:names
	    $xml:nmtoken $xml:nmtokens
	    $xml:entity-value $xml:attr-value
	    $xml:system-literal $xml:pubid-literal
	    $xml:pi
	    $xml:cd-sect
	    $xml:prolog $xml:xml-decl
	    
	    $xml:char-data $xml:comment
	    
	    $xml:char-ref $xml:entity-ref $xml:reference $xml:pe-reference
	    $xml:entity-value
	    )
    (import (rnrs)
	    (peg)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))
;; TODO maybe this should be move to constructing part
(define-record-type xml-document-parse-options
  (fields namespace-aware?
	  xinclude-aware?
	  validating?
	  whitespace?
	  expand-entity-reference?
	  ignore-comments?
	  coalescing?)
  (protocol (lambda (p)
	      ;; the options are taken from Java.
	      (lambda (:key (namespace-aware? #t)
			    (xinclude-aware? #f)
			    (validating? #f)
			    (whitespace? #f)
			    (expand-entity-reference? #t)
			    (ignore-comments? #f)
			    (coalescing? #f))
		(p namespace-aware? xinclude-aware?
		   validating? whitespace? expand-entity-reference?
		   ignore-comments? coalescing?)))))
(define-syntax %expand-entity?
  (syntax-rules ()
    ((_)
     (xml-document-parse-options-expand-entity-reference?
      (*xml:parse-option*)))))
(define +default-parse-option+ (make-xml-document-parse-options))
(define *xml:parse-option* (make-parameter +default-parse-option+))
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
  ($many ($or ($eqv? #\x20) ($eqv? #\x9) ($eqv? #\xD) ($eqv? #\xA)) 1))

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
(define ($in-set s) ($satisfy (lambda (c) (char-set-contains? s c))))
(define ($token s) (apply $seq (map $eqv? (string->list s))))

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
(define $xml:attr-value
  (let ((no-dquote (char-set-complement (string->char-set "<&\"")))
	(no-squote (char-set-complement (string->char-set "<&'"))))
    ($or ($do (($eqv? #\"))
	      (r ($many ($or ($do (c ($in-set no-dquote)) ($return c))
			     $xml:reference)))
	      (($eqv? #\"))
	      ($return `(attr-value ,@(merge-value r))))
	 ($do (($eqv? #\'))
	      (r ($many ($or ($do (c ($in-set no-squote)) ($return c))
			     $xml:reference)))
	      (($eqv? #\'))
	      ($return `(attr-value ,@(merge-value r)))))))
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
	 ($return (list->string c*)))))

;; [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
(define $xml:comment
  ($do (($token "<!--"))
       (c* ($many ($seq ($not ($token "--")) ($in-set +xml:char-set+))))
       (($token "-->"))
       ($return `(comment ,(list->string c*)))))

;; [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
(define $xml:pi-target
  ($seq ($or ($eqv? #\x) ($eqv? #\X))
	($or ($eqv? #\m) ($eqv? #\M))
	($or ($eqv? #\l) ($eqv? #\L))))
;; [16] PI   ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
(define $xml:pi
  ($do (($token "<?"))
       $xml:pi-target
       (v ($optional ($do $xml:s
			  (c* ($many ($seq ($not ($token "?>"))
					   ($in-set +xml:char-set+))))
			  ($return (list (list->string c*))))
		     '()))
       (($token "?>"))
       ($return `(PI . ,v))))

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
       ($return `(xml-decl ,vi ,@(if ed `(,ed) '()) ,@(if sd `(,sd) '())))))
;; [22] prolog    ::= XMLDecl? Misc* (doctypedecl Misc*)?
(define $xml:prolog
  ($do (decl ($optional $xml:xml-decl #f))
       (misc ($many $xml:misc))
       (doctype ($optional ($do ;;(dec $xml:doc-type-decl)
				(misc ($many $xml:misc))
				($return misc #;(cons dec misc)))
			   '()))
       ($return `(prolog ,@(if decl (list decl) '()) ,@misc ,@doctype))))

#;(define $xml:prolog
  ($do (decl $xml:xml-decl)
       (($many $xml:misc))
       (doctype ($optional
 ($do (d $xml:doctypedecl) (($many $xml:misc)) ($return d))
 #f))
       ($return #f)))

;; [1] document ::= prolog element Misc*
#;(define $xml:document
  ($do (prolog $xml:prolog)
       (element $xml:element)
       (($many $xml:misc))
       ($return #f)))

)
