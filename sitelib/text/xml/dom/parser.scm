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

(library (text xml dom parser)
    (export make-xml-document-parse-options
	    xml-document-parse-options?

	    ;; for testing
	    +xml:char-set+ +xml:name-start-char-set+ +xml:name-char-set+
	    $xml:s
	    $xml:name $xml:names
	    $xml:nmtoken $xml:nmtokens
	    )
    (import (rnrs)
	    (peg)
	    (srfi :14 char-sets))
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
(define +default-parse-option+ (make-xml-document-parse-options))
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
   $xml:name-start-char-set
   (string->char-set "-.0123456789")
   (char-set #\xB7)
   (ucs-range->char-set #x0300 #x0370)
   (ucs-range->char-set #x203F #x2041)))
;; helper
(define ($in-set s) ($satisfy (lambda (c) (char-set-contains? s c))))
;; [5] Name   ::= NameStartChar (NameChar)*
(define $xml:name
  ($do (s ($in-set $xml:name-start-char-set))
       (c* ($many ($in-set $xml:name-char-set)))
       ($return (list->string (cons s c*)))))
;; [6] Names  ::= Name (#x20 Name)*
(define $xml:names
  ($do (n $xml:name)
       (n* ($many ($do (($eqv? #\x20)) (n $xml:name) ($return n))))
       ($return (cons n n*))))
;; [7] Nmtoken ::= (NameChar)+
(define $xml:nmtoken
  ($do (c* ($many ($in-set $xml:name-char-set) 1))
       ($return (list->string c*))))
;; [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
(define $xml:nmtokens
  ($do (t $xml:nmtoken)
       (t* ($many ($do (($eqv? #\x20)) (n $xml:nmtoken) ($return n))))
       ($return (cons t t*))))


;; [27] Misc    ::= Comment | PI | S 
;; [26] VersionNum  ::= '1.' [0-9]+
;; [25] Eq    ::= S? '=' S?
;; [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
;; [23] XMLDecl    ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;; [22] prolog    ::= XMLDecl? Misc* (doctypedecl Misc*)?
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
