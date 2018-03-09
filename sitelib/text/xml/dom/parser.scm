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
	    xml-document-parse-options?)
    (import (rnrs)
	    (peg))
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

;; [27] Misc	    ::= Comment | PI | S 
;; [26] VersionNum  ::= '1.' [0-9]+
;; [25] Eq	    ::= S? '=' S?
;; [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
;; [23] XMLDecl	    ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;; [22] prolog	    ::= XMLDecl? Misc* (doctypedecl Misc*)?
(define $prolog
  ($do (decl $xml-decl)
       (($many $misc))
       (doctype ($optional
		 ($do (d $doctypedecl) (($many $misc)) ($return d))
		 #f))
       ($return #f)))

;; [1] 	document ::= prolog element Misc*
(define $document
  ($do (prolog $prolog)
       (element $element)
       (($many $misc))
       ($return #f)))

)
