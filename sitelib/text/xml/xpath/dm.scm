;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/xpath/dm.scm - XPath datamodel
;;;
;;;   Copyright (c) 2020  Takashi Kato  <ktakashi@ymail.com>
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

;; ref:
;;  XQuery and XPath Data Model 3.1
;;  https://www.w3.org/TR/xpath-datamodel-31/

#!nounbound
(library (text xml xpath dm)
    (export xpath-dm:attributes
	    xpath-dm:base-uri
	    xpath-dm:children
	    xpath-dm:node-name
	    xpath-dm:string-value
	    xpath-dm:typed-value)
    (import (rnrs)
	    (text xml dom nodes)
	    (srfi :13 strings))
;; NOTE
;; xs:Qname = string

;;; 5 Accessors
;;;; 5.1 attributes Accessor
(define (xpath-dm:attributes n)
  (if (element? n)
      (xpath-dm:element-attributes n)
      '()))

;;;; 5.2 base-uri Accessor
(define (xpath-dm:base-uri n)
  (cond ((document? n) (xpath-dm:document-base-uri n))
	((element? n) (xpath-dm:element-base-uri n))
	((or (attr? n) (comment? n) (text? n))
	 (cond ((node-parent-node n) => xpath-dm:base-uri)
	       (else '())))
	((namespace? n) '())
	((processing-instruction? n)
	 (xpath-dm:processing-instruction-base-uri n))
	(else (assertion-violation 'xpath-dm:base-uri "Unknown node" n))))

;;;; 5.3 children Accessor
(define (xpath-dm:children n)
  (cond ((document? n) (xpath-dm:document-children n))
	((element? n)  (xpath-dm:element-children n))
	((or (attr? n) (processing-instruction? n)
	     (comment? n) (text? n) (namespace? n)) '())
	(else (assertion-violation 'xpath-dm:children "Unknown node" n))))  

;;;; 5.12 string-value Accessor
(define (xpath-dm:node-name n)
  (cond ((element? n) (xpath-dm:element-node-name n))
	((attr? n)    (xpath-dm:attribute-node-name n))
	((namespace? n)
	 (let ((p (namespace-prefix n)))
	   (if (and p (not (zero? (string-length p))))
	       p
	       '())))
	((processing-instruction? n) (xpath-dm:processing-instruction-target n))
	((or (document? n) (comment? n) (text? n)) '())
	(else (assertion-violation 'xpath-dm:node-name "Unknown node" n))))
    
;;;; 5.12 string-value Accessor
;;;; dm:string-value($n as node()) as xs:string
(define (xpath-dm:string-value n)
  (cond ((document? n) (xpath-dm:document-string-value n))
	((element? n)  (xpath-dm:element-string-value n))
	((attr? n)     (xpath-dm:attribute-string-value n))
	((processing-instruction? n) (xpath-dm:processing-instruction-content n))
	((comment? n)  (xpath-dm:comment-content n))
	((text? n)     (xpath-dm:text-content n))
	((namespace? n) (xpath-dm:namespace-uri n))
	(else (assertion-violation 'xpath-dm:string-value "Unknown node" n))))


;;;; 5.14 typed-value Accessor
(define (xpath-dm:typed-value n)
  (cond ((document? n) (xpath-dm:document-typed-value n))
	((element? n)  (xpath-dm:element-typed-value n))
	((attr? n)     (xpath-dm:attribute-typed-value n))
	((processing-instruction? n) (xpath-dm:processing-instruction-content n))
	((comment? n)  (xpath-dm:comment-content n))
	((text? n)     (xpath-dm:text-content n))
	((namespace? n) (xpath-dm:namespace-uri n))
	(else (assertion-violation 'xpath-dm:typed-value "Unknown node" n))))

;;; 6 Nodes
(define (base-uri/empty n)
  (cond ((node-base-uri n))
	(else '())))
(define (children n) (node-list->list (node-child-nodes n)))
;; Underlying node property accessor.
;;;; 6.1 Document Nodes
(define (xpath-dm:document-string-value d)
  (define itr (document:create-node-iterator d (document-document-element d)
					     +node-filter-show-text+))
  (text-node-iterator->string itr))
;; from Construction from an Inforset
;; typed-value
;;   The attribute’s typed-value is its dm:string-value as an xs:untypedAtomic.
;; TODO should we wrap with xs:untypedAtomic?
(define xpath-dm:document-typed-value xpath-dm:document-string-value)
(define xpath-dm:document-base-uri base-uri/empty)
(define xpath-dm:document-children children)

;;;; 6.2 Element Nodes
(define (xpath-dm:element-string-value e)
  (define itr (document:create-node-iterator (node-owner-document e) e
					     +node-filter-show-text+))
  (text-node-iterator->string itr))
;; TODO should we wrap with xs:untypedAtomic?
(define xpath-dm:element-typed-value xpath-dm:element-string-value)
(define xpath-dm:element-node-name node-node-name)
(define (xpath-dm:element-attributes e)
  (named-node-map:fold (element-attributes e) '()
		       (lambda (k v r)
			 (if (string-prefix? "xmlns" (attr-name k))
			     r
			     (cons k r)))))
(define xpath-dm:element-base-uri base-uri/empty)
(define xpath-dm:element-children children)

;;;; 6.3 Attribute Nodes
;; NOTE: attribute nodes properties are described a bit vaguely, so
;;       not entirely sure if these're correct or not.
(define xpath-dm:attribute-string-value attr-value)
;; TODO should we wrap with xs:untypedAtomic?
(define xpath-dm:attribute-typed-value xpath-dm:attribute-string-value)
(define xpath-dm:attribute-node-name attr-name)

;;;; 6.4 Namespace Nodes
(define xpath-dm:namespace-uri namespace-uri)

;;;; 6.5 Processing Instruction Nodes
(define xpath-dm:processing-instruction-content character-data-data)
(define xpath-dm:processing-instruction-target processing-instruction-target)
(define xpath-dm:processing-instruction-base-uri base-uri/empty)

;;;; 6.6 Comment Nodes
(define xpath-dm:comment-content character-data-data)

;;;; 6.7 Text Nodes
(define xpath-dm:text-content text-whole-text)


;;; Helpers
(define (text-node-iterator->string itr)
  (let-values (((out e) (open-string-output-port)))
    (do ((n (node-iterator:next-node itr) (node-iterator:next-node itr)))
	((not n) (e))
      (put-string out (xpath-dm:string-value n)))))

)
