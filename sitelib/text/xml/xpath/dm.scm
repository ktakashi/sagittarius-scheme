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
	    xpath-dm:document-uri
	    xpath-dm:is-id
	    xpath-dm:is-idrefs
	    xpath-dm:namespace-nodes
	    xpath-dm:nilled
	    xpath-dm:node-kind
	    xpath-dm:node-name
	    xpath-dm:parent
	    xpath-dm:string-value
	    xpath-dm:type-name
	    xpath-dm:typed-value
	    xpath-dm:unparsed-entity-public-id
	    xpath-dm:unparsed-entity-system-id)
    (import (rnrs)
	    (text xml dom nodes)
	    (srfi :13 strings))
;; NOTE
;; xs:Qname = string

;;; 5 Accessors
;;;; 5.1 attributes Accessor
(define (xpath-dm:attributes n)
  (cond ((element? n) (xpath-dm:element-attributes n))
	((or (document? n) (attr? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:nilled "Unknown node" n))))

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

;;;; 5.4 document-uri Accessor
(define (xpath-dm:document-uri n)
  (cond ((document? n) (xpath-dm:document-base-uri n)) ;; it's the same
	((or (element? n) (attr? n) (processing-instruction? n)
	     (comment? n) (text? n) (namespace? n)) '())
	(else (assertion-violation 'xpath-dm:document-uri "Unknown node" n))))

;;;; 5.5 is-id Accessor
(define (xpath-dm:is-id n)
  (cond ((element? n) (xpath-dm:element-is-id n))
	((attr? n)    (xpath-dm:attribute-is-id n))
	((or (document? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:is-id "Unknown node" n))))

;;;; 5.6 is-idrefs Accessor
(define (xpath-dm:is-idrefs n)
  (cond ((element? n) (xpath-dm:element-is-idrefs n))
	((attr? n)    (xpath-dm:attribute-is-idrefs n))
	((or (document? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:is-idrefs "Unknown node" n))))

;;;; 5.7 namespace-nodes Accessor
(define (xpath-dm:namespace-nodes n)
  (cond ((element? n) (xpath-dm:element-namespace-nodes n))
	((or (document? n) (attr? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:namespace-nodes "Unknown node" n))))

;;;; 5.8 nilled Accessor
(define (xpath-dm:nilled n)
  (cond ((element? n) (xpath-dm:element-nilled n))
	((or (document? n) (attr? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:nilled "Unknown node" n))))

;;;; 5.9 node-kind Accessor
(define (xpath-dm:node-kind n)
  (cond ((document? n) "document")
	((element? n)  "element")
	((attr? n)     "attribute")
	((processing-instruction? n) "processing-instruction")
	((comment? n)  "comment")
	((text? n)     "text")
	((namespace? n) "namespace")
	(else (assertion-violation 'xpath-dm:string-value "Unknown node" n))))

;;;; 5.10 string-name Accessor
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

;;;; 5.11 parent Accessor
(define (xpath-dm:parent n)
  (cond ((document? n) '())
	((element? n)  (xpath-dm:element-parent n))
	((attr? n)     (xpath-dm:attribute-parent n))
	((processing-instruction? n) (xpath-dm:processing-instruction-parent n))
	((comment? n)  (xpath-dm:comment-parent n))
	((text? n)     (xpath-dm:text-parent n))
	((namespace? n) (xpath-dm:namespace-parent n))
	(else (assertion-violation 'xpath-dm:parent "Unknown node" n))))

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

;;;; 5.13 type-name Accessor
(define (xpath-dm:type-name n)
  (cond ((element? n)  (xpath-dm:element-type-name n))
	((attr? n)     (xpath-dm:attribute-type-name n))
	((or (document? n) (comment? n) (text? n) (namespace? n)
	     (processing-instruction? n)) '())
	(else (assertion-violation 'xpath-dm:type-name "Unknown node" n))))

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

;;;; 5.15 unparsed-entity-public-id Accessor
;;;; dm:unparsed-entity-public-id($node       as node(),
;;;;                              $entityname as xs:string) as xs:string?
(define (xpath-dm:unparsed-entity-public-id n entity-name)
  (cond ((document? n)
	 (xpath-dm:document-unparsed-entity-public-id n entity-name))
	((or (element? n) (attr? n) (processing-instruction? n)
	     (comment? n) (text? n) (namespace? n)) '())
	(else (assertion-violation 'xpath-dm:document-unparsed-entity-public-id
				   "Unknown node" n))))

;;;; 5.16 unparsed-entity-system-id Accessor
;;;; dm:unparsed-entity-system-id($node       as node(),
;;;;                              $entityname as xs:string) as xs:anyURI?
(define (xpath-dm:unparsed-entity-system-id n entity-name)
  (cond ((document? n)
	 (xpath-dm:document-unparsed-entity-system-id n entity-name))
	((or (element? n) (attr? n) (processing-instruction? n)
	     (comment? n) (text? n) (namespace? n)) '())
	(else (assertion-violation 'xpath-dm:document-unparsed-entity-system-id
				   "Unknown node" n))))

;;; 6 Nodes
(define (value/empty-sequence proc)
  (lambda (n)
    (cond ((proc n)) (else '()))))
(define base-uri/empty (value/empty-sequence node-base-uri))
(define (children n) (node-list->list (node-child-nodes n)))
(define parent-node (value/empty-sequence node-parent-node))
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

(define public-id (value/empty-sequence entity-public-id))
(define system-id (value/empty-sequence entity-system-id))
(define (xpath-dm:document-unparsed-entity-public-id d name)
  (cond ((get-entity d name) => public-id)
	(else '())))
(define (xpath-dm:document-unparsed-entity-system-id d name)
  (cond ((get-entity d name) => system-id)
	(else '())))

(define (get-entity d name)
  (define doctype (document-doctype d))
  (define entities (document-type-entities doctype))
  (hashtable-ref entities name #f))
  
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
(define (xpath-dm:element-is-id e) #f) ;; for now
(define (xpath-dm:element-is-idrefs e) #f) ;; for now
(define (xpath-dm:element-namespace-nodes e)
  (node-list->list (element:namespace-nodes e)))
(define (xpath-dm:element-nilled e) #f) ;; for now
(define xpath-dm:element-parent parent-node)
;; not sure what to do with this as we don't support type atm
(define (xpath-dm:element-type-name attr) 'xs:untypedAtomic)

;;;; 6.3 Attribute Nodes
;; NOTE: attribute nodes properties are described a bit vaguely, so
;;       not entirely sure if these're correct or not.
(define xpath-dm:attribute-string-value attr-value)
;; TODO should we wrap with xs:untypedAtomic?
(define xpath-dm:attribute-typed-value xpath-dm:attribute-string-value)
(define xpath-dm:attribute-node-name attr-name)
(define (xpath-dm:attribute-is-id attr) #f) ;; for now
(define (xpath-dm:attribute-is-idrefs attr) #f) ;; for now
(define xpath-dm:attribute-parent attr-owner-element)
;; not sure what to do with this as we don't support type atm
(define (xpath-dm:attribute-type-name attr) 'xs:untypedAtomic)

;;;; 6.4 Namespace Nodes
(define xpath-dm:namespace-uri namespace-uri)
(define xpath-dm:namespace-parent parent-node)

;;;; 6.5 Processing Instruction Nodes
(define xpath-dm:processing-instruction-content character-data-data)
(define xpath-dm:processing-instruction-target processing-instruction-target)
(define xpath-dm:processing-instruction-base-uri base-uri/empty)
(define xpath-dm:processing-instruction-parent parent-node)

;;;; 6.6 Comment Nodes
(define xpath-dm:comment-content character-data-data)
(define xpath-dm:comment-parent parent-node)

;;;; 6.7 Text Nodes
(define xpath-dm:text-content text-whole-text)
(define xpath-dm:text-parent parent-node)

;;; Helpers
(define (text-node-iterator->string itr)
  (let-values (((out e) (open-string-output-port)))
    (do ((n (node-iterator:next-node itr) (node-iterator:next-node itr)))
	((not n) (e))
      (put-string out (xpath-dm:string-value n)))))

)
