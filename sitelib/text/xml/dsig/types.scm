;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dsig/types.scm - XML signature types
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

;;; reference
;;; XML Signature Syntax and Processing Version 1.1
;;; Section 4: Core Signature Syntax
;;; - URL https://www.w3.org/TR/xmldsig-core1/

#!nounbound
(library (text xml dsig types)
    (export +xml:dsig-namespace+
	    ds:element? ds:element-id ds:element-tag-name
	    ds:element->dom-node
	    
	    ds:signature? (rename (make-ds:signature ds:make-signature))
	    ds:signature-signed-info ds:signature-signature-value
	    ds:signature-key-info ds:signature-object

	    ds:signature-value?
	    (rename (make-ds:signature-value ds:make-signature-value))
	    ds:signature-value-content

	    ds:signed-info? (rename (make-ds:signed-info ds:make-signed-info))
	    ds:signed-info-canonicalization-method
	    ds:signed-info-signature-method ds:signed-info-reference

	    ds:reference? (rename (make-ds:reference ds:make-reference))
	    ds:reference-transforms ds:reference-digest-method
	    ds:reference-digest-value ds:reference-uri ds:reference-type

	    ds:key-info? (rename (make-ds:key-info ds:make-key-info))
	    ds:key-info-elements

	    ds:key-name? (rename (make-ds:key-name ds:make-key-name))
	    ds:key-name-value

	    ds:key-value? (rename (make-ds:key-value ds:make-key-value))
	    ds:key-value-content

	    ds:rsa-key-value?
	    (rename (make-ds:rsa-key-value ds:make-rsa-key-value))
	    ds:rsa-key-value-modulus ds:rsa-key-value-exponent
	    )
    (import (rnrs)
	    (rfc base64)
	    (sagittarius)
	    (srfi :1 lists)
	    (srfi :117 list-queues)
	    (text xml dom))

(define +xml:dsig-namespace+ "http://www.w3.org/2000/09/xmldsig#")
(define (ds:create-element doc name)
  (document:create-element-ns doc +xml:dsig-namespace+ name))

;; TODO make better API on base64 side
(define (ds:encode-base64 bv)
  (let-values (((out e) (open-bytevector-output-port)))
    (define (put v)
      (if v
	  (put-u8 out v)
	  (begin (put-u8 out #x0d) (put-u8 out #x0a))))
    (define inp (open-bytevector-input-port bv))
    (define (in) (get-u8 inp))
    (define encoder (make-base64-encoder))
    (do () ((encoder in put)))
    (e)))

;; base record
(define-record-type ds:element
  (fields id				; optional attribute
	  tag-name
	  >dom)
  (protocol (lambda (p)
	      (case-lambda
	       ((tag-name ->dom) (p #f tag-name ->dom))
	       ((id tag-name ->dom) (p id tag-name ->dom))))))

(define (ds:element->dom-node ds:element document)
  ((ds:element->dom ds:element) ds:element document))
(define (ds:element-create-self-element e doc)
  (let ((elm (ds:create-element doc (ds:element-tag-name e))))
    (cond ((ds:element-id e) =>
	   (lambda (id) (element:set-attribute! elm "Id" id))))
    elm))

;; 4.2 The Signature Element
(define-record-type ds:signature
  (parent ds:element)
  (fields signed-info			; element
	  signature-value		; element
	  key-info			; optional element
	  object			; optional element *
	  )
  (protocol (lambda (n)
	      (case-lambda
	       ((si sv) ((n "Signature" ds:signature->dom) si sv #f '()))
	       ((si sv ki) ((n "Signature" ds:signature->dom) si sv ki '()))
	       ((si sv ki o) ((n "Signature" ds:signature->dom) si sv ki o))
	       ((si sv ki o id)
		((n id "Signature" ds:signature->dom) si sv ki o))))))
(define (ds:signature->dom sig document)
  (define sig-elem (ds:element-create-self-element sig document))
  (define (append-child ds:elm)
    (node:append-child! sig-elem (ds:element->dom-node ds:elm document)))

  (append-child (ds:signature-signed-info sig))
  (append-child (ds:signature-signature-value sig))
  (cond ((ds:signature-key-info sig) => append-child))
  (for-each append-child (ds:signature-object sig))
  sig-elem)

;; 4.3 The Signaturevalue Element
(define-record-type ds:signature-value
  (parent ds:element)
  (fields content			; base64 text value
	  )
  (protocol (lambda (n)
	      (case-lambda
	       ((content)
		((n "SignatureValue" ds:signature-value->dom) content))
	       ((content id)
		((n id "SignatureValue" ds:signature-value->dom) content))))))
(define (ds:signature-value->dom sv document)
  (define sv-elem (ds:element-create-self-element sv document))
  (let ((text (ds:signature-value-content sv)))
    (node:append-child! sv-elem
			(document:create-text-node document text))
    sv-elem))

;; 4.4 The SignedInfo Element
(define-record-type ds:signed-info
  (parent ds:element)
  (fields canonicalization-method	; element
	  signature-method		; element
	  reference			; element +
	  )
  (protocol (lambda (n)
	      (case-lambda
	       ((cm sm ref) ((n "SignedInfo" ds:signed-info->dom) cm sm ref))
	       ((cm sm ref id)
		((n id "SignedInfo" ds:signed-info->dom) cm sm ref))))))
(define (ds:signed-info->dom si doc)
  (define si-elm (ds:element-create-self-element si doc))
  (define cm (ds:create-element doc "CanonicalizationMethod"))
  (define sm (ds:create-element doc "SignatureMethod"))
  (define (set-algorithm e v) (element:set-attribute! e "Algorithm" v))
  
  (set-algorithm cm (ds:signed-info-canonicalization-method si))
  (set-algorithm sm (ds:signed-info-signature-method si))
  (node:append-child! si-elm cm)
  (node:append-child! si-elm sm)
  (for-each (lambda (ref)
	      (node:append-child! si-elm (ds:element->dom-node ref doc)))
	    (ds:signed-info-reference si))
  si-elm)

;;; 4.4.3 The Reference Element
(define-record-type ds:reference
  (parent ds:element)
  (fields transforms			; element *
	  digest-method			; element
	  digest-value			; element
	  uri				; optional attribute
	  type				; optional attribute
	  )
  (protocol (lambda (n)
	      (case-lambda
	       ((dm dv) ((n "Reference" ds:reference->dom) '() dm dv #f #f))
	       ((dm dv trns)
		((n "Reference" ds:reference->dom) trns dm dv #f #f))
	       ((dm dv trns id)
		((n id "Reference" ds:reference->dom) trns dm dv #f #f))
	       ((dm dv trns id uri)
		((n id "Reference" ds:reference->dom) trns dm dv uri #f))
	       ((dm dv trns id uri type)
		((n id "Reference" ds:reference->dom) trns dm dv uri type))))))
(define (ds:reference->dom ref doc)
  (define ref-elm (ds:element-create-self-element ref doc))
  (for-each (lambda (t) (node:append-child! ref-elm (ds:element->dom-node t)))
	    (ds:reference-transforms ref))
  (let ((dm (ds:create-element doc "DigestMethod")))
    (element:set-attribute! dm "Algorithm" (ds:reference-digest-method ref))
    (node:append-child! ref-elm dm))
  (let ((dv (ds:create-element doc "DigestValue"))
	(v (document:create-text-node doc (ds:reference-digest-value ref))))
    (node:append-child! dv v)
    (node:append-child! ref-elm dv))
  (cond ((ds:reference-uri ref) =>
	 (lambda (uri) (element:set-attribute! ref-elm "URI" uri))))
  (cond ((ds:reference-type ref) =>
	 (lambda (type) (element:set-attribute! ref-elm "Type" type))))
  ref-elm)

;;; TBD 4.4.3.4 The Transforms Element

;;; 4.5 The KeyInfo Element
(define +key-info:choices+
  '("KeyName" "KeyValue" "RetrievalMethod" "X509Data"
    "PGPData" "SPKIData" "MgmtData"))
(define-record-type ds:key-info
  (parent ds:element)
  (fields elements			; choice unbounded...
	  )
  (protocol (lambda (n)
	      (define (order-it c)
		(define q (list-queue))
		(define (pred name)
		  (lambda (e) (equal? name (ds:element-tag-name e))))
		(let loop ((n* +key-info:choices+) (c c))
		  (if (null? n*)
		      (list-queue-list q)
		      (let-values (((add next) (partition (pred (car n*)) c)))
			;; damn there's no procedure to add the elements of
			;; a list in one go...
			(for-each (lambda (n) (list-queue-add-back! q n)) add)
			(loop (cdr n*) next)))))
	      (case-lambda
	       ((c) ((n "KeyInfo" ds:key-info->dom) (order-it c)))
	       ((c id) ((n id "KeyInfo" ds:key-info->dom) (order-it c)))))))
(define (ds:key-info->dom ki document)
  (define ki-elm (ds:element-create-self-element ki document))
  (for-each (lambda (e)
	      (node:append-child! ki-elm (ds:element->dom-node e document)))
	    (ds:key-info-elements ki))
  ki-elm)

;;; 4.5.1 The KeyName Element
(define-record-type ds:key-name
  (parent ds:element)
  (fields value)
  (protocol (lambda (n)
	      (lambda (v)
		((n "KeyName" ds:key-name->dom) v)))))
(define (ds:key-name->dom kn document)
  (define kn-elm (ds:element-create-self-element kn document))
  (let ((text (document:create-text-node document (ds:key-name-value kn))))
    (node:append-child! kn-elm text))
  kn-elm)

;;; 4.5.1 The KeyValue Element
(define-record-type ds:key-value
  (parent ds:element)
  (fields content)			; choice
  (protocol (lambda (n)
	      (lambda (c)
		((n "KeyValue" ds:key-value->dom) c)))))
(define (ds:key-value->dom kv document)
  (define kv-elm (ds:element-create-self-element kv document))
  (let ((kv-impl (ds:element->dom-node (ds:key-value-content kv) document)))
    (node:append-child! kv-elm kv-impl))
  kv-elm)

;;; TBD 4.5.2.1 The DSAKeyValue Element

;;; 4.5.2.1 The RSAKeyValue Element
(define-record-type ds:rsa-key-value
  (parent ds:element)
  (fields modulus
	  exponent)
  (protocol (lambda (n)
	      (lambda (m e)
		((n "RSAKeyValue" ds:rsa-key-value->dom) m e)))))
(define (ds:rsa-key-value->dom rkv document)
  (define rkv-elm (ds:element-create-self-element rkv document))
  (define m (ds:create-element document "Modulus"))
  (define e (ds:create-element document "Exponent"))
  (define (b64 i) (utf8->string (ds:encode-base64 (integer->bytevector i))))
  (define (set-value e v)
    (let ((v (document:create-text-node document (b64 v))))
      (node:append-child! e v)))
  (set-value m (ds:rsa-key-value-modulus rkv))
  (set-value e (ds:rsa-key-value-exponent rkv))
  (node:append-child! rkv-elm m)
  (node:append-child! rkv-elm e)
  rkv-elm)

)
