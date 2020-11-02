;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dsig/signer.scm - XML signature signer
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
;;; Section 3.1: Signature Generation
;;; - URL https://www.w3.org/TR/xmldsig-core1/

;; For now, we don't implement 3.1.1 Reference Generation
#!nounbound
(library (text xml dsig signer)
    (export ds:signing-context ds:signing-context?
	    (rename (make-ds:signing-context ds:make-signing-context))

	    xmldsig:signed-info? xmldsig:make-signed-info
	    
	    xmldsig:sign
	    )
    (import (rnrs)
	    (crypto)
	    (math)
	    (rfc base64)
	    (sagittarius control) ;; for define-values
	    (text xml dom)
	    (text xml dom writer)
	    (text xml dsig types) 
	    (text xml dsig algorithms))

(define-record-type ds:signing-context
  (fields reference-uri
	  canonicalize-method
	  digest-method
	  signature-method))

(define-record-type xmldsig:signed-info
  (parent <ds:signed-info>)
  (fields signing-contect)
  (protocol (lambda (n)
	      (lambda (sc . args)
		((apply n args) sc)))))

(define (xmldsig:make-signed-info sc)
  (define dm (ds:signing-context-digest-method sc))
  (define cm (ds:signing-context-canonicalize-method sc))
  (define sm (ds:signing-context-signature-method sc))
  (let ((ref (ds:make-reference (ds:method-uri dm)
				(ds:signing-context-reference-uri sc))))
    (make-xmldsig:signed-info sc
			      (ds:method-uri cm)
			      (ds:method-uri sm)
			      (list ref))))
;; API
(define (xmldsig:sign dom si/sc keypair)
  (define private-key (keypair-private keypair))
  (define-values (si sc)
    (if (ds:signing-context? si/sc)
	(values (xmldsig:make-signed-info si/sc) si/sc)
	(values si/sc (xmldsig:signed-info-signing-contect si/sc))))
  (define dm (ds:signing-context-digest-method sc))
  (define cm (ds:signing-context-canonicalize-method sc))
  (define sm (ds:signing-context-signature-method sc))

  (define (canonicalise dom)
    (define writer (make-dom-writer (ds:canonicalize-method-options cm)))
    (let-values (((out e) (open-string-output-port)))
      (writer dom out)
      (e)))
  (define (digest content)
    (hash (ds:digest-method-algorithm dm) (string->utf8 content)))
  (define (sign key content)
    (define signer (make-cipher (ds:signature-method-cipher sm) key))
    (cipher-signature signer (string->utf8 content)
		      :encode pkcs1-emsa-v1.5-encode
		      :hash (ds:signature-method-digest sm)))
  
  (define sig (xmldsig:make-signature si keypair))
  
  (define signature (ds:element->dom-node sig dom))
  (define signed-info
    (node-list:item
     (element:get-elements-by-tag-name signature "SignedInfo") 0))
  (define (append-text e name text)
    (node:append-child!
     (node-list:item (element:get-elements-by-tag-name e name) 0)
     (document:create-text-node dom text)))
  ;; add ds:signature
  (node:append-child! (document-document-element dom) signature)
  (let ((digest (utf8->string (base64-encode (digest (canonicalise dom))))))
    ;; now get SignedInfo/DigestValue and set the above as a child text node
    (append-text signature "DigestValue" digest))
  (let ((dsig (utf8->string (base64-encode
			     (sign private-key (canonicalise signed-info))))))
    (append-text signature "SignatureValue" dsig))
  dom)

;; private API
(define (xmldsig:make-signature si keypair)
  (define public-key (keypair-public keypair))
  ;; TODO should we check with signature method and key?
  (let ((kv (ds:make-key-value
	     (if (rsa-public-key? public-key)
		 (ds:make-rsa-key-value (rsa-public-key-modulus public-key)
					(rsa-public-key-exponent public-key))
		 (assertion-violation 'xmldsig:make-signature "Not yet"
				      public-key)))))
    (ds:make-signature si
		       (ds:make-signature-value "")
		       (ds:make-key-info (list kv)))))
)
