;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; text/xml/dsig/verifier.scm - XML signature verifier
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
;;; Section 3.2: Core Validation
;;; - URL https://www.w3.org/TR/xmldsig-core1/

;; For now, we don't implement 3.2.1 Reference Validation
#!nounbound
(library (text xml dsig verifier)
    (export xmldsig:verify
	    *xmldsig:default-verification-context*
	    xmldsig:key-value-key-selector

	    xmldsig:verification-context?
	    (rename (make-xmldsig:verification-context
		     xmldsig:make-verification-context))
	    +xmldsig:default-canonicalization-methods+
	    +xmldsig:default-signature-methods+
	    +xmldsig:default-digest-methods+
	    )
    (import (rnrs)
	    (sagittarius) ;; for bytevector->integer
	    (rename (sagittarius crypto keys)
		    (key-pair-private keypair-private)
		    (key-pair-public keypair-public)
		    (*key:rsa* RSA))
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (rfc base64)
	    (text xml dom)
	    (text xml dom writer)
	    (text xml dsig types) 
	    (text xml dsig algorithms)
	    (text xml dsig utils)
	    (text xml xpath dm))

(define-record-type xmldsig:verification-context
  ;; supporting methods
  (fields digest-methods
	  signature-methods
	  canonicalization-methods))

(define +xmldsig:default-canonicalization-methods+
  `(
    ,*xmldsig:canonicalization-c14n*
    ,*xmldsig:canonicalization-c14n11*
    ,*xmldsig:canonicalization-exc-c14n*
    ,*xmldsig:canonicalization-c14n-w/comment*
    ,*xmldsig:canonicalization-c14n11-w/comment*
    ,*xmldsig:canonicalization-exc-c14n-w/comment*
    ))

(define +xmldsig:default-signature-methods+
  `(
    ,*xmldsig:rsa-sha1*
    ,*xmldsig:rsa-sha256*
    ,*xmldsig:rsa-sha224*
    ,*xmldsig:rsa-sha384*
    ,*xmldsig:rsa-sha512*
    ))

(define +xmldsig:default-digest-methods+
  `(
    ,*xmldsig:digest-sha1*
    ,*xmldsig:digest-sha256*
    ,*xmldsig:digest-sha224*
    ,*xmldsig:digest-sha384*
    ,*xmldsig:digest-sha512*
    ))

(define *xmldsig:default-verification-context*
  (make-xmldsig:verification-context
   +xmldsig:default-digest-methods+
   +xmldsig:default-signature-methods+
   +xmldsig:default-canonicalization-methods+))

(define (xmldsig:verify dom key-selector
	 :optional (context *xmldsig:default-verification-context*))
  (define signature
    (node-list:item
     (document:get-elements-by-tag-name-ns dom +xml:dsig-namespace+ "Signature")
     0))

  (define (get-algorithm e) (element:get-attribute e "Algorithm"))
  (define (search-method methods name)
    (find (lambda (method) (string=? (ds:method-uri method) name)) methods))
  (define si (get-element signature "SignedInfo"))
  (define cm (search-method
	      (xmldsig:verification-context-canonicalization-methods context)
	      (get-algorithm (get-element si "CanonicalizationMethod"))))
  (define sm (search-method
	      (xmldsig:verification-context-signature-methods context)
	      (get-algorithm (get-element si "SignatureMethod"))))
  (define dm (search-method
	      (xmldsig:verification-context-digest-methods context)
	      (get-algorithm (get-element si "DigestMethod"))))
  
  (define writer (make-dom-writer (ds:canonicalize-method-options cm)))
  (define (canonicalise si)
    (let-values (((out e) (open-string-output-port)))
      (writer si out)
      (e)))
  
  (define (check-digest dom dm)
    (define dve (get-element signature "DigestValue"))
    (define dv (base64-decode-string (xpath-dm:string-value dve) :transcoder #f))
    (define dv-nodes (node-child-nodes dve))
    (define sve (get-element signature "SignatureValue"))
    (define sv-nodes (node-child-nodes sve))
    (define (do-modify! proc e nl)
      (node-list-for-each (lambda (n) (proc e n)) nl))
    (define (do-check)
      (let ((s (canonicalise dom))
	    (md (make-message-digest (ds:digest-method-algorithm dm))))
	(unless (bytevector=? dv (digest-message md (string->utf8 s)))
	  (error 'xmldsig:verify "Invalid digest"))))
    (dynamic-wind
	(lambda ()
	  (do-modify! node:remove-child! dve dv-nodes)
	  (do-modify! node:remove-child! sve sv-nodes))
	do-check
	(lambda ()
	  (do-modify! node:append-child! dve dv-nodes)
	  (do-modify! node:append-child! sve sv-nodes))))
  
  (define (find-key sig)
    (define ki*
      (node-list->list
       (element:get-elements-by-tag-name-ns sig +xml:dsig-namespace+ "KeyInfo")))
    (cond ((exists key-selector ki*))
	  (else (error 'xmldsig:verify "No verify key found"))))
  (define (check-signature si sm)
    (define verifier (make-verifier (ds:signature-method-cipher sm)
				    (find-key signature)
				    :verifier pkcs1-emsa-v1.5-verify
				    :digest (ds:signature-method-digest sm)))
    (verifier-verify-signature verifier
			       (string->utf8 (canonicalise si))
			       (base64-decode-string
				(xpath-dm:string-value
				 (get-element signature "SignatureValue"))
				:transcoder #f)))
  (check-digest dom dm)
  (check-signature si sm))

(define (xmldsig:key-value-key-selector ki)
  (define kv (get-element ki "KeyValue"))
  (define (get-value e name)
    (bytevector->integer
     (base64-decode-string (xpath-dm:string-value (get-element e name))
			   :transcoder #f)))
  (define writer (make-dom-writer))

  (and kv 
       (cond ((get-element kv "DSAKeyValue") =>
	      (lambda (dsa)
		(error 'xmldsig:key-value-key-selector "not yet")))
	     ((get-element kv "RSAKeyValue") =>
	      (lambda (rsa)
		(generate-public-key RSA
				     (get-value rsa "Modulus")
				     (get-value rsa "Exponent"))))
	     (else
	      ;; TODO invalid format, should we raise an error here?
	      #f))))


;;; helper
(define (get-element e name)
  (let ((nl (element:get-elements-by-tag-name-ns e +xml:dsig-namespace+ name)))
    (and (not (zero? (node-list-length nl)))
	 (node-list:item nl 0))))
)
