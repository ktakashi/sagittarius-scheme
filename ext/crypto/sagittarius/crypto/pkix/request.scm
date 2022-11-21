;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/request.scm - X.509 Certificate Signing Request
;;;
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (sagittarius crypto pkix request)
    (export x509-attribute? <x509-attribute>
	    x509-attribute-type
	    x509-attribute-values
	    make-x509-extension-request-attribute
	    make-x509-challenge-password-attribute
	    x509-attribute-of
	    x509-attribute->x509-extension-list

	    *pkcs-9:channelge-password*
	    *pkcs-9:extenion-request*

	    x509-certification-request? <x509-certification-request>
	    x509-certification-request-version
	    x509-certification-request-subject
	    x509-certification-request-public-key
	    x509-certification-request-attributes
	    x509-certification-request-signature
	    x509-certification-request-signature-algorithm
	    read-x509-certification-request
	    bytevector->x509-certification-request
	    x509-certification-request->bytevector
	    write-x509-certification-request
	    validate-x509-certification-request
	    x509-certification-request-signature-validator
	    
	    x509-certification-request-template?
	    x509-certification-request-template-builder
	    sign-x509-certification-request-template)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix dn)
	    (sagittarius crypto pkix keys)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix attributes)
	    (sagittarius crypto pkix signatures)
	    (sagittarius crypto pkix extensions)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius combinators)
	    (sagittarius mop immutable)
	    (util bytevector)
	    (record builder)
	    (srfi :1 lists))
;; useful utility :)
(define (make-slot-ref getter conv) (lambda (o) (conv (getter o))))
(define ((list-of pred) l) (for-all pred l))

(define (make-x509-challenge-password-attribute (password string?))
  ;; TODO check alpha numerical...
  (make <x509-attribute>
    :type (der-object-identifier->oid-string *pkcs-9:channelge-password*)
    :values (der-set (make <directory-string>
		       :type 'utf8-string
		       :value (string->der-utf8-string password)))))

(define (make-x509-extension-request-attribute . x509-extensions)
  (unless (for-all x509-extension? x509-extensions)
    (assertion-violation 'make-x509-extension-request-attribute
			 "List of X509 extension is required" x509-extensions))
  (make <x509-attribute>
    :type (der-object-identifier->oid-string *pkcs-9:extenion-request*)
    :values (der-set (x509-extension-list->extensions x509-extensions))))

(define *extension-request*
  (der-object-identifier->oid-string *pkcs-9:extenion-request*))
(define (x509-attribute->x509-extension-list (x509-attribute x509-attribute?))
  (unless (equal? *extension-request* (x509-attribute-type x509-attribute))
    (assertion-violation 'x509-attribute->x509-extension-list
			 "Given X509 attribute is not extension request"
			 x509-attribute))
  (append-map extensions->x509-extension-list
	      (asn1-collection->list (x509-attribute-values x509-attribute))))

(define csr-info (.$ certification-request-certification-request-info
		     x509-signed-object-c))
(define-class <x509-certification-request> (<x509-signed-object>)
  ((encoded :init-keyword :encoded :init-value #f :mutable #t
	    :reader x509-certification-request-encoded
	    :writer x509-certification-request-encoded-set!)
   (version :allocation :virtual :cached #t
	    :slot-ref (make-slot-ref
		       (.$ certification-request-info-version csr-info)
		       (lambda (v) (+ (der-integer->integer v) 1)))
	    :reader x509-certification-request-version)
   (subject :allocation :virtual :cached #t
	    :slot-ref (make-slot-ref
		       (.$ certification-request-info-subject csr-info)
		       name->x509-name)
	    :reader x509-certification-request-subject)
   (public-key :allocation :virtual :cached #t
    :slot-ref (make-slot-ref
	       (.$ certification-request-info-subject-pk-info csr-info)
	       import-public-key)
    :reader x509-certification-request-public-key)
   (attributes :allocation :virtual :cached #t
	       :slot-ref (make-slot-ref
			  (.$ certification-request-info-attributes csr-info)
			  attributes->x509-attributes)
	       :reader x509-certification-request-attributes)))
(define (x509-certification-request-signature
	 (csr x509-certification-request?))
  (x509-signed-object-signature csr))
(define (x509-certification-request-signature-algorithm
	 (csr x509-certification-request?))
  (x509-signed-object-algorithm csr))



(define-method write-object ((o <x509-certification-request>) p)
  (define signature-hex
    (bytevector->hex-string (x509-certification-request-signature o)))
  (define attributes (x509-certification-request-attributes o))

  (let-values (((out e) (open-string-output-port)))
    (format out "#<x509-certification-request~%")
    (format out "              Version: ~a~%" (x509-certification-request-version o))
    (format out "            SubjectDN: ~a~%" (x509-name->string (x509-certification-request-subject o)))
    (format out "           Public Key: ~a~%" (x509-certification-request-public-key o))
    (format out "  Signature Algorithm: ~a~%" (x509-certification-request-signature-algorithm o))
    (cond ((< (string-length signature-hex) 40)
	   (format out "            Signature: ~a~%" signature-hex))
	  (else
	   (format out "            Signature: ~a~%" (substring signature-hex 0 40))
	   (do ((i 40 (+ i 40)) (len (string-length signature-hex))) ((>= i len))
	     (if (< i (- len 40))
		 (format out "                       ~a~%" (substring signature-hex i (+ i 40)))
		 (format out "                       ~a~%" (substring signature-hex i len))))))
    (unless (null? attributes)
      (format out "           Attributes: ~%")
      (for-each (lambda (e) (format out "             ~a~%" e))
		attributes))
    (display ">" out)
    (display (e) p)))

(define (x509-certification-request? o) (is-a? o <x509-certification-request>))

(define (read-x509-certification-request (in (and input-port? binary-port?)))
  (make <x509-certification-request>
    :c (asn1-object->asn1-encodable <certification-request>
				    (read-asn1-object in))))

(define (bytevector->x509-certification-request (bv bytevector?))
  (let ((r (read-x509-certification-request (open-bytevector-input-port bv))))
    (x509-certification-request-encoded-set! r bv)
    r))

(define (x509-certification-request->bytevector
	 (csr x509-certification-request?))
  (cond ((x509-certification-request-encoded csr))
	(else
	 (let ((bv (asn1-encodable->bytevector
		    (asn1-encodable->asn1-object
		     (x509-signed-object-c csr)))))
	   (x509-certification-request-encoded-set! csr bv)
	   bv))))
(define (write-x509-certification-request (csr x509-certification-request?)
					  :optional (out (current-output-port)))
  (let ((bv (x509-certification-request->bytevector csr)))
    (put-bytevector out bv)))

(define (validate-x509-certification-request (csr x509-certification-request?)
					     . validators)
  (for-all (lambda (v) (v csr)) validators))
(define (x509-certification-request-signature-validator
	 (csr x509-certification-request?))
  (define public-key (x509-certification-request-public-key csr))
  (unless (verify-x509-signed-object csr public-key)
    (assertion-violation 'x509-certification-request-signature-validator
			 "Invalid signature" csr)))

;; CSR template
(define-record-type x509-certification-request-template
  (fields subject-dn
	  attributes))

(define ((required who pred conv) o)
  (unless (pred o)
    (assertion-violation (list 'x509-certification-request-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (conv o))
(define ((optional who pred conv) o)
  (unless (or (not o) (pred o))
    (assertion-violation (list 'x509-certification-request-template who)
			 "Invalid value"
			 o `(must satisfy ,pred)))
  (and o (conv o)))
(define check-subject (required 'subject-dn x509-name? x509-name->name))
(define check-attributes
  (optional 'attributes (list-of x509-attribute?) values))
(define-syntax x509-certification-request-template-builder
  (make-record-builder x509-certification-request-template
   ((subject-dn #f check-subject)
    (attributes '() check-attributes))))

(define *v1* (integer->der-integer 0))
(define (x509-certification-request-template->certification-request-info
	 template signature-algorithm public-key)
  (let ((attrs (x509-certification-request-template-attributes template)))
    (make <certification-request-info>
      :version *v1*
      :subject (x509-certification-request-template-subject-dn template)
      :subject-pk-info (public-key->subject-public-key-info public-key)
      :attributes (make <attributes>
		    :elements (map x509-attribute->attribute attrs)))))

(define (sign-x509-certification-request-template
	 (template x509-certification-request-template?)
	 (aid (or string? x509-algorithm-identifier?))
	 (key-pair key-pair?))
  (define x509-aid (if (string? aid)
		       (make-x509-algorithm-identifier aid)
		       aid))
  (define signer (x509-algorithm-identifier->signer
		  x509-aid (key-pair-private key-pair)))
  (let* ((algorithm (x509-algorithm-identifier->algorithm-identifier x509-aid))
	 (cri (x509-certification-request-template->certification-request-info
	       template algorithm (key-pair-public key-pair)))
	 (signature (signer-sign-message signer
		     (asn1-encodable->bytevector cri)))
	 (cr (make <certification-request>
	       :c cri
	       :algorithm algorithm
	       :signature (bytevector->der-bit-string signature))))
    (make <x509-certification-request> :c cr)))

)
