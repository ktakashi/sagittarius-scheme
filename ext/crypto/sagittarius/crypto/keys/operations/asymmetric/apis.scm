;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/apis.scm - Asymmetric key op
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
(library (sagittarius crypto keys operations asymmetric apis)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key

	    import-public-key
	    import-private-key
	    export-public-key
	    export-private-key

	    oid->key-operation
	    key->oid
	    
	    public-key-format
	    *public-key-formats*
	    public-key-format?

	    calculate-key-agreement

	    private-key-format
	    *private-key-formats*
	    private-key-format?
	    )
    (import (rnrs)
	    (sagittarius crypto asn1)
	    (sagittarius crypto keys types)
	    (clos user))

(define-generic generate-key-pair)
(define-generic generate-public-key)
(define-generic generate-private-key)
(define-generic import-public-key)
(define-generic import-private-key)
(define-generic export-public-key)
(define-generic export-private-key)

(define-generic oid->key-operation :class <one-of-specializable-generic>)
(define-generic key->oid)

(define-generic calculate-key-agreement) ;; key agreement

(define-enumeration public-key-format (raw subject-public-key-info)
  public-key-formats)
(define *public-key-formats* (enum-set-universe (public-key-formats)))
(define (public-key-format? s) (enum-set-member? s *public-key-formats*))

(define-method import-public-key ((key <bytevector>)
				  (format (eq 'subject-public-key-info)))
  (import-public-key (open-bytevector-input-port key) format))
(define-method import-public-key ((key <port>)
				  (format (eq 'subject-public-key-info)))
  (import-public-key (read-asn1-object key) format))
#|
  SubjectPublicKeyInfo {PUBLIC-KEY: IOSet} ::= SEQUENCE {
      algorithm        AlgorithmIdentifier {PUBLIC-KEY, {IOSet}},
      subjectPublicKey BIT STRING
  }
|#
(define-method import-public-key ((key <der-sequence>)
				  (format (eq 'subject-public-key-info)))
  (let*-values (((aid pk) (deconstruct-asn1-collection key))
		((oid . maybe-param) (deconstruct-asn1-collection aid)))
    (let ((s (oid->key-operation (der-object-identifier->oid-string oid))))
      (apply import-public-key s (der-bit-string->bytevector pk) 'raw
	     (if (null? maybe-param)
		 '()
		 (extract-parameter (der-object-identifier->oid-string oid)
				    (car maybe-param)))))))

(define-enumeration private-key-format (raw private-key-info)
  private-key-formats)
(define *private-key-formats* (enum-set-universe (private-key-formats)))
(define (private-key-format? s) (enum-set-member? s *public-key-formats*))

(define-method import-private-key ((key <bytevector>)
				   (format (eq 'private-key-info)))
  (import-private-key (open-bytevector-input-port key) format))
(define-method import-private-key ((key <port>)
				   (format (eq 'private-key-info)))
  (import-private-key (read-asn1-object key) format))
;; OneAsymmetricKey ::= SEQUENCE {
;;   version                   Version,
;;   privateKeyAlgorithm       PrivateKeyAlgorithmIdentifier,
;;   privateKey                PrivateKey,
;;   attributes            [0] Attributes OPTIONAL,
;;   ...,
;;   [[2: publicKey        [1] PublicKey OPTIONAL ]],
;;   ...
;; }
(define-method import-private-key ((key <der-sequence>)
				   (format (eq 'private-key-info)))
  (import-pki-private-key #f key))
(define-method import-private-key (m
				   (key <der-sequence>)
				   (format (eq 'private-key-info)))
  (import-pki-private-key m key))

(define (import-pki-private-key m key)
  (let*-values (((version pka pk . ignore) (deconstruct-asn1-collection key))
		((oid . maybe-param) (deconstruct-asn1-collection pka)))
    (let ((s (oid->key-operation (der-object-identifier->oid-string oid))))
      (unless (or (not m) (eq? m s))
	(assertion-violation 'import-private-key
			     "Specified key scheme and actual OID mismatches"
			     m (der-object-identifier->oid-string oid)))
      (apply import-private-key s (der-octet-string->bytevector pk)
	     (if (null? maybe-param)
		 '()
		 (extract-parameter (der-object-identifier->oid-string oid)
				    (car maybe-param)))))))

;; Use :around specifier to let the subclass of the key match...
(define-method export-private-key :around (m k (format (eq 'private-key-info)))
  (export-private-key k format))
(define-method export-private-key :around ((key <private-key>)
					   (format (eq 'private-key-info)))
  (let ((raw (export-private-key key 'raw))
	(oid (key->oid key)))
    (asn1-encodable->bytevector
     (der-sequence
      (integer->der-integer 0)
      (der-sequence (oid-string->der-object-identifier oid)
		    (extract-aid-parameter oid raw))
      (bytevector->der-octet-string raw)))))

;; internal method
(define *dsa-oid*   "1.2.840.10040.4.1")
(define *ecdsa-oid* "1.2.840.10045.2.1")
(define-generic extract-parameter)
(define-method extract-parameter (oid raw) '())
(define-method extract-parameter ((oid (equal *ecdsa-oid*)) param)
  (list param))
(define-method extract-parameter ((oid (equal *dsa-oid*)) param)
  (list param))

(define-generic extract-aid-parameter)
(define-method extract-aid-parameter (oid raw) (make-der-null))
(define-method extract-aid-parameter ((oid (equal *ecdsa-oid*)) raw)
  ;; Fxxk, we need to deconstruct and provide curve parameter here...
  (let* ((seq (bytevector->asn1-object raw))
	 (obj (asn1-collection-find-tag seq 0)))
    (if obj
	(ber-tagged-object-obj obj)
	(make-der-null)))) ;; mustn't happen, but we don't want to fail it

)
