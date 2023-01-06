;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pem.scm - PEM
;;;  
;;;   Copyright (c) 2022-2023 Takashi Kato <ktakashi@ymail.com>
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7468
;; - https://datatracker.ietf.org/doc/html/rfc5915 (for EC PRIVATE KEY)
#!nounbound
(library (sagittarius crypto pem)
    (export pem-object? <pem-object>
	    read-pem-object string->pem-object
	    write-pem-object pem-object->string

	    pem-object-label
	    pem-object-header
	    pem-object-content

	    pem-object->object
	    ->pem-object

	    x509-certificate->pem-object
	    x509-crl->pem-object
	    x509-csr->pem-object
	    cms-content-info->pem-object
	    private-key->pem-object
	    pkcs-one-asymmetric-key->pem-object
	    pkcs-encrypted-private-key-info->pem-object
	    public-key->pem-object
	    subject-public-key-info->pem-object
	    ecdsa-private-key->pem-object
	    
	    *standard-style*
	    *lax-style*
	    *strict-style*
	    *rfc1421-style*

	    ;; for enhancement
	    dispatch-pem-label)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto pem io)
	    (sagittarius crypto keys)
	    (sagittarius crypto pkcs cms)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto pkix certificate)
	    (sagittarius crypto pkix keys)
	    (sagittarius crypto pkix request)
	    (sagittarius crypto pkix revocation))
(define-generic dispatch-pem-label)

(define (pem-object->object (pem-object pem-object?))
  (dispatch-pem-label (pem-object-label pem-object)
		      (pem-object-content pem-object)))
(define-generic ->pem-object)
(define-method ->pem-object ((x <x509-certificate>))
  (x509-certificate->pem-object x))
(define-method ->pem-object ((x <x509-certificate-revocation-list>))
  (x509-crl->pem-object x))
(define-method ->pem-object ((x <x509-certification-request>))
  (x509-csr->pem-object x))
(define-method ->pem-object ((x <cms-content-info>))
  (cms-content-info->pem-object x))
(define-method ->pem-object ((x <private-key>))
  (private-key->pem-object x))
(define-method ->pem-object ((x <pkcs-one-asymmetric-key>))
  (pkcs-one-asymmetric-key->pem-object x))
(define-method ->pem-object ((x <pkcs-encrypted-private-key-info>))
  (pkcs-encrypted-private-key-info->pem-object x))
(define-method ->pem-object ((x <public-key>))
  (public-key->pem-object x))
(define-method ->pem-object ((x <subject-public-key-info>))
  (subject-public-key-info->pem-object x))
#|
Sec. Label                  ASN.1 Type              Reference Module
----+----------------------+-----------------------+---------+----------
  5  CERTIFICATE            Certificate             [RFC5280] id-pkix1-e
  6  X509 CRL               CertificateList         [RFC5280] id-pkix1-e
  7  CERTIFICATE REQUEST    CertificationRequest    [RFC2986] id-pkcs10
  8  PKCS7                  ContentInfo             [RFC2315] id-pkcs7*
  9  CMS                    ContentInfo             [RFC5652] id-cms2004
 10  PRIVATE KEY            PrivateKeyInfo ::=      [RFC5208] id-pkcs8
                            OneAsymmetricKey        [RFC5958] id-aKPV1
 11  ENCRYPTED PRIVATE KEY  EncryptedPrivateKeyInfo [RFC5958] id-aKPV1
 12  ATTRIBUTE CERTIFICATE  AttributeCertificate    [RFC5755] id-acv2
 13  PUBLIC KEY             SubjectPublicKeyInfo    [RFC5280] id-pkix1-e
|#
(define label-certificate "CERTIFICATE")
(define label-x509-crl "X509 CRL")
(define label-csr "CERTIFICATE REQUEST")
(define label-pkcs7 "PKCS7")
(define label-cms "CMS")
(define label-private-key "PRIVATE KEY")
(define label-encrypted-private-key "ENCRYPTED PRIVATE KEY")
;; TODO ATTRIBUTE CERTIFICATE
(define label-public-key "PUBLIC KEY")

(define label-ec-private-key "EC PRIVATE KEY")

(define-method dispatch-pem-label ((l (equal label-certificate)) bv)
  (bytevector->x509-certificate bv))
;; Should not treat "X509 CERTIFICATE" or "X.509 CERTIFICATE"
;; as equivalent to "CERTIFICATE"...

(define-method dispatch-pem-label ((l (equal label-x509-crl)) bv)
  (bytevector->x509-certificate-revocation-list bv))
;; CRL should not be treated as equivalent to "X509 CRL"

(define-method dispatch-pem-label ((l (equal label-csr)) bv)
  (bytevector->x509-certification-request bv))
(define-method dispatch-pem-label ((l (equal "NEW CERTIFICATE REQUEST")) bv)
  (bytevector->x509-certification-request bv))

(define-method dispatch-pem-label ((l (equal label-pkcs7)) bv)
  (bytevector->cms-content-info bv))
(define-method dispatch-pem-label ((l (equal label-cms)) bv)
  (bytevector->cms-content-info bv))

(define-method dispatch-pem-label ((l (equal label-private-key)) bv)
  (bytevector->pkcs-one-asymmetric-key bv))

(define-method dispatch-pem-label ((l (equal label-encrypted-private-key)) bv)
  (bytevector->pkcs-encrypted-private-key-info bv))

(define-method dispatch-pem-label ((l (equal label-public-key)) bv)
  (bytevector->subject-public-key-info bv))

(define-method dispatch-pem-label ((l (equal label-ec-private-key)) bv)
  (import-private-key *key:ecdsa* bv))

(define (x509-certificate->pem-object (x509-certificate x509-certificate?))
  (make-pem-object label-certificate #f
		   (x509-certificate->bytevector x509-certificate)))
(define (x509-crl->pem-object (x509-crl x509-certificate-revocation-list?))
  (make-pem-object label-x509-crl #f
		   (x509-certificate-revocation-list->bytevector x509-crl)))
(define (x509-csr->pem-object (x509-csr x509-certification-request?))
  (make-pem-object label-csr #f
		   (x509-certification-request->bytevector x509-csr)))
(define (cms-content-info->pem-object (content-info cms-content-info?))
  (make-pem-object label-cms #f
		   (cms-content-info->bytevector content-info)))
(define (private-key->pem-object (private-key private-key?))
  (make-pem-object label-private-key #f
   (export-private-key private-key (private-key-format private-key-info))))
(define (pkcs-one-asymmetric-key->pem-object (oak pkcs-one-asymmetric-key?))
  (make-pem-object label-private-key #f
		   (pkcs-one-asymmetric-key->bytevector oak)))
(define (pkcs-encrypted-private-key-info->pem-object
	 (epki pkcs-encrypted-private-key-info?))
  (make-pem-object label-encrypted-private-key #f
		   (pkcs-encrypted-private-key-info->bytevector epki)))
(define (public-key->pem-object (public-key public-key?))
  (make-pem-object label-public-key #f
   (export-public-key public-key (public-key-format subject-public-key-info))))
(define (subject-public-key-info->pem-object (spki subject-public-key-info?))
  (make-pem-object label-public-key #f
   (subject-public-key-info->bytevector spki)))

(define (ecdsa-private-key->pem-object (private-key ecdsa-private-key?))
  (make-pem-object label-ec-private-key #f (export-private-key private-key)))
)
