;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/akp.scm - Asymmetric Key Packages
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

;; Asymmetric Key Packages is originally PKCS#8 so this library is
;; located in PKCS category
;; ref
;; - https://datatracker.ietf.org/doc/html/rfc5958
#!nounbound
(library (sagittarius crypto pkcs modules akp)
    (export asn1-object->asn1-encodable ;; for convenience
	    bytevector->asn1-encodable

	    *akp:asymmetric-key-package-content-type*
	    asymmetric-key-package? <asymmetric-key-package>
	    asymmetric-key-package->list

	    one-asymmetric-key? <one-asymmetric-key>
	    one-asymmetric-key-vertsion
	    one-asymmetric-key-private-key-algorithm
	    one-asymmetric-key-private-key
	    one-asymmetric-key-attributes
	    one-asymmetric-key-public-key

	    private-key-info?

	    encrypted-private-key-info? <encrypted-private-key-info>
	    encrypted-private-key-info-encryption-algorithm
	    encrypted-private-key-info-encrypted-data)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkix modules x509))
(define oid oid-string->der-object-identifier)
(define *akp:asymmetric-key-package-content-type*
  (oid "2.16.840.1.101.2.1.2.78.5"))

;; OneAsymmetricKey ::= SEQUENCE {
;;   version                   Version,
;;   privateKeyAlgorithm       PrivateKeyAlgorithmIdentifier,
;;   privateKey                PrivateKey,
;;   attributes            [0] Attributes OPTIONAL,
;;   ...,
;;   [[2: publicKey        [1] PublicKey OPTIONAL ]],
;;   ...
;; }
;; skip Pad bits
(define (bytevector/pad->der-bit-string bv)
  (bytevector->der-bit-string (bytevector-copy bv 1)))
(define-asn1-encodable <one-asymmetric-key>
  (asn1-sequence
   ((version :type <der-integer> :reader one-asymmetric-key-vertsion)
    (private-key-algorithm :type <algorithm-identifier>
     :reader one-asymmetric-key-private-key-algorithm)
    (private-key :type <der-octet-string> 
		 :reader one-asymmetric-key-private-key)
    (attributes :type <attributes> :tag 0 :optional #t :explicit #f
		:converter make-der-set
		:reader one-asymmetric-key-attributes)
    (public-key :type <der-bit-string> :tag 1 :optional #t :explicit #f
		:converter bytevector/pad->der-bit-string
		:reader one-asymmetric-key-public-key))))

(define (one-asymmetric-key? o) (is-a? o <one-asymmetric-key>))

(define (private-key-info? o)
  (and (one-asymmetric-key? o)
       (zero? (der-integer->integer (one-asymmetric-key-vertsion o)))))

;; AsymmetricKeyPackage ::= SEQUENCE SIZE (1..MAX) OF OneAsymmetricKey
(define-asn1-encodable <asymmetric-key-package>
  (asn1-sequence
   (of :type <one-asymmetric-key> :reader asymmetric-key-package->list)))
(define (asymmetric-key-package? o) (is-a? o <asymmetric-key-package>))

;; EncryptedPrivateKeyInfo ::= SEQUENCE {
;;   encryptionAlgorithm  EncryptionAlgorithmIdentifier,
;;   encryptedData        EncryptedData }
;; EncryptionAlgorithmIdentifier ::= AlgorithmIdentifier
;;                                    { CONTENT-ENCRYPTION,
;;                                      { KeyEncryptionAlgorithms } }
;; EncryptedData ::= OCTET STRING
(define-asn1-encodable <encrypted-private-key-info>
  (asn1-sequence
   ((encryption-algorithm :type <algorithm-identifier>
     :reader encrypted-private-key-info-encryption-algorithm)
    (encrypted-data :type <der-octet-string>
		    :reader encrypted-private-key-info-encrypted-data))))
(define (encrypted-private-key-info? o) (is-a? o <encrypted-private-key-info>))

)
