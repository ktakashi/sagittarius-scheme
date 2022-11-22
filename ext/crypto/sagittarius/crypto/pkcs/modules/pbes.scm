;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/pbes.scm - PKCS#5 modules
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc8018
#!nounbound
(library (sagittarius crypto pkcs modules pbes)
    (export asn1-object->asn1-encodable ;; for convenience
	    bytevector->asn1-encodable

	    *pbes:pbkdf2*
	    pbkdf2-params? <pbkdf2-params>
	    pbkdf2-params-salt
	    pbkdf2-params-iteration-count
	    pbkdf2-params-key-length
	    pbkdf2-params-prf

	    pbkdf2-salt-choice? <pbkdf2-salt-choice>
	    pbkdf2-salt-choice-value
	    ;; prf
	    *pbes:hmac/sha1*
	    *pbes:hmac/sha224*
	    *pbes:hmac/sha256*
	    *pbes:hmac/sha384*
	    *pbes:hmac/sha512*
	    *pbes:hmac/sha512/224*
	    *pbes:hmac/sha512/256*
	    *pbes:hmac/sha3-224*
	    *pbes:hmac/sha3-256*
	    *pbes:hmac/sha3-384*
	    *pbes:hmac/sha3-512*
	    ;; PBES1
	    *pbes:pbe/md2-des-cbc*
	    *pbes:pbe/md2-rc2-cbc*
	    *pbes:pbe/md5-des-cbc*
	    *pbes:pbe/md5-rc2-cbc*
	    *pbes:pbe/sha1-des-cbc*
	    *pbes:pbe/sha1-rc2-cbc*
	    pbe-parameter? <pbe-parameter>
	    pbe-parameter-salt
	    pbe-parameter-iteration-count
	    ;; PBES2
	    *pbes:pbes2*
	    pbes2-params? <pbes2-params>
	    pbes2-params-key-derivation-func
	    pbes2-params-encryption-scheme
	    ;; PBMAC1
	    *pbes:pbmac1*
	    pbmac1-params? <pbmac1-params>
	    pbmac1-params-key-derivation-func
	    pbmac1-params-message-auth-scheme
	    ;; supporting algorithms
	    *pbes:des-cbc*
	    *pbes:desede-cbc*
	    *pbes:rc2-cbc*
	    rc2-cbc-parameter? <rc2-cbc-parameter>
	    rc2-cbc-parameter-version
	    rc2-cbc-parameter-iv
	    *pbes:rc5-cbc-pad*
	    rc5-cbc-parameter? <rc5-cbc-parameter>
	    rc5-cbc-parameter-version
	    rc5-cbc-parameter-rounds
	    rc5-cbc-parameter-block-size-in-bits
	    rc5-cbc-parameter-iv
	    *pbes:aes128-cbc-pad*
	    *pbes:aes192-cbc-pad*
	    *pbes:aes256-cbc-pad*
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto mac hmac))
(define oid oid-string->der-object-identifier)
(define *pbes:hmac/sha1*       (oid *oid-hmac/sha1*))
(define *pbes:hmac/sha224*     (oid *oid-hmac/sha224*))
(define *pbes:hmac/sha256*     (oid *oid-hmac/sha256*))
(define *pbes:hmac/sha384*     (oid *oid-hmac/sha384*))
(define *pbes:hmac/sha512*     (oid *oid-hmac/sha512*))
(define *pbes:hmac/sha512/224* (oid *oid-hmac/sha512/224*))
(define *pbes:hmac/sha512/256* (oid *oid-hmac/sha512/256*))
;; Extra for SHA3, not defined in RFC8018, but soon?
(define *pbes:hmac/sha3-224*   (oid *oid-hmac/sha3-224*))
(define *pbes:hmac/sha3-256*   (oid *oid-hmac/sha3-256*))
(define *pbes:hmac/sha3-384*   (oid *oid-hmac/sha3-384*))
(define *pbes:hmac/sha3-512*   (oid *oid-hmac/sha3-512*))

(define *pbes:pbkdf2*  (oid "1.2.840.113549.1.5.12"))

;; PBKDF2-params ::= SEQUENCE {
;;     salt CHOICE {
;;       specified OCTET STRING,
;;       otherSource AlgorithmIdentifier {{PBKDF2-SaltSources}}
;;     },
;;     iterationCount INTEGER (1..MAX),
;;     keyLength INTEGER (1..MAX) OPTIONAL,
;;     prf AlgorithmIdentifier {{PBKDF2-PRFs}} DEFAULT
;;     algid-hmacWithSHA1
;; }
(define-asn1-encodable <pbkdf2-salt-choice>
  (asn1-choice
   ((specified :type <der-octet-string>)
    (other-source :type <algorithm-identifier>))
   :reader pbkdf2-salt-choice-value))
(define (pbkdf2-salt-choice? o) (is-a? o <pbkdf2-salt-choice>))
(define-asn1-encodable <pbkdf2-params>
  (asn1-sequence
   ((salt :type <pbkdf2-salt-choice> :reader pbkdf2-params-salt)
    (iteration-count :type <der-integer> :reader pbkdf2-params-iteration-count)
    (key-length :type <der-integer> :optional #t
		:reader pbkdf2-params-key-length)
    (prf :type <algorithm-identifier> :optional #t :reader pbkdf2-params-prf))))
(define (pbkdf2-params? o) (is-a? o <pbkdf2-params>))

;; PBES1
(define *pbes:pbe/md2-des-cbc*  (oid "1.2.840.113549.1.5.1"))
(define *pbes:pbe/md2-rc2-cbc*  (oid "1.2.840.113549.1.5.4"))
(define *pbes:pbe/md5-des-cbc*  (oid "1.2.840.113549.1.5.3"))
(define *pbes:pbe/md5-rc2-cbc*  (oid "1.2.840.113549.1.5.6"))
(define *pbes:pbe/sha1-des-cbc* (oid "1.2.840.113549.1.5.10"))
(define *pbes:pbe/sha1-rc2-cbc* (oid "1.2.840.113549.1.5.11"))

;; pbeparameter ::= sequence {
;;     salt octet string (size(8)),
;;     iterationcount integer
;; }
(define-asn1-encodable <pbe-parameter>
  (asn1-sequence
   ((salt :type <der-octet-string> :reader pbe-parameter-salt)
    (iteration-count :type <der-integer>
		     :reader pbe-parameter-iteration-count))))
(define (pbe-parameter? o) (is-a? o <pbe-parameter>))

;; PBES2
(define *pbes:pbes2* (oid "1.2.840.113549.1.5.13"))
;; PBES2-params ::= SEQUENCE {
;;    keyDerivationFunc AlgorithmIdentifier {{PBES2-KDFs}},
;;    encryptionScheme AlgorithmIdentifier {{PBES2-Encs}}
;; }
(define-asn1-encodable <pbes2-params>
  (asn1-sequence
   ((key-derivation-func :type <algorithm-identifier>
			 :reader pbes2-params-key-derivation-func)
    (encryption-scheme :type <algorithm-identifier>
		       :reader pbes2-params-encryption-scheme))))
(define (pbes2-params? o) (is-a? o <pbes2-params>))

;; PBMAC1
(define *pbes:pbmac1* (oid "1.2.840.113549.1.5.14"))
;; PBMAC1-params ::=  SEQUENCE {
;;     keyDerivationFunc AlgorithmIdentifier {{PBMAC1-KDFs}},
;;     messageAuthScheme AlgorithmIdentifier {{PBMAC1-MACs}}
;; }
(define-asn1-encodable <pbmac1-params>
  (asn1-sequence
   ((key-derivation-func :type <algorithm-identifier>
			 :reader pbmac1-params-key-derivation-func)
    (message-auth-scheme :type <algorithm-identifier>
			 :reader pbmac1-params-message-auth-scheme))))
(define (pbmac1-params? o) (is-a? o <pbmac1-params>))

;; supporting techniques
(define *pbes:des-cbc*    (oid "1.3.14.3.2.7"))
(define *pbes:desede-cbc* (oid "1.2.840.113549.3.7"))
(define *pbes:rc2-cbc* (oid "1.2.840.113549.3.2"))

;; RC2-CBC-Parameter ::= SEQUENCE {
;;    rc2ParameterVersion INTEGER OPTIONAL,
;;    iv OCTET STRING (SIZE(8))
;; }
(define-asn1-encodable <rc2-cbc-parameter>
  (asn1-sequence
   ((version :type <der-integer> :optional #t :reader rc2-cbc-parameter-version)
    (iv :type <der-octet-string> :reader rc2-cbc-parameter-iv))))
(define (rc2-cbc-parameter? o) (is-a? o <rc2-cbc-parameter>))

(define *pbes:rc5-cbc-pad* (oid "1.2.840.113549.3.9"))

;; RC5-CBC-Parameters ::= SEQUENCE {
;;    version INTEGER {v1-0(16)} (v1-0),
;;    rounds INTEGER (8..127),
;;    blockSizeInBits INTEGER (64 | 128),
;;    iv OCTET STRING OPTIONAL
;; }
(define-asn1-encodable <rc5-cbc-parameter>
  (asn1-sequence
   ((version :type <der-integer> :reader rc5-cbc-parameter-version)
    (rounds :type <der-integer> :reader rc5-cbc-parameter-rounds)
    (block-size-in-bits :type <der-integer>
			:reader rc5-cbc-parameter-block-size-in-bits)
    (iv :type <der-octet-string> :optional #t :reader rc5-cbc-parameter-iv))))
(define (rc5-cbc-parameter? o) (is-a? o <rc5-cbc-parameter>))

;;(define *pbes:aes*            (oid "2.16.840.1.101.3.4.1"))
;; Maybe these shouldn't be here
(define *pbes:aes128-cbc-pad* (oid "2.16.840.1.101.3.4.1.2"))
(define *pbes:aes192-cbc-pad* (oid "2.16.840.1.101.3.4.1.22"))
(define *pbes:aes256-cbc-pad* (oid "2.16.840.1.101.3.4.1.42"))

)
