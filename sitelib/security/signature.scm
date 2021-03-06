;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/signature.scm - Cryptographic Signature
;;;
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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
(library (security signature)
    (export algorithm-identifier->signature-verifier-provider
	    algorithm-identifier->signature-signer-provider

	    ;; named verifiers
	    *rsa/sha1-verifier-provider*
	    *rsa/sha256-verifier-provider*
	    *rsa/sha384-verifier-provider*
	    *rsa/sha512-verifier-provider*
	    *rsa/sha224-verifier-provider*
	    *rsa/sha512/224-verifier-provider*
	    *rsa/sha512/256-verifier-provider*

	    *rsassa-pss-verifier-provider*

	    *ecdsa/sha1-verifier-provider*
	    *ecdsa/sha224-verifier-provider*
	    *ecdsa/sha256-verifier-provider*
	    *ecdsa/sha384-verifier-provider*
	    *ecdsa/sha512-verifier-provider*

	    ;; named signers
	    *rsa/sha1-signer-provider*
	    *rsa/sha256-signer-provider*
	    *rsa/sha384-signer-provider*
	    *rsa/sha512-signer-provider*
	    *rsa/sha224-signer-provider*
	    *rsa/sha512/224-signer-provider*
	    *rsa/sha512/256-signer-provider*

	    *rsassa-pss-signer-provider*

	    *ecdsa/sha1-signer-provider*
	    *ecdsa/sha224-signer-provider*
	    *ecdsa/sha256-signer-provider*
	    *ecdsa/sha384-signer-provider*
	    *ecdsa/sha512-signer-provider*
	    )
    (import (rnrs)
	    (clos core)
	    (crypto)
	    (math)
	    (asn.1)
	    (rsa pkcs :10))

(define *rsassa-pss-oid* "1.2.840.113549.1.1.10")
(define (algorithm-identifier->signature-verifier-provider aid)
  (define oid (algorithm-identifier-id aid))
  (cond ((string=? oid *rsassa-pss-oid*)
	 (let-values (((algo mgf salt-len) (parse-rsassa-pss-parameter aid)))
	   ;; wrap it
	   (lambda (public-key . parameters)
	     (apply *rsassa-pss-verifier-provider* public-key
		    :digest algo
		    :mgf mgf
		    :salt-length salt-len
		    parameters))))
	((assp (lambda (known-oid) (string=? oid known-oid))
	       *provider-oid-map*) => cadr)
	(else
	 (assertion-violation 'algorithm-identifier->signature-verifier-provider
			      "Not supported OID" oid))))

(define (algorithm-identifier->signature-signer-provider aid)
  (define oid (algorithm-identifier-id aid))
  (cond ((string=? oid *rsassa-pss-oid*)
	 (let-values (((algo mgf salt-len) (parse-rsassa-pss-parameter aid)))
	   ;; wrap it
	   (lambda (private-key . parameters)
	     (apply *rsassa-pss-signer-provider* private-key
		    :digest algo
		    :mgf mgf
		    :salt-length salt-len
		    parameters))))
	((assp (lambda (known-oid) (string=? oid known-oid))
	       *provider-oid-map*) => caddr)
	(else
	 (assertion-violation 'algorithm-identifier->signature-signer-provider
			      "Not supported OID" oid))))

#|
      RSASSA-PSS-params  ::=  SEQUENCE  {
         hashAlgorithm      [0] HashAlgorithm DEFAULT
                                   sha1Identifier,
         maskGenAlgorithm   [1] MaskGenAlgorithm DEFAULT
                                   mgf1SHA1Identifier,
         saltLength         [2] INTEGER DEFAULT 20,
         trailerField       [3] INTEGER DEFAULT 1  }
|#

(define (parse-rsassa-pss-parameter aid)
  (define param (algorithm-identifier-parameters aid))
  (define (find-tag param tag)
    (define (check-tag o)
      (and (is-a? o <asn.1-tagged-object>)
	   (equal? tag (slot-ref o 'tag-no))))
    (do ((len (asn.1-sequence-size param))
	 (i 0 (+ i 1)))
	((or (= i len) (check-tag (asn.1-sequence-get param i)))
	 (and (not (= i len))
	      (slot-ref (asn.1-sequence-get param i) 'obj)))))
  (unless (is-a? param <asn.1-sequence>)
    (assertion-violation 'parse-rsassa-pss-parameter
			 "Invalid rsassa-pss parameter" aid))
  (let ((hash-func-aid (make-algorithm-identifier (find-tag param 0)))
	(mask-gen-func-aid (make-algorithm-identifier (find-tag param 1)))
	(salt-len (find-tag param 2)))
    (unless (string=? (algorithm-identifier-id mask-gen-func-aid)
		      "1.2.840.113549.1.1.8")
      (assertion-violation 'parse-rsassa-pss-parameter
			   "Unknown MGF OID" mask-gen-func-aid))
    (values (oid->hash-algorithm (algorithm-identifier-id hash-func-aid))
	    mgf-1
	    (der-integer->integer salt-len))))

;;; verifier providers
(define (make-rsa-verifier-provider digest verify)
  (lambda (public-key . parameter)
    (define cipher (make-cipher RSA public-key))
    (define algo (hash-algorithm digest))
    (lambda (message signnature)
      (apply cipher-verify cipher message signnature
	     :hash algo :verify verify parameter))))

(define *rsa/sha1-verifier-provider*
  (make-rsa-verifier-provider SHA-1 pkcs1-emsa-v1.5-verify))
(define *rsa/sha256-verifier-provider*
  (make-rsa-verifier-provider SHA-256 pkcs1-emsa-v1.5-verify))
(define *rsa/sha384-verifier-provider*
  (make-rsa-verifier-provider SHA-384 pkcs1-emsa-v1.5-verify))
(define *rsa/sha512-verifier-provider*
  (make-rsa-verifier-provider SHA-512 pkcs1-emsa-v1.5-verify))
(define *rsa/sha224-verifier-provider*
  (make-rsa-verifier-provider SHA-224 pkcs1-emsa-v1.5-verify))
(define *rsa/sha512/224-verifier-provider*
  (make-rsa-verifier-provider SHA-512/224 pkcs1-emsa-v1.5-verify))
(define *rsa/sha512/256-verifier-provider*
  (make-rsa-verifier-provider SHA-512/256 pkcs1-emsa-v1.5-verify))

(define (*rsassa-pss-verifier-provider* public-key
					:key (digest SHA-1)
					:allow-other-keys opts)
    (define cipher (make-cipher RSA public-key))
    (define algo (hash-algorithm digest))
    (lambda (message signnature)
      (apply cipher-verify cipher message signnature
	     :hash algo :verify pkcs1-emsa-pss-verify opts)))

(define (make-ecdsa-verifier-provider digest)
  (lambda (public-key . opts)
    (define cipher (make-cipher ECDSA public-key))
    (lambda (message signature)
      (apply cipher-verify cipher message signature :hash digest opts))))

(define *ecdsa/sha1-verifier-provider* (make-ecdsa-verifier-provider SHA-1))
(define *ecdsa/sha224-verifier-provider* (make-ecdsa-verifier-provider SHA-224))
(define *ecdsa/sha256-verifier-provider* (make-ecdsa-verifier-provider SHA-256))
(define *ecdsa/sha384-verifier-provider* (make-ecdsa-verifier-provider SHA-384))
(define *ecdsa/sha512-verifier-provider* (make-ecdsa-verifier-provider SHA-512))

;; signer providers
(define (make-rsa-signer-provider digest encode)
  (lambda (private-key . parameter)
    (define cipher (make-cipher RSA private-key))
    (define algo (hash-algorithm digest))
    (lambda (message)
      (apply cipher-signature cipher message
	     :hash algo :encode encode parameter))))
(define *rsa/sha1-signer-provider*
  (make-rsa-signer-provider SHA-1 pkcs1-emsa-v1.5-encode))
(define *rsa/sha256-signer-provider*
  (make-rsa-signer-provider SHA-256 pkcs1-emsa-v1.5-encode))
(define *rsa/sha384-signer-provider*
  (make-rsa-signer-provider SHA-384 pkcs1-emsa-v1.5-encode))
(define *rsa/sha512-signer-provider*
  (make-rsa-signer-provider SHA-512 pkcs1-emsa-v1.5-encode))
(define *rsa/sha224-signer-provider*
  (make-rsa-signer-provider SHA-224 pkcs1-emsa-v1.5-encode))
(define *rsa/sha512/224-signer-provider*
  (make-rsa-signer-provider SHA-512/224 pkcs1-emsa-v1.5-encode))
(define *rsa/sha512/256-signer-provider*
  (make-rsa-signer-provider SHA-512/256 pkcs1-emsa-v1.5-encode))

(define (*rsassa-pss-signer-provider* private-key
				      :key (digest SHA-1)
				      :allow-other-keys opts)
    (define cipher (make-cipher RSA private-key))
    (define algo (hash-algorithm digest))
    (lambda (message)
      (apply cipher-signature cipher message
	     :hash algo :encode pkcs1-emsa-pss-encode opts)))

(define (make-ecdsa-signer-provider digest)
  (lambda (private-key . opts)
    (define cipher (make-cipher ECDSA private-key))
    (lambda (message)
      (apply cipher-signature cipher message :hash digest opts))))

(define *ecdsa/sha1-signer-provider* (make-ecdsa-signer-provider SHA-1))
(define *ecdsa/sha224-signer-provider* (make-ecdsa-signer-provider SHA-224))
(define *ecdsa/sha256-signer-provider* (make-ecdsa-signer-provider SHA-256))
(define *ecdsa/sha384-signer-provider* (make-ecdsa-signer-provider SHA-384))
(define *ecdsa/sha512-signer-provider* (make-ecdsa-signer-provider SHA-512))

(define *provider-oid-map*
  `(;; RSA PKCS v1.5
    ("1.2.840.113549.1.1.5"  ,*rsa/sha1-verifier-provider*
			     ,*rsa/sha1-signer-provider*)
    ("1.2.840.113549.1.1.11" ,*rsa/sha256-verifier-provider*
			     ,*rsa/sha256-signer-provider*)
    ("1.2.840.113549.1.1.12" ,*rsa/sha384-verifier-provider*
			     ,*rsa/sha384-signer-provider*)
    ("1.2.840.113549.1.1.13" ,*rsa/sha512-verifier-provider*
			     ,*rsa/sha512-signer-provider*)
    ("1.2.840.113549.1.1.14" ,*rsa/sha224-verifier-provider*
			     ,*rsa/sha224-signer-provider*)
    ("1.2.840.113549.1.1.15" ,*rsa/sha512/224-verifier-provider*
			     ,*rsa/sha512/224-signer-provider*)
    ("1.2.840.113549.1.1.16" ,*rsa/sha512/256-verifier-provider*
			     ,*rsa/sha512/256-signer-provider*)
    ;; RSA PSSSSA-PSS
    (,*rsassa-pss-oid*       ,*rsassa-pss-verifier-provider*
			     ,*rsassa-pss-signer-provider*)
    ;; DSA
    ;; ECDSA
    ("1.2.840.10045.4.1"     ,*ecdsa/sha1-verifier-provider*
			     ,*ecdsa/sha1-signer-provider*)
    ("1.2.840.10045.4.3.1"   ,*ecdsa/sha224-verifier-provider*
			     ,*ecdsa/sha224-signer-provider*)
    ("1.2.840.10045.4.3.2"   ,*ecdsa/sha256-verifier-provider*
			     ,*ecdsa/sha256-signer-provider*)
    ("1.2.840.10045.4.3.3"   ,*ecdsa/sha384-verifier-provider*
			     ,*ecdsa/sha384-signer-provider*)
    ("1.2.840.10045.4.3.4"   ,*ecdsa/sha512-verifier-provider*
			     ,*ecdsa/sha512-signer-provider*)
    ))

)
