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

	    *eddsa-verifier-provider*

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

	    *eddsa-signer-provider*
	    )
    (import (rnrs)
	    (sagittarius crypto signatures)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto pkix algorithms)
	    (sagittarius crypto pkix signatures))

(define (algorithm-identifier->signature-verifier-provider aid)
  (define x509-aid (algorithm-identifier->x509-algorithm-identifier aid))
  (lambda (public-key . parameter)
    (apply x509-algorithm-identifier->verifier x509-aid public-key parameter)))

(define (algorithm-identifier->signature-signer-provider aid)
  (define x509-aid (algorithm-identifier->x509-algorithm-identifier aid))
  (lambda (private-key . parameter)
    (apply x509-algorithm-identifier->signer x509-aid private-key parameter)))

;;; verifier providers
(define (oid->verifier-provider oid)
  (define maker (oid->verifier-maker oid))
  (lambda (public-key . parameter)
    (let ((verifier (apply maker public-key parameter)))
      (lambda (message signnature)
	(verifier-verify-signature verifier message signnature)))))

(define *rsa/sha1-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha1*))
(define *rsa/sha256-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha256*))
(define *rsa/sha384-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha384*))
(define *rsa/sha512-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha512*))
(define *rsa/sha224-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha224*))
(define *rsa/sha512/224-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha512/224*))
(define *rsa/sha512/256-verifier-provider*
  (oid->verifier-provider *signature-algorithm:rsa-pkcs-v1.5-sha512/256*))

(define rsassa-pss-verifier-maker
  (oid->verifier-maker *signature-algorithm:rsa-ssa-pss*))
(define (*rsassa-pss-verifier-provider* public-key
					:key (digest *digest:sha-1*)
					:allow-other-keys opts)
  (define verifier
    (apply rsassa-pss-verifier-maker public-key :digest digest opts))
  (lambda (message signnature)
    (verifier-verify-signature verifier message signnature)))

(define *ecdsa/sha1-verifier-provider*
  (oid->verifier-provider *signature-algorithm:ecdsa-sha1*))
(define *ecdsa/sha224-verifier-provider*
  (oid->verifier-provider *signature-algorithm:ecdsa-sha224*))
(define *ecdsa/sha256-verifier-provider*
  (oid->verifier-provider *signature-algorithm:ecdsa-sha256*))
(define *ecdsa/sha384-verifier-provider*
  (oid->verifier-provider *signature-algorithm:ecdsa-sha384*))
(define *ecdsa/sha512-verifier-provider*
  (oid->verifier-provider *signature-algorithm:ecdsa-sha512*))

(define (*eddsa-verifier-provider* public-key . opts)
  (define verifier (apply make-verifier
			  (if (ed25519-key? public-key)
			      *signature:ed25519*
			      *signature:ed448*) public-key opts))
  (lambda (message signature)
    (verifier-verify-signature verifier message signature)))

;; signer providers
(define (oid->signer-provider oid)
  (define maker (oid->signer-maker oid))
  (lambda (private-key . parameter)
    (define signer (apply maker private-key parameter))
    (lambda (message) (signer-sign-message signer message))))

(define *rsa/sha1-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha1*))
(define *rsa/sha256-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha256*))
(define *rsa/sha384-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha384*))
(define *rsa/sha512-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha512*))
(define *rsa/sha224-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha224*))
(define *rsa/sha512/224-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha512/224*))
(define *rsa/sha512/256-signer-provider*
  (oid->signer-provider *signature-algorithm:rsa-pkcs-v1.5-sha512/256*))

(define rsassa-pss-signer-maker
  (oid->signer-maker *signature-algorithm:rsa-ssa-pss*))
(define (*rsassa-pss-signer-provider* private-key
				      :key (digest *digest:sha-1*)
				      :allow-other-keys opts)
  (define signer
    (apply rsassa-pss-signer-maker private-key :digest digest opts))
  (lambda (message) (signer-sign-message signer message)))

(define *ecdsa/sha1-signer-provider*
  (oid->signer-provider *signature-algorithm:ecdsa-sha1*))
(define *ecdsa/sha224-signer-provider*
  (oid->signer-provider *signature-algorithm:ecdsa-sha224*))
(define *ecdsa/sha256-signer-provider*
  (oid->signer-provider *signature-algorithm:ecdsa-sha256*))
(define *ecdsa/sha384-signer-provider*
  (oid->signer-provider *signature-algorithm:ecdsa-sha384*))
(define *ecdsa/sha512-signer-provider*
  (oid->signer-provider *signature-algorithm:ecdsa-sha512*))

(define (*eddsa-signer-provider* private-key . opts)
  (define signer (apply make-signer
			(if (ed25519-key? private-key)
			    *signature:ed25519*
			    *signature:ed448*)
			private-key opts))
  (lambda (message) (signer-sign-message signer message)))
)
