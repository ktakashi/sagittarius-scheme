;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures.scm - Signature signer / verifier
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
(library (sagittarius crypto signatures)
    (export signer? <signer> make-signer
	    signer-sign-message signer-init! signer-process! signer-sign!

	    verifier? <verifier> make-verifier
	    verifier-verify-signature verifier-init! verifier-process!
	    verifier-verify!
	    
	    *signature:rsa*
	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message

	    mgf-1 *oid:mgf1*
	    pkcs1-emsa-pss-encode pkcs1-emsa-pss-verify
	    pkcs1-emsa-v1.5-encode pkcs1-emsa-v1.5-verify

	    *signature:dsa*
	    *signature:ecdsa*
	    *signature:eddsa*
	    *signature:ed25519*
	    *signature:ed25519ctx*
	    *signature:ed25519ph*
	    *signature:ed448*
	    *signature:ed448ph*

	    oid->mgf
	    oid->signer-maker
	    oid->verifier-maker

	    *signature-algorithm:rsa-pkcs-v1.5-sha1*
	    *signature-algorithm:rsa-pkcs-v1.5-sha256*
	    *signature-algorithm:rsa-pkcs-v1.5-sha384*
	    *signature-algorithm:rsa-pkcs-v1.5-sha512*
	    *signature-algorithm:rsa-pkcs-v1.5-sha224*
	    *signature-algorithm:rsa-pkcs-v1.5-sha512/224*
	    *signature-algorithm:rsa-pkcs-v1.5-sha512/256*
	    *signature-algorithm:rsa-pkcs-v1.5-sha3-224*
	    *signature-algorithm:rsa-pkcs-v1.5-sha3-256*
	    *signature-algorithm:rsa-pkcs-v1.5-sha3-384*
	    *signature-algorithm:rsa-pkcs-v1.5-sha3-512*
	    *signature-algorithm:rsa-ssa-pss*
	    *signature-algorithm:dsa-sha224*
	    *signature-algorithm:dsa-sha256*
	    *signature-algorithm:dsa-sha384*
	    *signature-algorithm:dsa-sha512*
	    *signature-algorithm:ecdsa-sha1*
	    *signature-algorithm:ecdsa-sha224*
	    *signature-algorithm:ecdsa-sha256*
	    *signature-algorithm:ecdsa-sha384*
	    *signature-algorithm:ecdsa-sha512*
	    *signature-algorithm:ecdsa-sha3-224*
	    *signature-algorithm:ecdsa-sha3-256*
	    *signature-algorithm:ecdsa-sha3-384*
	    *signature-algorithm:ecdsa-sha3-512*
	    *signature-algorithm:ed25519*
	    *signature-algorithm:ed448*
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto signatures rsa)
	    (sagittarius crypto signatures dsa)
	    (sagittarius crypto signatures ecdsa)
	    (sagittarius crypto signatures eddsa)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius mop immutable))

(define-class <signature> (<immutable>) 
  ((state :init-keyword :state :reader signature-state)
   (processor :init-keyword :processor :reader signature-processor)))

(define-class <signer> (<signature>) ())
(define (signer? o) (is-a? o <signer>))
(define (make-signer scheme key . opts)
  (let ((state (apply make-signer-state scheme key opts)))
    (make <signer> :state state
	  :processor (signature-state-processor state))))

(define (signer-sign-message signer message)
  (signer-sign! (signer-process! (signer-init! signer) message)))

(define (signer-init! (signer signer?))
  (signature-state-init! (signature-state signer))
  signer)
(define (signer-process! (signer signer?) (msg bytevector?)
			 :optional (start 0) (end (bytevector-length msg)))
  ((signature-processor signer) (signature-state signer) msg start end)
  signer)
(define (signer-sign! (signer signer?))
  (signer-state->signature (signature-state signer)))

(define-class <verifier> (<signature>) ())
(define (verifier? o) (is-a? o <verifier>))

(define (make-verifier scheme key . opts)
  (let ((state (apply make-verifier-state scheme key opts)))
    (make <verifier> :state state
	  :processor (signature-state-processor state))))

(define (verifier-verify-signature verifier message signature)
  (verifier-verify! (verifier-process! (verifier-init! verifier) message)
		    signature))
(define (verifier-init! (verifier verifier?))
  (signature-state-init! (signature-state verifier))
  verifier)
(define (verifier-process! (verifier verifier?) (msg bytevector?)
			   :key (start 0) (end (bytevector-length msg)))
  ((signature-processor verifier) (signature-state verifier) msg start end)
  verifier)
(define (verifier-verify! (verifier verifier?) (signature bytevector?))
  (verifier-state-verify-message (signature-state verifier) signature))

;;; OID thing...
(define *oid:mgf1* "1.2.840.113549.1.1.8")

(define-generic oid->mgf)
(define-method oid->mgf ((oid (equal *oid:mgf1*))) mgf-1)

(define-generic oid->signer-maker)
(define-generic oid->verifier-maker)

(define-syntax define-oid->signer&verifier
  (syntax-rules ()
    ((_ name oid scheme opts ...)
     (begin
       (define name oid)
       ;; not sure if we want to have this though...
       (define-method oid->key-operation ((m (equal oid))) scheme)
       (define-method oid->signer-maker ((m (equal oid)))
	 (lambda (key . rest)
	   (apply make-signer scheme key opts ... rest)))
       (define-method oid->verifier-maker ((m (equal oid)))
	 (lambda (key . rest)
	   (apply make-verifier scheme key opts ... rest)))))))

(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha1*
  "1.2.840.113549.1.1.5" *signature:rsa*
  :digest *digest:sha-1*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha256*
  "1.2.840.113549.1.1.11" *signature:rsa*
  :digest *digest:sha-256*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha384*
  "1.2.840.113549.1.1.12" *signature:rsa*
  :digest *digest:sha-384*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha512*
  "1.2.840.113549.1.1.13" *signature:rsa*
  :digest *digest:sha-512*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha224*
  "1.2.840.113549.1.1.14" *signature:rsa*
  :digest *digest:sha-224*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha512/224*
  "1.2.840.113549.1.1.15" *signature:rsa*
  :digest *digest:sha-512/224*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha512/256*
  "1.2.840.113549.1.1.16" *signature:rsa*
  :digest *digest:sha-512/256*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha3-224*
  "2.16.840.1.101.3.4.3.13" *signature:rsa*
  :digest *digest:sha3-224*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha3-256*
  "2.16.840.1.101.3.4.3.14" *signature:rsa*
  :digest *digest:sha3-256*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha3-384*
  "2.16.840.1.101.3.4.3.15" *signature:rsa*
  :digest *digest:sha3-384*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)
(define-oid->signer&verifier *signature-algorithm:rsa-pkcs-v1.5-sha3-512*
  "2.16.840.1.101.3.4.3.16" *signature:rsa*
  :digest *digest:sha3-512*
  :encoder pkcs1-emsa-v1.5-encode :verifier pkcs1-emsa-v1.5-verify)

;; RSA SSA-PSS
(define-oid->signer&verifier *signature-algorithm:rsa-ssa-pss*
  "1.2.840.113549.1.1.10" *signature:rsa*
  :encoder pkcs1-emsa-pss-encode :verifier pkcs1-emsa-pss-verify)
  

(define-oid->signer&verifier *signature-algorithm:dsa-sha224*
  "2.16.840.1.101.3.4.3.1" *signature:dsa*
  :digest *digest:sha-224*)
(define-oid->signer&verifier *signature-algorithm:dsa-sha256*
  "2.16.840.1.101.3.4.3.2" *signature:dsa*
  :digest *digest:sha-256*)
(define-oid->signer&verifier *signature-algorithm:dsa-sha384*
  "2.16.840.1.101.3.4.3.3" *signature:dsa*
  :digest *digest:sha-384*)
(define-oid->signer&verifier *signature-algorithm:dsa-sha512*
  "2.16.840.1.101.3.4.3.4" *signature:dsa*
  :digest *digest:sha-512*)

(define-oid->signer&verifier *signature-algorithm:ecdsa-sha1*
  "1.2.840.10045.4.1" *signature:ecdsa*
  :digest *digest:sha-1*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha224*
  "1.2.840.10045.4.3.1" *signature:ecdsa*
  :digest *digest:sha-224*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha256*
  "1.2.840.10045.4.3.2" *signature:ecdsa*
  :digest *digest:sha-256*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha384*
  "1.2.840.10045.4.3.3" *signature:ecdsa*
  :digest *digest:sha-384*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha512*
  "1.2.840.10045.4.3.4" *signature:ecdsa*
  :digest *digest:sha-512*)
;; these seems draft OID
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha3-224*
  "2.16.840.1.101.3.4.3.9" *signature:ecdsa*
  :digest *digest:sha3-224*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha3-256*
  "2.16.840.1.101.3.4.3.10" *signature:ecdsa*
  :digest *digest:sha3-256*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha3-384*
  "2.16.840.1.101.3.4.3.11" *signature:ecdsa*
  :digest *digest:sha3-384*)
(define-oid->signer&verifier *signature-algorithm:ecdsa-sha3-512*
  "2.16.840.1.101.3.4.3.12" *signature:ecdsa*
  :digest *digest:sha3-512*)

(define-oid->signer&verifier *signature-algorithm:ed25519*
  "1.3.101.112" *signature:ed25519*)
(define-oid->signer&verifier *signature-algorithm:ed448*
  "1.3.101.113" *signature:ed448*)
;; Seems not defined?
;; (define-oid->signer&verifier "1.3.101.114" *signature:ed25519ph*)
;; (define-oid->signer&verifier "1.3.101.115" *signature:ed448ph*)
)
