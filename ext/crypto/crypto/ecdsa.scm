;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/key/ecdsa.scm - ECDSA cipher and so
;;;
;;;  Copyright (c) 2017 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; references:
;;  - https://tools.ietf.org/html/rfc5915
;;  - https://tools.ietf.org/html/rfc5480
;;  - https://tools.ietf.org/html/rfc6090
#!nounbound
(library (crypto ecdsa)
    (export ECDSA
	    <ecdsa-private-key> ecdsa-private-key?
	    ecdsa-private-key-d
	    (rename (ecdsa-key-parameter ecdsa-private-key-parameter))
	    ecdsa-private-key-public-key
	    <ecdsa-public-key> ecdsa-public-key?
	    ecdsa-public-key-Q 
	    (rename (ecdsa-key-parameter ecdsa-public-key-parameter))

	    ;; NIST parameters
	    NIST-P-192
	    NIST-P-224
	    NIST-P-256
	    NIST-P-384
	    NIST-P-521
	              
	    NIST-K-163
	    NIST-K-233
	    NIST-K-283
	    NIST-K-409
	    NIST-K-571
	              
	    NIST-B-163
	    NIST-B-233
	    NIST-B-283
	    NIST-B-409
	    NIST-B-571

	    ;; SEC 2 parameters
	    sect113r1
	    
	    secp192r1 ;; the same as NIST-P-*
	    secp224r1
	    secp256r1
	    secp384r1
	    secp521r1
	             
	    sect163k1 ;; the same as NIST-K-*
	    sect233k1
	    sect283k1
	    sect409k1
	    sect571k1
	             
	    sect163r2 ;; the same as NIST-B-*
	    sect233r1
	    sect283r1
	    sect409r1
	    sect571r1

	    curve25519
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (crypto spi)
	    (math hash)
	    (math ec) ;; lazy...
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (sagittarius crypto keys))

(define ECDSA *signature:ecdsa*)
(define-class <ecdsa-cipher-spi> (<legacy-cipher-spi>) ())
(define-method initialize ((o <ecdsa-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'ECDSA)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in EcDSA")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in EcDSA")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer ecdsa-sign)
    (slot-set! o 'verifier ecdsa-verify)
    (slot-set! o 'keysize #f)))
(register-spi ECDSA <ecdsa-cipher-spi>)

(define (get-digest hash)
  (if (hash-algorithm? hash)
      (hash-algorithm->digest-descriptor hash)
      hash))
(define (ecdsa-sign M key :key k-generator der-encode (hash *digest:sha-1*))
  (let ((signer (make-signer ECDSA key
			     :k-generator k-generator
			     :der-encode der-encode
			     :digest (get-digest hash))))
    (signer-sign-message signer M)))
(define (ecdsa-verify M S key :key der-encode (hash *digest:sha-1*))
  (let ((verifier (make-verifier ECDSA key
				 :der-encode der-encode
				 :digest (get-digest hash))))
    (verifier-verify-signature verifier M S)))
)
