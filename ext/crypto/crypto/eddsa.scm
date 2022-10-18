;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/eddsa.scm - EdDSA cipher
;;;
;;;  Copyright (c) 2021 Takashi Kato. All rights reserved.
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
;;  - [RFC 8032](https://datatracker.ietf.org/doc/html/rfc8032)
#!deprecated
#!nounbound
#!read-macro=sagittarius/bv-string
(library (crypto eddsa)
    (export EdDSA
	    Ed25519 Ed25519ctx Ed25519ph
	    Ed448 Ed448ph

	    <eddsa-key> eddsa-key?
	    eddsa-key-parameter
	    <eddsa-private-key> eddsa-private-key?
	    eddsa-private-key-random eddsa-private-key-public-key
	    <eddsa-public-key> eddsa-public-key?
	    eddsa-public-key-data

	    ed25519-key? ed25519-public-key? ed25519-private-key?
	    ed448-key? ed448-public-key? ed448-private-key?

	    make-eddsa-signer make-eddsa-verifier
	    eddsa-scheme?
	    ed25519-scheme
	    ed25519ctx-scheme
	    ed25519ph-scheme
	    ed448-scheme
	    ed448ph-scheme
	    )
    (import (rnrs)
	    (clos user)
	    (crypto spi)
	    (sagittarius crypto math ed)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures)
	    (sagittarius crypto signatures eddsa))
;;; Interfaces
(define EdDSA *key:eddsa*)
(define Ed25519 *signature:ed25519*)
(define Ed25519ctx *signature:ed25519ctx*)
(define Ed25519ph *signature:ed25519ph*)
(define Ed448 *signature:ed448*)
(define Ed448ph *signature:ed448ph*)

(define (make-eddsa-signer scheme)
  (lambda (M key :key (context #f))
    (let ((s (make-signer EdDSA key :scheme scheme
			  :context (or context #vu8()))))
      (signer-sign-message s M))))
(define (make-eddsa-verifier scheme)
  (lambda (M S key :key (context #vu8()))
    (let ((v (make-verifier EdDSA key :scheme scheme
			    :context (or context #vu8()))))
      (verifier-verify-signature v M S))))

(define-class <eddsa-cipher-spi> (<legacy-cipher-spi>) ())
(define-method initialize ((o <eddsa-cipher-spi>) initargs)
  (define (check-ed25519 key)
    (unless (ed25519-key? key)
      (assertion-violation 'eddsa-cipher "Wrong type for the key")))
  (define (check-ed448 key)
    (unless (ed448-key? key)
      (assertion-violation 'eddsa-cipher "Wrong type for the key")))
  (define (type->scheme key :key (type #f) :allow-other-keys)
    (cond ((eq? type Ed25519)    (check-ed25519 key) ed25519-scheme)
	  ((eq? type Ed25519ctx) (check-ed25519 key) ed25519ctx-scheme)
	  ((eq? type Ed25519ph)  (check-ed25519 key) ed25519ph-scheme)
	  ((eq? type Ed448)   (check-ed448 key) ed448-scheme)
	  ((eq? type Ed448ph) (check-ed448 key) ed448ph-scheme)
	  (else (if (ed25519-key? key) ed25519-scheme ed448-scheme))))
  (let* ((key (car initargs))
	 (scheme (apply type->scheme key (cdr initargs))))
      (slot-set! o 'name 'EdDSA)
      (slot-set! o 'key key)
      (slot-set! o 'encrypt
		 (lambda ignore (error 'encrypt "not supported in EdDSA")))
      (slot-set! o 'decrypt
		 (lambda ignore (error 'decrypt "not supported in EdDSA")))
      (slot-set! o 'padder #f)
      (slot-set! o 'signer (make-eddsa-signer scheme))
      (slot-set! o 'verifier (make-eddsa-verifier scheme))
      (slot-set! o 'keysize #f)))
(register-spi EdDSA <eddsa-cipher-spi>)

(define-class <ed25519-cipher-spi> (<eddsa-cipher-spi>) ())
(define-method initialize ((o <ed25519-cipher-spi>) initargs)
  (call-next-method o (list (car initargs) :type Ed25519)))
(register-spi Ed25519 <ed25519-cipher-spi>)

(define-class <ed25519ctx-cipher-spi> (<eddsa-cipher-spi>) ())
(define-method initialize ((o <ed25519ctx-cipher-spi>) initargs)
  (call-next-method o (list (car initargs) :type Ed25519ctx)))
(register-spi Ed25519ctx <ed25519ctx-cipher-spi>)

(define-class <ed25519ph-cipher-spi> (<eddsa-cipher-spi>) ())
(define-method initialize ((o <ed25519ph-cipher-spi>) initargs)
  (call-next-method o (list (car initargs) :type Ed25519ph)))
(register-spi Ed25519ph <ed25519ph-cipher-spi>)

(define-class <ed448-cipher-spi> (<eddsa-cipher-spi>) ())
(define-method initialize ((o <ed448-cipher-spi>) initargs)
  (call-next-method o (list (car initargs) :type Ed448)))
(register-spi Ed448 <ed448-cipher-spi>)

(define-class <ed448ph-cipher-spi> (<eddsa-cipher-spi>) ())
(define-method initialize ((o <ed448ph-cipher-spi>) initargs)
  (call-next-method o (list (car initargs) :type Ed448ph)))
(register-spi Ed448ph <ed448ph-cipher-spi>)
)
