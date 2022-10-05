;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/ciphers/asymmeteric/rsa.scm - RSA cipher
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
(library (sagittarius crypto ciphers asymmetric rsa)
    (export *scheme:rsa*
	    generate-key-pair
	    generate-public-key
	    generate-private-key
	    import-private-key
	    import-public-key
	    export-private-key
	    export-public-key
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto descriptors cipher)
	    (sagittarius crypto ciphers asymmetric state)
	    (sagittarius crypto keys operations asymmetric rsa)
	    (sagittarius crypto math modular))

(define (rsa-setup key . ignore) 
  (make-asymmetric-state key)) ;; use as it is, Elgamal may need random

(define (copy-if-needed bv s)
  (if (zero? s) bv (bytevector-copy bv s)))
(define (rsa-encrypt key pt ps)
  (rsa-mod-expt (asymmetric-state-key key)
		(copy-if-needed pt ps) (rsa-block-size key)))
(define (rsa-decrypt key ct cs)
  (rsa-mod-expt (asymmetric-state-key key)
		(copy-if-needed ct cs) (rsa-block-size key)))
;; Should we destroy the key? or at least asymmetric-state
(define (rsa-done key) #f)

(define (rsa-mod-expt key bv block-size)
  (let ((chunk (bytevector->uinteger bv)))
    (cond ((rsa-public-key? key)
	   (let ((r (mod-expt chunk
			      (rsa-public-key-exponent key)
			      (rsa-public-key-modulus key))))
	     (uinteger->bytevector r block-size)))
	  ((rsa-crt-private-key? key)
	   (let ((p (rsa-crt-private-key-p key))
		 (q (rsa-crt-private-key-q key))
		 (dP (rsa-crt-private-key-dP key))
		 (dQ (rsa-crt-private-key-dQ key))
		 (qP (rsa-crt-private-key-qP key)))
	     (let* ((a (mod-expt chunk dP p))  ; b ^ dP mod p
		    (b (mod-expt chunk dQ q))  ; b ^ dQ mod q
		    (c (mod (* (- a b) qP) p)) ; (a - b) * qP (mod p)
		    (d (+ b (* q c))))	       ; b + q * c
	       (uinteger->bytevector d block-size))))
	  ((rsa-private-key? key)
	   (let ((a (mod-expt chunk
			      (rsa-private-key-private-exponent key)
			      (rsa-private-key-modulus key))))
	     (uinteger->bytevector a (endianness big) block-size)))
	  (else
	   (assertion-violation 'rsa-mod-expt "Unknown key" key)))))

(define (rsa-block-size state)
  (define key (asymmetric-state-key state))
  (let ((modulus (if (rsa-public-key? key)
		     (rsa-public-key-modulus key)
		     (rsa-private-key-modulus key))))
    (div (+ (bitwise-length modulus) 7) 8)))

(define *scheme:rsa*
  (make-asymmetric-cipher-descriptor
   "RSA" rsa-block-size rsa-setup rsa-encrypt rsa-decrypt rsa-done))

(define-method generate-key-pair ((m (eq *scheme:rsa*)) . opts)
  (apply generate-key-pair *key:rsa* opts))
(define-method generate-public-key ((m (eq *scheme:rsa*)) . opts)
  (apply generate-public-key *key:rsa* opts))
(define-method generate-private-key ((m (eq *scheme:rsa*)) . opts)
  (apply generate-private-key *key:rsa* opts))
(define-method import-private-key ((m (eq *scheme:rsa*)) . opts)
  (apply import-private-key *key:rsa* opts))
(define-method import-public-key ((m (eq *scheme:rsa*)) . opts)
  (apply import-public-key *key:rsa* opts))
(define-method export-private-key ((m (eq *scheme:rsa*)) . opts)
  (apply export-private-key *key:rsa* opts))
(define-method export-public-key ((m (eq *scheme:rsa*)) . opts)
  (apply export-public-key *key:rsa* opts))
)
