;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/asymmetric/rsa.scm - RSA key op
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
(library (sagittarius crypto keys operations asymmetric rsa)
    (export generate-key-pair
	    generate-public-key
	    generate-private-key
	    *key:rsa*
	    )
    (import (rnrs)
	    (clos user)
	    (math modular)
	    (srfi :39 parameters)
	    (sagittarius mop immutable)
	    (sagittarius crypto keys types)
	    (sagittarius crypto keys operations asymmetric apis)
	    (sagittarius crypto random)
	    (sagittarius crypto math prime))

(define *key:rsa* :rsa)
(define *rsa-min-keysize* (make-parameter 1024))

(define-class <rsa-public-key> (<public-key> <immutable>)
  ((modulus :init-keyword :modulus)
   (exponent :init-keyword :exponent)))
(define (rsa-public-key? o) (is-a? o <rsa-private-key>))
(define (make-rsa-public-key m e)
  (unless (integer? m)
    (assertion-violation 'generate-public-key
			 "Public modulus must be an integer" m))
  (unless (probable-prime? e)
    (assertion-violation 'generate-public-key
			 "Public exponent must be a prime number" e))
  (make <rsa-public-key> :modulus m :exponent e))

(define-class <rsa-private-key> (<private-key> <immutable>)
  ((modulus :init-keyword :modulus)
   (private-exponent :init-keyword :private-exponent)))
(define (rsa-private-key? o) (is-a? o <rsa-private-key>))
(define (make-rsa-private-key m d)
  (make <rsa-private-key> :modulus m :private-exponent d))

(define-class <rsa-private-crt-key> (<rsa-private-key>)
  ((public-exponent :init-keyword :public-exponent)
   (p :init-keyword :p)
   (q :init-keyword :q)
   (dP :init-keyword :dP)
   (dQ :init-keyword :dQ)
   (qP :init-keyword :qP)))
(define (rsa-private-crt-key? o) (is-a? o <rsa-private-crt-key>))
(define (make-rsa-private-crt-key m e d p q :key (dP (mod d (- p 1)))
						 (dQ (mod d (- q 1)))
						 (qP (mod-inverse q p)))
  (make <rsa-private-crt-key> :modulus m :private-exponent d
	:public-exponent e :p p :q q :dP dP :dQ dQ :qP qP))

(define-method generate-key-pair ((m (eql *key:rsa*))
				  :key (size 2048)
				       (prng (secure-random-generator *prng:chacha20*)))
  (rsa-generate-key-pair size prng #x10001))

(define-method generate-public-key ((n (eql *key:rsa*)) m e)
  (make-rsa-public-key m e))
(define-method generate-private-key ((n (eql *key:rsa*)) m e . rest)
  (apply rsa-generate-private-key m e rest))

(define (rsa-generate-private-key m private-exponent
				  :key (e #f) (p #f) (q #f)
				  :allow-other-keys rest)
  (if (and e p q)
      (apply make-rsa-private-crt-key m e private-exponent p q rest)
      (make-rsa-private-crt-key m private-exponent)))

(define (rsa-generate-key-pair size prng e)
  (define (create-key-pair n e d p q)
    (let ((private (make-rsa-private-crt-key n e d p q))
	  (public (make-rsa-public-key n e)))
      (make-key-pair private public)))

  (define (rsa-random-prime check)
    (let loop ((p (generate-random-prime (/ size 16) prng)))
      (if (and (or (not check) (not (= p check))) (= 1 (gcd (- p 1) e)))
	  p
	  (loop (generate-random-prime (/ size 16) prng)))))
  (when (< size (*rsa-min-keysize*))
    (assertion-violation 'rsa-generate-key-pair
			 "Generating key size is too small"))
  (unless (probable-prime? e)
    (assertion-violation 'rsa-generate-key-pair
			 "exponent is not prime number" e))

  (let* ((p (rsa-random-prime #f))
	 (q (rsa-random-prime p))
	 (n (* p q))
	 (phi (* (- p 1) (- q 1))))
    (let ((d (mod-inverse e phi)))
      (create-key-pair n e d p q))))

;;; TODO export and import

)
