;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;; 
(library (crypto key rsa)
    (export rsa-generate-key-pair)
    (import (rnrs)
	    (srfi :2 and-let*)
	    (crypto random)
	    (crypto prime)
	    (crypto key pair)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto))

  (define-record-type rsa-private-key
    (parent private-key)
    (fields (immutable modulus)
	    (immutable public-exponent)
	    (immutable exponent)
	    ;; for CRT
	    (immutable p)		;; prime factor 1
	    (immutable q)		;; prime factor 2
	    (mutable dP)		;; e*dP = 1 mod p-1
	    (mutable dQ)		;; e*dQ = 1 mod q-1
	    (mutable qP)		;; q*qP = 1/q mod p
	    )
    (protocol (lambda (n)
		(lambda (m e d p q)
		  (let ((c (n)))
		    (c m e d p q (mod d (- p 1)) (mod d (- q 1)) (mod (/ q) p)))))))


  (define-record-type rsa-public-key
    (parent public-key)
    (fields (immutable modulus)
	    (immutable exponent)))

  (define *rsa-min-keysize* 256)
  (define *rsa-max-keysize* 4096)

  ;; RSA key-pair generator
  (define-with-key (rsa-generate-key-pair size :key (prng (pseudo-random RC4)))
    (define (generate-exponent phi)
      (let ((e (random-prime (/ size 8) :prng prng)))
	(let loop ((e e)
		   (d (mod (/ e) phi)))
	  (if (= (* e d) (mod 1 phi))
	      (values e d)
	      (let ((e (random-prime (/ size 8) :prng prng)))
		(loop e (mod (/ e) phi)))))))
		  
    (when (or (< size *rsa-min-keysize*)
	      (> size *rsa-max-keysize*))
      (assertion-violation 'rsa-generate-key-pair
			   "invalid RSA key size" size))
    (let* ((p (random-prime (/ size 16) :prng prng))
	   (q (random-prime (/ size 16) :prng prng))
	   (n (* p q))
	   (phi (* (- p 1) (- q 1))))
      (receive (e d) (generate-exponent phi)
	(let ((private (make-rsa-private-key n e d p q))
	      (public (make-rsa-public-key n e)))
	  (make-keypair private public)))))


)
