;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;; 
(library (crypto key rsa)
    (export rsa-generate-key-pair
	    rsa-generate-private-key
	    rsa-generate-public-key
	    ;; padding block type
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA
	    ;; cipher
	    rsa-cipher
	    ;; encrypt/decrypt
	    rsa-encrypt
	    rsa-decrypt
	    ;; signing/verify
	    rsa-sign
	    rsa-verify)
    (import (rnrs)
	    (crypto helper)
	    (crypto random)
	    (crypto prime)
	    (crypto key pair)
	    (math)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto))

  (define-record-type rsa-private-key
    (parent private-key)
    (fields (immutable modulus)
	    (immutable private-exponent))
    (protocol (lambda (p)
		(lambda (m pe)
		  (let ((c (p)))
		    (c m pe))))))

  (define-record-type rsa-private-crt-key
    (parent rsa-private-key)
    (fields (immutable public-exponent)
	    ;; for CRT
	    (immutable p)		;; prime factor 1
	    (immutable q)		;; prime factor 2
	    (immutable dP)		;; e*dP = 1 mod p-1
	    (immutable dQ)		;; e*dQ = 1 mod q-1
	    (immutable qP)		;; q*qP = 1/q mod p
	    )
    (protocol (lambda (n)
		(lambda (m e d p q)
		  (let ((c (n m d)))
		    (c e p q
		       (mod d (- p 1))
		       (mod d (- q 1))
		       (mod-inverse q p)))))))

  (define-record-type rsa-public-key
    (parent public-key)
    (fields (immutable modulus)
	    (immutable exponent)))

  (define *rsa-min-keysize* 256)
  (define *rsa-max-keysize* 4096)

  (define-with-key (rsa-generate-private-key modulus private-exponent
					     :key (public-exponent #f)
					          (p #f)
						  (q #f))
    ;; if crt-key missing one of them
    (when (and (or public-exponent p q)
	       (not (and public-exponent p q)))
      (assertion-violation 'rsa-generate-private-key
			   "invalid crt-key generation"
			   public-exponent p q))
    (if (and public-exponent p q)
	(make-rsa-private-crt-key modulus public-exponent private-exponent p q)
	(make-rsa-private-key modulus private-exponent)))
					     
  (define (rsa-generate-public-key modulus exponent)
    (make-rsa-public-key modulus exponent))

  ;; RSA key-pair generator
  (define (rsa-generate-key-pair size prng e)
    (define (generate-key-pair n e d p q)
      (let ((private (make-rsa-private-crt-key n e d p q))
	    (public (make-rsa-public-key n e)))
	(make-keypair private public)))

    (define (rsa-random-prime)
      (let loop ((p (random-prime (/ size 16) :prng prng)))
	(if (= 1 (gcd (- p 1) e))
	    p
	    (loop (random-prime (/ size 16 :prng prng))))))

    (when (or (< size *rsa-min-keysize*)
	      (> size *rsa-max-keysize*))
      (assertion-violation 'rsa-generate-key-pair
			   "invalid RSA key size" size))
    (unless (is-prime? e)
      (assertion-violation 'rsa-generate-key-pair
			   "exponent is not prime number" e))

    (let* ((p (rsa-random-prime))
	   (q (rsa-random-prime))
	   (n (* p q))
	   (phi (* (- p 1) (- q 1))))
      (let ((d (mod-inverse e phi)))
	(generate-key-pair n e d p q))))

  ;; cipher
  (define-with-key (rsa-cipher key prng :key (padding pkcs-v1.5-padding) (block-type PKCS-1-EME))
    (let ((padder (if padding (padding prng key block-type) #f)))
      (make-public-key-cipher 'RSA key rsa-encrypt rsa-decrypt padder)))

  ;; padding
  ;; PKCS#1 EME
  (define PKCS-1-EMSA 1)
  (define PKCS-1-EME  2)

  (define (pkcs-v1.5-padding prng key block-type)
    (define (encode data modulus)
      (let ((modulus-length (align-size modulus))
	    (message-length (bytevector-length data)))
	(when (> (+ message-length 11) modulus-length)
	  (raise-encrypt-error 'pkcs-v1.5-padding
			       "too much data for RSA block"
			       'RSA))
	(let* ((ps-length (- modulus-length message-length 3))
	       (ps (if (= block-type PKCS-1-EME)
		       (read-random-bytes prng ps-length)
		       (make-bytevector ps-length #xFF))))
	  (if (= block-type PKCS-1-EME)
	      (do ((i 0 (+ i 1)))
		  ((= i ps-length) #t)
		;; transform zero bytes (if any) to non-zero random bytes
		(when (zero? (bytevector-u8-ref ps i))
		  (bytevector-u8-set! ps i (bytevector-u8-ref (read-random-bytes prng 1) 0)))))
	  (let ((bv (make-bytevector (+ 2 ps-length 1 message-length) 0)))
	    ;; set block-type
	    (bytevector-u8-set! bv 1 block-type)
	    (bytevector-copy! ps 0 bv 2 ps-length)
	    (bytevector-u8-set! bv (+ 2 ps-length) 0)
	    (bytevector-copy! data 0 bv (+ 2 ps-length 1) message-length)
	    bv))))
    
    (define (decode data modulus)
      (let ((modulus-length (align-size modulus))
	    (message-length (bytevector-length data))
	    (type (bytevector-u8-ref data 0)))
	(unless (= type block-type)
	  (raise-decrypt-error 'pkcs-v1.5-padding
			       "invalid block type" 'RSA))
	(let ((from (do ((i 1 (+ i 1)))
			((= (bytevector-u8-ref data i) 0) (+ i 1))
		      (unless (or (= type PKCS-1-EME)
				  (= (bytevector-u8-ref data i) #xff))
			(raise-decrypt-error 'pkcs-v1.5-padding
					     "invalid EMSA padding"
					     'RSA (bytevector-u8-ref data i))))))
	  (when (or (>= from modulus-length)
		    (< from 9))
	    (raise-decrypt-error 'pkcs-v1.5-padding
				 "invalid padding length" 'RSA))
	  (let* ((len (- message-length from))
		 (bv (make-bytevector len 0)))
	    (bytevector-copy! data from bv 0 len)
	    bv))))

    (lambda (data pad?)
      (unless (or (rsa-public-key? key)
		  (rsa-private-key? key))
	(if pad?
	    (raise-encrypt-error 'pkcs-v1.5-padding
				 "public key required"
				 'RSA key)
	    (raise-decrypt-error 'pkcs-v1.5-padding
				 "private key required"
				 'RSA key)))
      (let ((modulus (if (rsa-public-key? key)
			 (rsa-public-key-modulus key)
			 (rsa-private-key-modulus key))))
      (if pad?
	  (encode data modulus)
	  (decode data modulus)))))

  ;; encrypt/decrypt
  ;; This procedure must be called from C and bv may be padded there.
  (define (rsa-encrypt bv key)
    (unless (rsa-public-key? key)
      (raise-encrypt-error 'rsa-encrypt
			   "public key required"
			   'RSA key))
    (let ((key-length (align-size (rsa-public-key-modulus key)))
	  (data-length (bytevector-length bv)))
      ;; for consistancy with JCE
      (when (> data-length key-length)
	(raise-encrypt-error 'rsa-encrypt
			     "too much data for RSA block"
			     'RSA))
      (call-with-bytevector-output-port
       (lambda (port)
	 (let ((chunk (bytevector->integer bv)))
	   (put-bytevector port (integer->bytevector
				 (mod-expt chunk
					   (rsa-public-key-exponent key)
					   (rsa-public-key-modulus key)))))))))
    
  (define (rsa-decrypt bv key)
    (unless (rsa-private-key? key)
      (raise-decrypt-error 'rsa-encrypt
			   "private key required"
			   'RSA key))
    (let ((key-length (align-size (rsa-private-key-modulus key)))
	  (data-length (bytevector-length bv)))
      ;; for consistancy with JCE
      (when (> data-length key-length)
	(raise-encrypt-error 'rsa-encrypt
			     "too much data for RSA block"
			     'RSA))
      (call-with-bytevector-output-port
       (lambda (port)
	 (let ((chunk (bytevector->integer bv)))
	   (if (rsa-private-crt-key? key)
	       ;; use CRT
	       (let ((p  (rsa-private-crt-key-p key))
		     (q  (rsa-private-crt-key-q key))
		     (dp (rsa-private-crt-key-dP key))
		     (dq (rsa-private-crt-key-dQ key))
		     (qp (rsa-private-crt-key-qP key)))
		 ;; b = chunk
		 (let* ((a (mod-expt chunk dp p)) ; b ^ dP mod p
			(b (mod-expt chunk dq q)) ; b ^ dQ mod q
			(c (mod (* (- a b) qp) p)) ; (a - b) * qp (mod p)
			(d (+ b (* q c))))	   ; b + q * c
		   (put-bytevector port (integer->bytevector d))))
	       (let* ((modulus (rsa-private-key-modulus key))
		      (private-exponent (rsa-private-key-private-exponent key))
		      (a (mod-expt chunk private-exponent modulus)))
		 (put-bytevector port (integer->bytevector a)))))))))
	    
)