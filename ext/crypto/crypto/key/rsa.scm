;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;; 
(library (crypto key rsa)
    (export rsa-generate-key-pair
	    rsa-generate-private-key
	    rsa-generate-public-key
	    ;; cipher
	    rsa-cipher
	    ;; encrypt/decrypt
	    rsa-encrypt
	    rsa-decrypt
	    ;; signing/verify
	    rsa-sign
	    rsa-verify)
    (import (rnrs)
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
    (let* ((p (rsa-random-prime))
	   (q (rsa-random-prime))
	   (n (* p q))
	   (phi (* (- p 1) (- q 1))))
      (let ((d (mod-inverse e phi)))
	(generate-key-pair n e d p q))))

  ;; cipher
  (define-with-key (rsa-cipher key prng :key (padding pkcs-v1.5-padding))
    (let ((padder (padding prng key)))
      (make-public-key-cipher 'RSA key rsa-encrypt rsa-decrypt padder)))

  ;; padding
  ;; PKCS#1 EME
  (define (pkcs-v1.5-padding prng key)
    (define (encode data bitlen)
      (let ((modulus-length (+ (bitwise-arithmetic-shift-right bitlen 3)
			       (if (zero? (bitwise-and bitlen 7)) 0 1)))
	    (message-length (bytevector-length data)))
	(when (> (+ message-length 11) modulus-length)
	  (raise-encrypt-error 'pkcs-v1.5-padding
			       "invalid sise message"
			       'RSA modulus-length))
	(let* ((ps-length (- modulus-length message-length 3))
	       (ps (read-random-bytes prng ps-length)))
	  (do ((i 0 (+ i 1)))
	      ((= i ps-length) #t)
	    ;; transform zero bytes (if any) to non-zero random bytes
	    (when (zero? (bytevector-u8-ref ps i))
	      (bytevector-u8-set! ps i (bytevector-u8-ref (read-random-bytes prng 1) 0))))
	  
	  (let ((bv (make-bytevector (+ 2 ps-length 1 message-length) 0)))
	    ;; set block-type
	    (bytevector-u8-set! bv 1 2)	    ;; EME = 2
	    (bytevector-copy! ps 0 bv 2 ps-length)
	    (bytevector-u8-set! bv (+ 2 ps-length) 0)
	    (bytevector-copy! data 0 bv (+ 2 ps-length 1) message-length)
	    bv))))
    
    (define (decode data bitlen)
      #vu8(1 2 3))

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
      (let ((modulus-bit-length (bitwise-length (if (rsa-public-key? key)
						    (rsa-public-key-modulus key)
						    (rsa-private-key-modulus key)))))
      (if pad?
	  (encode data modulus-bit-length)
	  (decode data modulus-bit-length)))))

  ;; encrypt/decrypt
  ;; This procedure must be called from C and bv may be padded there.
  (define (rsa-encrypt bv key)
    (unless (rsa-public-key? key)
      (raise-encrypt-error 'rsa-encrypt
			   "public key required"
			   'RSA key))
    (let ((key-length (let ((bitlen (bitwise-length (rsa-public-key-modulus key))))
			(+ (bitwise-arithmetic-shift-right bitlen 3)
			   (if (zero? (bitwise-and bitlen 7)) 0 1))))
	  (data-length (bytevector-length bv)))
      (call-with-bytevector-output-port
       (lambda (port)
	 (define (read-bytevector-as-integer bv start length)
	   (do ((ret 0) (i 0 (+ i 1)))
	       ((= i length) ret)
	     (set! ret (bitwise-ior (bitwise-arithmetic-shift-left (bytevector-u8-ref bv (+ i start)) (* i 8))
				    ret))))
	 (define (integer->bytevector int)
	   (let ((bv (make-bytevector key-length 0)))
	     (do ((int (bitwise-arithmetic-shift-right int 8) (bitwise-arithmetic-shift-right int 8))
		  (i (- key-length 1) (- i 1)))
		 ((< i 0) bv)
	       (bytevector-u8-set! bv i (bitwise-and int #xFF)))))
	 (let ((chunk (bytevector->number bv)))
	   (put-bytevector port (integer->bytevector
				 (mod-exp chunk
					  (rsa-public-key-exponent key)
					  (rsa-public-key-modulus key)))))))))
    
  (define (rsa-decrypt bv key)
    (unless (rsa-private-key? key)
      (raise-decrypt-error 'rsa-encrypt
			   "private key required"
			   'RSA key)))
)