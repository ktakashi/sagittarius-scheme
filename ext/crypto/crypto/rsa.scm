;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; rsa.scm Cryptographic library
;;;
#!deprecated
#!nounbound
(library (crypto rsa)
    (export (rename (pkcs1-v1.5-encoding pkcs-v1.5-padding))
	    rsa-oaep-padding
	    PKCS-1-EME
	    PKCS-1-EMSA
	    
	    ;; encrypt/decrypt
	    rsa-encrypt
	    rsa-decrypt
	    ;; signing/verify
	    rsa-sign
	    rsa-verify
	    ;; marker
	    RSA

	    <rsa-private-key>
	    rsa-private-key?
	    rsa-private-key-modulus rsa-private-key-private-exponent
	    (rename (<rsa-crt-private-key> <rsa-private-crt-key>)
		    (rsa-crt-private-key?  rsa-private-crt-key?)
		    (rsa-crt-private-key-public-exponent
		     rsa-private-crt-key-public-exponent)
		    (rsa-crt-private-key-p  rsa-private-crt-key-p)
		    (rsa-crt-private-key-q  rsa-private-crt-key-q)
		    (rsa-crt-private-key-dP rsa-private-crt-key-dP)
		    (rsa-crt-private-key-dQ rsa-private-crt-key-dQ)
		    (rsa-crt-private-key-qP rsa-private-crt-key-qP))
	    <rsa-public-key>
	    rsa-public-key? rsa-public-key-modulus rsa-public-key-exponent
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto keys operations asymmetric rsa)
	    (sagittarius crypto signatures)
	    (sagittarius crypto ciphers asymmetric)
	    (sagittarius crypto random)
	    (sagittarius crypto digests)
	    (math hash)
	    (rename (crypto spi)
		    (<legacy-cipher-spi> <cipher-spi>)))

;; marker class for generic functions
(define RSA *key:rsa*)
(define PKCS-1-EMSA 1)
(define PKCS-1-EME 2)

(define *rsa-max-keysize* 4096)
(define (no-padding . ignore)
  (values
   (lambda (bv . ignore) (integer->bytevector (bytevector->integer bv)))
   (lambda (bv . ignore) (integer->bytevector (bytevector->integer bv)))))

;; cipher
(define-class <rsa-cipher-spi> (<cipher-spi>) ())
(define-method initialize ((o <rsa-cipher-spi>) initargs)
  (define (make-cipher :key (prng (secure-random-generator *prng:chacha20*))
			    (padding pkcs1-v1.5-encoding)
			    (block-type #f) ;; we ignore block type
		       :allow-other-keys)
    (make-asymmetric-cipher *scheme:rsa*
			    :encoding (or padding no-padding)
			    :prng prng))
  (let ((key (car initargs))
	(cipher (apply make-cipher (cdr initargs))))
    (slot-set! o 'name 'RSA)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt (rsa-encrypt cipher))
    (slot-set! o 'decrypt (rsa-decrypt cipher))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer rsa-sign)
    (slot-set! o 'verifier rsa-verify)
    (slot-set! o 'keysize (lambda (ignore) *rsa-max-keysize*))
    o))
;; use marker
(register-spi RSA <rsa-cipher-spi>)

(define (rsa-oaep-padding hasher :key mgf label)
  (if (hash-algorithm? hasher)
      (rsa-oaep-padding (hash-algorithm->digest-descriptor hasher)
			:mgf mgf :label label)
      (lambda (descriptor . opts)
	(apply oaep-encoding descriptor :digest hasher
	       :mgf mgf :label label opts))))

(define ((rsa-encrypt cipher) bv key)
  (asymmetric-cipher-init! cipher key)
  (let ((r (asymmetric-cipher-encrypt-bytevector cipher bv)))
    (asymmetric-cipher-done! cipher)
    r))
(define ((rsa-decrypt cipher) bv key)
  (asymmetric-cipher-init! cipher key)
  (let ((r (asymmetric-cipher-decrypt-bytevector cipher bv)))
    (asymmetric-cipher-done! cipher)
    r))

(define (->descriptor hash)
  (if (hash-algorithm? hash)
      (hash-algorithm->digest-descriptor hash)
      hash))
(define (rsa-sign bv key :key (encode pkcs1-emsa-pss-encode)
			      (hash *digest:sha-1*)
			 :allow-other-keys opts)
  (let ((signer (apply make-signer *signature:rsa* key
		       :encoder encode :digest (->descriptor hash) opts)))
    (signer-sign-message signer bv)))
(define (rsa-verify M S key :key (verify pkcs1-emsa-pss-verify)
				 (hash *digest:sha-1*)
			    :allow-other-keys opts)
  (let ((verifier (apply make-verifier *signature:rsa* key
			 :verifier verify :digest (->descriptor hash) opts)))
    (verifier-verify-signature verifier M S)))
    
)
