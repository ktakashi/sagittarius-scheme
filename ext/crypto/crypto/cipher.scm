;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;; 
#!core
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher-blocksize
	    cipher-iv
	    cipher-update-aad!
	    cipher-tag! cipher-tag
	    cipher-max-tag-size
	    cipher?
	    make-cipher
	    cipher-encrypt
	    cipher-decrypt
	    cipher-signature
	    cipher-verify
	    key-check-value

	    cipher-encrypt/tag
	    cipher-decrypt/tag
	    cipher-decrypt/verify
	    ;; signing
	    ;; supported algorithms
	    Blowfish
	    X-Tea
	    RC2
	    RC5-32/12/b
	    RC6-32/20/b
	    SAFER+
	    SAFER-K64
	    SAFER-SK64
	    SAFER-K128
	    SAFER-SK128
	    AES
	    Twofish
	    DES
	    DES3
	    DESede
	    CAST5
	    CAST-128
	    Noekeon
	    Skipjack
	    Khazad
	    SEED
	    KASUMI
	    Camellia
	    ;; supported modes
	    MODE_ECB
	    MODE_CBC
	    MODE_CFB
	    MODE_OFB
	    MODE_CTR
	    MODE_GCM
	    ;; ctr conter mode
	    CTR_COUNTER_LITTLE_ENDIAN
	    CTR_COUNTER_BIG_ENDIAN
	    LTC_CTR_RFC3686

	    <crypto>
	    <cipher>
	    <cipher-spi>
	    <key>
	    <symmetric-key>
	    <asymmetric-key>
	    ;; for backward compatibility
	    (rename (make-cipher cipher)
		    (cipher-encrypt encrypt)
		    (cipher-decrypt decrypt)
		    (cipher-signature sign)
		    (cipher-verify verify))
	    )
    (import (core)
	    (core base)
	    (crypto key)
	    (crypto pkcs)
	    (sagittarius)
	    (clos core)
	    (sagittarius crypto))

  (define (cipher-keysize cipher test)
    (unless (cipher? cipher)
      (assertion-violation 'cipher-keysize
			   (format "cipher required but got ~s" cipher)))
    (suggest-keysize cipher test))

  (define (make-cipher type key 
		  :key (mode MODE_ECB)
		  (iv #f)
		  (padder pkcs5-padder)
		  (rounds 0)
		  (ctr-mode CTR_COUNTER_BIG_ENDIAN)
		  :allow-other-keys
		  :rest rest)
    (unless (or (= mode MODE_ECB) (bytevector? iv))
      (assertion-violation 'cipher
			   "on the given mode iv id required"))
    (let ((spi (cond ((lookup-cipher-spi type)
		      => (lambda (spi)
			   (if (boolean? spi)
			       (make-builtin-cipher-spi
				type mode key iv rounds padder ctr-mode)
			       (apply make spi key rest))))
		     ((cipher-spi? type) type) ;; reuse 
		     (else
		      (assertion-violation 'cipher
					   "unknown cipher type" type)))))
      (create-cipher spi)))

  (define-constant +check-value+ (make-bytevector 8 0))
  (define (key-check-value type key :optional (size 3))
    (unless (<= 3 size 8)
      (assertion-violation 'key-check-value "size must be between 3 to 8"
			   size))
    (let ((c (make-cipher type key)))
      (bytevector-copy (cipher-encrypt c +check-value+) 0 size)))

  ;; with authentication
  (define (cipher-tag cipher :key (size (cipher-max-tag-size cipher)))
    (let ((tag (make-bytevector size)))
      (cipher-tag! cipher tag)
      tag))

  (define (cipher-encrypt/tag cipher data
			      :key (tag-size (cipher-max-tag-size cipher)))
    (let ((encrypted (cipher-encrypt cipher data)))
      (values encrypted (cipher-tag cipher :size tag-size))))
  (define (cipher-decrypt/tag cipher data
			      :key (tag-size (cipher-max-tag-size cipher)))
    (let ((pt (cipher-encrypt cipher data)))
      (values pt (cipher-tag cipher :size tag-size))))
  (define (cipher-decrypt/verify cipher encrypted tag)
    (let-values (((pt target)
		  (cipher-decrypt/tag cipher encrypted
				      :tag-size (bytevector-length tag))))
      (unless (bytevector=? tag target)
	(raise-decrypt-error (slot-ref cipher 'name)
			     'cipher-decrypt/verify
			     "invalid tag is given"))
      pt))

)
