;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;; 
#!compatible
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher-blocksize
	    cipher-iv
	    cipher
	    cipher?
	    encrypt
	    decrypt
	    key-check-value
	    ;; signing
	    sign
	    verify
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
	    ;; supported modes
	    MODE_ECB
	    MODE_CBC
	    MODE_CFB
	    MODE_OFB
	    MODE_CTR
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
	    )
    (import (core)
	    (core base)
	    (crypto key)
	    (crypto pkcs)
	    (sagittarius)
	    (clos core)
	    (sagittarius control)
	    (sagittarius crypto))

  ;; symmetric types
  (define-constant Blowfish    :blowfish)
  (define-constant X-Tea       :xtea)
  (define-constant RC2         :rc2)
  (define-constant RC5-32/12/b :rc5)
  (define-constant RC6-32/20/b :rc6)
  (define-constant SAFER+      :safer+)
  (define-constant SAFER-K64   :safer-k64)
  (define-constant SAFER-SK64  :safer-sk64)
  (define-constant SAFER-K128  :safer-k128)
  (define-constant SAFER-SK128 :safer-sk128)
  (define-constant AES         :aes)
  (define-constant Twofish     :twofish)
  (define-constant DES         :des)
  (define-constant DES3        :3des)
  (define-constant DESede      DES3)
  (define-constant CAST5       :cast5)
  (define-constant CAST-128    CAST5)
  (define-constant Noekeon     :noekeon)
  (define-constant Skipjack    :skipjack)
  (define-constant Khazad      :khazad)
  (define-constant SEED        :seed)
  (define-constant KASUMI      :kasumi)

  (define (cipher-keysize cipher test)
    (unless (cipher? cipher)
      (assertion-violation 'cipher-keysize
			   (format "cipher required but got ~s" cipher)))
    (suggest-keysize cipher test))

  (define (cipher type key 
		  :key (mode MODE_ECB)
		  (iv #f)
		  (padder pkcs5-padder)
		  (rounds 0)
		  (ctr-mode CTR_COUNTER_LITTLE_ENDIAN)
		  :allow-other-keys
		  :rest rest)
    (unless (or (= mode MODE_ECB) (bytevector? iv))
      (assertion-violation 'cipher
			   "on the given mode iv id required"))
    (let ((spi (cond ((lookup-spi type)
		      => (lambda (spi)
			   (if (boolean? spi)
			       (make-builtin-cipher-spi
				type mode key iv rounds padder ctr-mode)
			       (apply make spi key rest))))
		     (else
		      (assertion-violation 'cipher
					   "unknown cipher type" type)))))
	(make-cipher spi)))

  (define-constant +check-value+ (make-bytevector 8 0))
  (define (key-check-value type key :optional (size 3))
    (unless (<= 3 size 8)
      (assertion-violation 'key-check-value "size must be between 3 to 8"
			   size))
    (let ((c (cipher type key)))
      (bytevector-copy (encrypt c +check-value+) 0 size)))

)
