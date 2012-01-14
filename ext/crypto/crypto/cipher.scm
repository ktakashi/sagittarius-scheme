;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; cipher.scm Cryptographic library
;;; 
#!compatible
(library (crypto cipher)
    (export crypto-object?
	    cipher-keysize
	    cipher
	    cipher?
	    encrypt
	    decrypt
	    ;; signing
	    signature
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
	    LTC_CTR_RFC3686)
    (import (core)
	    (core base)
	    (crypto key)
	    (crypto pkcs)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius crypto))

  ;; symmetric types
  (define-constant Blowfish    "blowfish")
  (define-constant X-Tea       "xtea")
  (define-constant RC2         "rc2")
  (define-constant RC5-32/12/b "rc5")
  (define-constant RC6-32/20/b "rc6")
  (define-constant SAFER+      "safer+")
  (define-constant SAFER-K64   "safer-k64")
  (define-constant SAFER-SK64  "safer-sk64")
  (define-constant SAFER-K128  "safer-k128")
  (define-constant SAFER-SK128 "safer-sk128")
  (define-constant AES         "aes")
  (define-constant Twofish     "twofish")
  (define-constant DES         "des")
  (define-constant DES3        "3des")
  (define-constant DESede      DES3)
  (define-constant CAST5       "cast5")
  (define-constant CAST-128    CAST5)
  (define-constant Noekeon     "noekeon")
  (define-constant Skipjack    "skipjack")
  (define-constant Khazad      "khazad")
  (define-constant SEED        "seed")
  (define-constant KASUMI      "kasumi")

  (define-constant *symmetric-types*
    '("blowfish" "xtea" "rc2" "rc5" "rc6"
      "safer+" "safer-k64" "safer-sk64"
      "safer-k128" "safer-sk128"
      "aes" "twofish" "des" "3des"
      "cast5" "noekeon" "skipjack"
      "khazad" "seed" "kasumi"))

  (define (cipher-keysize type test)
    (suggest-keysize type test))

  (define-with-key (cipher type key 
			   :key (mode MODE_ECB)
			        (iv #f)
				(padder pkcs5-padder)
				(rounds 0)
				(ctr-mode CTR_COUNTER_LITTLE_ENDIAN)
			   :allow-other-keys rest)
    (unless (or (= mode MODE_ECB)
		(bytevector? iv))
      (assertion-violation 'cipher
			   "on the given mode iv id required"))
    (if (member type *symmetric-types*)
	(make-symmetric-cipher type mode key iv rounds padder ctr-mode)
	(apply public-key-cipher type key rest)))
)
