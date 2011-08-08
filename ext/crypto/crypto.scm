;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(library (crypto)
    (export crypto-object?
	    cipher-keysize
	    cipher
	    cipher?
	    encrypt
	    decrypt
	    sign
	    verify
	    ;; key
	    key?
	    generate-secret-key
	    generate-key-pair
	    generate-private-key
	    generate-public-key
	    RSA
	    keypair-private
	    keypair-public
	    private-key
	    private-key?
	    public-key
	    public-key?
	    ;;
	    private-key
	    private-key?
	    public-key
	    public-key?
	    ;; padder
	    pkcs5-padder
	    pkcs-v1.5-padding
	    PKCS-1-EME
	    PKCS-1-EMSA
	    pkcs1-emsa-pss-encode
	    pkcs1-emsa-pss-verify
	    mgf-1
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
	    ;; conditions
	    &crypto-error crypto-error?
	    &encrypt-error encrypt-error?
	    &decrypt-error decrypt-error?
	    &encode-error encode-error?
	    &decode-error decode-error?
	    raise-encrypt-error
	    raise-decrypt-error
	    raise-encode-error
	    raise-decode-error)
    (import (crypto cipher)
	    (crypto key)
	    (crypto pkcs)
	    ;; for conditions
	    (sagittarius crypto))
)
