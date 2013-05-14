;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(library (sagittarius crypto)
    (export crypto-object?
	    cipher?
	    make-builtin-cipher-spi
	    make-cipher
	    encrypt
	    decrypt
	    suggest-keysize
	    cipher-blocksize
	    cipher-iv
	    sign
	    verify
	    ;;
	    register-spi
	    lookup-spi

	    ;; key
	    key?
	    generate-secret-key
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
	    ;; condition
	    &crypto-error crypto-error?
	    &encrypt-error encrypt-error? condition-encrypt-mechanism
	    &decrypt-error decrypt-error? condition-decrypt-mechanism
	    &encode-error encode-error?
	    &decode-error decode-error?
	    raise-encrypt-error
	    raise-decrypt-error
	    raise-encode-error
	    raise-decode-error
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
	    ;;
	    <crypto>
	    <cipher>
	    <cipher-spi>
	    <builtin-cipher-spi>
	    <key>
	    <symmetric-key>
	    <builtin-symmetric-key>
	    <asymmetric-key>
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius dynamic-module))

  (load-dynamic-module "sagittarius--crypto")

  (define-condition-type &crypto-error &error
    make-crypto-error crypto-error?)

  (define-condition-type &encrypt-error &crypto-error
    make-encrypt-error encrypt-error?
    (mechanism condition-encrypt-mechanism))

  (define-condition-type &decrypt-error &crypto-error
    make-decrypt-error decrypt-error?
    (mechanism condition-decrypt-mechanism))

  (define-condition-type &encode-error &crypto-error
    make-encode-error encode-error?)

  (define-condition-type &decode-error &crypto-error
    make-decode-error decode-error?)

  (define-syntax define-raise-error
    (syntax-rules ()
      ((_ name error)
       (define (name who message mechanism . irritants)
	 (raise
	  (apply condition 
		 (filter values
			 (list (error mechanism)
			       (and who (make-who-condition who))
			       (make-message-condition message)
			       (make-irritants-condition irritants)))))))))
  (define-raise-error raise-encrypt-error make-encrypt-error)
  (define-raise-error raise-decrypt-error make-decrypt-error)

  (define (raise-encode-error who message . irritants)
    (raise
     (apply condition
	    (filter values
		    (list (make-encode-error)
			  (and who (make-who-condition who))
			  (make-message-condition message)
			  (make-irritants-condition irritants))))))

  (define (raise-decode-error who message . irritants)
    (raise
     (apply condition
	    (filter values
		    (list (make-decode-error)
			  (and who (make-who-condition who))
			  (make-message-condition message)
			  (make-irritants-condition irritants))))))

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

)