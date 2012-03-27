;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(load-dynamic-library "sagittarius--crypto")
(library (sagittarius crypto)
    (export crypto-object?
	    cipher?
	    make-builtin-cipher-spi
	    make-cipher
	    encrypt
	    decrypt
	    suggest-keysize
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
	    ;;
	    <crypto>
	    <cipher>
	    <cipher-spi>
	    <key>
	    <symmetric-key>
	    <asymmetric-key>
	    )
    (import (rnrs)
	    (sagittarius crypto impl))

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
)