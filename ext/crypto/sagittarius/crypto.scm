;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(load-dynamic-library "sagittarius--crypto")
(library (sagittarius crypto)
    (export crypto-object?
	    cipher?
	    make-symmetric-cipher
	    make-public-key-cipher
	    encrypt
	    decrypt
	    suggest-keysize
	    ;; key
	    key
	    key?
	    generate-secret-key
	    ;; random number generator
	    make-pseudo-random
	    read-random-bytes
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
	    &encrypt-error encrypt-error?
	    &decrypt-error decrypt-error?
	    raise-encrypt-error
	    raise-decrypt-error)
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
)