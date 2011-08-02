;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(load-dynamic-library "sagittarius--crypto")
(library (sagittarius crypto)
    (export crypto-object?
	    cipher?
	    make-cipher
	    encrypt
	    decrypt
	    suggest-keysize
	    ;; key
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
	    LTC_CTR_RFC3686)
    (import (sagittarius crypto impl))
)
