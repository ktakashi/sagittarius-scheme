;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; crypto.scm Cryptographic library
;;; 
;;; This library is a wrapper for tomcrypto.
(library (crypto)
    ;; we exports even super low level APIs
    ;; but it won't be documented, so if you use make-cipher, suggest-keysize
    ;; or make-builtin-cipher-spi it might be changed in future.
    (export :all)
    (import (crypto cipher)
	    (crypto key)
	    (crypto pkcs)
	    ;; for conditions
	    (sagittarius crypto))
)
