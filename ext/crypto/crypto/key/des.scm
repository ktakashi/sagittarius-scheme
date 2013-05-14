;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; des.scm Cryptographic library
;;; 
#!compatible
(library (crypto key des)
    (export generate-secret-key)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto))

  ;; To accept 8 or 16 bytes des key
  ;; the triple des key structure is like this
  ;;   encrypt(8 bytes) | descrypt (8 bytes) | encrypt (8 bytes)
  ;; now 16 bytes key contains only first 16 bytes means we need to
  ;; make it 24 bytes.
  ;; it's simply needed to be appended the first 8 bytes to the last
  (define-method generate-secret-key ((type (eql DES3)) (key <bytevector>))
    (case (bytevector-length key)
      ((8) (call-next-method type (bytevector-append key key key)))
      ((16) (call-next-method
	     type (bytevector-append key (bytevector-copy key 0 8))))
      (else (call-next-method))))
)