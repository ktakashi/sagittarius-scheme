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

  ;; we can't refer DES3 here (i'm too lazy) so use raw keyword.
  ;; To accept 16 bytes des key
  ;; the triple des key structure is like this
  ;;   encrypt(8 bytes) | descrypt (8 bytes) | encrypt (8 bytes)
  ;; now 16 bytes key contains only first 16 bytes means we need to
  ;; make it 26 bytes.
  ;; it's simply needed to be appended the first 8 bytes to the last
  (define-method generate-secret-key ((type (eql :3des)) (key <bytevector>))
    (if (= (bytevector-length key) 16)
	(call-next-method type
	 (bytevector-append key (bytevector-copy key 0 8)))
	(call-next-method)))
)