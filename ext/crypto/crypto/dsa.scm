;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; dsa.scm Cryptographic library
;;;
#!deprecated
#!core
(library (crypto dsa)
    (export DSA
	    generate-dsa-parameter
	    <dsa-private-key>
	    <dsa-public-key>)
    (import (rnrs)
	    (clos user)
	    (crypto spi)
	    (sagittarius crypto keys)
	    (sagittarius crypto signatures))

(define DSA *signature:dsa*)

(define (dsa-sign M key :key k-generator der-encode)
  (let ((signer (make-signer DSA key
			     :k-generator k-generator
			     :der-encode der-encode)))
    (signer-sign-message signer M)))

(define (dsa-verify M S key :key der-encode)
  (let ((verifier (make-verifier DSA key :der-encode der-encode)))
    (verifier-verify-signature verifier M S)))

;; cipher
(define-class <dsa-cipher-spi> (<legacy-cipher-spi>) ())
(define-method initialize ((o <dsa-cipher-spi>) initargs)
  (let ((key (car initargs)))
    (slot-set! o 'name 'DSA)
    (slot-set! o 'key key)
    (slot-set! o 'encrypt 
	       (lambda ignore (error 'encrypt "not supported in DSA")))
    (slot-set! o 'decrypt
	       (lambda ignore (error 'decrypt "not supported in DSA")))
    (slot-set! o 'padder #f)
    (slot-set! o 'signer dsa-sign)
    (slot-set! o 'verifier dsa-verify)
    (slot-set! o 'keysize (lambda (target) 1024)) ;should we?
    o))
(register-spi DSA <dsa-cipher-spi>)

)
