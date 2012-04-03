;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;; 
#!compatible
(library (math hash)
    (export hash-algorithm?
	    hash-algorithm
	    hash-init!
	    hash-process!
	    hash-done!
	    hash-size
	    hash-block-size
	    hash-oid
	    WHIRLPOOL SHA-512 SHA-384 RIPEMD-320
	    SHA-256 RIPEMD-256 SHA-224 SHA-224   
	    Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
	    MD5 MD4 MD2
	    ;; for convenience
	    hash hash!
	    register-hash
	    lookup-hash
	    <hash-algorithm>
	    <user-hash-algorithm>
	    <builtin-hash-algorithm>
	    )

    (import (core)
	    (clos core)
	    (sagittarius control)
	    (sagittarius math))

  (define (hash-algorithm name . opts)
    (cond ((lookup-hash name)
	   => (lambda (clazz)
		(if (boolean? clazz)
		    (make-hash-algorithm name)
		    (apply make clazz opts))))
	  (else
	   (assertion-violation 'hash-algorithm
				"unknown hash" name))))

  (define (hash type bv . opts)
    (let* ((algo (if (hash-algorithm? type)
		     type
		     (apply hash-algorithm type opts)))
	   (out (make-bytevector (hash-size algo))))
      (apply hash! algo out bv opts)))

  (define (hash! type out bv . opts)
    (let* ((algo (if (hash-algorithm? type)
		     type
		     (apply hash-algorithm type opts)))
	   (size (hash-size algo)))
      (when (< (bytevector-length out) size)
	(assertion-violation 'hash!
			     "output buffer is too short"))
      (hash-init! algo)
      (hash-process! algo bv)
      (hash-done! algo out)
      out))
)
