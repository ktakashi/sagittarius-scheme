;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;; 
#!core
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
	    SHA-256 RIPEMD-256 SHA-224 SHA-512/224 SHA-512/256
	    SHA-3-224 SHA-3-256 SHA-3-384 SHA-3-512
	    Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
	    MD5 MD4 MD2
	    BLAKE2s-128 BLAKE2s-160 BLAKE2s-224 BLAKE2s-256
	    BLAKE2b-160 BLAKE2b-256 BLAKE2b-384 BLAKE2b-512
	    SHAKE128 SHAKE256
	    oid->hash-algorithm
	    
	    ;; for convenience
	    hash hash!
	    register-hash
	    lookup-hash
	    <hash-algorithm>
	    <user-hash-algorithm>
	    <builtin-hash-algorithm>
	    )

    (import (core)
	    (core base)
	    (clos core)
	    (sagittarius math))

  (define (hash-algorithm name . opts)
    (cond ((hash-algorithm? name) name) ;; for convenience
	  ((lookup-hash name)
	   => (lambda (clazz)
		(if (boolean? clazz)
		    (make-hash-algorithm name)
		    (apply make clazz opts))))
	  (else
	   (assertion-violation 'hash-algorithm
				"unknown hash" name))))

  (define (get-size algo :key (size (hash-block-size algo)))
    (define hsize (hash-size algo))
    (if (zero? hsize)
	size
	hsize))
  
  (define (hash type bv . opts)
    (let* ((algo (if (hash-algorithm? type)
		     type
		     (apply hash-algorithm type opts)))
	   (out (make-bytevector (apply get-size algo opts))))
      (apply hash! algo out bv opts)))

  (define (hash! type out bv . opts)
    (let* ((algo (if (hash-algorithm? type)
		     type
		     (apply hash-algorithm type opts)))
	   (size (hash-size algo)))
      (when (and (> size 0) (< (bytevector-length out) size))
	(assertion-violation 'hash!
			     "output buffer is too short"))
      (hash-init! algo)
      (hash-process! algo bv)
      (hash-done! algo out)
      out))

  (define *oid/algorithm*
    (filter values
	    (map (lambda (name)
		   (let ((algo (hash-algorithm name)))
		     (cons (hash-oid algo) algo)))
		 (list WHIRLPOOL SHA-512 SHA-384 RIPEMD-320
		       SHA-256 RIPEMD-256 SHA-224 SHA-512/224 SHA-512/256
		       SHA-3-224 SHA-3-256 SHA-3-384 SHA-3-512
		       Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
		       MD5 MD4 MD2
		       BLAKE2s-128 BLAKE2s-160 BLAKE2s-224 BLAKE2s-256
		       BLAKE2b-160 BLAKE2b-256 BLAKE2b-384 BLAKE2b-512))))
  (define (oid->hash-algorithm oid)
    ;; we don't do user hash for now...
    (cond ((assoc oid *oid/algorithm*) => cdr)
	  (else #f)))
  
)
