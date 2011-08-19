;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;; 
(library (math hash)
    (export hash-algorithm?
	    hash-algorithm
	    hash-init!
	    hash-process!
	    hash-done!
	    hash-size
	    hash-oid
	    WHIRLPOOL SHA-512 SHA-384 RIPEMD-320
	    SHA-256 RIPEMD-256 SHA-224 SHA-224   
	    Tiger-192 SHA-1 RIPEMD-160 RIPEMD-128
	    MD5 MD4 MD2
	    ;; for convenience
	    hash
	    )

    (import (core)
	    (sagittarius control)
	    (sagittarius math))

  (define-with-key (hash-algorithm name :key (process #f))
    (make-hash-algorithm name process))

  (define-with-key (hash type bv :key (process #f))
    (let* ((algo (if (hash-algorithm? type)
		     type
		     (hash-algorithm type :process process)))
	   (out  (make-bytevector (hash-size algo) 0)))
      (hash-init! algo)
      (hash-process! algo bv)
      (hash-done! algo out)
      out))
)
