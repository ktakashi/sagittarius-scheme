;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm Cryptographic library
;;; 
(library (crypto random)
    (export ;; random number generator
	    pseudo-random
	    random
	    read-random-bytes
	    bytevector->number
	    Yarrow Fortuna RC4 SOBER-128)
    (import (core)
	    (sagittarius control)
	    (sagittarius crypto))
  ;; pseudo-random type
  (define Yarrow "yarrow")
  (define Fortuna "fortuna")
  (define RC4 "rc4")
  (define SOBER-128 "sober128")

  (define-with-key (pseudo-random type :key (bits 128))
    (make-pseudo-random type bits))

  ;; TODO 100 bytes read-size are good enough?
  (define (bytevector->number bv)
    (let ((len (bytevector-length bv)))
      (let loop ((i 0)
		 (ans 0))
	(if (= i len)
	    ans
	    (loop (+ i 1)
		  (+ ans (bitwise-arithmetic-shift (bytevector-u8-ref bv i)
						   (* i 8))))))))

  (define-with-key (random prng size :key (read-size 100))
    (let* ((bv (read-random-bytes prng read-size))
	   (rnd (bytevector->number bv)))
      (if (> rnd size)
	  (modulo rnd size)
	  rnd)))
)
