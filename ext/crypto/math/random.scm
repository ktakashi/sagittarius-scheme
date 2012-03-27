;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;; 
#!compatible
(library (math random)
    (export prng?
	    pseudo-random?
	    secure-random?
	    custom-random?
	    ;; random number generator
	    pseudo-random
	    secure-random
	    random-seed-set!
	    random
	    read-random-bytes
	    Yarrow Fortuna RC4 SOBER-128

	    <prng>)
    (import (core)
	    (math helper)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius math))

  (define-with-key (pseudo-random type :key (seed #f) (reader #f))
    (or (not seed)
	(integer? seed)
	(bytevector? seed)
	(assertion-violation 'pseudo-random 
			     "integer or bytevector required" seed))
    (if reader
	(make-custom-prng type reader)
	(make-pseudo-random type (if (integer? seed)
				     (integer->bytevector seed)
				     seed))))

  (define-with-key (secure-random type :key (bits 128))
    (make-secure-random type bits))

  (define (random-seed-set! prng seed)
    (or (integer? seed)
	(bytevector? seed)
	(assertion-violation 'pseudo-random 
			     "integer or bytevector required" seed))
    (if (integer? seed)
	(%random-seed-set! prng (integer->bytevector seed))
	(%random-seed-set! prng seed)))

  ;; TODO 100 bytes read-size are good enough?
  (define-with-key (random prng size :key (read-size 100))
    (let* ((bv (read-random-bytes prng read-size))
	   (rnd (bytevector->integer bv)))
      (if (> rnd size)
	  (modulo rnd size)
	  rnd)))
)
