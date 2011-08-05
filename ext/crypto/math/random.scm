;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; random.scm math library
;;; 
(library (math random)
    (export (rename (prng? pseudo-random?))
	    ;; random number generator
	    pseudo-random
	    random
	    read-random-bytes
	    bytevector->number
	    Yarrow Fortuna RC4 SOBER-128)
    (import (core)
	    (math helper)
	    (sagittarius control)
	    (sagittarius math))

  (define-with-key (pseudo-random type :key (bits 128) (reader #f))
    (if reader
	(make-custom-prng type reader) 
	(make-pseudo-random type bits)))

  ;; TODO 100 bytes read-size are good enough?
  (define-with-key (random prng size :key (read-size 100))
    (let* ((bv (read-random-bytes prng read-size))
	   (rnd (bytevector->integer bv)))
      (if (> rnd size)
	  (modulo rnd size)
	  rnd)))
)
