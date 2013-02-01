;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; component Cryptographic library
;;; key pair
(library (crypto key component)
    (export split-key
	    (rename (bytevector-xor  combine-key-components)
		    (bytevector-xor! combine-key-components!)))
    (import (rnrs)
	    (math random)
	    (only (sagittarius) append!)
	    (util bytevector))

  (define (split-key key :optional (count 3) (prng (secure-random RC4)))
    (define component-size (bytevector-length key))
    ;; bit unefficiency
    (define (random-components count)
      (define (random-component)
	(let ((bv (make-bytevector component-size)))
	  (do ((i 0 (+ i 1)))
	      ((= i component-size) bv)
	    (bytevector-u8-set! bv i (random prng 256)))))
      (do ((i 0 (+ i 1)) (r '() (cons (random-component) r)))
	  ((= i count) r)))
    (let* ((components (random-components (- count 1)))
	   (component  (apply bytevector-xor key components)))
      (apply values (append! components (list component)))))

)