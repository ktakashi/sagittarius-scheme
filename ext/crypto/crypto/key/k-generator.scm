;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; DSA K-generator
;;;

;; ref https://tools.ietf.org/html/rfc6979
(library (crypto key k-generator)
    (export random-k-generator
	    determistic-k-generator)
    (import (rnrs)
	    (sagittarius)
	    (math))
  (define (read-random-bits prng nbits)
    (bytevector->uinteger (read-random-bytes prng (div nbits 8))))
  
  (define (random-k-generator prng)
    (lambda (n d)
      (let ((bits (bitwise-length n)))
	(do ((r (read-random-bits prng bits) (read-random-bits prng bits)))
	    ((and (not (zero? r)) (< r n)) r)))))

  ;; RFC 6979
  (define (determistic-k-generator digest message)
    (error 'determistic-k-generator "not supported yet"))
)
