;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; helper.scm math library
;;; 
(library (math helper)
    (export align-size
	    mod-inverse
	    mod-expt)
    (import (rnrs) (sagittarius))

  (define (mod-inverse u v)
    (let ((u1 1)
	  (u3 u)
	  (v1 0)
	  (v3 v)
	  (bi 1))
      (do ()
	  ((zero? v3) #t)
	(receive (q t3) (div-and-mod u3 v3)
	  (let* ((w (* q v1))
		 (t1 (+ u1 w)))
	    (set! u1 v1)
	    (set! v1 t1)
	    (set! u3 v3)
	    (set! v3 t3)
	    (set! bi (- bi)))))
      (if (negative? bi)
	  (- v u1)
	  u1)))

  ;; compute x ^ n mod d
  ;; This is actually the same as (mod (expt x n) d). However,  if we use
  ;; builtin 'expt' for this calculation, it raises an error when x or d is
  ;; bignum. So here we define the better way to compute.
  (define (mod-expt x n d)
    (do ((y 1) (n n))
	((<= n 0) y)
      (if (odd? n)
	  (set! y (mod (* y x) d)))
      (set! n (bitwise-arithmetic-shift-right n 1))
      (if (positive? n)
	  (set! x (mod (* x x) d))))
    )
  (define-syntax align-size
    (syntax-rules (bit)
      ((_ (bit n))
       (let ((bitlen n))
	 (+ (bitwise-arithmetic-shift-right bitlen 3)
	    (if (zero? (bitwise-and bitlen 7)) 0 1))))
      ((_ n)
       (let ((bitlen (bitwise-length n)))
	 (+ (bitwise-arithmetic-shift-right bitlen 3)
	    (if (zero? (bitwise-and bitlen 7)) 0 1))))))

 )
