;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/math/modular.scm - Modular arithmetic
;;;
;;;  Copyright (c) 2021-2022 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#!nounbound
(library (sagittarius crypto math modular)
    (export mod-inverse
	    mod-expt
	    mod-add
	    mod-sub
	    mod-mul
	    mod-div
	    mod-square
	    mod-negate
	    mod-sqrt
	    )
    (import (rnrs)
	    (sagittarius))

;; modular arithmetic
;; a + b (mod p)
(define (mod-add a b p) (mod (+ a b) p))
;; a - b (mod p)
(define (mod-sub a b p) (mod (- (+ a p) b) p))
;; a * b (mod p)
(define (mod-mul a b p) (mod (* a b) p))
;;(define (mod-mul a b p) (* (mod a p) (mod b p)))
;; a / b (mod p)
(define (mod-div a b p) (mod-mul a (mod-inverse b p) p))
;; a^2 (mod p)
(define (mod-square a p) (mod-expt a 2 p))
;; -a (mod p)
(define (mod-negate a p) (mod (- p a) p))
;; FWIW
#|
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
|#
;; mod-inverse is defined in (sagittarius)
;; mod-expt is defined in (sagittarius)

;; This only works for prime number (for now)
;; https://www.rieselprime.de/ziki/Modular_square_root
(define (mod-sqrt x p)
  (define (sqrt4k3 x p) (mod-expt x (div (+ p 1) 4) p))
  (define (sqrt8k5 x p)
    (let ((y (mod-expt x (div (+ p 3) 8) p)))
      (if (= (mod (* y y) p) (mod x p))
	  y
	  (let ((z (mod-expt 2 (div (- p 1) 4) p)))
	    (mod (* y z) p)))))
  ;; Tonelli Shanks
  (define (sqrt8k1 x p)
    (define (legendre-symbol x p)
      (mod-expt x (bitwise-arithmetic-shift-right (- p 1) 1) p))
    (define (choose-b p)
      (do ((b 2 (+ b 1)) (try 1 (+ try 1)))
	  ((not (= (legendre-symbol b p) 1)) b)))
    (define (tonelli-shanks x k p b 1/b)
      (define (compute-x^m x m p k)
	(let loop ((m m) (x^m 1) (k k))
	  (if (and (even? m) (= x^m 1))
	      (let ((m (bitwise-arithmetic-shift-right m 1)))
		(loop m (mod-expt x m p) (+ k 1)))
	      (values m x^m k))))
      (let ((m (bitwise-arithmetic-shift-right (- p 1) k)))
	(let-values (((m x^m k) (compute-x^m x m p k)))
	  (if (= x^m (- p 1))
	      (let* ((bp (bitwise-arithmetic-shift-left 1 (- k 1)))
		     (bp/2 (bitwise-arithmetic-shift-left 1 (- k 2)))
		     (xn (mod (* x (mod-expt b bp p)) p))
		     (xnr (tonelli-shanks xn k p b 1/b))
		     (xr (* xnr (mod-expt 1/b bp/2 p))))
		(mod xr p))
	      (mod-expt x (bitwise-arithmetic-shift-right (+ m 1) 1) p)))))
    (and (= (legendre-symbol x p) 1)
	 (let* ((b (choose-b p))
		(1/b (mod-inverse b p)))
	   (tonelli-shanks x 1 p b 1/b))))
  (let ((y (mod (cond ((= (mod p 4) 3) (sqrt4k3 x p))
		      ((= (mod p 8) 5) (sqrt8k5 x p))
		      ((= (mod p 8) 1) (sqrt8k1 x p))
		      (else
		       (assertion-violation 'mod-sqrt "Not implemented")))
		p)))
    (and y (= x (mod-square y p)) y)))

)
