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
(define (mod-div a b p) (mod (* a (mod-inverse b p)) p))
;; a^2 (mod p)
(define (mod-square a p) (mod-expt a 2 p))
;; -a (mod p)
(define (mod-negate a p) (mod (- p a) p))
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
  (let ((y (mod (cond ((= (mod p 4) 3) (sqrt4k3 x p))
		      ((= (mod p 8) 5) (sqrt8k5 x p))
		      ;; TODO 8m+1
		      (else
		       (assertion-violation 'mod-sqrt "Not implemented")))
		p)))
    (and (= x (mod-square y p)) y)))

)
