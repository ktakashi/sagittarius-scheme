;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/math/ec/fields.scm - EC fields
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

#!nounbound
(library (sagittarius crypto math ec fields)
    (export ec-field-fp? make-ec-field-fp
	    ec-field-fp-p

	    ec-field-f2m? make-ec-field-f2m
	    ec-field-f2m-m
	    ec-field-f2m-k1
	    ec-field-f2m-k2
	    ec-field-f2m-k3

	    f2m-ppb? f2m-add f2m-mul
	    f2m-div f2m-square f2m-inverse)
    (import (rnrs)
	    (core misc))
;;; Finite field
;; Fp
(define-vector-type ec-field-fp (make-ec-field-fp p) ec-field-fp?
  (p ec-field-fp-p))

;; F2m
;; TODO check valid reduction polynominal
;;      It must be either trinominal (X^m + X^k + 1 with m > k >= 1) or
;;      pentanominal (X^m X^k3 + X^k2 + X^k1 + 1 with m > k3 > k2 > k1 >= 1)
(define-vector-type ec-field-f2m (make-ec-field-f2m m k1 k2 k3) ec-field-f2m?
  (m  ec-field-f2m-m)
  (k1 ec-field-f2m-k1)
  (k2 ec-field-f2m-k2)
  (k3 ec-field-f2m-k3))

;; F2m field operations
(define (f2m-ppb? f)
  (not (or (zero? (ec-field-f2m-k2 f)) (zero? (ec-field-f2m-k3 f)))))
(define (f2m-add field x y)
  (if (zero? y)
      x
      (bitwise-xor x y)))

(define (f2m-mul field x y)
  (define ax x)
  (define bx y)
  (define cz (if (bitwise-bit-set? ax 0) bx 0))
  (define m  (ec-field-f2m-m field))
  (define mm (bitwise-arithmetic-shift-left 1 (ec-field-f2m-m field)))
  (define k1m (bitwise-arithmetic-shift-left 1 (ec-field-f2m-k1 field)))
  (define k2m (bitwise-arithmetic-shift-left 1 (ec-field-f2m-k2 field)))
  (define k3m (bitwise-arithmetic-shift-left 1 (ec-field-f2m-k3 field)))
  (define pbp? (f2m-ppb? field))

  (define (mult-z-mod pbp? m mm k1m k2m k3m a)
    (define az (* a 2))
    (if (bitwise-bit-set? az m)
	(let* ((bl (bitwise-length az))
	       (bm (- (- (bitwise-arithmetic-shift-left 1 bl) 1) mm))
	       (cm 1)
	       (r (bitwise-and az bm)))
	  (if pbp?
	      (bitwise-xor r cm k1m k2m k3m)
	      (bitwise-xor r cm k1m)))
	az))
  
  (do ((i 1 (+ i 1))
       (bx (mult-z-mod pbp? m mm k1m k2m k3m bx)
	   (mult-z-mod pbp? m mm k1m k2m k3m bx))
       (cz cz (if (bitwise-bit-set? ax i)
		  (bitwise-xor cz bx)
		  cz)))
      ((= i m) cz)))

(define (f2m-div field x y) (f2m-mul field x (f2m-inverse field y)))
(define (f2m-square field x) (f2m-mul field x x))
(define (f2m-inverse field x)
  (define m  (ec-field-f2m-m field))
  (define k1 (ec-field-f2m-k1 field))
  (define k2 (ec-field-f2m-k2 field))
  (define k3 (ec-field-f2m-k3 field))
    (define uz x)
    (define vz
      (let ((ppb? (f2m-ppb? field)))
	(bitwise-ior (bitwise-arithmetic-shift-left 1 m)
		     1
		     (bitwise-arithmetic-shift-left 1 k1)
		     (if ppb? (bitwise-arithmetic-shift-left 1 k2) 0)
		     (if ppb? (bitwise-arithmetic-shift-left 1 k3) 0))))
    (when (<= uz 0)
      (assertion-violation 'f2m-inverse "x is zero or negative" x))
    (let loop ((uz uz) (vz vz) (g1z 1) (g2z 0))
      (if (= uz 0)
	  g2z
	  (let ((j (- (bitwise-length uz) (bitwise-length vz))))
	    (let-values (((uz vz g1z g2z j)
			  (if (< j 0)
			      (values vz uz g2z g1z (- j))
			      (values uz vz g1z g2z j))))
	      (loop (bitwise-xor uz (bitwise-arithmetic-shift-left vz j))
		    vz
		    (bitwise-xor g1z (bitwise-arithmetic-shift-left g2z j))
		    g2z))))))
)
