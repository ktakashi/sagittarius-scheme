;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a144/flonums.scm - Flonums
;;;
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :144 flonums)
    (export fl-e
	    fl-1/e
	    fl-e-2
	    fl-e-pi/4
	    fl-log2-e
	    fl-log10-e
	    fl-log-2
	    fl-1/log-2
	    fl-log-3
	    fl-log-pi
	    fl-log-10
	    fl-1/log-10
	    fl-pi
	    fl-1/pi
	    fl-2pi
	    fl-pi/2
	    fl-pi/4
	    fl-2/sqrt-pi
	    fl-pi-squared
	    fl-degree
	    fl-2/pi
					;  fl-2/sqrt-pi    ; FIXME: duplicate
	    fl-sqrt-2
	    fl-sqrt-3
	    fl-sqrt-5
	    fl-sqrt-10
	    fl-1/sqrt-2
	    fl-cbrt-2
	    fl-cbrt-3
	    fl-4thrt-2
	    fl-phi
	    fl-log-phi
	    fl-1/log-phi
	    fl-euler
	    fl-e-euler
	    fl-sin-1
	    fl-cos-1
	    fl-gamma-1/2
	    fl-gamma-1/3
	    fl-gamma-2/3

	    ;; Implementation Constants

	    fl-greatest
	    fl-least
	    fl-epsilon
	    fl-fast-fl+*
	    fl-integer-exponent-zero
	    fl-integer-exponent-nan

	    ;; Constructors

	    flonum
	    fladjacent
	    flcopysign
	    make-flonum

	    ;; Accessors

	    flinteger-fraction
	    flexponent
	    flinteger-exponent
	    flnormalized-fraction-exponent
	    flsign-bit

	    ;; Predicates

	    flonum?
	    fl=?
	    fl<?
	    fl>?
	    fl<=?
	    fl>=?
	    flunordered?
	    flmax
	    flmin
	    flinteger?
	    flzero?
	    flpositive?
	    flnegative?
	    flodd?
	    fleven?
	    flfinite?
	    flinfinite?
	    flnan?
	    flnormalized?
	    fldenormalized?

	    ;; Arithmetic

	    fl+
	    fl*
	    fl+*
	    fl-
	    fl/
	    flabs
	    flabsdiff
	    flposdiff
	    flsgn
	    flnumerator
	    fldenominator
	    flfloor
	    flceiling
	    flround
	    fltruncate

	    ;; Exponents and logarithsm

	    flexp
	    flexp2
	    flexp-1
	    flsquare
	    flsqrt
	    flcbrt
	    flhypot
	    flexpt
	    fllog
	    fllog1+
	    fllog2
	    fllog10
	    make-fllog-base

	    ;; Trigonometric functions

	    flsin
	    flcos
	    fltan
	    flasin
	    flacos
	    flatan
	    flsinh
	    flcosh
	    fltanh
	    flasinh
	    flacosh
	    flatanh

	    ;; Integer division

	    flquotient
	    flremainder
	    flremquo

	    ;; Special functions

	    flgamma
	    flloggamma
	    flfirst-bessel
	    flsecond-bessel
	    flerf
	    flerfc)
    (import (rnrs base)
	    (rename (sagittarius flonums)
		    (flmax s:flmax)
		    (flmin s:flmin)))
(define (flmax . args)
  (if (null? args)
      -inf.0
      (apply s:flmax args)))

(define (flmin . args)
  (if (null? args)
      +inf.0
      (apply s:flmin args)))
)

