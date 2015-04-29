;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; math/modulus-check-digit.scm - Modulus check digit framework
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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


;; this library provides frame work of modulus check digit.
;; users need to provide code converter which needs convert
;; given code to a list of integer, weight calculator which
;; calculates the weight of a code point which is added the
;; total sum and modulus which is used to calculate modulo
;; of the sum.
;; how to use: see sitelib/math/luhn.scm
(library (math modulus-check-digit)
    (export define-check-digit)
    (import (rnrs))

  (define-syntax define-check-digit
    (syntax-rules ()
      ((_ validate calculate modulus weight converter)
       (begin
	 (define %c converter)
	 (define %w weight)
	 (define (validate code)
	   (zero? (%calculate (%c code) #f modulus %w)))
	 (define (calculate code)
	   (let ((n (%calculate (%c code) #t modulus %w)))
	     (mod (- modulus n) modulus)))))))

  (define (%calculate code calc? modulus weight)
    (define (sum-them n* off)
      (define lth (+ (length n*) off))
      (let loop ((i 0) (n* n*) (sum 0))
	(if (null? n*)
	    sum
	    (loop (+ i 1) (cdr n*)
		  (+ sum (weight (car n*) (+ i 1) (- lth i)))))))
    (let ((sum (sum-them code (if calc? 1 0))))
      (mod sum modulus)))

  )
