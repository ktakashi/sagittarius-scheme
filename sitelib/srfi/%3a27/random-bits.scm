;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; random-bits.scm - implementation of SRFI-27
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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
#!compatible

(library (srfi :27 random-bits)
    (export random-integer
	    random-real
	    default-random-source
	    make-random-source
	    random-source?
	    random-source-state-ref
	    random-source-state-set!
	    random-source-randomize!
	    random-source-pseudo-randomize!
	    random-source-make-integers
	    random-source-make-reals)
    (import (rnrs)
	    (math)
	    (math mt-random))
  ;; default random source is mt-random without seed
  (define default-random-source (pseudo-random MT))
  ;; we supports other random number generators, so take optional arguments
  (define (make-random-source :key (prng MT) :allow-other-keys rest)
    (apply pseudo-random prng rest))
  (define (random-source? obj) (prng? obj))
  ;; states
  ;; if given ransom-source is implementes <secure-random> then, we don't
  ;; return state. well, this is just an excuse not to return state for
  ;; builtin prngs...
  (define (random-source-state-ref s)
    (unless (random-source? s)
      (assertion-violation 'random-source-state-ref
			   "random-source required" s))
    (prng-state s))
  (define (random-source-state-set! s state)
    (unless (random-source? s)
      (assertion-violation 'random-source-state-ref
			   "random-source required" s))
    (prng-state s state))
  
  (define (random-source-randomize! s)
    (unless (random-source? s)
      (assertion-violation 'random-source-randomize!
			   "random-source required" s))
    ;; well mt-random uses 64 bit seed, so read 16 bytes from system random
    (let ((seed (read-sys-random 16)))
      (random-seed-set! s seed)))

  ;; implementation bases on Gauche
  (define (random-source-pseudo-randomize! s i j)
    ;; This procedure is effectively required to map integers (i,j) into
    ;; a seed value in a deterministic way.

    ;; interleave-i and interleave-j creates a list of integers, each
    ;; is less than 2^64, consisted by interleaving each 64-bit
    ;; chunk of i and j.
    (define (interleave-i i j lis)
      (if (zero? i)
	  (if (zero? j) lis (interleave-j 0 j (cons 0 lis)))
	  (receive (q r) (div-and-mod i #x10000000000000000)
	    (interleave-j q j (cons r lis)))))
    
    (define (interleave-j i j lis)
      (if (zero? j)
	  (if (zero? i) lis (interleave-i i 0 (cons 0 lis)))
	  (receive (q r) (div-and-mod j #x10000000000000000)
	    (interleave-i i q (cons r lis)))))

    (define (u64-list->bytevector lis)
      (let* ((len (length lis))
	     (bv  (make-bytevector (* len 8))))
	(let loop ((i 0) (l lis))
	  (cond ((null? l) bv)
		(else
		 (bytevector-u64-native-set! bv i (car l))
		 (loop (+ i 8) (cdr l)))))))

    (unless (random-source? s)
      (assertion-violation 'random-source-pseudo-randomize!
			   "random-source required" s))
    ;; to avoid useless calculation, we check if given source supports
    ;; prng-state
    (if (prng-state s)
	(prng-state 
	 s (u64-list->bytevector (interleave-i i j '(#xFFFFFFFFFFFFFFFF))))
	;; well, i don't know if this is correct or not...
	#f))

  ;; obtain generators from random source
  (define (random-source-make-integers s)
    (unless (random-source? s)
      (assertion-violation 'random-source-make-integers
			   "random-source required" s))
    (lambda (n) (random s n)))

  (define (random-real-helper s)
    ;; for mt-random, we only read 8 bytes, should be enough, right?
    (let ((x (bytevector->integer (read-random-bytes s 8))))
      (* (bitwise-arithmetic-shift-right x 11)
	 (/ 1.0 9007199254740992.0))))

  (define (random-source-make-reals s . maybe-uint)
    (unless (random-source? s)
      (assertion-violation 'random-source-make-reals
			   "random-source required" s))
    (if (null? maybe-uint)
	(lambda () (random-real-helper s))
	(let ((uint (car maybe-uint)))
	  (unless (< 0 uint 1)
	    (assertion-violation 'random-source-make-reals
				 "uint must be between 0.0 and 1.0 (exclusive)"
				 uint))
	  (let* ((1/uint (inexact (/ uint)))
		 (range  (exact (ceiling 1/uint))))
	    (lambda ()
	      (/ (random s range) 1/uint))))))

  ;; random generators
  (define (random-integer n)
    (random default-random-source n))

  (define (random-real)
    (random-real-helper default-random-source))
)