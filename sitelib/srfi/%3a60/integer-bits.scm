;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; integer-bits.scm - SRFI-60 Integers as Bits
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (srfi :60 integer-bits)
    (export (rename (bitwise-and logand)
		    (bitwise-ior logior)
		    (bitwise-xor logxor)
		    (bitwise-not lognot)
		    (bitwise-if  bitwise-merge)
		    
		    (bitwise-first-bit-set log2-binary-factors)
		    (bitwise-first-bit-set first-set-bit)
		    (bitwise-bit-field bit-field)
		    (bitwise-arithmetic-shift ash)
		    (bitwise-arithmetic-shift arithmetic-shift)
		    (bitwise-reverse-bit-field reverse-bit-field))
	    bitwise-and
	    bitwise-ior
	    bitwise-xor
	    bitwise-not
	    bitwise-if
	    integer-length
	    logtest
	    (rename (logtest any-bits-set?))
	    logcount
	    (rename (logcount bit-count))
	    logbit?
	    (rename (logbit? bit-set?))
	    copy-bit
	    copy-bit-field
	    rotate-bit-field
	    integer->list
	    list->integer
	    booleans->integer
	    )
    (import (rnrs)
	    (only (core) integer-length)
	    (srfi :42 eager-comprehensions))

  (define (logtest j k) (not (zero? (bitwise-and j k))))
  (define (logcount n)
    (if (negative? n)
	(bitwise-not (bitwise-bit-count n))
	(bitwise-bit-count n)))
  ;; the argument order is not the same as R6RS...
  (define (logbit? index n) (bitwise-bit-set? n index))
  (define (copy-bit index from bit) (bitwise-copy-bit from index (if bit 1 0)))
  (define (copy-bit-field to from start end)
    (bitwise-copy-bit-field to start end from))
  (define (rotate-bit-field n count start end)
    (bitwise-rotate-bit-field n start end count))

  ;; bits as booleans
  (define (integer->list k :optional (len (integer-length k)))
    (list-ec (: i (- len 1) -1 -1) (bitwise-bit-set? k i)))

  ;; From Gauche
  (define (list->integer lis)
    (cond ((null? lis) 0)
	  ((<= (length lis) (integer-length (greatest-fixnum)))
	   ;; fixnum range
	   (fold-left (lambda (v bit) (+ v v (if bit 1 0))) 0 lis))
	  (else
	   ;; bignum rage
	   (let loop ((i 0) (bits (reverse lis)) (acc '()))
	     (if (null? bits)
		 (apply bitwise-ior acc)
		 (loop (+ i 1) (cdr bits) 
		       (if (car bits) (cons (expt 2 i) acc) acc)))))))

  (define (booleans->integer . bools) (list->integer bools))

)
