;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a143/fixnums.scm - Fixnums
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

(library (srfi :143 fixnums)
    (export fx-width fx-greatest fx-least
	    fixnum? fx=? fx<? fx>? fx<=? fx>=? fxzero?
	    fxpositive? fxnegative? fxodd? fxeven? fxmax fxmin
	    fx+ fx- fx* fx+/carry fx-/carry fx+*/carry
	    fxnot fxand fxior fxxor
	    fxarithmetic-shift fxarithmetic-shift-left fxarithmetic-shift-right
	    fxbit-count fxlength fxif
	    fxbit-field
	    ;; non R6RS
	    fxabs fxsqrt fxneg fxsquare fxquotient fxremainder
	    ;; something is different from R6RS
	    fxbit-set? fxcopy-bit fxbit-field-rotate fxbit-field-reverse
	    fxfirst-set-bit)
    (import (rename (except (rnrs) fxcopy-bit)
		    (fxbit-set? r6rs:fxbit-set?)
		    (fxbit-count r6rs:fxbit-count)
		    (fxreverse-bit-field fxbit-field-reverse)
		    (fxfirst-bit-set fxfirst-set-bit))
	    (only (rnrs r5rs) quotient remainder))

  (define fx-width    (fixnum-width))
  (define fx-least    (least-fixnum))
  (define fx-greatest (greatest-fixnum))

  (define (fxabs i)
    (unless (fixnum? i) (assertion-violation 'fxabs "fixnum required" i))
    (if (fxnegative? i) (fx- i) i))
  (define (fxsqrt i)
    (unless (fixnum? i) (assertion-violation 'fxsqrt "fixnum required" i))
    (exact-integer-sqrt i))
  (define (fxneg i) (fx- i))
  (define (fxsquare x) (fx* x x))
  (define (fxquotient i j)
    (unless (and (fixnum? i) (fixnum? j))
      (assertion-violation 'fxquotient "fixnum required" i j))
    (quotient i j))
  (define (fxremainder i j)
    (unless (and (fixnum? i) (fixnum? j))
      (assertion-violation 'fxremainder "fixnum required" i j))
    (remainder i j))

  (define (fxbit-count i)
    (unless (fixnum? i) (assertion-violation 'fxbit-count "fixnum required" i))
    (if (negative? i)
	(r6rs:fxbit-count (fxnot i))
	(r6rs:fxbit-count i)))
  (define (fxcopy-bit index i boolean)
    (unless (fixnum? i) (assertion-violation 'fxcopy-bit "fixnum required" i))
    (unless (<= index fx-width)
      (assertion-violation 'fxcopy-bit "index is bigger than fixnum width" i))
    (if boolean
	(fxior i (fxarithmetic-shift-left 1 index))
	(fxand i (fxnot (fxarithmetic-shift-left 1 index)))))
  (define (fxbit-set? index i)
    (r6rs:fxbit-set? i index))

  (define (fxbit-field-rotate i count start end)
    (fxrotate-bit-field i start end count))
)	    
