;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; bytevector.scm - bytevector utility
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

(library (util bytevector)
    (export bytevector-xor
	    bytevector-xor!
	    bytevector-ior
	    bytevector-ior!
	    bytevector-and
	    bytevector-and!
	    bytevector-slices
	    bytevector-split-at*
	    ;; parity stuff
	    ->odd-parity
	    ->odd-parity!

	    bytevector->hex-string
	    bytevector-reverse!
	    bytevector-reverse
	    )
    (import (rnrs)
	    (sagittarius)
	    (sagittarius control))
(define (process-bytevector! op out . bvs)
  (let ((len (apply min (map bytevector-length bvs))))
    (dotimes (i len)
      (bytevector-u8-set! out i
			  (apply op
				 (map (^(bv) (bytevector-u8-ref bv i))
				      bvs))))
    out))

(define (bytevector-xor! out . bvs)
  (apply process-bytevector! bitwise-xor out bvs))

(define (bytevector-xor . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-xor! out bvs)))

(define (bytevector-ior! out . bvs)
  (apply process-bytevector! bitwise-ior out bvs))

(define (bytevector-ior . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-ior! out bvs)))

(define (bytevector-and! out . bvs)
  (apply process-bytevector! bitwise-and out bvs))

(define (bytevector-and . bvs)
  (let* ((len (apply min (map bytevector-length bvs)))
	 (out (make-bytevector len 0)))
    (apply bytevector-and! out bvs)))

(define (->odd-parity bv . args)
  (apply ->odd-parity! (bytevector-copy bv) args))

(define (->odd-parity! bv :optional (start 0) (end (bytevector-length bv)))
  (define (fixup b)
    (let ((parity (bitwise-bit-count b)))
      (if (even? parity)
	  (if (even? b)
	      (bitwise-ior b #x01)
	      (bitwise-and b #xFE))
	  b)))
  (do ((i start (+ i 1)))
      ((= i end) bv)
    (bytevector-u8-set! bv i (fixup (bytevector-u8-ref bv i)))))

;; analogy of slices and split-at* in (util list)
(define (bytevector-slices bv k . args)
  (let1 len (bytevector-length bv)
    (let loop ((bv bv) (r '()) (i 0))
      (if (< i len)
	  (let-values (((h t) (apply bytevector-split-at* bv k args)))
	    (loop t (cons h r) (+ i k)))
	  (reverse! r)))))

(define (bytevector-split-at* bv k :key (padding #f))
  (let1 len (bytevector-length bv)
    (if (< k len)
	(let ((r1 (bytevector-copy bv 0 k))
		 (r2 (bytevector-copy bv k)))
	     (values r1 r2))
	(values (if padding (padding bv) bv) #vu8()))))

(define (bytevector->hex-string bv)
  (define (fixup x)
    (if (= (string-length x) 1)
	(string-append "0" x)
	x))
  (string-concatenate (map (lambda (u8) (fixup (number->string u8 16)))
			   (bytevector->u8-list bv))))

)