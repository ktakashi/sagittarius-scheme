;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; bytevector.scm - bytevector utility
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

(library (util bytevector)
    (export bytevector-xor
	    bytevector-xor!
	    bytevector-ior
	    bytevector-ior!
	    bytevector-and
	    bytevector-and!
	    ;; parity stuff
	    ->odd-parity
	    ->odd-parity!
	    )
    (import (rnrs) (sagittarius control) (shorten))
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

)