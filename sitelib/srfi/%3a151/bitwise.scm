;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a151/bitwise.scm - Bitwise Operations
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

(library (srfi :151 bitwise)
    (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
	    bitwise-nand bitwise-nor bitwise-andc1 bitwise-andc2
	    bitwise-orc1 bitwise-orc2
	    bit-count bitwise-if
	    bit-set? copy-bit bit-swap any-bit-set? every-bit-set?
	    bit-field-any? bit-field-every? bit-field-clear
	    bit-field-set
	    bit-field-replace bit-field-replace-same
	    bit-field-rotate
	    bits->list list->bits bits->vector vector->bits bits
	    bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator

	    (rename (bitwise-arithmetic-shift arithmetic-shift)
		    (bitwise-length integer-length)
		    (bitwise-first-bit-set first-set-bit)
		    (bitwise-bit-field bit-field)
		    (bitwise-reverse-bit-field bit-field-reverse)))
    (import (rnrs)
	    (only (srfi :1) reverse!))

(define (bit-count i)
  (if (>= i 0)
      (bitwise-bit-count i)
      (bitwise-bit-count (bitwise-not i))))

;; TODO should we implement this in C code for performance?
(define (bitwise-nand  i j)  (bitwise-not (bitwise-and i j)))
(define (bitwise-nor   i j)  (bitwise-not (bitwise-ior i j)))
(define (bitwise-andc1 i j)  (bitwise-and (bitwise-not i) j))
(define (bitwise-andc2 i j)  (bitwise-and i (bitwise-not j)))
(define (bitwise-orc1  i j)  (bitwise-ior (bitwise-not i) j))
(define (bitwise-orc2  i j)  (bitwise-ior i (bitwise-not j)))

(define (bitwise-eqv . i*)
  (do ((ans -1 (bitwise-not (bitwise-xor ans (car i*))))
       (i* i* (cdr i*)))
      ((null? i*) ans)))

(define (bit-set? index i) (bitwise-bit-set? i index))
(define (copy-bit index i boolean)
  (if boolean
      (bitwise-ior i (bitwise-arithmetic-shift 1 index))
      (bitwise-and i (bitwise-not (bitwise-arithmetic-shift 1 index)))))

(define (bit-swap index1 index2 i)
  (let ((n1 (bit-set? index1 i))
	(n2 (bit-set? index2 i)))
    (copy-bit index2 (copy-bit index1 i n2) n1)))

(define (any-bit-set? test-bits i) (not (zero? (bitwise-and test-bits i))))
(define (every-bit-set? test-bits i) (= test-bits (bitwise-and test-bits i)))

(define (mask start end)
  (bitwise-not (bitwise-arithmetic-shift -1 (- end start))))
(define (bit-field-any? i start end)
  (not (zero? (bitwise-and (bitwise-arithmetic-shift i (- start))
			   (mask start end)))))
(define (bit-field-every? i start end)
  (let ((m (mask start end)))
    (eqv? m (bitwise-and (bitwise-arithmetic-shift i (- start)) m))))
(define (bit-field-replace i new start end)
  (bitwise-copy-bit-field i start end new))
(define (bit-field-clear i start end) (bitwise-copy-bit-field i start end 0))
(define (bit-field-set i start end)  (bitwise-copy-bit-field i  start end -1))
(define (bit-field-replace-same dest source start end)
  (bitwise-if (bitwise-arithmetic-shift (mask start end) start) source dest))

(define (bit-field-rotate i count start end)
  (bitwise-rotate-bit-field i start end count))

;; constructors
(define (bits->list k . len)
  (if (null? len)
      (do ((k k (bitwise-arithmetic-shift k -1))
	   (lst '() (cons (odd? k) lst)))
	  ((<= k 0) (reverse! lst)))
      (do ((idx (+ -1 (car len)) (+ -1 idx))
	   (k k (bitwise-arithmetic-shift k -1))
	   (lst '() (cons (odd? k) lst)))
	  ((< idx 0) (reverse! lst)))))
  
(define (list->bits lis)
  (do ((bs (reverse lis) (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))
(define (bits->vector i . len) (list->vector (apply bits->list i len)))
(define (vector->bits vector . opt)
  (list->bits (apply vector->list vector opt)))
(define (bits . bool) (list->bits bool))

(define (bitwise-fold proc seed i)
  (do ((len (bitwise-length i))
       (n 0 (+ n 1))
       (r seed (proc (bit-set? n i) r)))
      ((= n len) r)))
(define (bitwise-for-each proc i)
  (do ((len (bitwise-length i))
       (n 0 (+ n 1)))
      ((= n len) (if #f #t))
    (proc (bit-set? n i))))

(define (bitwise-unfold stop? mapper successor seed)
  (do ((n 0 (+ n 1))
       (state seed (successor state))
       (result 0 (copy-bit n result (mapper state))))
      ((stop? state) result)))
(define (make-bitwise-generator i)
  (lambda ()
    (let ((bit (bit-set? 0 i)))
      (set! i (bitwise-arithmetic-shift i -1))
      bit)))

)
