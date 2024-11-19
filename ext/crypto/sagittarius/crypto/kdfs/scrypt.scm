;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/kdfs/scrypt.scm - scrypt
;;;  
;;;   Copyright (c) 2024  Takashi Kato  <ktakashi@ymail.com>
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

;; ref: https://datatracker.ietf.org/doc/html/rfc7914

#!nounbound
(library (sagittarius crypto kdfs scrypt)
    (export scrypt
	    scrypt-ro-mix
	    scrypt-block-mix
	    salsa20/8)
    (import (rnrs)
	    (sagittarius crypto digests)
	    (sagittarius crypto mac)
	    (sagittarius crypto kdfs pbkdf-2)
	    (util bytevector)
	    (srfi :1 lists))

(define *hmac-sha256-prf* 
  (mac->prf-provider *mac:hmac* :digest *digest:sha-256*))

(define (scrypt P S N r p dk-len)
  (define (pbkdf2-hmac-sha256 P S c dk-len)
    (pbkdf-2 P S c dk-len :prf *hmac-sha256-prf*))
  (define size (* 128 r))
  (define buf (make-bytevector size))
  (do ((i 0 (+ i 1)) (B (pbkdf2-hmac-sha256 P S 1 (* p size))))
      ((= i p) (pbkdf2-hmac-sha256 P B 1 dk-len))
    (bytevector-copy! B (* i size) buf 0 size)
    (bytevector-copy! (scrypt-ro-mix buf N r) 0 B (* i size) size)))

(define (scrypt-ro-mix B N r)
  (define V (make-vector N))
  (define (integerify X r)
    (bytevector-u64-ref X (* (- (* r 2) 1) 64) (endianness little)))
  (define (step-2 V X N)
    (do ((i 0 (+ i 1)) (X X (scrypt-block-mix X r)))
	((= i N) (step-3 V X N))
      (vector-set! V i X)))
  (define (step-3 V X N)
    (let loop ((i 0) (X X))
      (if (= i N)
	  X
	  (let* ((j (mod (integerify X r) N))
		 (T (bytevector-xor X (vector-ref V j))))
	    (loop (+ i 1) (scrypt-block-mix T r))))))
  (step-2 V B N)) ;; step 1

(define (scrypt-block-mix B r)
  (define 2r (* 2 r))
  (define s (* 64 (- 2r 1)))
  (define (order Y)
    (let-values (((o e) (open-bytevector-output-port)))
      (let loop ((i 0) (Y Y) (odds '()))
	(cond ((null? Y)
	       (for-each (lambda (v) (put-bytevector o v)) (reverse! odds))
	       (e))
	      ((even? i)
	       (put-bytevector o (car Y))
	       (loop (+ i 1) (cdr Y) odds))
	      (else
	       (loop (+ i 1) (cdr Y) (cons (car Y) odds)))))))
  (let loop ((i 0) (X (bytevector-copy B s (+ s 64))) (Y '()))
    (if (= i 2r)
	(order (reverse! Y))
	(let* ((s (* i 64))
	       (T (bytevector-xor X (bytevector-copy B s (+ s 64))))
	       (X (salsa20/8 T)))
	  (loop (+ i 1) X (cons X Y))))))

;; B = 64 octets
(define (salsa20/8 B)
  (define (u32 bv i) (bytevector-u32-ref bv (* i 4) (endianness little)))
  (define (u32! bv i v) (bytevector-u32-set! bv (* i 4) v (endianness little)))
  (define x (bytevector-copy B))
  ;;  (((a) << (b)) | ((a) >> (32 - (b))))
  (define (R a b)
    (bitwise-and 
     (bitwise-ior
      (bitwise-arithmetic-shift-left a b)
      (bitwise-arithmetic-shift-right (bitwise-and a #xFFFFFFFF) (- 32 b)))
     #xFFFFFFFF))
  (define (^= bv i v) (u32! bv i (bitwise-xor (u32 bv i) v)))
  (define r u32)
  (do ((i 0 (+ i 2)))
      ((= i 8)
       (do ((i 0 (+ i 1)))
	   ((= i 16) B)
	 (u32! B i (bitwise-and (+ (u32 B i) (u32 x i)) #xFFFFFFFF))))
    (^= x  4 (R (+ (r x  0) (r x 12))  7)) (^= x  8 (R (+ (r x  4) (r x  0))  9))
    (^= x 12 (R (+ (r x  8) (r x  4)) 13)) (^= x  0 (R (+ (r x 12) (r x  8)) 18))

    (^= x  9 (R (+ (r x  5) (r x  1))  7)) (^= x 13 (R (+ (r x  9) (r x  5))  9))
    (^= x  1 (R (+ (r x 13) (r x  9)) 13)) (^= x  5 (R (+ (r x  1) (r x 13)) 18))

    (^= x 14 (R (+ (r x 10) (r x  6))  7)) (^= x  2 (R (+ (r x 14) (r x 10))  9))
    (^= x  6 (R (+ (r x  2) (r x 14)) 13)) (^= x 10 (R (+ (r x  6) (r x  2)) 18))

    (^= x  3 (R (+ (r x 15) (r x 11))  7)) (^= x  7 (R (+ (r x  3) (r x 15))  9))
    (^= x 11 (R (+ (r x  7) (r x  3)) 13)) (^= x 15 (R (+ (r x 11) (r x  7)) 18))
    
    (^= x  1 (R (+ (r x  0) (r x  3))  7)) (^= x  2 (R (+ (r x  1) (r x  0))  9))
    (^= x  3 (R (+ (r x  2) (r x  1)) 13)) (^= x  0 (R (+ (r x  3) (r x  2)) 18))
    
    (^= x  6 (R (+ (r x  5) (r x  4))  7)) (^= x  7 (R (+ (r x  6) (r x  5))  9))
    (^= x  4 (R (+ (r x  7) (r x  6)) 13)) (^= x  5 (R (+ (r x  4) (r x  7)) 18))
    
    (^= x 11 (R (+ (r x 10) (r x  9))  7)) (^= x  8 (R (+ (r x 11) (r x 10))  9))
    (^= x  9 (R (+ (r x  8) (r x 11)) 13)) (^= x 10 (R (+ (r x  9) (r x  8)) 18))
    
    (^= x 12 (R (+ (r x 15) (r x 14))  7)) (^= x 13 (R (+ (r x 12) (r x 15))  9))
    (^= x 14 (R (+ (r x 13) (r x 12)) 13)) (^= x 15 (R (+ (r x 14) (r x 13)) 18))
    ))
)
