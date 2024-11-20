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
	    (sagittarius crypto logic salsa)
	    (util bytevector)
	    (srfi :1 lists))

(define *hmac-sha256-prf* 
  (mac->prf-provider *mac:hmac* :digest *digest:sha-256*))

(define (scrypt P S N r p dk-len)
  (define (pbkdf2-hmac-sha256 P S c dk-len)
    (pbkdf-2 P S c dk-len :prf *hmac-sha256-prf*))
  (define size (* 128 r))
  (define buf (make-bytevector size))
  (unless (and (> N 1) (= (bitwise-bit-count N) 1))
    (assertion-violation 'scrypt
     "Cost parameter must be greater than 1 and power of 2" N))
  (do ((i 0 (+ i 1)) (B (pbkdf2-hmac-sha256 P S 1 (* p size))))
      ((= i p) (pbkdf2-hmac-sha256 P B 1 dk-len))
    (bytevector-copy! B (* i size) buf 0 size)
    (bytevector-copy! (scrypt-ro-mix buf N r) 0 B (* i size) size)))

(define (scrypt-ro-mix B N r)
  (define V (make-vector N))
  (define index (* (- (* r 2) 1) 64))
  (define (integerify X) (bytevector-u64-ref X index (endianness little)))
  (define (step-2 V X N)
    (do ((i 0 (+ i 1)) (X X (scrypt-block-mix X r)))
	((= i N) (step-3 V X N))
      (vector-set! V i X)))
  (define (step-3 V X N)
    (let loop ((i 0) (X X))
      (if (= i N)
	  X
	  (let* ((j (mod (integerify X) N))
		 (T (bytevector-xor X (vector-ref V j))))
	    (loop (+ i 1) (scrypt-block-mix T r))))))
  (step-2 V B N)) ;; step 1

(define (scrypt-block-mix B r)
  (define 2r (* 2 r))
  (define s (* 64 (- 2r 1)))
  (define buf (make-bytevector 64))
  (define (fill-buffer! B s)
    (bytevector-copy! B s buf 0 64)
    buf)
  (let loop ((i 0) (X (bytevector-copy B s (+ s 64))) (Ye '()) (Yo '()))
    (if (= i 2r)
	(bytevector-concatenate (append! (reverse! Ye) (reverse! Yo)))
	(let* ((s (* i 64))
	       (T (bytevector-xor X (fill-buffer! B s)))
	       (X (salsa20/8 T))
	       (e? (even? i)))
	  (loop (+ i 1) X (if e? (cons X Ye) Ye) (if e? Yo (cons X Yo)))))))

;; B = 64 octets
(define (salsa20/8 B) (salsa-core! B 8))
)
