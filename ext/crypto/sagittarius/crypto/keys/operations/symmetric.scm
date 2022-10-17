;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/keys/operations/symmetric.scm - Symmetric key operations
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (sagittarius crypto keys operations symmetric)
    (export generate-symmetric-key

	    make-rfc3394-key-wrap make-rfc3394-key-unwrap
	    make-aes-key-wrap make-aes-key-unwrap
	    make-camellia-key-wrap make-camellia-key-unwrap
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto keys types)
	    (sagittarius crypto random)
	    (sagittarius crypto descriptors)
	    (sagittarius crypto secure))
(define-generic generate-symmetric-key)
(define-method generate-symmetric-key ((cipher <block-cipher-descriptor>))
  (generate-symmetric-key cipher (secure-random-generator *prng:chacha20*)))
(define-method generate-symmetric-key ((cipher <block-cipher-descriptor>)
				       (prng <random-generator>))
  (let ((size (block-cipher-descriptor-suggested-key-length cipher)))
    (make-symmetric-key (random-generator-read-random-bytes prng size))))
(define-method generate-symmetric-key ((cipher <block-cipher-descriptor>)
				       (bv <bytevector>))
  (make-symmetric-key bv))

;; key wrap
(define +default-iv+ #vu8(#xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6 #xa6))

(define (make-rfc3394-key-wrap scheme key :key ((iv bytevector?) +default-iv+))
  ;; it's ECB, so we can reuse without rest ;)
  (define cipher (mode-start *mode:ecb* scheme (symmetric-key-value key) #f))
  (define iv-length (bytevector-length iv))
  ;; 2.2.1 Key Wrap
  (lambda (pt)
    (define pt-len (bytevector-length pt))
    (define n (div pt-len 8))
    (unless (zero? (mod pt-len 8))
      (assertion-violation 'rfc3394-key-wrap
			   "wrapping data length must be multiple of 8"))
    (let ((block (make-bytevector (+ pt-len iv-length)))
	  (buf   (make-bytevector (+ 8 iv-length))))
      (bytevector-copy! iv 0 block 0 iv-length)
      (bytevector-copy! pt 0 block iv-length pt-len)
      (do ((i 0 (+ i 1)))
	  ((= i 6) block)
	(do ((j 1 (+ j 1)))
	    ((> j n))
	  (bytevector-copy! block 0 buf 0 iv-length)
	  (bytevector-copy! block (* j 8) buf iv-length 8)
	  (mode-encrypt! cipher buf 0 buf 0 (bytevector-length buf))
	  (do ((k 1 (+ k 1))
	       (t (+ (* n i) j) (bitwise-arithmetic-shift t -8)))
	      ((zero? t))
	    (let* ((v (bitwise-and t #xFF))
		   (p (- iv-length k))
		   (b (bytevector-u8-ref buf p)))
	      (bytevector-u8-set! buf p (bitwise-xor b v))))
	  (bytevector-copy! buf 0 block 0 8)
	  (bytevector-copy! buf 8 block (* 8 j) 8))))))

(define (make-rfc3394-key-unwrap scheme key :key ((iv bytevector?) +default-iv+))
  ;; Because the cipher is ECB, it's thread safe
  (define cipher (mode-start *mode:ecb* scheme (symmetric-key-value key) #f))
  (define iv-length (bytevector-length iv))
  (lambda (ct)
    (define ct-len (bytevector-length ct))
    (define n (div ct-len 8))
    (define (check a iv)
      (unless (safe-bytevector=? a iv)
	(error 'rfc3394-key-unwrap "Invalid cipher text")))
    (unless (zero? (mod ct-len 8))
      (assertion-violation 'rfc3394-key-unwrap
			   "unwrapping data length must be multiple of 8"))
    (let ((block (make-bytevector (- ct-len iv-length)))
	  (a     (make-bytevector iv-length))
	  (buf   (make-bytevector (+ 8 iv-length)))
	  (n     (- n 1)))
      (bytevector-copy! ct 0 a 0 iv-length)
      (bytevector-copy! ct iv-length block 0 (- ct-len iv-length))
      (do ((i 5 (- i 1)))
	  ((< i 0) (check a iv) block)
	(do ((j n (- j 1)))
	    ((= j 0))
	  (bytevector-copy! a 0 buf 0 iv-length)
	  (bytevector-copy! block (* (- j 1) 8) buf iv-length 8)
	  (do ((k 1 (+ k 1))
	       (t (+ (* n i) j) (bitwise-arithmetic-shift t -8)))
	      ((zero? t))
	    (let* ((v (bitwise-and t #xFF))
		   (p (- iv-length k))
		   (b (bytevector-u8-ref buf p)))
	      (bytevector-u8-set! buf p (bitwise-xor b v))))
	  (mode-decrypt! cipher buf 0 buf 0 (bytevector-length buf))
	  (bytevector-copy! buf 0 a 0 8)
	  (bytevector-copy! buf 8 block (* 8 (- j 1)) 8))))))

(define (make-aes-key-wrap key . rest)
  (apply make-rfc3394-key-wrap *scheme:aes* key rest))
(define (make-aes-key-unwrap key . rest)
  (apply make-rfc3394-key-unwrap *scheme:aes* key rest))
(define (make-camellia-key-wrap key . rest)
  (apply make-rfc3394-key-wrap *scheme:camellia* key rest))
(define (make-camellia-key-unwrap key . rest)
  (apply make-rfc3394-key-unwrap *scheme:camellia* key rest))
)
