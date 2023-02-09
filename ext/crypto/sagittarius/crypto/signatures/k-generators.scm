;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/k-generators.scm - K generator
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
(library (sagittarius crypto signatures k-generators)
    (export make-random-k-generator default-k-generator
	    make-hmac-k-generator)
    (import (rnrs)
	    (core errors)
	    (sagittarius)
	    (sagittarius crypto random)
	    (sagittarius crypto digests)
	    (sagittarius crypto mac))
(define ((make-random-k-generator (prng random-generator?)) n d M)
  (define (read-random-bits prng buf)
    (random-generator-read-random-bytes! prng buf)
    (bytevector->uinteger buf))
  (let* ((bits (bitwise-length n))
	 (buf (make-bytevector (div bits 8))))
    (do ((r (read-random-bits prng buf) (read-random-bits prng buf)))
	((and (not (zero? r)) (< r n)) r))))

(define default-k-generator
  (make-random-k-generator (secure-random-generator *prng:chacha20*)))

;; RFC 6979
(define (make-hmac-k-generator (digest digest-descriptor?))
  (define size (digest-descriptor-digest-size digest))

  (define (bits->integer t n)
    (let ((v (bytevector->uinteger t))
	  (nl (bitwise-length n)))
      (if (> (* (bytevector-length t) 8) nl)
	  (bitwise-arithmetic-shift-right v (- (* (bytevector-length t) 8) nl))
	  v)))
  (define (bits->octets t n)
    (let* ((tmp (bits->integer t n))
	   (mi (if (> tmp n) (- tmp n) tmp)))
      (sinteger->bytevector mi)))
  (lambda (n d M) ;; M = H(m) = h1
    (define (setup-K hmac V mark K)
      (mac-init! hmac)
      (mac-process! hmac V)
      (mac-process! hmac mark)
      (mac-process! hmac (sinteger->bytevector d))
      (mac-process! hmac (bits->octets M n))
      (mac-done! hmac K)
      K)
    (define (setup-V K V)
      (define hmac (make-mac *mac:hmac* K :digest digest))
      (mac-init! hmac)
      (mac-process! hmac V)
      (mac-done! hmac V)
      hmac)

    ;; b.
    (define V (make-bytevector size #x01))
    ;; c. 
    (define K (make-bytevector size #x00))
    (define hmac0 (make-mac *mac:hmac* K :digest digest))

    ;; d.
    (setup-K hmac0 V #vu8(#x00) K)
    ;; e.
    (let ((hmac (setup-V K V)))
      ;; f.
      (setup-K hmac V #vu8(01) K))
    ;; g.
    (let* ((hmac (setup-V K V))
	   ;; h.
	   (Ts (if (zero? n) 1 (div (+ (bitwise-length n) 7) 8)))
	   (T (make-bytevector Ts 0)))
      (define (populate-T hmac V T)
	(define Ts (bytevector-length T))
	(define size (bytevector-length V))
	(let loop ((off 0))
	  (when (< off Ts)
	    (mac-init! hmac)
	    (mac-process! hmac V)
	    (mac-done! hmac V)
	    (let ((len (min (- Ts off) size)))
	      (bytevector-copy! V 0 T off len)
	      (loop (+ off len))))))
      (let loop ((hmac hmac))
	(populate-T hmac V T)
	(let ((k (bits->integer T n)))
	  (if (and (> k 0) (< k n))
	      k
	      (begin
		(mac-init! hmac)
		(mac-process! hmac V)
		(mac-process! hmac #vu8(#x00))
		(mac-done! hmac K)
		(loop (setup-V K V)))))))))
  

)
