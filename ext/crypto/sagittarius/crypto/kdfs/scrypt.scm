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
	    (sagittarius crypto logic scrypt)
	    (sagittarius crypto logic salsa)
	    )

(define *hmac-sha256-prf* 
  (mac->prf-provider *mac:hmac* :digest *digest:sha-256*))

(define (scrypt P S (N (and integer? exact?)) (r (and integer? exact?))
		(p (and integer? exact?)) (dk-len (and integer? exact?)))
  (define (pbkdf2-hmac-sha256 P S c dk-len)
    (pbkdf-2 P S c dk-len :prf *hmac-sha256-prf*))
  (unless (and (> N 1) (= (bitwise-bit-count N) 1))
    (assertion-violation 'scrypt
     "Cost parameter must be greater than 1 and power of 2" N))
  (unless (> r 0)
    (assertion-violation 'scrypt "Block size parameter must be greater than 0"
			 r))
  (unless (> p 0)
    (assertion-violation 'scrypt
			 "Parallelisation parameter must be greater than 0"
			 p))
  (when (>= (* r p) #x40000000)
    (assertion-violation 'scrypt "r * p must be less than 2^30"
			 (list r p)))
  (let* ((size (* 128 r))
	 (buf (make-bytevector size)))
    (do ((i 0 (+ i 1)) (B (pbkdf2-hmac-sha256 P S 1 (* p size))))
	((= i p) (pbkdf2-hmac-sha256 P B 1 dk-len))
      (bytevector-copy! B (* i size) buf 0 size)
      (scrypt-romix! buf N r)
      (bytevector-copy! buf 0 B (* i size) size))))

(define (scrypt-ro-mix B (N (and integer? exact?)) (r (and integer? exact?)))
  (let ((out (bytevector-copy B 0 (bytevector-length B))))
    (scrypt-romix! out N r)))

(define (scrypt-block-mix B (r (and integer? exact?)))
  (let ((out (bytevector-copy B 0 (bytevector-length B))))
    (scrypt-block-mix! out r)))

;; B = 64 octets
(define (salsa20/8 B) (salsa-core! B 8))
)
