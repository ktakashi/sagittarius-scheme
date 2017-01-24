;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/hotp.scm - An HMAC-Based One-Time Password Algorithm
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

;; reference
;;  - https://tools.ietf.org/html/rfc4226

(library (rfc hotp)
    (export generate-hmac-one-time-password
	    (rename (generate-hmac-one-time-password hotp)))
    (import (rnrs)
	    (math)
	    (rfc hmac)
	    (sagittarius))

    (define (generate-hmac-one-time-password K C digits :key (algo :hash SHA-1))
      (define hmac (hash-algorithm HMAC :key K :hash algo))
      (define size 4)
      (define (dynamic-truncate HS)
	(define last (- (hash-size hmac) 1))
	(let* ((offset (bitwise-and (bytevector-u8-ref HS last) #x0F)))
	  (do ((bv (make-bytevector size)) (i 0 (+ i 1)))
	      ((= i size)
	       ;; make it only last 31 bits (big endian)
	       (bytevector-u8-set! bv 0
		(bitwise-and (bytevector-u8-ref bv 0) #x7F))
	       bv)
	    (bytevector-u8-set! bv i (bytevector-u8-ref HS (+ i offset))))))
      (let* ((c (integer->bytevector C 8))
	     (HS (hash hmac c))
	     (Sbits (dynamic-truncate HS))
	     (Snum (bytevector->uinteger Sbits)))
	(mod Snum (expt 10 digits))))
  )
