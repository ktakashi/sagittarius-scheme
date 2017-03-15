;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jose.scm - Javascript Object Signing and Encryption
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

;; NB: this library's specification is extracted from JWT, JWS and JWE
;;     means there's no actual specification but usecase on RFC
;;     see: https://tools.ietf.org/html/rfc7165

(library (rfc jose)
    (export (rename (jose-header <jose-header>))
	    make-jose-header jose-header?
	    jose-header-typ jose-header-cty

	    (rename (jose-crypto-header <jose-crypto-header>))
	    make-jose-crypto-header jose-crypto-header?
	    jose-crypt-header-alg jose-crypt-header-jku
	    jose-crypt-header-jwk jose-crypt-header-kid
	    jose-crypt-header-x5u jose-crypt-header-x5c
	    jose-crypt-header-x5t jose-crypt-header-x5t-s256
	    jose-crypt-header-crit

	    ;; put utility here...
	    base64-url-encode
	    base64-url-decode
	    )
    (import (rnrs)
	    (rfc base64))

  (define-record-type jose-header
    ;; only these 2 are the same amoung JWS, JWS and JWE
    (fields typ cty))

  ;; TODO should we define this in (rfc jws)?
  (define-record-type jose-crypto-header
    (parent jose-header)
    (fields alg jku jwk kid x5u x5c x5t x5t-s256 crit))

  (define (base64-url-decode bv)
    (define (base64-url-decode-value-port src)
      (define pos 0)
      (define len (bytevector-length src))
      (define (read! bv start count)
	(if (= len pos)
	    0
	    (let ((size (min count (- len pos))))
	      (bytevector-copy! src pos bv start size)
	      (set! pos (+ pos size))
	      (let loop ((i start))
		(if (= i size)
		    size
		    (let ((b (bytevector-u8-ref bv i)))
		      (case (integer->char b)
			((#\-) (bytevector-u8-set! bv i (char->integer #\+)))
			((#\_) (bytevector-u8-set! bv i (char->integer #\/))))
		      (loop (+ i 1))))))))
      (make-custom-binary-input-port "base64-url-decode-value-port" read!
				     #f #f #f))
    (get-bytevector-all
     (open-base64-decode-input-port (base64-url-decode-value-port bv))))

  (define (base64-url-encode bv)
    (define (base64-url-encode-sink out)
      (define (write! bv start count)
	(do ((i 0 (+ i 1)))
	    ((= i count) i)
	  (let ((b (bytevector-u8-ref bv (+ start i))))
	    (case (integer->char b)
	      ((#\+) (put-u8 out (char->integer #\-)))
	      ((#\/) (put-u8 out (char->integer #\_)))
	      ;; do nothing
	      ((#\=) )
	      (else (put-u8 out b))))))
      (make-custom-binary-output-port "base64-url-encode-sink" write!
				      #f #f #f))
    (let-values (((out extract) (open-bytevector-output-port)))
      (let ((bout (open-base64-encode-output-port
		   (base64-url-encode-sink out))))
	(put-bytevector bout bv)
	(close-output-port bout)
	(extract))))
)
