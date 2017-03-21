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

	    x5c-parameter->x509-certificates
	    )
    (import (rnrs)
	    (rfc x.509))

  (define-record-type jose-header
    ;; only these 2 are the same amoung JWS, JWS and JWE
    (fields typ cty))

  ;; TODO should we define this in (rfc jws)?
  (define-record-type jose-crypto-header
    (parent jose-header)
    (fields alg jku jwk kid x5u x5c x5t x5t-s256 crit))

  (define (x5c-parameter->x509-certificates x5c)
    (map make-x509-certificate x5c))
)
