;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jwa.scm - JSON Web Algorithms (JWK)
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

;; ref: https://tools.ietf.org/html/rfc7518
(library (rfc jwa)
    (export jwa:make-ec-public-key
	    jwa:make-ec-private-key
	    jwa:make-rsa-public-key
	    jwa:make-rsa-private-key
	    jwa:make-rsa-crt-private-key)
    (import (rnrs)
	    (sagittarius)
	    (crypto))

  (define-constant +curve-names+
    `((P-256 ,NIST-P-256)
      (P-384 ,NIST-P-384)
      (P-521 ,NIST-P-521)))
  
  (define (jwa:curve->parameter crv)
    (cond ((assq crv +curve-names+) => cadr)
	  (else (assertion-violation
		 'jwa:curve->parameter "unsupported crv name" crv))))
  (define (jwa:make-ec-public-key crv x y)
    (generate-public-key ECDSA x y (jwa:curve->parameter crv)))
  
  (define (jwa:make-ec-private-key crv d x y)
    (generate-private-key ECDSA d (jwa:curve->parameter crv)
			  (jwa:make-ec-public-key crv x y)))
  
  (define (jwa:make-rsa-public-key n e) (generate-public-key RSA n e))
  (define (jwa:make-rsa-private-key n e d) (generate-private-key RSA n d))
  (define (jwa:make-rsa-crt-private-key n e d p q dp dq qi)
    (generate-private-key RSA n d :public-exponent e :p p :q q))
  
  )
	    
