;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jws.scm - JSON Web Signature (JWS)
;;;
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; ref: https://tools.ietf.org/html/rfc7515
#!nounbound
(library (rfc jws)
    (export jws-header?
	    (rename (jws-header <jws-header>)
		    (jose-header-typ jws-header-typ)
		    (jose-header-cty jws-header-cty)
		    (jose-crypto-header-alg jws-header-alg)
		    (jose-crypto-header-jku jws-header-jku)
		    (jose-crypto-header-jwk jws-header-jwk)
		    (jose-crypto-header-kid jws-header-kid)
		    (jose-crypto-header-x5u jws-header-x5u)
		    (jose-crypto-header-x5c jws-header-x5c)
		    (jose-crypto-header-x5t jws-header-x5t)
		    (jose-crypto-header-x5t-s256 jws-header-x5t-s256)
		    (jose-crypto-header-crit jws-header-crit)
		    (jose-crypto-header-custom-parameters
		     jws-header-custom-parameters))

	    jws-object? (rename (jws-object <jws-object>))
	    jws-object-header jws-object-payload

	    jws-signed-object? (rename (jws-signed-object <jws-signed-object>))
	    jws-signed-object-signature
	    )
    (import (rnrs)
	    (rfc jose))

(define-record-type jws-header
  (parent <jose-crypto-header>))

(define-record-type jws-object
  (fields header
	  payload))

(define-record-type jws-signed-object
  (parent jws-object)
  (fields signature))

)
