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

#!nounbound
(library (rfc jose)
    (export (rename (jose-header <jose-header>))
	    make-jose-header jose-header?
	    jose-header-typ jose-header-cty

	    (rename (jose-crypto-header <jose-crypto-header>))
	    make-jose-crypto-header jose-crypto-header?
	    jose-crypto-header-alg jose-crypto-header-jku
	    jose-crypto-header-jwk jose-crypto-header-kid
	    jose-crypto-header-x5u jose-crypto-header-x5c
	    jose-crypto-header-x5t jose-crypto-header-x5t-s256
	    jose-crypto-header-crit
	    jose-crypto-header-custom-parameters

	    ;; for header serialization and deserialization
	    jose-header-object-builder
	    jose-crypto-header-object-builder
	    )
    (import (rnrs)
	    (rfc x.509)
	    (rfc jwk) ;; it might look weird but we need jwk here
	    (text json object-builder))

(define-record-type jose-header
  ;; only these 2 are the same amoung JWS, JWS and JWE
  (fields typ cty))

;; TODO should we define this in (rfc jws)?
(define-record-type jose-crypto-header
  (parent jose-header)
  (fields alg jku jwk kid x5u x5c x5t x5t-s256 crit custom-parameters))

(define jose-header-object-builder
  (json-object-builder
   (make-jose-header
    (? "typ" #f string->symbol)
    (? "cty" #f))))
(define jose-crypto-header-object-builder
  (json-object-builder
   (make-jose-crypto-header
    jose-header-object-builder
    ("alg" string->symbol) ;; for now
    (? "jku" #f)
    (? "jwk" #f json-string->jwk)
    (? "kid" #f)
    (? "x5u" #f) ;; TODO convert it to certificate
    (? "x5c" #f) ;; TODO convert it to certificate chain
    (? "x5t" #f) ;; TODO convert it fingerprint (bytevector)
    (? "x5t#S256" #f) ;; ditto
    (? "crit" #f)
    ;; dummy custom-parameters
    (? "___" #f))))

;; (define jose-header-serializer
;;   (json-object-serializer
;;    ((? "typ" #f jose-header-typ symbol->string)
;;     (? "cty" #f jose-header-cty))))

;; (define custom-serializer
;;   (make-hashtable-serializer jose-crypto-header-custom-parameters))
;; (define jose-crypto-header-serializer
;;   (json-object-serializer (parent jose-header-serializer)
;;    ("alg" jose-crypto-header-alg jws-algorithm->string)
;;     (? "jku" #f jose-crypto-header-jku)
;;     (? "jwk" #f jose-crypto-header-jwk)
;;     (? "kid" #f jose-crypto-header-kid)
;;     (? "x5u" #f jose-crypto-header-x5u)
;;     (? "x5c" #f jose-crypto-header-x5c)
;;     (? "x5t" #f jose-crypto-header-x5t)
;;     (? "x5t#S256" #f jose-crypto-header-x5t-s256)
;;     (? "crit" #f jose-crypto-header-crit)
;;     custom-serializer))

)
