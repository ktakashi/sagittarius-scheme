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

	    jws:parse
	    )
    (import (rnrs)
	    (rfc jose)
	    (rfc jwk)
	    (rfc base64)
	    (record builder)
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (text json)
	    (text json object-builder))

(define-record-type jws-header
  (parent <jose-crypto-header>))
(define-syntax jws-header-builder (make-record-builder jws-header))
		       

;; (define-enumeration jws:algorithm
;;   (HS256 HS384 HS512 RS256 RS384 RS512 ES256 ES384 ES512 PS256 PS384 PS512)
;;  jws-algorithm)

(define (string->jws-algorithm s) (string->symbol s)) ;; for now
(define jws-header-object-builder
  (json-object-builder
   (make-jws-header
    (? "typ" #f string->symbol)
    (? "cty" #f)
    ("alg" string->jws-algorithm)
    (? "jku" #f)
    (? "jwk" #f json-string->jwk)
    (? "kid" #f)
    (? "x5u" #f) ;; TODO convert it to certificate
    (? "x5c" '());; TODO convert it to certificate chain
    (? "x5t" #f) ;; TODO convert it fingerprint (bytevector)
    (? "x5t#S256" #f) ;; ditto
    (? "crit" #f)
    ;; dummy custom-parameters
    (? "___" #f)
    )))

(define (json->jws-header json)
  (define custom-parameters (make-hashtable string-hash string=?))
  (define (parameter-handler k v) (hashtable-set! custom-parameters k v))
  (define (post-object-build obj)
    (if (jws-header? obj)
	(jws-header-builder (from obj) (custom-parameters custom-parameters))))
  (parameterize ((*post-json-object-build* post-object-build))
    (json->object json jws-header-object-builder parameter-handler)))

(define (read-jws-header port) (json->jws-header (json-read port)))
(define (json-string->jws-header json-string)
  (read-jws-header (open-string-input-port json-string)))

(define-record-type jws-object
  (fields header
	  payload))

(define-record-type jws-signed-object
  (parent jws-object)
  (fields signature))

(define *base64url-charset*
  (list->char-set (map integer->char (vector->list *base64-encode-url-table*))))

(define (jws:parse s)
  (define (string->json s) (json-read (open-string-input-port s)))
  (let ((part* (string-tokenize s *base64url-charset*)))
    (unless (= (length part*) 3)
      (assertion-violation 'jws:parse "Invalid JWS format" s))
    (let ((header (base64url-decode-string (car part*)))
	  ;; we hold the below as bytevectors (for convenience)
	  (payload (base64url-decode (string->utf8 (cadr part*))))
	  (signature (base64url-decode (string->utf8 (caddr part*)))))
      (make-jws-signed-object
       (json-string->jws-header header) payload signature))))
  
)
