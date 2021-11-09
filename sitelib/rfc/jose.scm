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
	    jose-header-custom-parameters

	    (rename (jose-crypto-header <jose-crypto-header>))
	    make-jose-crypto-header jose-crypto-header?
	    jose-crypto-header-alg jose-crypto-header-jku
	    jose-crypto-header-jwk jose-crypto-header-kid
	    jose-crypto-header-x5u jose-crypto-header-x5c
	    jose-crypto-header-x5t jose-crypto-header-x5t-s256
	    jose-crypto-header-crit

	    (rename (jose-object <jose-object>))
	    jose-object?
	    jose-object-parts
	    
	    ;; for header serialization and deserialization
	    jose-header-object-builder
	    jose-crypto-header-object-builder

	    jose-header-serializer
	    jose-crypto-header-serializer

	    make-json->header

	    jose-split jose-part-count
	    ;; for record builder
	    ->jose-header-custom-parameter

	    jose-crypto-header-check-critical-headers

	    ;; for convenience
	    base64url-string->bytevector bytevector->base64url-string
	    )
    (import (rnrs)
	    (rfc x.509)
	    (rfc jwk) ;; it might look weird but we need JWK for jwk parameter
	    (rfc base64)
	    (srfi :1 lists) ;; for reverse!
	    (srfi :13 strings)
	    (srfi :14 char-sets)
	    (srfi :39 parameters)
	    (text json object-builder))

(define-record-type jose-header
  ;; only these 2 are the same amoung JWT, JWS and JWE
  (fields typ cty custom-parameters))

(define-record-type jose-crypto-header
  (parent jose-header)
  (fields alg jku jwk kid x5u x5c x5t x5t-s256 crit))

;; sort of base object
(define-record-type jose-object
  (fields parts))

(define (->jose-header-custom-parameter v)
  (cond ((hashtable? v) (hashtable-copy v)) ;; make it immutable
	((null? v)
	 (->jose-header-custom-parameter (make-hashtable string-hash string=?)))
	((pair? v)
	 (let ((ht (make-hashtable string-hash string=?)))
	   (for-each (lambda (slot)
		       (hashtable-set! ht (car slot) (cadr slot))) v)
	   (->jose-header-custom-parameter ht)))
	(else (assertion-violation '->jose-header-custom-parameter
				   "Custom parameter must be alist or hashtable"
				   v))))

(define (jose-crypto-header-check-critical-headers jose-header critical-headers)
  (let ((crit (jose-crypto-header-crit jose-header)))
    (or (not crit) (null? crit)
	(for-all (lambda (l)
		   (or (string=? "b64" l)
		       (find (lambda (v) (string=? v l)) critical-headers)))
		 crit))))

;; some convenient procedures
(define (base64url-string->bytevector b64)
  (if (bytevector? b64)
      b64
      (base64url-decode-string b64 :transcoder #f)))
(define (bytevector->base64url-string bv) (utf8->string (base64url-encode bv)))

(define jose-header-object-builder
  (json-object-builder
   (make-jose-header
    (? "typ" #f string->symbol)
    (? "cty" #f)
    ;; dummy custom-parameters
    (? "___" #f))))

(define (b64-string->bytevector s) (base64-decode-string s :transcoder #f))
(define jose-crypto-header-object-builder
  (json-object-builder
   (make-jose-crypto-header
    jose-header-object-builder
    ("alg" string->symbol) ;; for now
    (? "jku" #f)
    (? "jwk" #f json-string->jwk)
    (? "kid" #f)
    (? "x5u" #f)
    (? "x5c" #f (@ list b64-string->bytevector))
    (? "x5t" #f base64url-string->bytevector)
    (? "x5t#S256" #f base64url-string->bytevector)
    (? "crit" #f))))

(define custom-serializer
  (make-hashtable-serializer jose-header-custom-parameters))
(define jose-header-serializer
  (json-object-serializer
   ((? "typ" #f jose-header-typ symbol->string)
    (? "cty" #f jose-header-cty)
    custom-serializer)))

(define (bytevector->b64-string bv)
  (utf8->string (base64-encode bv :line-width #f)))
(define jose-crypto-header-serializer
  (json-object-serializer 
   (jose-header-serializer
    ("alg" jose-crypto-header-alg symbol->string) ;; for now
    (? "jku" #f jose-crypto-header-jku)
    (? "jwk" #f jose-crypto-header-jwk jwk->json)
    (? "kid" #f jose-crypto-header-kid)
    (? "x5u" #f jose-crypto-header-x5u)
    (? "x5c" #f jose-crypto-header-x5c (-> bytevector->b64-string))
    (? "x5t" #f jose-crypto-header-x5t bytevector->base64url-string)
    (? "x5t#S256" #f jose-crypto-header-x5t-s256 bytevector->base64url-string)
    (? "crit" #f jose-crypto-header-crit))))

(define (make-json->header builder post-build)
  (lambda (json)
    (define custom-parameters (make-hashtable string-hash equal?))
    (define (parameter-handler k v) (hashtable-set! custom-parameters k v))
    (define (post-object-build obj)
      (post-build obj custom-parameters))
    (parameterize ((*post-json-object-build* post-object-build))
      (json->object json builder parameter-handler))))

(define *base64url-charset*
  (list->char-set (map integer->char (vector->list *base64-encode-url-table*))))
(define (jose-split s)
  (define (split-by-dot s)
    (let loop ((offset 0) (e* '()))
      (cond ((string-index s #\. offset) =>
	     (lambda (index)
	       (loop (+ index 1)
		     (cons (substring s offset index) e*))))
	    (else
	     (reverse! (cons (substring s offset (string-length s)) e*))))))
  (let ((e* (split-by-dot s)))
    (unless (for-all (lambda (s) (string-every *base64url-charset* s)) e*)
      (assertion-violation 'jose-split "Invalid JWS/JWE format" s))
    e*))

(define (jose-part-count s)
  (if (zero? (string-length s))
      0
      ;; count starts with 1, as long as string isn't empty
      (let loop ((count 1) (offset 0))
	(cond ((string-index s #\. offset) =>
	       (lambda (index) (loop (+ count 1) (+ index 1))))
	      (else count)))))

)
