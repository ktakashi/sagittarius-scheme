;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/jwt.scm - JSON Web Token (JWT)
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

;; ref: https://tools.ietf.org/html/rfc7519
#!nounbound
(library (rfc jwt)
    (export jwt-claims? jwt-claims-builder
	    (rename (jwt-claims <jwt-claims>))
	    jwt-claims-iss jwt-claims-sub jwt-claims-aud jwt-claims-exp
	    jwt-claims-nbf jwt-claims-iat jwt-claims-jti
	    jwt-claims-custom-claims
	    jwt-claims->json write-jwt-claims jwt-claims->json-string
	    json->jwt-claims read-jwt-claims json-string->jwt-claims

	    jwt-consumer? jwt-consumer-builder
	    jwt:iss-required-validator jwt:iss-value-validator
	    jwt:sub-required-validator jwt:sub-value-validator
	    jwt:aud-required-validator jwt:aud-value-validator
	    jwt:exp-required-validator jwt:exp-validator
	    jwt:nbf-required-validator jwt:nbf-validator
	    jwt:iat-required-validator jwt:iat-validator
	    jwt:jti-required-validator

	    (rename (make-claims-validator jwt:make-claims-validator))
	    
	    jwt:consume)
    (import (rnrs)
	    (rfc jose)
	    (rfc jwe)
	    (rfc jwk)
	    (rfc jws)
	    (record builder)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (text json)
	    (text json object-builder))

(define-record-type jwt-claims
  (fields iss
	  sub
	  aud
	  exp
	  nbf
	  iat
	  jti
	  custom-claims))
;; it's the same thing but put an alias ;)
(define ->jwt-custom-claims ->jose-header-custom-parameter)
(define-syntax jwt-claims-builder
  (make-record-builder jwt-claims
   ((custom-claims '() ->jwt-custom-claims))))

;; Utilties for object builder and serializer
(define (seconds->time-utc s) (and s (make-time time-utc 0 s)))
(define (time-utc->seconds t)
  (define (nano->second n) (div n (expt 10 9)))
  (and t (+ (time-second t) (nano->second (time-nanosecond t)))))
(define (check-string s)
  (and s
       (or (string? s) (assertion-violation 'jwt-claim "Must be a string" s))
       s))
(define jwt-claims-object-builder
  (json-object-builder
   (make-jwt-claims
    (? "iss" #f check-string)
    (? "sub" #f check-string)
    (? "aud" #f check-string)
    (? "exp" #f seconds->time-utc)
    (? "nbf" #f seconds->time-utc)
    (? "iat" #f seconds->time-utc)
    (? "jti" #f check-string)
    ;; custom-claims
    (? "___" #f))))
(define custom-claims-serializer
  (make-hashtable-serializer jwt-claims-custom-claims))
(define jwt-claims-object-serializer
  (json-object-serializer
   ((? "iss" #f jwt-claims-iss)
    (? "sub" #f jwt-claims-sub)
    (? "aud" #f jwt-claims-aud)
    (? "exp" #f jwt-claims-exp time-utc->seconds)
    (? "nbf" #f jwt-claims-nbf time-utc->seconds)
    (? "iat" #f jwt-claims-iat time-utc->seconds)
    (? "jti" #f jwt-claims-jti)
    custom-claims-serializer)))

;; APIs for serialization and deserialization
(define (json->jwt-claims json)
  (define custom-claims (make-hashtable string-hash string=?))
  (define (parameter-handler k v) (hashtable-set! custom-claims k v))
  (define (post-object-build obj)
    (if (jwt-claims? obj)
	(jwt-claims-builder (from obj) (custom-claims custom-claims))
	(assertion-violation 'json->jwt-claims "Something went wrong" obj)))
  (parameterize ((*post-json-object-build* post-object-build))
    (json->object json jwt-claims-object-builder parameter-handler)))

(define (read-jwt-claims port) (json->jwt-claims (json-read port)))
(define (json-string->jwt-claims json-string)
  (read-jwt-claims (open-string-input-port json-string)))

(define (jwt-claims->json jwt-claims)
  (object->json jwt-claims jwt-claims-object-serializer))
(define write-jwt-claims
  (case-lambda
   ((jwt-claims) (write-jwt-claims jwt-claims (current-output-port)))
   ((jwt-claims port)
    (json-write/normalized (jwt-claims->json jwt-claims) port))))
(define (jwt-claims->json-string jwt-claims)
  (let-values (((out e) (open-string-output-port)))
    (write-jwt-claims jwt-claims out)
    (e)))

(define-record-type jwt-consumer
  (fields decryptor
	  verifier
	  claims-validator))
(define-syntax jwt-consumer-builder (make-record-builder jwt-consumer))

(define (jwt:consume consumer jwt-string/object)
  (define (get-payload consumer jwt-object)
    (cond ((jwe-object? jwt-object)
	   (jwe:decrypt (jwt-consumer-decryptor consumer) jwt-object))
	  ((jws-object? jwt-object)
	   (and (jws:verify jwt-object (jwt-consumer-verifier consumer))
		(jws-object-payload jwt-object)))
	  (else (assertion-violation 'jwt:consume
		 "jwt:parse returnes bogus object" jwt-object))))
	  
  (let* ((jwt-object (if (jose-object? jwt-string/object)
			 jwt-string/object
			 (jwt:parse jwt-string/object)))
	 (payload (get-payload consumer jwt-object))
	 (claims (json-string->jwt-claims (utf8->string payload)))
	 (claims-validator (jwt-consumer-claims-validator consumer)))
    (if claims-validator
	(claims-validator claims)
	claims)))


;;; Claims validators
(define (make-claims-validator accessor pred . reasons)
  (lambda (claims)
    (let ((v (accessor claims)))
      ;; TODO make proper conndition
      (and (or (pred v) (apply assertion-violation `(,@reasons ,v)))
	   claims))))
(define (set-pred set)
  (lambda (v)
    (or (not v) (memp (lambda (e) (equal? e v)) set))))
(define (required-pred v) v)

(define jwt:iss-required-validator
  (make-claims-validator jwt-claims-iss required-pred 'iss "`iss` is required"))
(define jwt:sub-required-validator
  (make-claims-validator jwt-claims-sub required-pred 'sub "`sub` is required"))
(define jwt:aud-required-validator
  (make-claims-validator jwt-claims-aud required-pred 'aud "`aud` is required"))
(define jwt:exp-required-validator
  (make-claims-validator jwt-claims-exp required-pred 'exp "`exp` is required"))
(define jwt:nbf-required-validator
  (make-claims-validator jwt-claims-nbf required-pred 'nbf "`nbf` is required"))
(define jwt:iat-required-validator
  (make-claims-validator jwt-claims-iat required-pred 'iat "`iat` is required"))
(define jwt:jti-required-validator
  (make-claims-validator jwt-claims-jti required-pred 'jti "`jti` is required"))


(define (jwt:iss-value-validator . iss)
  (make-claims-validator jwt-claims-iss (set-pred iss)
			 'iss "`iss` must be one of" iss))
(define (jwt:sub-value-validator sub)
  (make-claims-validator jwt-claims-sub (lambda (v) (or (not v) (equal? v sub)))
			 'sub (string-append "`sub` must be " sub)))
(define (jwt:aud-value-validator . aud)
  (make-claims-validator jwt-claims-aud (set-pred aud)
			 'aud "`aud` must be one of" aud))

(define jwt:exp-validator
  (case-lambda
   (() (jwt:exp-validator 0))
   ((clock-skew) (jwt:exp-validator clock-skew #f))
   ((clock-skew future-bound-seconds)
    (define delta (make-time time-duration 0 clock-skew))
    (define max-future-delta
      (and future-bound-seconds
	   (make-time time-duration 0 future-bound-seconds)))
    (define (expiration-time-check exp)
      (define real-now (current-time))
      (define now (add-duration real-now delta))
      (define max-future
	(and max-future-delta (add-duration real-now max-future-delta)))
      (and (time<=? now exp)
	   (or (not max-future) (time<=? exp max-future))))
    (make-claims-validator jwt-claims-exp expiration-time-check
			   'exp "JWT is expired"))))

(define jwt:nbf-validator
  (case-lambda
   (() (jwt:nbf-validator 0))
   ((clock-skew)
    (define delta (make-time time-duration 0 clock-skew))
    (define (not-before-check nbf)
      (define now (add-duration (current-time) delta))
      (time<? nbf now))
    (make-claims-validator jwt-claims-nbf not-before-check
			   'nbf "JWT is not in use"))))

(define jwt:iat-validator 
  (case-lambda
   ((not-before not-after) (jwt:iat-validator not-before not-after 0))
   ((not-before not-after clock-skew)
    (define delta (make-time time-duration 0 clock-skew))
    (define (issued-between iat)
      (define now (add-duration (current-time) delta))
      (and (time<=? not-before now) (time<=? now not-after)))
    (unless (and (time? not-before) (time? not-after))
      (assertion-violation 'jwt:iat-validator
			   "not-before and not-after must be a time object"
			   not-before not-after))
    (make-claims-validator jwt-claims-iat issued-between
			   'iat "JWT is issued unaccepted time range"
			   not-before not-after))))

(define (jwt:parse s)
  (case (jose-part-count s)
    ((3) (jws:parse s))
    ((5) (jwe:parse s))
    (else (assertion-violation 'jwt:parse "Not a JWT string" s))))
  
)
	    
