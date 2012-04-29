;;; -*- Scheme -*-
;;;
;;; token.scm - OAuth 1.0 library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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


;; based on cl-oauth
(library (net oauth token)
    (export <token>
	    token-key token-secret token-user-data token-consumer

	    register-token unregister-token

	    <consumer-token>
	    make-consumer-token

	    <request-token>
	    make-request-token
	    request-token-authorized?
	    request-token-authorized-set!
	    request-token-callback-uri
	    request-token-verification-code

	    <access-token>
	    make-access-token
	    access-token-session-handle
	    access-token-expires
	    access-token-authorization-expires
	    access-token-expired?
	    ;; predicate
	    access-token?
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop validator)
	    (net oauth misc)
	    (srfi :27 random-bits))

  ;; (expt 36 25)
  (define-constant random-n 808281277464764060643139600456536293376)
  
  (define (random-key)
    (number->string (random-source random-n) 36))
  (define (random-secret)
    (number->string (random-source random-n) 36))
  (define (random-verification-code)
    (number->string (random-source random-n) 36))

  (define-class <token> ()
    ((key    	:init-keyword :key :init-form (random-key))
     (secret 	:init-keyword :secret :init-form (random-secret))
     ;; Application-specific data associated
     (user-data :init-keyword :user-data :init-value #f
		:accessor token-user-data)))
  (define-method token-key ((t <token>)) (slot-ref t 'key))
  (define-method token-secret ((t <token>)) (slot-ref t 'secret))

  (define-class <consumer-token> (<token>)
    (;; The Consumer that originally requested this token.
     (last-timestamp :init-value 0 :accessor consumer-token-last-timestamp)))
  (define (make-consumer-token . args)
    (apply make <consumer-token> args))

  (define-class <consumer-ref-mixin> ()
    ((consumer :init-keyword :consumer :accessor token-consumer)))

  ;; request tokens
  (define-class <request-token> (<token> <consumer-ref-mixin>)
    (;; Callback URI for this request token, #f means oob
     (callback-uri :init-keyword :callback-uri :init-value #f)
     ;; Might be #f for OAuth 1.0
     (verification-code :init-keyword :verification-code
			:accessor request-token-verification-code
			:init-form (random-verification-code))
     (authorized? :init-value #f)))
  (define-method request-token-authorized? ((t <request-token>))
    (slot-ref t 'authorized?))
  (define-method request-token-authorized-set! ((t <request-token>)
						(b <boolean>))
    (slot-set! t 'authorized? b))

  (define (make-request-token . args)
    (apply make <request-token> args))

  (define (time-validator o v)
    (unless (or (not v) (time? v))
      (assertion-violation 'time-validator
			   "slot value must be #f or time object"))
    v)
  (define-class <access-token> (<token> <consumer-ref-mixin> <validator-mixin>)
    ((session-handle :init-keyword :session-handle :init-value #f)
     ;; Universal time when this token expires.
     (expires :init-keyword :expires :init-value #f
	      :validator time-validator)
     ;; Universal time when this token's session expires
     (authorization-expires :init-keyword :authorization-expires
			    :init-value #f :validator time-validator)
     ;; URI this access token has been obtained form. Needed for refresh.
     (origin-uri :init-keyword :origin-uri :init-value #f)))
  (define-method access-token-session-handle ((t <access-token>))
    (slot-ref t 'session-handle))
  (define-method access-token-expires ((t <access-token>))
    (slot-ref t 'expires))
  (define-method access-token-authorization-expires ((t <access-token>))
    (slot-ref t 'authorization-expires))
  (define-method access-token-origin-uri ((t <access-token>))
    (slot-ref t 'origin-uri))

  (define (make-access-token . args)
    (apply make <access-token> args))
  (define (access-token-expired? access-token)
    (unless (is-a? access-token <access-token>)
      (assertion-violation 'access-token-expired?
			   "access token required") access-token)
    (and (access-token-session-handle access-token)
	 (or (and-let* ((expires (access-token-expires access-token)))
	       (time>? (current-time) expires))
	     (and-let* ((expires (access-token-authorization-expires
				  access-token)))
	       (time>? (current-time) expires)))))

  (define (access-token? token)
    (is-a? token <access-token>))
  )

