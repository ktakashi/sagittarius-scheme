;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; rfc/sasl/api.scm - SASL framework
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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

;; ref:
;; https://datatracker.ietf.org/doc/html/rfc4422
#!nounbound
(library (rfc sasl api)
    (export make-sasl-authentication-context
	    (rename (sasl-authentication-context <sasl-authentication-context>))
	    sasl-authentication-context?
	    sasl-select-mechanism
	    sasl-find-mechanism

	    sasl-authentication-mechanism?
	    (rename (sasl-authentication-mechanism
		     <sasl-authentication-mechanism>))
	    sasl-authentication-mechanism-name
	    
	    sasl-authentication-state?
	    make-sasl-authentication-state
	    sasl-authentication-state-message

	    sasl-start-client-authentication
	    sasl-process-client-authentication
	    )
    (import (rnrs))

;; This library basically makes an abstract layer of the below flow
;;  C: Request authentication exchange
;;  S: Initial challenge
;;  C: Initial response
;;  <additional challenge/response messages>
;;  S: Outcome of authentication exchange
;; The idea is that, 

(define-record-type sasl-authentication-context
  (fields mechanisms))

(define-record-type sasl-authentication-mechanism
  (fields name initial-state))

(define-record-type sasl-authentication-state
  (fields message next-state-generator))

(define (sasl-select-mechanism context mechanisms)
  (exists (lambda (m) (sasl-find-mechanism context m)) mechanisms))

(define (sasl-find-mechanism context mechanism)
  (define mechanisms (sasl-authentication-context-mechanisms context))
  (define (mechanism=? m)
    (eq? (sasl-authentication-mechanism-name m) mechanism))
  (find mechanism=? mechanisms))
  
(define (sasl-start-client-authentication mechanism initial-challenge)
  ((sasl-authentication-mechanism-initial-state mechanism)
   mechanism initial-challenge))
  
(define (sasl-process-client-authentication state server-response)
  (define generator (sasl-authentication-state-next-state-generator state))
  (and generator (generator state server-response)))

)
