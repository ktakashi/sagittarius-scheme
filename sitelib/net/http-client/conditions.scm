;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/conditions.scm - Conditions for HTTP client
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (net http-client conditions)
    (export http-client-error?
	    http-connection-error?
	    http-protocol-error?

	    raise-http-connection-error
	    raise-http-protocol-error)
    (import (rnrs))

(define-condition-type &http-client &error
  make-http-client-error http-client-error?)

(define-condition-type &http-connection &http-client
  make-http-connection-error http-connection-error?)

(define-condition-type &http-protocol &http-client
  make-http-protocol-error http-protocol-error?)

(define (raise-http-connection-error who message . irritants)
  (raise (condition
	  (make-http-connection-error)
	  (make-who-condition who)
	  (make-message-condition message)
	  (make-irritants-condition irritants))))

(define (raise-http-protocol-error who message . irritants)
  (raise (condition
	  (make-http-protocol-error)
	  (make-who-condition who)
	  (make-message-condition message)
	  (make-irritants-condition irritants))))

)
