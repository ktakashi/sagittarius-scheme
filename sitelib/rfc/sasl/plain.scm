;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; rfc/sasl/plain.scm - SASL PLAIN mechanism
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
;; https://datatracker.ietf.org/doc/html/rfc4616
#!nounbound
(library (rfc sasl plain)
    (export sasl-client-plain-mechanism?
	    make-sasl-client-plain-mechanism)
    (import (rnrs)
	    (rfc sasl api)
	    (record builder))
  
(define-record-type sasl-client-plain-mechanism
  (parent <sasl-authentication-mechanism>)
  (fields authzid authcid password)
  (protocol (lambda (n)
	      (case-lambda
	       ((authcid password)
		((n 'PLAIN plain-mechanism-initial-state) #f authcid password))
	       ((authzid authcid password)
		((n 'PLAIN plain-mechanism-initial-state)
		 authzid authcid password))))))

(define (plain-mechanism-initial-state mechanism challenge)
  (define authzid (sasl-client-plain-mechanism-authzid mechanism))
  (define authcid (sasl-client-plain-mechanism-authcid mechanism))
  (define password (sasl-client-plain-mechanism-password mechanism))
  (make-sasl-authentication-state
   (string->utf8 (string-append (or authzid "") "\x0;" authcid "\x0;" password))
   #f))

)
