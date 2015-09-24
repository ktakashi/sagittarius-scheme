;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/conditions.scm - SMTP conditions
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc smtp conditions)
    (export &smtp-error make-smtp-error smtp-error?
	    ;; commonly used conditions?
	    &smtp-invalid-response-error make-smtp-invalid-response-error
	    smtp-invalid-response-error?
	    &smtp-authentication-failure make-smtp-authentication-failure
	    smtp-authentication-failure?
	    )
    (import (rnrs))

;; base
(define-condition-type &smtp-error &error make-smtp-error smtp-error?)

(define-condition-type &smtp-invalid-response-error  &smtp-error
  make-smtp-invalid-response-error smtp-invalid-response-error?)

(define-condition-type &smtp-authentication-failure &smtp-error
  make-smtp-authentication-failure smtp-authentication-failure?
  (method smtp-authentication-method))
)
