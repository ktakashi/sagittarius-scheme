;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/auth/api.scm - SSH2 authentication API
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh client auth api)
    (export ssh-read-auth-response
	    ssh-authenticate-method)
    (import (rnrs)
	    (clos user)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport))

(define-generic ssh-authenticate-method)
  
(define (ssh-read-auth-response transport callback)
  (let* ((payload (ssh-read-packet transport))
	 (type (bytevector-u8-ref payload 0)))
    (cond ((= type +ssh-msg-userauth-banner+)
           (let ((bp (ssh-read-packet transport))) ;; ignore success
             (values
      	      (= (bytevector-u8-ref bp 0) +ssh-msg-userauth-success+)
      	      (bytevector->ssh-message <ssh-msg-userauth-banner> payload))))
          ((= type +ssh-msg-userauth-success+) (values #t #f))
          ((= type +ssh-msg-userauth-failure+)
           (values #f
      		   (bytevector->ssh-message <ssh-msg-userauth-failure> payload)))
          (else (callback payload)))))
)
