;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth/public-key.scm - SSH2 server authentication API
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh server service auth public-key)
    (export ssh-handle-userauth-request
	    <ssh-pubic-key-credential> ssh-pubic-key-credential?
	    ssh-credential-public-key)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (sagittarius crypto signatures)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport)
	    (rfc ssh server service auth api))
(define-class <ssh-pubic-key-credential> (<ssh-credential>)
  ((public-key :init-keyword :public-key :reader ssh-credential-public-key)))
(define (ssh-pubic-key-credential? o) (is-a? o <ssh-pubic-key-credential>))

(define-method ssh-handle-userauth-request 
  ((m (equal +ssh-auth-method-public-key+)) transport packet)
  (define (verify verifier session-id msg)
    (let-values (((out e) (open-bytevector-output-port)))
      (ssh-write-message :string session-id out #f)
      (put-u8 out (~ msg 'type))
      (ssh-write-message :string (~ msg 'user-name) out #f)
      (ssh-write-message :string (~ msg 'service-name) out #f)
      (ssh-write-message :string (~ msg 'method) out #f)
      (ssh-write-message :boolean #t out #f)
      (ssh-write-message :string (~ msg 'algorithm-name) out #f)
      (ssh-write-message :string (~ msg 'blob) out #f)
      (let ((sig (bytevector->ssh-message <ssh-signature> (~ msg 'signature))))
	(verifier-verify-signature verifier (e) (~ sig 'signature)))))
  (define msg
    (bytevector->ssh-message <ssh-msg-public-key-userauth-request> packet))
  (let* ((public-key (ssh-message-bytevector->public-key (~ msg 'blob)))
	 (verifier (make-ssh-verifier (~ msg 'algorithm-name) public-key)))
    (if (~ msg 'has-signature?)
	(and (verify verifier (~ transport 'session-id) msg)
	     (ssh-authenticate-user (~ msg 'service-name)
				    (make <ssh-pubic-key-credential>
				      :username (~ msg 'user-name)
				      :public-key public-key)))
	;; okay, verifier can be created, so return pk-ok
	(ssh-write-ssh-message transport
			       (make <ssh-msg-userauth-pk-ok>
				 :algorithm-name (~ msg 'algorithm-name)
				 :blob (~ msg 'blob))))))
)
