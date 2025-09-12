;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth/keyboard.scm - SSH2 server authentication API
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
(library (rfc ssh server service auth keyboard)
    (export ssh-handle-userauth-request
	    *ssh:keybord-interactive-prompt-message*

	    <ssh-username&password-credential> ssh-username&password-credential?
	    ssh-credential-password)
    (import (rnrs)
	    (clos user)
	    (srfi :39 parameters)
	    (sagittarius object)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh server service auth api))

(define-class <ssh-username&password-credential> (<ssh-credential>)
  ((password :init-keyword :password :reader ssh-credential-password)))
(define (ssh-username&password-credential? o)
  (is-a? o <ssh-username&password-credential>))

(define *ssh:keybord-interactive-prompt-message*
  (make-parameter "Password for Sagittarius SSH server:"))

;; TODO implement multiple prompts authentication capability
(define-method ssh-handle-userauth-request 
  ((m (equal +ssh-auth-method-keyboard-interactive+)) transport packet)
  (define msg 
    (bytevector->ssh-message <ssh-msg-keyboard-interactive-userauth-request>
			     packet))
  (send-info-request transport)
  (let-values (((resp bin) (receive-info-response transport)))
    (unless (= (~ resp 'num-response) 1)
      (error 'keyboard-interactive "too many responses"))
    (let ((password (ssh-read-message :utf8-string bin #f)))
      (ssh-authenticate-user (~ msg 'service-name)
			     (make <ssh-username&password-credential>
			       :username (~ msg 'user-name)
			       :password password)))))

(define (send-info-request transport)
  (let ((info-request (make <ssh-msg-userauth-info-request>
			:name ""
			:instruction ""
			:num-prompts 1))
	(prompt (make <ssh-msg-userauth-prompt>
		   :prompt (*ssh:keybord-interactive-prompt-message*)
		   :echo #f)))
    (let-values (((out e) (open-bytevector-output-port)))
      (ssh-write-message info-request out)
      (ssh-write-message prompt out)
      (ssh-write-packet transport (e)))))
(define (receive-info-response transport)
  (let ((bv (ssh-read-packet transport)))
    (unless (= (bytevector-u8-ref bv 0) +ssh-msg-userauth-info-response+)
      (error 'keyboard-interactive "unexpected message"))
    (let ((bin (open-bytevector-input-port bv)))
      (values (ssh-read-message <ssh-msg-userauth-info-response> bin) bin))))

)
