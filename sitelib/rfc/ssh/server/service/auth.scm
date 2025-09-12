;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth.scm - SSH2 server authentication
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
(library (rfc ssh server service auth)
    (export ssh-handle-service-request
	    *ssh-authentication-error-handler*

	    ssh-handle-userauth-request-packet
	    ssh-handle-userauth-request
	    ssh-authenticate-user

	    <ssh-credential> ssh-credential?
	    <ssh-auth-ticket> ssh-auth-ticket?
	    <ssh-username&password-credential> ssh-username&password-credential?
	    <ssh-pubic-key-credential> ssh-pubic-key-credential?
	    ssh-credential-username
	    ssh-credential-password
	    ssh-credential-public-key
	    )
    (import (rnrs)
	    (clos user)
	    (srfi :39 parameters)
	    (sagittarius object)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh server transport)
	    (rfc ssh server service api)
	    (rfc ssh server service auth api)
	    (rfc ssh server service auth keyboard)
	    (rfc ssh server service auth public-key))

(define *ssh-authentication-error-handler*
  ;; default do nothing :)
  (make-parameter (lambda (service-name username method e))))

(define-method ssh-handle-service-request ((m (equal +ssh-userauth+)) transport)
  (set! (~ transport 'state) 'authenticating)
  (let ((msg (make <ssh-msg-service-accept> :service-name m)))
    (ssh-write-ssh-message transport msg)))

(define (ssh-handle-userauth-request-packet transport packet)
  (define msg (bytevector->ssh-message <ssh-msg-userauth-request> packet))
  (define service-name (~ msg 'service-name))
  (define username (~ msg 'user-name))
  (define method (~ msg 'method))
  (define (finish transport ticket)
    (when (ssh-auth-ticket? ticket)
      (ssh-handle-service-request service-name transport ticket)
      (set! (~ transport 'state) 'authenticated)
      (send-userauth-success transport))
    #t)
  (guard (e (else
	     ((*ssh-authentication-error-handler*)
	      service-name username method e)
	     (send-userauth-failure transport)))
    (cond ((not (eq? (~ transport 'state) 'authenticating))
	   (ssh-unimplemented transport))
	  ((ssh-handle-userauth-request method transport packet) =>
	   (lambda (ticket) (finish transport ticket)))
	  (else (send-userauth-failure transport)))))

(define builtin-supporting-method
  (list +ssh-auth-method-keyboard-interactive+
	+ssh-auth-method-public-key+))

(define (send-userauth-failure transport)
  (let ((msg (make <ssh-msg-userauth-failure>
	       :list (list->name-list builtin-supporting-method))))
    (ssh-write-ssh-message transport msg)))

(define (send-userauth-success transport)
  ;; should we send banner here?
  (ssh-write-packet transport (make-bytevector 1 +ssh-msg-userauth-success+)))
)
