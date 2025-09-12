;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth/api.scm - SSH2 server authentication API
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
(library (rfc ssh server service auth api)
    (export ssh-handle-userauth-request
	    ssh-authenticate-user
	    <ssh-credential> ssh-credential?
	    ssh-credential-username
	    <ssh-auth-ticket> ssh-auth-ticket?)
    (import (rnrs)
	    (clos core)
	    (clos user)
	    (rfc base64)
	    (sagittarius mop immutable)
	    (sagittarius mop allocation)
	    (sagittarius crypto random)
	    (sagittarius crypto digests))

(define-class <ssh-credential> (<immutable>)
  ;; list of authenticate method this credential can support
  ((username :init-keyword :username :reader ssh-credential-username)))
(define (ssh-credential? o) (is-a? o <ssh-credential>))

(define-class <ssh-auth-ticket> (<immutable>) ())
(define (ssh-auth-ticket? o) (is-a? o <ssh-auth-ticket>))

(define-generic ssh-handle-userauth-request
  :class <predicate-specializable-generic>)
;; custom point, we don't hold credential
(define-method ssh-handle-userauth-request (method transport packet)
  (error 'ssh-handle-userauth-request "Failed to authenticate" method))

(define-generic ssh-authenticate-user
  :class <predicate-specializable-generic>)
(define-method  ssh-authenticate-user (service credential)
  (error 'ssh-authenticate-user "Failed to authenticate" service))

)
