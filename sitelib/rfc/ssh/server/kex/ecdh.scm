;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/kex/ecdh.scm - SSH2 server ECDH KEX
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
(library (rfc ssh server kex ecdh)
    (export ssh-server-exchange-kex-message)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius crypto keys)
	    (rfc ssh constants)
	    (rfc ssh crypto)
	    (rfc ssh types)
	    (rfc ssh transport kex ecdh)
	    (rfc ssh server kex api))
(define-method ssh-server-exchange-kex-message
  ((m (?? ssh-kex-ecdh-sha2?)) transport server-packet client-packet)
  (define identity (ssh-kex-ecdh-identity m))
  (define parameter (ssh-ecdsa-identifier->ec-parameter
		     (ssh-kex-ecdh-identity m)))
  (define (compute-k K-S init)
    (calculate-shared-secret transport *key:ecdsa* parameter K-S init
			     client-packet server-packet))
  (ssh-kex-receive/send transport <ssh-msg-kex-ecdh-init> compute-k
			make-ecdh-reply))

(define-method ssh-server-exchange-kex-message
  ((m (member ssh-curve-25519/448)) transport server-packet client-packet)
  (define parameter (if (string=? m +kex-curve448-sha512+)
		    x448-curve-parameter
		    x25519-curve-parameter))
  (define (compute-k K-S init)
    (calculate-shared-secret transport *key:ecdh* parameter K-S init
			     client-packet server-packet))
  (ssh-kex-receive/send transport <ssh-msg-kex-ecdh-init> compute-k
			make-ecdh-reply))

(define (calculate-shared-secret transport scheme parameter K-S init
				 client-packet server-packet)
  (define kp (generate-key-pair scheme :ec-parameter parameter))
  (define Q-C (~ init 'Q-C))
  (define Q-S (export-public-key (key-pair-public kp) (public-key-format raw)))
  (define cpub (generate-public-key scheme Q-C parameter))
  (let* ((K (bytevector->integer
	     (calculate-key-agreement *key:ecdh* (key-pair-private kp) cpub)))
	 (msg (make <SSH-ECDH-H>
		:V-C (~ transport 'client-version)
		:V-S (~ transport 'server-version)
		:I-C client-packet :I-S server-packet
		:K-S K-S :Q-C Q-C :Q-S Q-S :K K)))
    (values K msg Q-S)))

;; host public key, signature, ephemeral public key
(define (make-ecdh-reply K-S sig Q-S)
  (make <ssh-msg-kex-ecdh-reply> :K-S K-S :Q-S Q-S
	:signature (ssh-message->bytevector sig)))

)
