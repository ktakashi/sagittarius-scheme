;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/kex/ecdh.scm - SSH2 protocol ECDH key exchange
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
(library (rfc ssh client kex ecdh)
    (export ssh-client-exchange-kex-message)
    (import (rnrs)
	    (clos user)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto random)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport)
	    (rfc ssh transport kex ecdh) ;; ecdh specifics
	    (rfc ssh client kex api))

(define-method ssh-client-exchange-kex-message
  ((m (?? ssh-kex-ecdh-sha2?)) transport client-packet server-packet)
  (define identity (ssh-kex-ecdh-identity m))
  (define curve (ssh-ecdsa-identifier->ec-parameter (ssh-kex-ecdh-identity m)))
  (calculate-shared-secret transport *key:ecdsa* curve
			   client-packet server-packet))

(define-method ssh-client-exchange-kex-message
  ((m (member ssh-curve-25519/448)) transport client-packet server-packet)
  (define curve (if (string=? m +kex-curve448-sha512+)
		    x448-curve-parameter
		    x25519-curve-parameter))
  (calculate-shared-secret transport *key:ecdh* curve
			   client-packet server-packet))

(define (calculate-shared-secret transport scheme parameter 
				 client-packet server-packet)
  (define kp (generate-key-pair scheme :ec-parameter parameter))
  (define Q-C (export-public-key (key-pair-public kp) (public-key-format raw)))
  (define (make-kex-ecdh-init)
    (make <ssh-msg-kex-ecdh-init> :Q-C Q-C))
  (define (compute-k transport kex-reply)
    (let* ((K-S (~ kex-reply 'K-S))
	   (Q-S (~ kex-reply 'Q-S))
	   (sig (~ kex-reply 'signature))
	   (spub (generate-public-key scheme Q-S parameter))
	   (K (bytevector->integer
	       (calculate-key-agreement *key:ecdh* (key-pair-private kp) spub)))
	   (msg (make <SSH-ECDH-H>
		  :V-C (~ transport 'client-version)
		  :V-S (~ transport 'server-version)
		  :I-C client-packet :I-S server-packet
		  :K-S K-S :Q-C Q-C :Q-S Q-S :K K)))
      (values K (ssh-verify-signature transport msg K-S sig))))
  (ssh-kex-send/receive transport make-kex-ecdh-init <ssh-msg-kex-ecdh-reply>
			compute-k))

)
