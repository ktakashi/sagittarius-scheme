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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh transport kex ecdh)
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
	    (sagittarius crypto math ec)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport io)
	    (rfc ssh transport kex api))

(define (ecdh-sha2? n) (string-prefix? "ecdh-sha2" n))
(define-method ssh-client-exchange-kex-message
  ((m (?? ecdh-sha2?)) transport client-packet server-packet)
  (define identity (extract-identity m))
  (define curve (ssh-ecdsa-identifier->curve (extract-identity m)))
  (define kp (generate-key-pair *key:ecdsa* :ec-parameter curve))
  (define Q-C (encode-ec-point (ec-parameter-curve curve)
			       (ecdsa-public-key-Q (key-pair-public kp))))
  (define (make-kex-ecdh-init)
    (make <ssh-msg-kex-ecdh-init> :Q-C Q-C))
  (define (compute-k transport kex-reply)
    (let* ((K-S (~ kex-reply 'K-S))
	   (Q-S (~ kex-reply 'Q-S))
	   (sig (~ kex-reply 'signature))
	   (spub (import-public-key *key:ecdsa* Q-S
				    (public-key-format raw) curve))
	   (K (bytevector->integer
	       (calculate-key-agreement *key:ecdh* (key-pair-private kp) spub)))
	   (msg (make <ECDH-H>
		  :V-C (~ transport 'client-version)
		  :V-S (~ transport 'server-version)
		  :I-C client-packet :I-S server-packet
		  :K-S K-S :Q-C Q-C :Q-S Q-S :K K)))
      (values K (ssh-verify-signature transport msg K-S sig))))
  (ssh-kex-send/receive transport make-kex-ecdh-init <ssh-msg-kex-ecdh-reply>
			compute-k))

(define-method ssh-kex-digest ((n (?? ecdh-sha2?)))
  (make-message-digest
   (ssh-ecdsa-digest-descriptor
    (ssh-ecdsa-identifier->curve (extract-identity n)))))
  

(define (extract-identity n)
  (string->keyword (substring n 10 (string-length n))))

(define-ssh-message <ECDH-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (Q-C :string)
   (Q-S :string)
   (K   :mpint)))
)
