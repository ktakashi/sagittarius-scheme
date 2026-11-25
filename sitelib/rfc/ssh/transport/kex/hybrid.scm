;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/kex/hybrid.scm - SSH hybrid post-quantum KEX
;;;
;;;   Copyright (c) 2026  Sagittarius contributors.
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
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;;   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;;   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!nounbound
(library (rfc ssh transport kex hybrid)
    (export ssh-kex-digest
	    <SSH-HYBRID-H>
	    ssh-hybrid-kex-names
	    ssh-hybrid-combine-secrets
	    ssh-hybrid-client-init-components
	    ssh-hybrid-server-reply-components
	    ssh-hybrid-valid-packet?)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto digests)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport kex api))

(define ssh-hybrid-kex-names (list +kex-mlkem768x25519-sha256+))

(define-method ssh-kex-digest ((name (member ssh-hybrid-kex-names)))
  (make-message-digest *digest:sha-256*))

(define-ssh-message <SSH-HYBRID-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (C-INIT :string)
   (S-REPLY :string)
   (K :string)))

(define (ssh-hybrid-combine-secrets pq-secret classical-secret)
  (unless (and (= (bytevector-length pq-secret) 32)
	       (= (bytevector-length classical-secret) 32))
    (error 'ssh-hybrid-combine-secrets "Invalid hybrid shared secret length"))
  (digest-message (make-message-digest *digest:sha-256*)
		  (bytevector-append pq-secret classical-secret)))

(define (ssh-hybrid-client-init-components init)
  (unless (= (bytevector-length init) (+ 1184 32))
    (error 'ssh-hybrid-client-init-components "Invalid hybrid initialization length"))
  (values (bytevector-copy init 0 1184)
	  (bytevector-copy init 1184)))

(define (ssh-hybrid-server-reply-components reply)
  (unless (= (bytevector-length reply) (+ 1088 32))
    (error 'ssh-hybrid-server-reply-components "Invalid hybrid reply length"))
  (values (bytevector-copy reply 0 1088)
	  (bytevector-copy reply 1088)))

(define (ssh-hybrid-valid-packet? packet message expected-type)
  (and (> (bytevector-length packet) 0)
       (= (bytevector-u8-ref packet 0) expected-type)
       (bytevector=? packet (ssh-message->bytevector message))))
)
