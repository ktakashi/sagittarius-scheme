;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/kex/api.scm - SSH2 server KEX API
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
(library (rfc ssh server kex api)
    (export ssh-server-exchange-kex-message
	    ssh-kex-receive/send)
    (import (rnrs)
	    (clos user)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport)
	    (rfc ssh server types)
	    (sagittarius object)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures))

(define-generic ssh-server-exchange-kex-message 
  :class <predicate-specializable-generic>)

(define (ssh-kex-receive/send transport init-class compute-k make-reply)
  (define (message-hash m) (digest-message (~ transport 'kex-digester) m))
  (let* ((init (ssh-read-packet transport))
	 (kex-init (bytevector->ssh-message init-class init)))
    (let*-values (((type signer verifier K-S) (host-key-signer transport))
		  ((K h-seed . rest) (compute-k K-S kex-init)))
      (let* ((H (message-hash (ssh-message->bytevector h-seed)))
	     (sig (signer-sign-message signer H))
	     (msg (apply make-reply K-S
			 (make <ssh-signature> :type type :signature sig)
			 rest)))
	(ssh-write-ssh-message transport msg)
	;; wait for newkeys
	(ssh-read-packet transport)
	(ssh-write-packet transport (make-bytevector 1 +ssh-msg-newkeys+))
	(unless (~ transport 'session-id) (set! (~ transport 'session-id) H))
	(ssh-compute-keys! transport K H #f server-configure)))))

(define (server-configure transport
			  client-cipher server-cipher
			  client-mac server-mac)
  (set! (~ transport 'host-cipher) server-cipher)
  (set! (~ transport 'peer-cipher) client-cipher)
  (set! (~ transport 'host-mac) server-mac)
  (set! (~ transport 'peer-mac) client-mac))

(define (host-key-signer transport)
  (define algorithm-name (~ transport 'public-key-algorithm))
  (define host-keys (~ transport 'host-keys))
  (define (pred o)
    (string=? algorithm-name (ssh-host-key-algorithm-name o)))
  (define (search)
    (cond ((memp pred host-keys) => car)
	  (else (error 'ssh-server-kex "Host key not found" algorithm-name))))
  (let* ((host-key (search))
	 (pk (ssh-host-key-private host-key))
	 (pub (ssh-host-key-public host-key)))
    (values algorithm-name
	    (make-ssh-signer algorithm-name pk)
	    (make-ssh-verifier algorithm-name pub)
	    (ssh-message->bytevector (make-ssh-public-key pub)))))

)
