;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/kex/api.scm - SSH2 protocol key exchange API
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
(library (rfc ssh client kex api)
    (export ssh-client-exchange-kex-message
	    ssh-kex-send/receive
	    ssh-verify-signature)
    (import (rnrs)
	    (clos user)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures))
(define-generic ssh-client-exchange-kex-message
  :class <predicate-specializable-generic>)
;; possibly server, but later

(define (ssh-kex-send/receive transport make-init-class reply-class compute-k)
  (ssh-write-ssh-message transport (make-init-class))
  (let* ((reply (ssh-read-packet transport))
	 (kex-reply (bytevector->ssh-message reply-class reply)))
    ;; verify signature
    (let-values (((K h) (compute-k transport kex-reply)))
      ;; send newkeys
      (ssh-write-packet transport (make-bytevector 1 +ssh-msg-newkeys+))
      ;; receive newkeys
      (ssh-read-packet transport)
      ;; compute keys
      (ssh-compute-keys! transport K h #t client-configure))))
(define (client-configure transport
			  client-cipher server-cipher
			  client-mac server-mac)
  (set! (~ transport 'host-cipher) client-cipher)
  (set! (~ transport 'peer-cipher) server-cipher)
  (set! (~ transport 'host-mac) client-mac)
  (set! (~ transport 'peer-mac) server-mac))

(define (ssh-verify-signature transport m K-S signature)
  ;; K-S is either RSA or DSA certificate structure
  ;; so parse it and get the key for verify
  (define (parse-k-s) (ssh-message-bytevector->public-key K-S))
  (define (parse-h key)
    (let ((sig (bytevector->ssh-message <ssh-signature> signature)))
      (values (~ sig 'signature)
	      (verifier-init!
	       (make-ssh-verifier (string->keyword (~ sig 'type)) key)))))
  (define (compute-message-hash transport m)
    (define bv (ssh-message->bytevector m))
    (digest-message (~ transport 'kex-digester) bv))
    
  (let ((key (parse-k-s))
	(h (compute-message-hash transport m)))
    (let-values (((signature verifier) (parse-h key)))
      (unless (~ transport 'session-id) (set! (~ transport 'session-id) h))
      (verifier-process! verifier h)
      (unless (verifier-verify! verifier signature)
	(error 'verify-signature "Invalid siganature"))
      h)))
)
