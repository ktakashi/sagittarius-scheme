;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/kex.scm - SSH2 protocol transport key exchange
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh transport kex)
    (export ssh-client-key-exchange
	    ssh-key-exchange)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport io)
	    (rfc ssh transport kex api)
	    (rfc ssh transport kex dh)
	    (rfc ssh transport kex ecdh)
	    (clos user)
	    (srfi :13 strings)
	    (sagittarius)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto keys)
	    (sagittarius crypto mac)
	    (sagittarius crypto random)
	    (sagittarius crypto signatures)
	    (sagittarius control)
	    (sagittarius object))

(define ((ssh-key-exchange key-algorithms exchange-kex-message) transport . opts)
  (define (fill-slot transport-slot req res kex-slot)
    (let ((cnl (~ req kex-slot 'names))
	  (snl (~ res kex-slot 'names)))
      (let loop ((lis cnl))
	(cond ((null? lis)
	       (error 'key-exchange "algorithm not supported" cnl snl))
	      ((member (car lis) snl) =>
	       (lambda (l)
		 (rlet1 v (string->symbol (car l))
		  (when transport-slot
		    (set! (~ transport transport-slot) (car l))))))
	      (else (loop (cdr lis)))))))
  (define cookie
    (random-generator-read-random-bytes (~ transport 'prng) 16))
  (let* ((host-kex (apply make <ssh-msg-keyinit> :cookie cookie
			  :kex-algorithms (key-algorithms) opts))
	 (host-packet (ssh-message->bytevector host-kex)))
    (ssh-write-packet transport host-packet)
    (let* ((peer-packet (ssh-read-packet transport))
	   (peer-kex (bytevector->ssh-message <ssh-msg-keyinit> peer-packet)))
      ;; ok do key exchange
      ;; first decide the algorithms
      (fill-slot 'kex host-kex peer-kex 'kex-algorithms)
      (fill-slot 'client-enc host-kex peer-kex
		 'encryption-algorithms-client-to-server)
      (fill-slot 'server-enc host-kex peer-kex
		 'encryption-algorithms-server-to-client)
      (fill-slot 'client-mac host-kex peer-kex 'mac-algorithms-client-to-server)
      (fill-slot 'server-mac host-kex peer-kex 'mac-algorithms-server-to-client)
      (fill-slot 'public-key-algorithm host-kex peer-kex
		 'server-host-key-algorithms)
      ;; dispatch
      (set! (~ transport 'kex-digester) (ssh-kex-digest (~ transport 'kex)))
      (exchange-kex-message (~ transport 'kex)
	transport host-packet peer-packet))))

(define ssh-client-key-exchange
  (ssh-key-exchange *ssh-client-kex-list* ssh-client-exchange-kex-message))
  
)
