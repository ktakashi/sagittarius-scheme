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
    (export ssh-client-key-exchange)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport io)
	    (rfc ssh transport kex dh)
	    (clos user)
	    (binary pack)
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

(define (ssh-client-key-exchange transport)
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
  (let1 client-kex (make <ssh-msg-keyinit> :cookie cookie)
    (let-values (((in/out size) (ssh-message->binary-port client-kex)))
      (ssh-write-packet-port transport in/out size)
      (set-port-position! in/out 0)
      (let* ((client-packet (get-bytevector-all in/out))
	     (server-packet (ssh-read-packet transport))
	     (server-kex (read-message <ssh-msg-keyinit> 
			  (open-bytevector-input-port server-packet))))
	;; ok do key exchange
	;; first decide the algorithms
	(fill-slot 'kex client-kex server-kex 'kex-algorithms)
	(fill-slot 'client-enc client-kex server-kex
		   'encryption-algorithms-client-to-server)
	(fill-slot 'server-enc client-kex server-kex
		   'encryption-algorithms-server-to-client)
	(fill-slot 'client-mac client-kex server-kex
		   'mac-algorithms-client-to-server)
	(fill-slot 'server-mac client-kex server-kex
		   'mac-algorithms-server-to-client)
	;; dispatch
	(set! (~ transport 'kex-digester) (ssh-kex-digest (~ transport 'kex)))
	(ssh-client-exchange-kex-message (string->keyword (~ transport 'kex))
	 transport client-packet server-packet))))))
