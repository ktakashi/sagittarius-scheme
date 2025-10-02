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
    (export ssh-key-exchange
	    ssh-compute-keys!
	    ssh-kex-digest)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport io)
	    (rfc ssh transport kex api)
	    (rfc ssh transport kex dh)
	    (rfc ssh transport kex ecdh)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto digests)
	    (sagittarius crypto mac)
	    (sagittarius crypto random)
	    (sagittarius control)
	    (sagittarius object))

(define ((ssh-key-exchange exchange-kex-message client?) transport
	 :key (peer-packet #f)
	 :allow-other-keys opts)
  (define (fill-slot transport-slot req res kex-slot)
    (let ((cnl (~ (if client? req res) kex-slot 'names))
	  (snl (~ (if client? res req) kex-slot 'names)))
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
  (let* ((host-kex (apply make <ssh-msg-keyinit> :cookie cookie opts))
	 (host-packet (ssh-message->bytevector host-kex)))
    (ssh-write-packet transport host-packet)
    (let* ((peer-packet (or peer-packet (ssh-read-packet transport)))
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

(define (ssh-compute-keys! transport K H client? configure)
  (define k (let-values (((out e) (open-bytevector-output-port)))
	      (ssh-write-message :mpint K out #f)
	      (e)))
  (define d (~ transport 'kex-digester))
  (define (digest salt) (digest-message d (bytevector-append k H salt)))
  ;; returns cipher and key size (in bytes)
  (define ((make-key-retriever key) size)
    (let loop ((key key))
      (let ((s (bytevector-length key)))
	(cond ((= s size) key)
	      ((> s size) (bytevector-copy key 0 size))
	      ;; compute and append
	      (else (loop (bytevector-append key (digest key))))))))

  (define (create-mac key v) (make-ssh-mac v (make-key-retriever key)))
  (define client-enc (~ transport 'client-enc))
  (define server-enc (~ transport 'server-enc))
  (define c->s-direction (if client? 'encrypt 'decrypt))
  (define s->c-direction (if client? 'decrypt 'encrypt))
  (define sid (~ transport 'session-id))

  (let ((client-iv   (digest (bytevector-append #vu8(#x41) sid)))  ;; "A"
	(server-iv   (digest (bytevector-append #vu8(#x42) sid)))  ;; "B"
	(client-key  (digest (bytevector-append #vu8(#x43) sid)))  ;; "C"
	(server-key  (digest (bytevector-append #vu8(#x44) sid)))  ;; "D"
	(client-mkey (digest (bytevector-append #vu8(#x45) sid)))  ;; "E"
	(server-mkey (digest (bytevector-append #vu8(#x46) sid)))) ;; "F"
    (configure transport
     (make-ssh-cipher client-enc c->s-direction
		      (make-key-retriever client-key) 
		      (make-key-retriever client-iv))
     (make-ssh-cipher server-enc s->c-direction
		      (make-key-retriever server-key)
		      (make-key-retriever server-iv))
     (create-mac client-mkey (~ transport 'client-mac))
     (create-mac server-mkey (~ transport 'server-mac)))
    transport))
)
