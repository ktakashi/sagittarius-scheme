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
    (export ssh-key-exchange)
    (import (rnrs)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh transport io)
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

(define (ssh-key-exchange transport)
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
	(ssh-exchange-kex-message (string->keyword (~ transport 'kex))
				  transport client-packet server-packet)))))

;; send init and receive reply
;; both DH and GEX have the same init/reply structure so
;; for laziness
(define (send/receive transport init-class reply-class p e x make-verify-message)
  (let1 kex-init (make init-class :e e)
    (ssh-write-ssh-message transport kex-init)
    (let* ((reply (ssh-read-packet transport))
	   (kex-reply (read-message reply-class
				    (open-bytevector-input-port reply)))
	   (K-S (~ kex-reply 'K-S))
	   (H   (~ kex-reply 'H))
	   (f   (~ kex-reply 'f))
	   (K   (mod-expt f x p)))
      ;; verify signature
      (let1 h (verify-signature transport (make-verify-message K-S H f K)
				K-S H)
	;; send newkeys
	(ssh-write-packet transport (make-bytevector 1 +ssh-msg-newkeys+))
	;; receive newkeys
	(ssh-read-packet transport)
	;; compute keys
	(compute-keys! transport 
		       (call-with-bytevector-output-port 
			(lambda (out) (write-message :mpint K out #f)))
		       h)))))
(define (generate-x prng r)
  (define size (div (bitwise-length r) 8))
  (let loop ()
    (let ((v (bytevector->integer
	      (random-generator-read-random-bytes prng size))))
      (or (and (< 1 v) v)
	  (loop)))))
(define-generic ssh-exchange-kex-message
  :class <predicate-specializable-generic>)
(define group-exchanges
  (map string->keyword (list +kex-diffie-hellman-group-exchange-sha256+
			     +kex-diffie-hellman-group-exchange-sha1+)))
(define-method ssh-exchange-kex-message ((m (memq group-exchanges))
					 transport client-packet server-packet)
  (define (generate-e&x transport)
    (let1 gex-req (make <ssh-msg-kex-dh-gex-request>)
      (ssh-write-ssh-message transport gex-req)
      (let* ((reply (ssh-read-packet transport))
	     (gex-group (read-message <ssh-msg-kex-dh-gex-group>
				      (open-bytevector-input-port reply)))
	     (p (~ gex-group 'p))
	     (g (~ gex-group 'g)))
	(let1 x (generate-x (~ transport 'prng) (div (- p 1) 2))
	  (values p g x (mod-expt g x p) gex-req)))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (send/receive transport <ssh-msg-kex-dh-gex-init>
		  <ssh-msg-kex-dh-gex-reply> p e x
		  (lambda (K-S H f K)
		    (make <GEX-H> 
		      :V-C (~ transport 'client-version)
		      :V-S (~ transport 'target-version)
		      :I-C client-packet
		      :I-S server-packet
		      :K-S K-S
		      :min (~ req 'min) :n (~ req 'n) :max (~ req 'max)
		      :p p :g g :e e :f f :K K)))))

(define dh-group
  (map string->keyword (list +kex-diffie-hellman-group14-sha256+
			     +kex-diffie-hellman-group14-sha1+
			     +kex-diffie-hellman-group1-sha1+)))
(define-method ssh-exchange-kex-message ((m (memq dh-group))
					 transport client-packet server-packet)
  (define (group-n kex)
    (cond ((#/group(\d+)/ kex) =>
	   (lambda (m) (string->number (m 1))))
	  (else (error 'ssh-exchange-kex-message "must not happen"))))
  (define (generate-e&x transport)
    (let-values (((p g) (case (group-n (~ transport 'kex))
			  ((1)  (values +dh-group1-p+ +dh-group1-g+))
			  ((14) (values +dh-group14-p+ +dh-group14-g+))
			  (else (error 'ssh-exchange-kex-message "unknown group"
				       (~ transport 'kex))))))
      (let1 x (generate-x (~ transport 'prng) p)
	(values p g x (mod-expt g x p) #f))))
  (let-values (((p g x e req) (generate-e&x transport)))
    (send/receive transport <ssh-msg-kexdh-init> <ssh-msg-kexdh-reply> p e x
		  (lambda (K-S H f K)
		    (make <DH-H> 
		      :V-C (~ transport 'client-version)
		      :V-S (~ transport 'target-version)
		      :I-C client-packet
		      :I-S server-packet
		      :K-S K-S
		      :e e :f f :K K)))))

(define (compute-keys! transport k H)
  (define d (~ transport 'kex-digester))
  (define (digest salt) (digest-message d (bytevector-append k H salt)))
  ;; returns cipher and key size (in bytes)
  (define ((make-key-retriever key) size)
    (let loop ((key key))
      (let1 s (bytevector-length key)
	(cond ((= s size) key)
	      ((> s size) (bytevector-copy key 0 size))
	      (else
	       ;; compute and append
	       (let1 k (digest key)
		 (loop (bytevector-append key k))))))))

  (define (create-mac key v) (make-ssh-mac v (make-key-retriever key)))
  (define client-enc (~ transport 'client-enc))
  (define server-enc (~ transport 'server-enc))
  (define sid (~ transport 'session-id))
  (let ((client-iv   (digest (bytevector-append #vu8(#x41) sid)))  ;; "A"
	(server-iv   (digest (bytevector-append #vu8(#x42) sid)))  ;; "B"
	(client-key  (digest (bytevector-append #vu8(#x43) sid)))  ;; "C"
	(server-key  (digest (bytevector-append #vu8(#x44) sid)))  ;; "D"
	(client-mkey (digest (bytevector-append #vu8(#x45) sid)))  ;; "E"
	(server-mkey (digest (bytevector-append #vu8(#x46) sid)))) ;; "F"
    (set! (~ transport 'client-cipher)
	  (make-ssh-cipher client-enc (cipher-direction encrypt)
			   (make-key-retriever client-key) client-iv))
    (set! (~ transport 'server-cipher)
	  (make-ssh-cipher server-enc (cipher-direction decrypt)
			   (make-key-retriever server-key)  server-iv))
    (set! (~ transport 'client-mac)
	  (create-mac client-mkey (~ transport 'client-mac)))
    (set! (~ transport 'server-mac)
	  (create-mac server-mkey (~ transport 'server-mac))))
  transport)

(define (verify-signature transport m K-S H)
  ;; K-S is either RSA or DSA certificate structure
  ;; so parse it and get the key for verify
  (define (parse-k-s) (ssh-message-bytevector->public-key K-S))
  (define (parse-h key)
    (let ((sig (read-message <ssh-signature> (open-bytevector-input-port H))))
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

(define-ssh-message <DH-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (e   :mpint)
   (f   :mpint)
   (K   :mpint)))

(define-ssh-message <GEX-H> ()
  ((V-C :utf8-string)
   (V-S :utf8-string)
   (I-C :string)
   (I-S :string)
   (K-S :string)
   (min :uint32)
   (n   :uint32)
   (max :uint32)
   (p   :mpint)
   (g   :mpint)
   (e   :mpint)
   (f   :mpint)
   (K   :mpint)))

)
