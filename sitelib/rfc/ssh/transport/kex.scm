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
	    (rfc ssh util)
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
  (define (generate-e&x transport gex?)
    (define (gen-x r)
      (let loop ()
	(let ((v (bytevector->integer
		     (random-generator-read-random-bytes
		      (~ transport 'prng) (div (bitwise-length r) 8)))))
	  (or (and (< 1 v) v)
	      (loop)))))
    (define (group14? transport) (#/group14/ (~ transport 'kex)))
    (if gex?
	;; TODO min n max range
	(let1 gex-req (make <ssh-msg-kex-dh-gex-request>)
	  (ssh-write-ssh-message transport gex-req)
	  (let* ((reply (ssh-read-packet transport))
		 (gex-group (read-message <ssh-msg-kex-dh-gex-group>
					  (open-bytevector-input-port reply)))
		 (p (~ gex-group 'p))
		 (g (~ gex-group 'g)))
	    (let1 x (gen-x (div (- p 1) 2))
	      (values p g x (mod-expt g x p) gex-req))))
	;; basically p's length is less than or equal to q's so shouldn't
	;; matter, i think
	(let-values (((p g) (if (group14? transport)
				(values +dh-group14-p+ +dh-group14-g+)
				(values +dh-group1-p+ +dh-group1-g+))))
	  (let1 x (gen-x p)
	    (values p g x (mod-expt g x p) #f)))))
  ;; send init and receive reply
  ;; both DH and GEX have the same init/reply structure so
  ;; for laziness
  (define (send/receive transport init-class reply-class p e x
			make-verify-message)
    (let1 dh-init (make init-class :e e)
      (ssh-write-ssh-message transport dh-init)
      (let* ((reply (ssh-read-packet transport))
	     (dh-reply (read-message reply-class
				     (open-bytevector-input-port reply)))
	     (K-S (~ dh-reply 'K-S))
	     (H   (~ dh-reply 'H))
	     (f   (~ dh-reply 'f))
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

  (define (do-dh transport client-packet packet)
    ;; exchange key!
    (let-values (((p g x e req) (generate-e&x transport #f)))
      (send/receive transport <ssh-msg-kexdh-init> <ssh-msg-kexdh-reply> p e x
		    (lambda (K-S H f K)
		      (make <DH-H> 
			:V-C (~ transport 'client-version)
			:V-S (~ transport 'target-version)
			:I-C client-packet
			:I-S packet
			:K-S K-S
			:e e :f f :K K)))))

  (define (do-gex transport client-packet packet)
    (let-values (((p g x e req) (generate-e&x transport #t)))
      (send/receive transport <ssh-msg-kex-dh-gex-init>
		    <ssh-msg-kex-dh-gex-reply> p e x
		    (lambda (K-S H f K)
		      (make <GEX-H> 
			:V-C (~ transport 'client-version)
			:V-S (~ transport 'target-version)
			:I-C client-packet
			:I-S packet
			:K-S K-S
			:min (~ req 'min) :n (~ req 'n) :max (~ req 'max)
			:p p :g g :e e :f f :K K)))))
  (define cookie
    (random-generator-read-random-bytes (~ transport 'prng) 16))
  (let1 client-kex (make <ssh-msg-keyinit> :cookie cookie)
    (let-values (((in/out size) (ssh-message->binary-port client-kex)))
      (ssh-write-packet-port transport in/out size)
      (set-port-position! in/out 0)
      (let* ((client-packet (get-bytevector-all in/out))
	     (packet (ssh-read-packet transport))
	     (server-kex (read-message <ssh-msg-keyinit> 
				       (open-bytevector-input-port packet))))
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
	(set! (~ transport 'kex-digester)
	      (make-message-digest (extract-digest (~ transport 'kex))))
	(cond ((#/group-exchange/ (~ transport 'kex))
	       (do-gex transport client-packet packet))
	      ((#/group\d+/ (~ transport 'kex))
	       (do-dh transport client-packet packet))
	      (else
	       (error 'key-exchange "unknown KEX")))))))

(define (compute-keys! transport k H)
  (define d (~ transport 'kex-digester))
  (define (digest salt) (digest-message d (bytevector-append k H salt)))
  ;; returns cipher and key size (in bytes)
  (define (cipher&keysize c)
    (cond ((string-prefix? "aes128"   c) (values *scheme:aes-128*  16))
	  ((string-prefix? "aes192"   c) (values *scheme:aes-192*  24))
	  ((string-prefix? "aes256"   c) (values *scheme:aes-256*  32))
	  ((string-prefix? "3des"     c) (values *scheme:desede*   24))
	  ((string-prefix? "blowfish" c) (values *scheme:blowfish* 16))
	  (else (error 'compute-keys! "cipher not supported" c))))
  (define (cipher-mode c)
    (cond ((string-suffix? "cbc" c) *mode:cbc*)
	  ((string-suffix? "ctr" c) *mode:ctr*)
	  ;; TODO counter mode
	  (else (error 'compute-keys! "mode not supported" c))))
  (define (create-mac key v)
    (define (make-mac/adjusted-key mac key digest)
      (make-mac mac (adjust-keysize key (digest-descriptor-digest-size digest))
		:digest digest))
    (cond ((string=? "hmac-sha1" v)
	   (make-mac/adjusted-key *mac:hmac* key *digest:sha-1*))
	  ((string=? "hmac-sha2-256" v)
	   (make-mac/adjusted-key *mac:hmac* key *digest:sha-256*))
	  ((string=? "hmac-sha2-512" v)
	   (make-mac/adjusted-key *mac:hmac* key *digest:sha-512*))
	  ((string=? "hmac-md5" v)
	   (make-mac/adjusted-key *mac:hmac* key *digest:md5*))
	  (else (error 'create-mac "Only HMAC is supported" v))))
  (define (adjust-keysize key size) 
    (let loop ((key key))
      (let1 s (bytevector-length key)
	(cond ((= s size) key)
	      ((> s size) (bytevector-copy key 0 size))
	      (else
	       ;; compute and append
	       (let1 k (digest key)
		 (loop (bytevector-append key k))))))))

  (define client-enc (~ transport 'client-enc))
  (define server-enc (~ transport 'server-enc))

  (define (make-cipher c mode key size iv direction)
    (block-cipher-init!
     (make-block-cipher c mode no-padding)
     direction
     (generate-symmetric-key c (adjust-keysize key size))
     (make-cipher-parameter
      (make-iv-parameter iv)
      (make-counter-mode-parameter *ctr-mode:big-endian*))))
  (define sid (~ transport 'session-id))

  (let ((client-iv   (digest (bytevector-append #vu8(#x41) sid))) ;; "A"
	(server-iv   (digest (bytevector-append #vu8(#x42) sid))) ;; "B"
	(client-key  (digest (bytevector-append #vu8(#x43) sid))) ;; "C"
	(server-key  (digest (bytevector-append #vu8(#x44) sid))) ;; "D"
	(client-mkey (digest (bytevector-append #vu8(#x45) sid))) ;; "E"
	(server-mkey (digest (bytevector-append #vu8(#x46) sid))) ;; "F"
	(client-mode (cipher-mode client-enc))
	(server-mode (cipher-mode server-enc)))
    (let-values (((client-cipher client-size) (cipher&keysize client-enc))
		 ((server-cipher server-size) (cipher&keysize server-enc)))
      (set! (~ transport 'client-cipher)
	    (make-cipher client-cipher client-mode
			 client-key client-size client-iv
			 (cipher-direction encrypt)))
      (set! (~ transport 'server-cipher)
	    (make-cipher server-cipher server-mode
			 server-key server-size server-iv
			 (cipher-direction decrypt)))
      (set! (~ transport 'client-mac)
	    (create-mac client-mkey (~ transport 'client-mac)))
      (set! (~ transport 'server-mac)
	    (create-mac server-mkey (~ transport 'server-mac))))
    transport))

(define (verify-signature transport m K-S H)
  ;; K-S is either RSA or DSA certificate structure
  ;; so parse it and get the key for verify
  (define (parse-k-s) (bytevector->ssh-public-key K-S))
  (define (parse-h key)
    (let ((sig (read-message <ssh-signature> (open-bytevector-input-port H))))
      (values (~ sig 'signature)
	      (verifier-init! 
	       (case (string->symbol (~ sig 'type))
		 ((ssh-rsa)
		  (make-verifier *signature:rsa* key
				 :digest *digest:sha-1*
				 :verifier pkcs1-emsa-v1.5-verify))
		 ((ssh-dss)
		  (make-verifier *signature:dsa* key :der-encode #f))
		 ((rsa-sha2-256)
		  (make-verifier *signature:rsa* key
				 :digest *digest:sha-256*
				 :verifier pkcs1-emsa-v1.5-verify))
		 ((rsa-sha2-512)
		  (make-verifier *signature:rsa* key
				 :digest *digest:sha-512*
				 :verifier pkcs1-emsa-v1.5-verify)))))))
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

(define (extract-digest v)
  ;; a bit lazy way of extracting hash algorithm...
  (cond ((#/sha(\d+)/ v)
	 => (lambda (m) (case (string->number (m 1))
			  ((1) *digest:sha-1*)
			  ((256) *digest:sha-256*)
			  ((384) *digest:sha-384*)
			  ((512) *digest:sha-512*))))
	(else (error 'extract-digest "Hash algorighm not supported" v))))

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
