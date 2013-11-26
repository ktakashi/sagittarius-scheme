;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport.scm - SSH2 protocol transport layer.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh transport)
    (export make-client-ssh-socket
	    ;; parameter
	    *ssh-version-string*
	    ;; for testing
	    version-exchange
	    key-exchange)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (srfi :13 strings)
	    (srfi :39 parameters)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc hmac)
	    (util bytevector)
	    (math)
	    (crypto)
	    (clos user)
	    (binary pack)
	    (binary data))

  ;; must do until handshake but for now
  (define (make-client-ssh-socket server port . options)
    (let1 socket (make-client-socket server port)
      (make <ssh-socket> :socket (socket-port socket))))

  (define-constant cr #x0D)
  (define-constant lf #x0A)

  (define-constant +default-version-string+ 
    (string-append "SSH-2.0-Sagittarius_" (sagittarius-version)))
  (define *ssh-version-string* (make-parameter +default-version-string+))

  ;; utility
  (define (read-ascii-line in)
    (call-with-string-output-port
     (lambda (out)
       (let1 buf (make-bytevector 1)
	 (let loop ((r (get-bytevector-n! in buf 0 1)))
	   (unless (zero? r)
	     (let1 u8 (bytevector-u8-ref buf 0)
	       (cond ((= u8 cr) (get-bytevector-n! in buf 0 1))
		     ;; version string must end CR LF however
		     ;; OpenSSH 4.3 returns only LF...
		     ((= u8 lf))
		     (else
		      (let1 ch (integer->char u8)
			(put-char out ch)
			(loop (get-bytevector-n! in buf 0 1))))))))))))
  
  ;; write line with CR LF
  (define (write-line out str)
    (put-bytevector out (string->utf8 str))
    (put-bytevector out #vu8(#x0D #x0A)))

  (define (version-exchange socket)
    (let1 in/out (~ socket 'socket)
      (let loop ()
	(let1 vs (read-ascii-line in/out)
	  (cond ((zero? (string-length vs))
		 ;; shutdown raw socket
		 (socket-shutdown raw-socket SHUT_RDWR)
		 (socket-close raw-socket)
		 (error 'version-exchange "no version string"))
		((#/(SSH-2.0-[\w.-]+)\s*/ vs) => 
		 (lambda (m)
		   (set! (~ socket 'target-version) (m 1))
		   ;; send
		   (write-line in/out (*ssh-version-string*))))
		(else (loop)))))))

  (define (ssh-socket-send socket payload :optional (flags 0))
    (put-bytevector (~ socket 'socket) (construct-packet socket payload) flags))

  ;; think about it...
  (define (ssh-socket-recv socket ))

  ;; very *LISPY* isn't it? ... orz
  (define *mac-length* (make-parameter 0))
  (define-simple-datum-define define-packet packet-read packet-write)
  (define-packet <ssh-binary-packet> ()
    (payload padding mac)
    (lambda (in)
      (let-values (((total-size pad-size) (get-unpack in "!LC")))
	(let ((payload (get-bytevector-n in (- total-size pad-size 1)))
	      (padding (get-bytevector-n in pad-size))
	      (mac     (get-bytevector-n in (*mac-length*))))
	  (values payload padding (if (eof-object? mac) #vu8() mac)))))
    (lambda (out payload padding mac)
      (let1 size (+ (bytevector-length payload) (bytevector-length padding) 1)
	(put-u32 out size (endianness big))
	(put-u8 out (bytevector-length padding))
	(put-bytevector out payload)
	(put-bytevector out padding)
	(put-bytevector out mac))))

  (define (read-packet context)
    ;; TODO get mac size
    (parameterize ((*mac-length* (or (and-let* ((k (~ context 'client-key))
						(h (~ context 'server-mac)))
				       (hash-size h))
				     0)))
      (packet-read <ssh-binary-packet> (~ context 'socket))))
  (define (write-packet context msg)
    ;; TODO compute mac!
    (define (compute-mac socket payload) 
      (or (and-let* ((h (~ context 'client-mac))
		     (key (~ context 'client-mkey))
		     (c (~ context 'sequence))
		     (buf (make-bytevector (+ 4 (bytevector-length payload)))))
	    (bytevector-u32-set! buf 0 c (endianness big))
	    (bytevector-copy! payload 0 buf 4 (bytevector-length payload))
	    (hash HMAC buf :hash h :key key))
	  #vu8()))
    (define (construct-packet socket payload)
      (define (total-size data-len)
	;; TODO cipher block size or 8
	(define (round-up l)
	  (bitwise-and (+ l 7) (bitwise-not 7)))
	(round-up (+ data-len 4 1 4)))
      (let* ((data-len (bytevector-length payload))
	     (total-len (total-size data-len))
	     (padding (read-random-bytes (~ socket 'prng)
					 (- total-len 4 1 data-len)))
	     (mac     (compute-mac socket payload)))
	(make <ssh-binary-packet>
	  :payload payload :padding padding :mac mac)))

    (let1 packet (construct-packet context msg)
      (packet-write <ssh-binary-packet> packet (~ context 'socket))
      packet))

  ;; for my laziness
  (define-ssh-message <DH-H> ()
    ((V-C :string)
     (V-S :string)
     (I-C :string)
     (I-S :string)
     (K-S :string)
     (e   :mpint)
     (f   :mpint)
     (K   :mpint)))

  (define-ssh-message <GEX-H> ()
    ((V-C :string)
     (V-S :string)
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

  (define (kex-digester socket) 
    (cond ((#/sha(\d+)/ (~ socket 'kex))
	   => (lambda (m) (case (string->number (m 1))
			    ((1) SHA-1)
			    ((256) SHA-256))))
	  (else (error 'kex-digester "Hash algorighm not supported"
		       (~ socket 'kex)))))

  (define (verify-signature socket m K-S H)
    ;; K-S is either RSA or DSA certificate structure
    ;; so parse it and get the key for verify
    (define (parse-k-s)
      (let* ((in (open-bytevector-input-port K-S))
	     (size (get-unpack in "!L"))
	     (name (utf8->string (get-bytevector-n in size))))
	(case (string->symbol name)
	  ((ssh-dss)
	   (let1 c (read-message <ssh-dss-certificate> in)
	     (values DSA
	      (generate-public-key DSA (~ c 'p) (~ c 'q) (~ c 'g) (~ c 'y))
	      '())))
	  ((ssh-rsa)
	   (let1 c (read-message <ssh-rsa-certificate> in)
	     (values RSA
	      (generate-public-key RSA (~ c 'n) (~ c 'e))
	      (list :verify pkcs1-emsa-v1.5-verify))))
	  (else
	   (error 'verify-signature "unknown method" name)))))
    (define (parse-h)
      (read-message <ssh-signature> (open-bytevector-input-port H)))
    (let*-values (((type key verify-options) (parse-k-s))
		  ((sig) (parse-h))
		  ((vc) (cipher type key)))
      (apply verify vc 
	     (hash (kex-digester socket) (ssh-message->bytevector m))
	     (~ sig 'signature) 
	     verify-options)))

  (define (compute-keys! socket K H)
    (define k (integer->bytevector K))
    (define d (kex-digester socket))
    (define (digest mark)
      (hash d (bytevector-append k H mark (~ socket 'session-id))))
    (set! (~ socket 'client-iv)   (digest #vu8(#x41))) ;; "A"
    (set! (~ socket 'server-iv)   (digest #vu8(#x42))) ;; "B"
    (set! (~ socket 'client-key)  (digest #vu8(#x43))) ;; "C"
    (set! (~ socket 'server-key)  (digest #vu8(#x44))) ;; "D"
    (set! (~ socket 'client-mkey) (digest #vu8(#x45))) ;; "E"
    (set! (~ socket 'server-mkey) (digest #vu8(#x46))) ;; "F"
    socket)

  (define (key-exchange socket)
    (define macs `((hmac-sha1 . ,SHA-1)))
    (define (fill-slot socket-slot req res kex-slot)
      (let ((cnl (~ req kex-slot 'names))
	    (snl (~ res kex-slot 'names)))
	(let loop ((lis cnl))
	  (cond ((null? lis) (error 'key-exchange "algorithm not supported"))
		((member (car lis) snl) =>
		 (lambda (l) 
		   (rlet1 v (string->symbol (car l))
		     (when socket-slot
		       (set! (~ socket socket-slot) 
			     (cond ((assq v macs) => cdr)
				   (else (car l))))))))
		(else (loop (cdr lis)))))))
    (define (generate-e&x socket gex?)
      (define (gen-x r)
	(bytevector->integer (read-random-bytes (~ socket 'prng) 
						(div (bitwise-length r) 8))))
      (define (group14? socket) (#/group14/ (~ socket 'kex)))
      (if gex?
	  ;; TODO min n max range
	  (let1 gex-req (make <ssh-msg-kex-dh-gex-request>)
	    (write-packet socket (ssh-message->bytevector gex-req))
	    (let* ((reply (read-packet socket))
		   (gex-group (read-message <ssh-msg-kex-dh-gex-group>
					(open-bytevector-input-port 
					 (~ reply 'payload))))
		   (p (~ gex-group 'p))
		   (g (~ gex-group 'g)))
	      (let1 x (gen-x (div (- p 1) 2))
		(values p g x (mod-expt g x p)))))
	  ;; basically p's length is less than or equal to q's so shouldn't
	  ;; matter, i think
	  (let-values (((p g) (if (group14? socket)
				  (values +dh-group14-p+ +dh-group14-g+)
				  (values +dh-group1-p+ +dh-group1-g+))))
	    (let1 x (gen-x p)
	      (values p g x (mod-expt g x p))))))
    ;; send init and receive reply
    ;; both DH and GEX have the same init/reply structure so
    ;; for laziness
    (define (send/receive socket init-class reply-class p e x
			  make-verify-message)
      (let1 dh-init (make init-class :e e)
	(write-packet socket (ssh-message->bytevector dh-init))
	(let* ((reply (read-packet socket))
	       (dh-reply (read-message reply-class
				       (open-bytevector-input-port 
					(~ reply 'payload))))
	       (K-S (~ dh-reply 'K-S))
	       (H   (~ dh-reply 'H))
	       (f   (~ dh-reply 'f))
	       (K   (mod-expt f x p)))
	  ;; verify signature
	  (verify-signature socket (make-verify-message K-S H f K) K-S H)
	  (unless (~ socket 'session-id)
	    (set! (~ socket 'session-id) K-S))
	  ;; send newkeys
	  (write-packet socket (make-bytevector 1 +ssh-msg-newkeys+))
	  ;; compute keys
	  (compute-keys! socket K H))))

    (define (do-dh socket client-packet packet)
      ;; exchange key!
      (let-values (((p g x e) (generate-e&x socket #f)))
	(send/receive socket <ssh-msg-kexdh-init> <ssh-msg-kexdh-reply> p e x
		      (lambda (K-S H f K)
			(make <DH-H> 
			  :V-C (string->utf8 (*ssh-version-string*))
			  :V-S (string->utf8 (~ socket 'target-version))
			  :I-C (~ client-packet 'payload)
			  :I-S (~ packet 'payload)
			  :K-S K-S
			  :e e :f f :K K)))))

    (define (do-gex socket client-packet packet)
      (let-values (((p g x e) (generate-e&x socket #t)))
	(send/receive socket <ssh-msg-kex-dh-gex-init>
		      <ssh-msg-kex-dh-gex-reply> p e x
		      (lambda (K-S H f K)
			(make <GEX-H> 
			  :V-C (string->utf8 (*ssh-version-string*))
			  :V-S (string->utf8 (~ socket 'target-version))
			  :I-C (~ client-packet 'payload)
			  :I-S (~ packet 'payload)
			  :K-S K-S
			  ;; TODO get size
			  :min 1024 :n 1024 :max 2048
			  :p p :g g :e e :f f :K K)))))

    (let1 client-kex (make <ssh-msg-keyinit> 
		       :cookie (read-random-bytes (~ socket 'prng) 16))
      (let* ((client-packet
	      (write-packet socket (ssh-message->bytevector client-kex)))
	     (packet (read-packet socket))
	     (server-kex (read-message <ssh-msg-keyinit> 
				       (open-bytevector-input-port 
					(~ packet 'payload)))))
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
	(cond ((#/group-exchange/ (~ socket 'kex))
	       (do-gex socket client-packet packet))
	      ((#/group\d+/ (~ socket 'kex))
	       (do-dh socket client-packet packet))
	      (else
	       (error 'key-exchange "unknown KEX"))))))
)