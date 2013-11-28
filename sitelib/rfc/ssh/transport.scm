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
	    key-exchange
	    service-request
	    ssh-disconnect

	    write-packet
	    read-packet)
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

  ;; I don't think these are useful on SSH since payload must be
  ;; a message format
  (define (ssh-socket-send socket payload . ignore)
    (write-packet socket payload))
  ;; TODO we need a buffer to store the message from peer.
  (define (ssh-socket-recv socket size . ignore)
    (error 'ssh-socket-recv "not supported yet"))


  ;; FIXME
  ;;   currently it's really bad way to read and write a packet
  ;;   (especially read).
  ;;   try read maximum number of packet and encrypt/decrypt if needed.
  ;; TODO
  ;;   * make buffer in socket and read&decrypt per cipher block size
  (define-constant +max-packet-size+ 35000)

  (define (read-packet context)
    (define (verify-mac socket payload mac)
      (and-let* ((h (~ context 'server-mac))
		 (key (~ context 'server-mkey))
		 (c (~ context 'server-sequence))
		 (buf (make-bytevector (+ 4 (bytevector-length payload)))))
	(bytevector-u32-set! buf 0 c (endianness big))
	(bytevector-copy! payload 0 buf 4 (bytevector-length payload))
	(bytevector=? mac (hash HMAC buf :hash h :key key))))
    ;; FIXME this is really bad implementation...
    ;; not efficient at all
    (define (read&decrypt mac-length)
      (let ((c (~ context 'server-cipher))
	    (buf (get-bytevector-all (~ context 'socket))))
	(let*-values (((ct mac) (bytevector-split-at* 
				   buf (- (bytevector-length buf) mac-length)))
		      ((pt) (decrypt c ct)))
	  (verify-mac context pt mac)
	  (open-bytevector-input-port (bytevector-append pt mac)))))

    (let* ((mac-length (or (and-let* ((k (~ context 'client-cipher))
				     (h (~ context 'server-mac)))
			    (hash-size h))
			  0))
	   (in (if (zero? mac-length)
		   (~ context 'socket)
		   (read&decrypt mac-length))))
      (set! (~ context 'server-sequence) (+ (~ context 'server-sequence) 1))
      (let-values (((total-size pad-size) (get-unpack in "!LC")))
	(let ((payload (get-bytevector-n in (- total-size pad-size 1)))
	      (padding (get-bytevector-n in pad-size))
	      (mac     (get-bytevector-n in mac-length)))
	  payload))))

  (define (write-packet context msg)
    (define (compute-mac socket payload) 
      (or (and-let* ((h (~ context 'client-mac))
		     (key (~ context 'client-mkey))
		     (c (~ context 'client-sequence))
		     (buf (make-bytevector (+ 4 (bytevector-length payload)))))
	    (bytevector-u32-set! buf 0 c (endianness big))
	    (bytevector-copy! payload 0 buf 4 (bytevector-length payload))
	    (hash HMAC buf :hash h :key key))
	  #vu8()))
    (define (construct-packet socket payload)
      (define (total-size data-len)
	(define (round-up l)
	  (let* ((c (~ socket 'client-cipher))
		 (size (if c (- (cipher-blocksize c) 1) 7)))
	    (bitwise-and (+ l size) (bitwise-not size))))
	(round-up (+ data-len 4 1 4)))
      (let* ((data-len (bytevector-length payload))
	     (total-len (total-size data-len))
	     (padding (read-random-bytes (~ socket 'prng)
					 (- total-len 4 1 data-len)))
	     (content (call-with-bytevector-output-port
			 (lambda (out)
			   (put-u32 out (- total-len 4) (endianness big))
			   (put-u8 out (bytevector-length padding))
			   (put-bytevector out payload)
			   (put-bytevector out padding))))
	     (mac     (compute-mac socket content)))
	(values payload content mac)))
    (define (encrypt-packet payload)
      (let1 c (~ context 'client-cipher)
	(if c
	    (encrypt c payload)
	    payload)))

    (let-values (((payload content mac) (construct-packet context msg)))
      (set! (~ context 'client-sequence) (+ (~ context 'client-sequence) 1))
      (let1 out (~ context 'socket)
	(put-bytevector out (encrypt-packet content))
	(put-bytevector out mac))
      payload))

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
	(set-port-position! in 0)
	(case (string->symbol name)
	  ((ssh-dss)
	   (let1 c (read-message <ssh-dss-certificate> in)
	     (values DSA
	      (generate-public-key DSA (~ c 'p) (~ c 'q) (~ c 'g) (~ c 'y))
	      '(:der-encode #f))))
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
		  ((vc) (cipher type key))
		  ((h) (hash (kex-digester socket) 
			     (ssh-message->bytevector m))))
      (unless (~ socket 'session-id) (set! (~ socket 'session-id) h))
      (apply verify vc h (~ sig 'signature) verify-options)
      h))

  (define (compute-keys! socket k H)
    (define d (kex-digester socket))
    (define (digest salt) (hash d (bytevector-append k H salt)))
    ;; returns cipher and key size (in bytes)
    (define (cipher&keysize c)
      (cond ((string-prefix? "aes128"   c) (values AES      16))
	    ((string-prefix? "aes256"   c) (values AES      32))
	    ((string-prefix? "3des"     c) (values DESede   24))
	    ((string-prefix? "blowfish" c) (values Blowfish 16))
	    (else (error 'compute-keys! "cipher not supported" c))))
    (define (cipher-mode c)
      (cond ((string-suffix? "cbc" c) MODE_CBC)
	    ;; TODO counter mode
	    (else (error 'compute-keys! "mode not supported" c))))
    (define client-enc (~ socket 'client-enc))
    (define server-enc (~ socket 'server-enc))

    (define (adjust-keysize key size) 
      (let loop ((key key))
	(let1 s (bytevector-length key)
	  (cond ((= s size) key)
		((> s size) (bytevector-copy key 0 size))
		(else
		 ;; compute and append
		 (let1 k (digest key)
		   (loop (bytevector-append key k))))))))
    (define (make-cipher c mode key size iv)
      ;; TODO counter mode
      (cipher c (generate-secret-key c (adjust-keysize key size))
	      :mode mode :iv iv :padder #f))
    (define sid (~ socket 'session-id))
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
	(set! (~ socket 'client-cipher)
	      (make-cipher client-cipher client-mode
			   client-key client-size client-iv))
	(set! (~ socket 'server-cipher)
	      (make-cipher server-cipher server-mode
			   server-key server-size server-iv))
	(set! (~ socket 'client-mkey)
	      (adjust-keysize client-mkey (hash-size (~ socket 'client-mac))))
	(set! (~ socket 'server-mkey)
	      (adjust-keysize server-mkey (hash-size (~ socket 'server-mac)))))
      socket))

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
					(open-bytevector-input-port reply)))
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
				       (open-bytevector-input-port reply)))
	       (K-S (~ dh-reply 'K-S))
	       (H   (~ dh-reply 'H))
	       (f   (~ dh-reply 'f))
	       (K   (mod-expt f x p)))
	  ;; verify signature
	  (let1 h (verify-signature socket (make-verify-message K-S H f K)
				    K-S H)
	    ;; send newkeys
	    (write-packet socket (make-bytevector 1 +ssh-msg-newkeys+))
	    ;; receive newkeys
	    (read-packet socket)
	    ;; compute keys
	    (compute-keys! socket 
			   (call-with-bytevector-output-port 
			    (lambda (out) (write-message :mpint K out #f)))
			   h)))))

    (define (do-dh socket client-packet packet)
      ;; exchange key!
      (let-values (((p g x e) (generate-e&x socket #f)))
	(send/receive socket <ssh-msg-kexdh-init> <ssh-msg-kexdh-reply> p e x
		      (lambda (K-S H f K)
			(make <DH-H> 
			  :V-C (string->utf8 (*ssh-version-string*))
			  :V-S (string->utf8 (~ socket 'target-version))
			  :I-C client-packet
			  :I-S packet
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
			  :I-C client-packet
			  :I-S packet
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
	(cond ((#/group-exchange/ (~ socket 'kex))
	       (do-gex socket client-packet packet))
	      ((#/group\d+/ (~ socket 'kex))
	       (do-dh socket client-packet packet))
	      (else
	       (error 'key-exchange "unknown KEX"))))))

  (define (ssh-disconnect socket :key (code +ssh-disconnect-by-application+)
			  (description "finish"))
    (write-packet socket (ssh-message->bytevector
			  (make <ssh-msg-disconnect>
			    :code code
			    :description description))))

  (define (service-request socket name)
    (write-packet socket (ssh-message->bytevector 
			  (make <ssh-msg-service-request>
			    :service-name (string->utf8 name))))
    (let1 resp (read-message <ssh-msg-service-accept>
			     (open-bytevector-input-port (read-packet socket)))
      resp))
)