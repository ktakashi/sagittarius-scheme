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
	    (math)
	    (crypto)
	    (clos user)
	    (binary pack)
	    (binary data))

  ;; must do until handshake but for now
  (define (make-client-ssh-socket server port . options)
    (let1 socket (make-client-socket server port)
      (make <ssh-socket> :socket socket)))

  (define-constant cr #x0D)
  (define-constant lf #x0A)

  (define-constant +default-version-string+ "SSH-2.0-Sagittarius")
  (define *ssh-version-string* (make-parameter +default-version-string+))

  ;; utility
  (define (read-ascii-line sock)
    (call-with-string-output-port
     (lambda (out)
       (let1 buf (make-bytevector 1)
	 (let loop ((r (socket-recv! sock buf 0 1)))
	   (unless (zero? r)
	     (let1 u8 (bytevector-u8-ref buf 0)
	       (cond ((= u8 cr) (socket-recv! sock buf 0 1))
		     ;; version string must end CR LF however
		     ;; OpenSSH 4.3 returns only LF...
		     ((= u8 lf))
		     (else
		      (let1 ch (integer->char u8)
			(put-char out ch)
			(loop (socket-recv! sock buf 0 1))))))))))))

  ;; write line with CR LF
  (define (write-line sock str)
    (socket-send sock (string->utf8 str))
    (socket-send sock #vu8(#x0D #x0A)))

  (define (version-exchange socket)
    (let1 raw-socket (~ socket 'socket)
      (let loop ()
	(let1 vs (read-ascii-line raw-socket)
	  (cond ((zero? (string-length vs))
		 ;; shutdown raw socket
		 (socket-shutdown raw-socket SHUT_RDWR)
		 (socket-close raw-socket)
		 (error 'version-exchange "no version string"))
		((#/(SSH-2.0-[\w.-]+)\s*/ vs) => 
		 (lambda (m)
		   (set! (~ socket 'target-version) (m 1))
		   ;; send
		   (write-line raw-socket (*ssh-version-string*))))
		(else (loop)))))))

  (define (ssh-socket-send socket payload :optional (flags 0))
    (socket-send (~ socket 'socket) (construct-packet socket payload) flags))

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

  (define (read-packet in context)
    ;; TODO get mac size
    (parameterize ((*mac-length* (or (and-let* ((k (~ context 'shared-key))
						(h (~ context 'server-mac)))
				       (hash-size h))
				     0)))
      (packet-read <ssh-binary-packet> in)))
  (define (write-packet out msg context)
    ;; TODO compute mac!
    (define (compute-mac socket payload) 
      (or (and-let* ((h (~ context 'client-mac))
		     (key (~ context 'shared-key))
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
      (packet-write <ssh-binary-packet> packet out)))

  (define (verify-signature socket I-C I-S K-S e f K H)
    (let1 m (bytevector-append (string->utf8 (*ssh-version-string*))
			       (string->utf8 (~ socket 'target-version))
			       I-C I-S K-S
			       (integer->bytevector e)
			       (integer->bytevector f)
			       (integer->bytevector K))
      (format #t "~X~%" (bytevector->integer m))
      (format #t "~X~%" (bytevector->integer H))
      (format #t "~X~%" (bytevector->integer (hash SHA-1 m))))
    )

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
			     (cond ((assq (car l) macs) => cdr)
				   (else (car l))))))))
		(else (loop (cdr lis)))))))
    (let1 client-kex (make <ssh-msg-keyinit> 
		:cookie (read-random-bytes (~ socket 'prng) 16))
      (call-with-port (socket-port (~ socket 'socket) #f)
	(lambda (in/out)
	  (write-packet in/out (ssh-message->bytevector client-kex) socket)
	  (let* ((packet (read-packet in/out socket))
		 (server-kex (read-message <ssh-msg-keyinit> 
				(open-bytevector-input-port 
				 (~ packet 'payload)))))
	    ;; ok do key exchange
	    ;; first decide the algorithms
	    (fill-slot 'client-enc client-kex server-kex
		       'encryption-algorithms-client-to-server)
	    (fill-slot 'server-enc client-kex server-kex
		       'encryption-algorithms-server-to-client)
	    (fill-slot 'client-mac client-kex server-kex
		       'mac-algorithms-client-to-server)
	    (fill-slot 'server-mac client-kex server-kex
		       'mac-algorithms-server-to-client)
	    (display socket) (newline)
	    ;; ignore compression and language those are not supporetd
	    (let ((kex (fill-slot #f client-kex server-kex 'kex-algorithms))
		  (key-algo (fill-slot #f client-kex server-kex 
				       'server-host-key-algorithms)))
	      ;; exchange key!
	      ;; pqg can be create by dsa parameter for my laziness
	      ;; TODO check the group 
	      (let* ((pqg (generate-dsa-parameter 1024))
		     (p   (~ pqg 'p))
		     (q   (~ pqg 'q))
		     (g   (~ pqg 'g))
		     (x   (bytevector->integer (read-random-bytes 
						(~ socket 'prng)
						(div (bitwise-length q) 8))))
		     (e   (mod-expt g x p))
		     (dh-init (make <ssh-msg-kexdh-init> :e e)))
		(write-packet in/out (ssh-message->bytevector dh-init) socket)
		(let* ((reply (read-packet in/out socket))
		       (dh-reply (read-message <ssh-msg-kexdh-reply>
					       (open-bytevector-input-port 
						(~ reply 'payload))))
		       (K-S (~ dh-reply 'K-S))
		       (H   (~ dh-reply 'H))
		       (K   (mod-expt (~ dh-reply 'f) x p)))
		  ;; verify signature
		  (verify-signature socket (ssh-message->bytevector client-kex)
				    (ssh-message->bytevector server-kex)
				    K-S e (~ dh-reply 'f) K H)
		  (set! (~ socket 'shared-key) K)
		  )
		)
	      )
	    socket)))))
)