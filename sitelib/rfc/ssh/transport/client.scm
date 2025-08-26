;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport/client.scm - SSH2 protocol client transport
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
(library (rfc ssh transport client)
    (export make-client-ssh-transport
	    socket->client-ssh-transport
	    open-client-ssh-transport!
	    close-client-ssh-transport!
	    ssh-client-transport?

	    *ssh-version-string*)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius regex)
	    (sagittarius socket)
	    (srfi :39 parameters)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport io)
	    (rfc ssh transport kex))

(define-class <ssh-client-transport> (<ssh-transport>)
  ((server-signature-algorithms :init-value #f)))

;; must do until handshake but for now
(define (make-client-ssh-transport server port)
  (make <ssh-client-transport> :server server :port port))
;; should work but not tested
(define (socket->client-ssh-transport socket)
  (make <ssh-client-transport> :socket socket))
(define (ssh-client-transport? transport)
  (is-a? <ssh-client-transport> transport))

(define (open-client-ssh-transport! transport)
  (unless (~ transport 'socket)
    (let ((server (~ transport 'server))
	  (port (~ transport 'port)))
      (set! (~ transport 'socket) (make-client-socket server port))))
  ;; didn't make performance better
  ;; (socket-setsockopt! socket IPPROTO_TCP TCP_NODELAY 1)
  ;; (socket-setsockopt! socket SOL_SOCKET SO_RCVBUF #x40000)
  (version-exchange transport)
  (ssh-client-key-exchange transport)
  transport)

(define-constant +default-version-string+ 
  (string-append "SSH-2.0-Sagittarius_" (sagittarius-version)))
(define *ssh-version-string* (make-parameter +default-version-string+))


;; utility
(define (read-ascii-line in)
  (define buf (make-bytevector 1))
  (let-values (((out e) (open-string-output-port)))
    (let loop ((r (socket-recv! in buf 0 1)))
      (unless (zero? r)
	(case (bytevector-u8-ref buf 0)
	  ;; version string must end CR LF however
	  ;; OpenSSH 4.3 returns only LF...
	  ((#x0D) (socket-recv! in buf 0 1))
	  ((#x0A))
	  (else => (lambda (u8)
		     (put-char out (integer->char u8))
		     (loop (socket-recv! in buf 0 1)))))))
    (e)))

;; write line with CR LF
(define (write-line out str)
  (socket-send out (string->utf8 str))
  (socket-send out #vu8(#x0D #x0A)))

;; RFC 4253 defines identification string like the followings;
;;  SSH-protoversion-softwareversion SP comments CR LF
;; Thus 'comments' must be a part of identification string
;; to be used as a part of MAC.
(define (version-exchange transport)
  (let ((in/out (~ transport 'socket)))
    (let loop ()
      (let ((vs (read-ascii-line in/out)))
	(cond ((zero? (string-length vs))
	       (socket-shutdown in/out SHUT_RDWR)
	       (socket-close in/out)
	       (error 'version-exchange "no version string"))
	      ((#/(SSH-2.0-[\w.-]+)\s*/ vs) => 
	       (lambda (m)
		 (set! (~ transport 'server-version) vs)
		 (let ((version (*ssh-version-string*)))
		   (set! (~ transport 'client-version) version)
		   ;; send
		   (write-line in/out version))))
	      (else (loop)))))))

(define (close-client-ssh-transport! transport
				     :key (code +ssh-disconnect-by-application+)
				     (description "finish"))
  (let-values (((in size) (ssh-message->binary-port
			   (make <ssh-msg-disconnect>
			     :code code
			     :description description))))
    (ssh-write-packet-port transport in size)
    (socket-shutdown (~ transport 'socket) SHUT_RDWR)
    (socket-close (~ transport 'socket))))
)
	    
