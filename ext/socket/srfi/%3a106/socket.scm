;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; srfi/%3a106/socket.scm - SRFI-106 basic socket
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

(library (srfi :106 socket)
    (export make-client-socket make-server-socket
            socket? 
	    socket-input-port socket-output-port
            call-with-socket
            (rename bitwise-ior socket-merge-flags)
	    (rename bitwise-xor socket-purge-flags)
            socket-accept socket-send socket-recv socket-shutdown socket-close
            *af-unspec* *af-inet* *af-inet6*
            *sock-stream* *sock-dgram*
            *ai-canonname* *ai-numerichost*
            *ai-v4mapped* *ai-all* *ai-addrconfig*
            *ipproto-ip* *ipproto-tcp* *ipproto-udp*
            *shut-rd* *shut-wr* *shut-rdwr*
            address-family socket-domain address-info
            ip-protocol message-type shutdown-method)
    (import (rnrs) 
	    (rename (sagittarius socket) 
		    (AF_UNSPEC *af-unspec*)
		    (AF_INET   *af-inet*)
		    (AF_INET6  *af-inet6*)
		    (SOCK_STREAM *sock-stream*)
		    (SOCK_DGRAM  *sock-dgram*)
		    (AI_CANONNAME   *ai-canonname*)
		    (AI_NUMERICHOST *ai-numerichost*)
		    (AI_V4MAPPED    *ai-v4mapped*)
		    (AI_ALL         *ai-all*)
		    (AI_ADDRCONFIG  *ai-addrconfig*)
		    (IPPROTO_IP  *ipproto-ip*)
		    (IPPROTO_TCP *ipproto-tcp*)
		    (IPPROTO_UDP *ipproto-udp*)
		    (MSG_PEEK     *msg-peek*)
		    (MSG_OOB      *msg-oob*)
		    (MSG_WAITALL  *msg-waitall*)
		    (SHUT_RD   *shut-rd*)
		    (SHUT_WR   *shut-wr*)
		    (SHUT_RDWR *shut-rdwr*)))
  
  (define-syntax define-one-flag-operation
    (syntax-rules ()
      ((_ ?name (?op ?flag) ...)
       (define-syntax ?name
	 (lambda (x)
	   (define (lookup name)
	     (case (syntax->datum name)
	       ((?op)   #'?flag)
	       ...
	       (else (syntax-violation '?name "unknown flag" name))))
	   (syntax-case x ()
	     ((_ flag)
	      (lookup #'flag))))))))

  (define-one-flag-operation address-family
    (inet   *af-inet*)
    (inet6  *af-inet6*)
    (unspec *af-unspec*))

  (define-one-flag-operation ip-protocol
    (ip  *ipproto-ip*)
    (tcp *ipproto-tcp*)
    (udp *ipproto-udp*))

  (define-one-flag-operation socket-domain 
    (stream   *sock-stream*)
    (datagram *sock-dgram*))

  (define-syntax define-flag-operation
    (syntax-rules ()
      ((_ ?name (?op ?flags) ...)
       (define-syntax ?name
	 (lambda (x)
	   (define (lookup names flags)
	     (syntax-case names (?op ...)
	       (() flags)
	       ((?op rest (... ...)) 
		(lookup #'(rest (... ...)) (bitwise-ior ?flags)))
	       ...
	       (_ (syntax-violation '?name "unknown flag" names))))
	   (syntax-case x ()
	     ((_ names (... ...))
	      (with-syntax ((flags (lookup #'(names (... ...)) 0)))
		#'flags))))))))

  (define-flag-operation address-info
    (canoname     *ai-canonname*)
    (numerichost  *ai-numerichost*)
    (v4mapped     *ai-v4mapped*)
    (all          *ai-all*)
    (addrconfig   *ai-addrconfig*))

  (define-flag-operation message-type
    (none 0)
    (peek *msg-peek*)
    (oob  *msg-oob*)
    (wait-all *msg-waitall*))

  (define-flag-operation shutdown-method
    (read  *shut-rd*)
    (write *shut-wr*))
)