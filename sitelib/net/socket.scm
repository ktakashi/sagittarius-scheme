;;; net/socket.scm -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/socket.scm - Modern socket library
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

;; Legacy one doesn't even have timeout control,
;; so try to make it a bit better connection library here
#!nounbound
(library (net socket)
    (export socket-options? make-socket-options
	    (rename (socket-options <socket-options>)
		    (socket-options-builder socket-options))
	    socket-options-non-blocking? socket-options-connection-timeout
	    socket-options-read-timeout socket-options-ai-family
	    socket-options-ai-socktype socket-options-ai-flags
	    socket-options-ai-protocol

	    make-client-socket
	    
	    ;; TLSs
	    tls-socket-options? make-tls-socket-options
	    (rename (tls-socket-options <tls-socket-options>)
		    (tls-socket-options-builder tls-socket-options))
	    tls-socket-options-handshake tls-socket-options-certificates
	    tls-socket-options-private-key tls-socket-options-hello-extensions
	    tls-socket-options-certificate-verifier
	    
	    make-client-tls-socket

	    ;; re-export from (sagittarius socket)
	    ;; make-server-socket ;; TODO how should we do this?
	    call-with-socket
	    shutdown-output-port
	    socket?
	    socket-closed?
	    make-socket
	    socket-port socket-input-port socket-output-port
	    shutdown-port shutdown-input-port shutdown-output-port
	    
	    socket-setsockopt!
	    socket-getsockopt
	    socket-connect!
	    socket-bind!
	    socket-listen!
	    socket-error-message
	    socket-last-error

	    socket-accept
	    socket-send socket-sendto
	    socket-recv socket-recv! socket-recvfrom
	    socket-shutdown
	    socket-close
	    socket-fd

	    ;; select
	    ;;socket-select ;; should we expose fd set here?
	    ;;socket-select!
	    socket-read-select
	    socket-write-select
	    socket-error-select
	    socket-nonblocking!
	    socket-blocking!
	    
	    AF_UNSPEC AF_INET AF_INET6

	    SOCK_STREAM SOCK_DGRAM SOCK_RAW SOCK_RDM SOCK_SEQPACKET

	    AI_PASSIVE AI_CANONNAME AI_NUMERICHOST
	    AI_V4MAPPED AI_ALL AI_ADDRCONFIG

	    IPPROTO_IP IPPROTO_TCP IPPROTO_UDP IPPROTO_RAW IPPROTO_IPV6
	    IPPROTO_ICMP IPPROTO_ICMPV6

	    SHUT_RD SHUT_WR SHUT_RDWR

	    MSG_OOB MSG_PEEK MSG_DONTROUTE MSG_CTRUNC
	    MSG_PROBE MSG_TRUNC MSG_DONTWAIT MSG_EOR
	    MSG_WAITALL MSG_FIN MSG_SYN MSG_CONFIRM
	    MSG_RST MSG_ERRQUEUE MSG_NOSIGNAL
	    MSG_MORE MSG_EOF

	    ;;  socket options
	    SOL_SOCKET
	    SOMAXCONN
	    SO_ACCEPTCONN SO_BINDTODEVICE SO_BROADCAST
	    SO_DEBUG SO_DONTROUTE SO_ERROR
	    SO_KEEPALIVE SO_LINGER SO_OOBINLINE
	    SO_PASSCRED SO_PEERCRED SO_PRIORITY
	    SO_RCVBUF SO_RCVLOWAT SO_RCVTIMEO
	    SO_REUSEADDR SO_REUSEPORT SO_SNDBUF
	    SO_SNDLOWAT SO_SNDTIMEO SO_TIMESTAMP
	    SO_TYPE

	    SOL_TCP
	    TCP_NODELAY TCP_MAXSEG TCP_CORK

	    SOL_IP
	    IP_OPTIONS IP_PKTINFO IP_RECVTOS
	    IP_RECVTTL IP_RECVOPTS IP_TOS
	    IP_TTL IP_HDRINCL IP_RECVERR
	    IP_MTU_DISCOVER IP_MTU IP_ROUTER_ALERT
	    IP_MULTICAST_TTL IP_MULTICAST_LOOP
	    IP_ADD_MEMBERSHIP IP_DROP_MEMBERSHIP
	    IP_MULTICAST_IF

	    ;; errno
	    EAGAIN EWOULDBLOCK EPIPE EINTR ETIMEDOUT EINPROGRESS
	    
	    &host-not-found host-not-found-error? make-host-not-found-error
	    host-not-found-error-node host-not-found-error-service
	    
	    &socket socket-error? make-socket-error
	    socket-error-socket
	    
	    &socket-connection socket-connection-error? 
	    make-socket-connection-error
	    
	    &socket-closed socket-closed-error? make-socket-closed-error
	    &socket-port socket-port-error? make-socket-port-error
	    socket-error-port

	    ;; from (rfc tls)
	    make-server-tls-socket

	    tls-socket?
	    tls-socket-send
	    tls-socket-recv
	    tls-socket-recv!
	    tls-socket-shutdown
	    tls-socket-close
	    tls-socket-closed?
	    tls-socket-accept
	    tls-socket-peer
	    tls-socket-name
	    tls-socket-info
	    tls-socket-info-values
	    call-with-tls-socket
	    tls-socket-peer-certificate
	    tls-socket-selected-alpn
	    ;; blocking
	    tls-socket-nonblocking!
	    tls-socket-blocking!

	    tls-socket-nonblocking!
	    tls-socket-blocking!

	    tls-socket-port
	    tls-socket-input-port
	    tls-socket-output-port

	    make-hello-extension
	    make-server-name-indication
	    make-protocol-name-list
	    )
    (import (rnrs)
	    (record builder)
	    (sagittarius time) ;; for time
	    (except (sagittarius socket) make-client-socket)
	    (except (rfc tls) make-client-tls-socket))

(define-record-type socket-options
  (fields non-blocking?
	  connection-timeout
	  read-timeout
	  ;; well we need them as well
	  ai-family
	  ai-socktype
	  ai-flags
	  ai-protocol))

(define-syntax socket-options-builder
  (make-record-builder socket-options
		       ((ai-family AF_INET)
			(ai-socktype SOCK_STREAM)
			(ai-flags (+ (or AI_V4MAPPED 0) (or AI_ADDRCONFIG 0)))
			(ai-protocol 0))))

(define (setup-socket socket options)
  (let ((read-timeout (socket-options-read-timeout options))
	(non-blocking? (socket-options-non-blocking? options)))
    (when non-blocking? (socket-nonblocking! socket))
    (when read-timeout
      (cond ((and (exact? read-timeout) (integer? read-timeout))
	     ;; in millis
	     (let ((time (make-time time-duration
				    (* (mod read-timeout 1000) 1000000)
				    (div read-timeout 1000))))
	       (socket-setsockopt! socket SOL_SOCKET SO_RCVTIMEO time)))
	    ((time? read-timeout)
	     (socket-setsockopt! socket SOL_SOCKET SO_RCVTIMEO read-timeout))))
    socket))

;; TODO make better name...
(define (make-client-socket node service
			    :optional (options (socket-options-builder)))
  (define ai-family (socket-options-ai-family options))
  (define ai-socktype (socket-options-ai-socktype options))
  (define ai-flags (socket-options-ai-flags options))
  (define ai-protocol (socket-options-ai-protocol options))
  (define timeout (socket-options-connection-timeout options))

  (define (setup socket)
    (when socket (setup-socket socket options))
    socket)
  
  (unless (zero? (bitwise-and ai-flags AI_PASSIVE))
    (assertion-violation 'make-client-socket
			 "client socket must not have AI_PASSIVE"))
  (let* ((hints (make-hint-addrinfo :family ai-family
				    :socktype ai-socktype
				    :flags ai-flags
				    :protocol ai-protocol))
	 (info (get-addrinfo node service hints)))
    (let loop ((socket (create-socket info)) (info info))
      (define (retry info)
	(let ((next (next-addrinfo info)))
	  (if next
	      (loop (create-socket next) next)
	      (raise (condition (make-host-not-found-error node service)
				(make-who-condition 'make-client-socket)
				(make-message-condition "no next addrinfo")
				(make-irritants-condition 
				 (list node service)))))))
      (or (and socket info (setup (socket-connect! socket info timeout)))
	  (and info (and socket (socket-close socket)) (retry info))
	  (raise (condition (make-socket-error socket)
			    (make-who-condition 'make-client-socket)
			    (make-message-condition
			     (if socket
				 (socket-error-message socket)
				 "creating a socket failed"))
			    (make-irritants-condition 
			       (list node service))))))))

(define-record-type tls-socket-options
  (parent socket-options)
  (fields handshake
	  certificates
	  private-key
	  hello-extensions
	  certificate-verifier))
(define-syntax tls-socket-options-builder
  (make-record-builder tls-socket-options
		       ((ai-family AF_INET)
			(ai-socktype SOCK_STREAM)
			(ai-flags (+ (or AI_V4MAPPED 0) (or AI_ADDRCONFIG 0)))
			(ai-protocol 0)
			(handshake #t)
			(certificates '())
			(hello-extensions '()))))

(define (make-client-tls-socket server service
			:optional (options (tls-socket-options-builder)))
  (let ((s (make-client-socket server service options)))
    (socket->tls-socket s
			:certificates (tls-socket-options-certificates options)
			:private-key (tls-socket-options-private-key options)
			:handshake (tls-socket-options-handshake options)
			:client-socket #t
			:hello-extensions
			  (tls-socket-options-hello-extensions options)
			:peer-certificate-required? #t
			:certificate-verifier
			  (tls-socket-options-certificate-verifier options))))

)
