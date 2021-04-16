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

	    make-client-socket make-server-socket
	    
	    ;; TLSs
	    tls-socket-options? make-tls-socket-options
	    (rename (tls-socket-options <tls-socket-options>)
		    (tls-socket-options-builder tls-socket-options))
	    
	    tls-socket-options-handshake tls-socket-options-certificates
	    tls-socket-options-private-key tls-socket-options-hello-extensions
	    tls-socket-options-certificate-verifier

	    server-tls-socket-options? make-server-tls-socket-options
	    (rename (server-tls-socket-options <server-tls-socket-options>)
		    (server-tls-options-builder server-tls-socket-options))
	    server-tls-socket-options-trusted-certificates
	    server-tls-socket-options-client-certificate-required?
	    
	    make-client-tls-socket make-server-tls-socket

	    socket-options->client-socket

	    ;; re-export from (sagittarius socket)
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
	    socket-set-read-timeout!
	    nonblocking-socket?

	    socket-info?
	    socket-peer
	    socket-name
	    socket-info
	    socket-info-hostname
	    socket-info-ip-address
	    socket-info-port
	    
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
	    ETIMEDOUT
	    
	    &host-not-found host-not-found-error? make-host-not-found-error
	    host-not-found-error-node host-not-found-error-service
	    
	    &socket socket-error? make-socket-error
	    socket-error-socket
	    
	    &socket-connection socket-connection-error? 
	    make-socket-connection-error

	    &socket-read-timeout socket-read-timeout-error?
	    make-socket-read-timeout-error
	    
	    &socket-closed socket-closed-error? make-socket-closed-error
	    &socket-port socket-port-error? make-socket-port-error
	    socket-error-port

	    ;; from (rfc tls)
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
	    tls-socket-set-read-timeout!
	    
	    tls-socket-port
	    tls-socket-input-port
	    tls-socket-output-port

	    make-hello-extension
	    make-server-name-indication
	    make-protocol-name-list
	    )
    (import (rnrs)
	    (record builder)
	    (clos core)
	    (sagittarius time) ;; for time
	    (rename (except (sagittarius socket) make-client-socket)
		    (make-server-socket socket:make-server-socket))
	    (except (rfc tls) make-client-tls-socket
		    make-server-tls-socket)
	    (rfc x.509)
	    (crypto))

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
    (when read-timeout (socket-set-read-timeout! socket read-timeout))
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

(define (make-server-socket service :optional (options (socket-options-builder)))
  (define ai-family (socket-options-ai-family options))
  (define ai-socktype (socket-options-ai-socktype options))
  (define ai-protocol (socket-options-ai-protocol options))
  (setup-socket
   (socket:make-server-socket service ai-family ai-socktype ai-protocol)
   options))

(define-record-type tls-socket-options
  (parent socket-options)
  (fields handshake
	  certificates
	  private-key
	  certificate-verifier
	  sni*				; list of sni
	  alpn*				; list of alpn
	  hello-extensions))
(define-syntax tls-socket-options-builder
  (make-record-builder tls-socket-options
		       ((ai-family AF_INET)
			(ai-socktype SOCK_STREAM)
			(ai-flags (+ (or AI_V4MAPPED 0) (or AI_ADDRCONFIG 0)))
			(ai-protocol 0)
			(handshake #t)
			(certificates '())
			(sni* '())
			(alpn* '())
			(hello-extensions '()))))

(define-record-type server-tls-socket-options
  (parent tls-socket-options)
  (fields client-certificate-required?
	  trusted-certificates))
(define-syntax server-tls-options-builder
  (make-record-builder server-tls-socket-options
		       ((ai-family AF_INET)
			(ai-socktype SOCK_STREAM)
			(ai-protocol 0)
			(certificate-verifier #t)
			(certificates '()) ;; in case...
			(trusted-certificates '()))))

(define (make-client-tls-socket server service
				:optional (options (tls-socket-options-builder)))
  (define (get-option getter default)
    (if (tls-socket-options? options)
	(getter options)
	default))
  (define sni* (get-option tls-socket-options-sni* '()))
  (define alpn* (get-option tls-socket-options-alpn* '()))
  (define extensions (get-option tls-socket-options-hello-extensions '()))
  (define (make-extension ctr v*)
    (if (null? v*)
	v*
	(list (ctr v*))))
  (let ((s (make-client-socket server service
			       (socket-options-builder
				(from options)
				(read-timeout #f)
				(non-blocking? #f)))))
    (setup-socket
     (socket->tls-socket
      s
      :certificates (get-option tls-socket-options-certificates '())
      :private-key (get-option tls-socket-options-private-key #f)
      :handshake (get-option tls-socket-options-handshake #t)
      :client-socket #t
      :hello-extensions `(,@extensions
			  ,@(make-extension
			     make-server-name-indication sni*)
			  ,@(make-extension
			     make-protocol-name-list alpn*))
      :peer-certificate-required? #t
      :certificate-verifier
        ;; default #f to allow self signed certificate
        (get-option tls-socket-options-certificate-verifier #f))
     options)))

(define (make-server-tls-socket port options)
  (define certificates (tls-socket-options-certificates options))
  (define private-key (tls-socket-options-private-key options))
  (define client-cert-needed?
    (and (server-tls-socket-options? options)
	 (server-tls-socket-options-client-certificate-required? options)))
  (define trusted-certificates
    (or (and (server-tls-socket-options? options)
	     (server-tls-socket-options-trusted-certificates options))
	'()))
  (when (or (null? certificates) (not (for-all x509-certificate? certificates)))
    (assertion-violation 'make-server-tls-socket
			 "Certificates are empty or non X509 certificates"
			 certificates))
  (unless (private-key? private-key)
    (assertion-violation 'make-server-tls-socket
			 "Private key is missing" private-key))
  (let ((s (make-server-socket port (socket-options-builder
				     (from options)
				     (read-timeout #f)
				     (non-blocking? #f)))))
    (setup-socket
     (socket->tls-socket
      s
      :certificates certificates
      :private-key private-key
      :client-socket #f
      :peer-certificate-required? client-cert-needed?
      :authorities '()
      :certificate-verifier (tls-socket-options-certificate-verifier options))
     options)))

;; For convenience
(define (socket-options->client-socket option node service)
  (cond ((tls-socket-options? option)
	 (make-client-tls-socket node service option))
	((socket-options? option)
	 (make-client-socket node service option))
	(else (assertion-violation 'socket-options->client-socket
				   "Not a socket-options" option))))

)
