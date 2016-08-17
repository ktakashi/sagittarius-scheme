;;; -*- Scheme -*-
;;;
;;; socket.scm - socket library
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

(library (sagittarius socket)
    (export make-client-socket
	    make-server-socket
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

	    socket-accept
	    socket-send socket-sendto
	    socket-recv socket-recv! socket-recvfrom
	    socket-shutdown
	    socket-close
	    socket-fd

	    ;; select
	    socket-select
	    socket-select!
	    socket-read-select
	    socket-write-select
	    socket-error-select
	    socket-nonblocking!
	    socket-blocking!
	    ;; addrinfo
	    make-addrinfo

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

	    ;; addrinfo
	    addrinfo? make-addrinfo make-hint-addrinfo get-addrinfo
	    next-addrinfo addrinfo-sockaddr
	    ;; sockaddr
	    sockaddr?
	    ;; socket-info
	    socket-info?
	    socket-peer
	    socket-name
	    socket-info
	    socket-info-values
	    ;; ip-address
	    ip-address?
	    ip-address->string
	    ip-address->bytevector
	    ;; fdset
	    make-fdset
	    fdset?
	    sockets->fdset
	    collect-sockets
	    fdset-set!
	    fdset-ref
	    
	    ;; clos
	    <socket>
	    <addrinfo>
	    <socket-info>
	    
	    ;; conditions
	    &host-not-found host-not-found-error? make-host-not-found-error
	    host-not-found-error-node host-not-found-error-service
	    &socket socket-error? make-socket-error
	    &socket-socket socket-error-socket
	    &socket-connection socket-connection-error? 
	    make-socket-connection-error
	    &socket-closed socket-closed-error? make-socket-closed-error
	    &socket-port socket-port-error? make-socket-port-error
	    &socket-port socket-error-port
	    )
    (import (core)
	    (core errors)
	    (core conditions)
	    (clos user)
	    (sagittarius)
	    (sagittarius dynamic-module)
	    )
  (load-dynamic-module "sagittarius--socket")

  (initialize-builtin-condition &host-not-found &i/o node service)
  (initialize-builtin-condition &socket &i/o socket)
  (initialize-builtin-condition &socket-connection &socket)
  (initialize-builtin-condition &socket-closed &socket)
  (initialize-builtin-condition &socket-port &socket port)

  (define-condition-accessor host-not-found-error-node &host-not-found 
    &host-not-found-error-node)
  (define-condition-accessor host-not-found-error-service &host-not-found 
    &host-not-found-error-service)

  (define-condition-accessor socket-error-socket &socket &socket-error-socket)
  (define-condition-accessor socket-error-port &socket-port &socket-error-port)

  (define (call-with-socket socket proc)
    (receive args (proc socket)
      (socket-close socket)
      (apply values args)))

  (define (make-hint-addrinfo :key family socktype flags protocol)
    (let ((info (make-addrinfo)))
      (unless (undefined? family) (slot-set! info 'family family))
      (unless (undefined? socktype) (slot-set! info 'socktype socktype))
      (unless (undefined? flags) (slot-set! info 'flags flags))
      (unless (undefined? protocol) (slot-set! info 'protocol protocol))
      info))

  (define (next-addrinfo info) (slot-ref info 'next))
  (define (addrinfo-sockaddr info) (slot-ref info 'addr))

  (define (create-socket info)
    (make-socket (slot-ref info 'family) (slot-ref info 'socktype)
		 (slot-ref info 'protocol)))

  (define (make-client-socket node service
			      :optional (ai-family AF_INET)
					(ai-socktype SOCK_STREAM)
					(ai-flags (+ (or AI_V4MAPPED 0)
						     (or AI_ADDRCONFIG 0)))
					(ai-protocol 0))
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
	  (let ((next (slot-ref info 'next)))
	    (if next
		(loop (create-socket next) next)
		(raise (condition (make-host-not-found-error node service)
				  (make-who-condition 'make-client-socket)
				  (make-message-condition "no next addrinfo")
				  (make-irritants-condition 
				   (list node service)))))))
	(or (and-let* (( socket )
		       ( info ))
	      (socket-connect! socket info))
	    (and info (and socket (socket-close socket)) (retry info))
	    (raise (condition (make-socket-error socket)
			      (make-who-condition 'make-client-socket)
			      (make-message-condition
			       (if socket
				   (socket-error-message socket)
				   "creating a socket failed"))
			      (make-irritants-condition 
			       (list node service))))))))

  (define (make-server-socket service 
			      :optional (ai-family AF_INET)
					(ai-socktype SOCK_STREAM)
					(ai-protocol 0))
    (let* ((hints (make-hint-addrinfo :family ai-family
				      :socktype ai-socktype
				      :flags AI_PASSIVE
				      :protocol ai-protocol))
	   (info (get-addrinfo #f service hints)))
      (let loop ((socket (create-socket info)) (info info))
	(define (retry info)
	  (let ((next (slot-ref info 'next)))
	    (if next
		(loop (create-socket next) next)
		(raise (condition (make-host-not-found-error node service)
				  (make-who-condition 'make-client-socket)
				  (make-message-condition "no next addrinfo")
				  (make-irritants-condition
				   (list node service)))))))

	(or (and-let* (( socket )
		       ( info )
		       ( (socket-setsockopt! socket SOL_SOCKET SO_REUSEADDR 1) )
		       ( (socket-bind! socket info) )
		       ( (if (= ai-socktype SOCK_STREAM)
			     (socket-listen! socket SOMAXCONN)
			     #t) ))
	      socket)
	    (and info (and socket (socket-close socket)) (retry info))
	    (raise (condition (make-socket-error socket)
			      (make-who-condition 'make-server-socket)
			      (make-message-condition 
			       (if socket
				   (socket-error-message socket)
				   "creating a socket failed"))
			      (make-irritants-condition service)))))))
  ;; for convenience
  (define (socket-read-select timeout . rest)
    (let ((rfds (sockets->fdset rest)))
      (receive (n r w e) (socket-select! rfds #f #f timeout) 
	(collect-sockets r))))

  (define (socket-write-select timeout . rest)
    (let ((wfds (sockets->fdset rest)))
      (receive (n r w e) (socket-select! #f wfds #f timeout) 
	(collect-sockets w))))

  (define (socket-error-select timeout . rest)
    (let ((efds (sockets->fdset rest)))
      (receive (n r w e) (socket-select! #f #f efds timeout) 
	(collect-sockets e))))

  ;; for backward compatibility
  (define (socket-info-values socket :key (type 'peer))
    (let ((peer (cond ((eq? type 'peer) (socket-peer socket))
		      ((eq? type 'info) (socket-info socket))
		      (else (error 'socket-info-values "unknown type" type)))))
      (if peer
	  (values (slot-ref peer 'hostname)
		  (slot-ref peer 'ip-address)
		  (slot-ref peer 'port))
	  (values #f #f #f))))
)
