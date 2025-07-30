;;; -*- Scheme -*-
;;;
;;; socket.scm - socket library
;;;  
;;;   Copyright (c) 2010-2021  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
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
	    
	    create-socket ;; for convenience
	    
	    socket-setsockopt!
	    socket-getsockopt
	    socket-connect!
	    socket-bind!
	    socket-listen!
	    socket-error-message
	    socket-last-error

	    socket-accept
	    socket-send socket-sendto
	    socket-send/range socket-sendto/range
	    socket-recv socket-recv! socket-recvfrom
	    socket-shutdown
	    socket-close
	    socket-fd
	    socket-node
	    socket-service

	    ;; select
	    socket-select
	    socket-select!
	    socket-read-select
	    socket-write-select
	    socket-error-select
	    socket-nonblocking!
	    socket-blocking!
	    socket-set-read-timeout!
	    socket-get-read-timeout
	    nonblocking-socket?
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

	    ;; errno
	    EAGAIN EWOULDBLOCK EPIPE EINTR ETIMEDOUT EINPROGRESS
	    ETIMEDOUT
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
	    socket-info-hostname
	    socket-info-ip-address
	    socket-info-port
	    socket-info-values
	    ;; ip-address
	    ip-address?
	    ip-address->string
	    ip-address->bytevector
	    ;; fdset
	    make-fdset fdset?
	    fdset-sockets
	    sockets->fdset
	    collect-sockets
	    fdset-set!
	    fdset-ref
	    fdset-clear!
	    
	    ;; clos
	    <socket>
	    <addrinfo>
	    <socket-info>
	    
	    ;; conditions
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

	    ;; socket selector
	    <socket-selector>
	    socket-selector?
	    make-socket-selector
	    close-socket-selector!
	    socket-selector-add!
	    socket-selector-clear!
	    socket-selector-wait!
	    socket-selector-waiting?
	    socket-selector-closed?
	    socket-selector-interrupt!
	    socket-selector-size
	    )
    (import (core)
	    (core errors)
	    (core conditions)
	    (clos user)
	    (sagittarius)
	    ;; damn
	    (only (sagittarius time)
		  time? make-time time-duration time-second time-nanosecond)
	    (sagittarius dynamic-module))
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

  (define-condition-type &socket-read-timeout &socket
    make-socket-read-timeout-error socket-read-timeout-error?
    (timeout socket-read-timeout-error-timeout))

  (define (socket-recv! sock bv start len :optional (flags 0))
    (let ((r (%socket-recv! sock bv start len flags)))
      (when (and (< r 0) (not (nonblocking-socket? sock)))
	(raise-read-timeout 'socket-recv sock))
      r))
  (define (socket-recv sock len :optional (flags 0))
    (let ((r (%socket-recv sock len flags)))
      (unless (or r (nonblocking-socket? sock))
	(raise-read-timeout 'socket-recv sock))
      r))

  (define (raise-read-timeout who sock)
    ;; convert it to ms if possible 
    (define (format-timeout t)
      (let* ((s (time-second t))
	     (ns (time-nanosecond t))
	     (ms (+ (* s 1000) (div ns 1000000))))
	(cond ((and (zero? ms) (zero? ns)) "") ;; no timeout
	      ((zero? ms) (format ", timeout: ~a" t)) ;; less then millis
	      (else (format ", timeout: ~ams" ms)))))
    (let ((to (socket-get-read-timeout sock)))
      (raise (condition (make-socket-read-timeout-error sock to)
			(make-who-condition who)
			(make-message-condition
			 (format "Read timeout! node: ~a, service: ~a~a"
				 (socket-node sock)
				 (socket-service sock)
				 (format-timeout to)))))))
  (define (socket-set-read-timeout! socket read-timeout)
    (cond ((and (integer? read-timeout) (exact? read-timeout))
	   ;; in milli second, for Windows...
	   (let ((time (make-time time-duration
				  (* (mod read-timeout 1000) 1000000)
				  (div read-timeout 1000))))
	     (socket-set-read-timeout! socket time)))
	  ((time? read-timeout)
	   (socket-setsockopt! socket SOL_SOCKET SO_RCVTIMEO read-timeout))
	  (else (assertion-violation 'socket-set-read-timeout!
		  "Timeout value must be an exact integer (milliseconds) or time"
		  read-timeout))))
  (define (socket-get-read-timeout socket)
    (socket-getsockopt socket SOL_SOCKET SO_RCVTIMEO -1))

  
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
	(define (retry info msg)
	  (let ((next (next-addrinfo info)))
	    (if next
		(loop (create-socket next) next)
		(raise (condition (make-host-not-found-error node service)
				  (make-who-condition 'make-client-socket)
				  (make-message-condition
				   (format "no next addrinfo: ~a" msg))
				  (make-irritants-condition 
				   (list node service)))))))
	(define (debug)
	  (let-values (((errno msg) (last-error-detail)))
	    (format "~a (~a)" msg errno)))
	(or (and-let* (( socket )
		       ( info ))
	      (socket-connect! socket info))
	    (and info 
		 (let ((msg (debug)))
		   (and socket (socket-close socket)) (retry info msg)))
	    (raise (condition (make-socket-error socket)
			      (make-who-condition 'make-client-socket)
			      (make-message-condition
			       (if socket
				   (socket-error-message socket)
				   (let-values (((errno msg) (last-error-detail)))
				     (format "creating a socket failed: ~a (~a)" msg errno))))
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
	  (let ((next (next-addrinfo info)))
	    (if next
		(loop (create-socket next) next)
		(raise (condition (make-host-not-found-error #f service)
				  (make-who-condition 'make-client-socket)
				  (make-message-condition "no next addrinfo")
				  (make-irritants-condition service))))))

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

  (define (socket-info-hostname si) (slot-ref si 'hostname))
  (define (socket-info-ip-address si) (slot-ref si 'ip-address))
  (define (socket-info-port si) (slot-ref si 'port))
)
