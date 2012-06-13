;;; -*- Scheme -*-
;;;
;;; socket.scm - socket library
;;;  
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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
	    make-socket
	    socket-port
	    socket-accept
	    socket-send
	    socket-recv
	    socket-shutdown
	    socket-close
	    socket-fd
	    AF_UNSPEC
	    AF_INET
	    AF_INET6
	    SOCK_STREAM
	    SOCK_DGRAM
	    SOCK_RAW
	    SOCK_RDM
	    SOCK_SEQPACKET
	    AI_PASSIVE
	    AI_CANONNAME
	    AI_NUMERICHOST
	    AI_V4MAPPED
	    AI_ALL
	    AI_ADDRCONFIG
	    SHUT_RD
	    SHUT_WR
	    SHUT_RDWR
	    MSG_OOB
	    MSG_PEEK
	    MSG_DONTROUTE
	    MSG_CTRUNC
	    MSG_PROBE
	    MSG_TRUNC
	    MSG_DONTWAIT
	    MSG_EOR
	    MSG_WAITALL
	    MSG_FIN
	    MSG_SYN
	    MSG_CONFIRM
	    MSG_RST
	    MSG_ERRQUEUE
	    MSG_NOSIGNAL
	    MSG_MORE
	    MSG_EOF
	    ;; clos
	    <socket>
	    )
    (import (core)
	    (sagittarius)
	    #;(sagittarius socket impl)
	    )
  (load-dynamic-library "sagittarius--socket")
  (define call-with-socket
    (lambda (socket proc)
      (receive args (proc socket)
	(socket-close socket)
	(apply values args))))
)
