;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/packet-handler.scm - SSH2 server service API
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (rfc ssh server service packet-handler)
    (export ssh-handle-service-packet
	    ssh-handle-service-request-packet)
    (import (rnrs)
	    (clos user)
	    (rfc ssh constants)
	    (rfc ssh server service api)
	    (rfc ssh server service auth)
	    (rfc ssh server service connection))
;; it's a bit too much to create a library
;;(rfc ssh server service request) or so, only for these 4 lines
(define (ssh-handle-service-request-packet transport packet)
  (let* ((len (bytevector-u32-ref packet 1 (endianness big)))
	 (name (utf8->string packet 5 (+ 5 len))))
    (ssh-handle-service-request name transport)))

(define (ssh-handle-service-packet transport packet)
  (define type (bytevector-u8-ref packet 0))
  (cond ((= type +ssh-msg-service-request+)
	 (ssh-handle-service-request-packet transport packet))
	((= type +ssh-msg-userauth-request+)
	 (ssh-handle-userauth-request-packet transport packet))
	((and (ssh-server-connection? transport)
	      (ssh-handle-connection-packet transport packet)))
	(else #f)))

)
