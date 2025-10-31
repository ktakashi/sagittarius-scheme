;;; -*- Scheme -*-
;;;
;;; tls-socket.scm - tls-socket library
;;;  
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius tls-socket)
    (export socket->tls-socket tls-socket?
	    tls-socket-connect! ;; client handshake
	    tls-socket-accept   ;; server handshake sort of
	    tls-socket-send tls-socket-send/range
	    tls-socket-recv tls-socket-recv!
	    tls-socket-pending?
	    tls-socket-shutdown tls-socket-close tls-socket-closed?
	    tls-server-socket-handshake
	    tls-socket-peer-certificate
	    tls-socket-peer-certificate-verifier
	    tls-socket-peer-certificate-verifier-set!
	    tls-socket-peer-certificate-required?
	    tls-socket-selected-alpn
	    tls-socket-authorities
	    tls-socket-authorities-set!
	    tls-socket-client-certificate-callback-set!
	    <tls-socket>
	    ;; don't use this cacually
	    tls-socket-raw-socket)
    (import (core)
	    (core errors)
	    (clos core)
	    (sagittarius)
	    (only (sagittarius socket)
		  nonblocking-socket? make-socket-read-timeout-error
		  socket-node socket-service
		  socket-get-read-timeout)
	    (sagittarius dynamic-module))
(load-dynamic-module "sagittarius--tls-socket")

(define (tls-socket-raw-socket sock) (slot-ref sock 'raw-socket))
  
(define (tls-socket-recv! sock bv start len :optional (flags 0))
  (let ((r (%tls-socket-recv! sock bv start len flags)))
    (when (and (< r 0) (not (nonblocking-socket? (slot-ref sock 'raw-socket))))
      (let* ((raw-sock (slot-ref sock 'raw-socket))
	     (to (socket-get-read-timeout raw-sock)))
	(raise (condition (make-socket-read-timeout-error sock to)
			  (make-who-condition 'tls-socket-recv!)
			  (make-message-condition
			   (format "Read timeout! node: ~a, service: ~a"
				   (socket-node raw-sock)
				   (socket-service raw-sock)))
			  (make-irritants-condition r)))))
    r))
(define (tls-socket-recv sock len :optional (flags 0))
  (let ((r (%tls-socket-recv sock len flags)))
    (unless (or r (nonblocking-socket? (slot-ref sock 'raw-socket)))
      (let* ((raw-sock (slot-ref sock 'raw-socket))
	     (to (socket-get-read-timeout raw-sock)))
	(raise (condition (make-socket-read-timeout-error sock to)
			  (make-who-condition 'tls-socket-recv)
			  (make-message-condition
			   (format "Read timeout! node: ~a, service: ~a"
				   (socket-node raw-sock)
				   (socket-service raw-sock)))
			  (make-irritants-condition r)))))
    r))
)
