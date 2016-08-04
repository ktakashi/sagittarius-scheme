;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/websocket/connection.scm - RFC 6455 Websocket connection
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

;; client connection
(library (rfc websocket connection)
  (export make-websocket-connection
	  websocket-connection?
	  websocket-connection-handshake!
	  websocket-connection-close!
	  websocket-connection-closed?
	  websocket-connection-closing?

	  websocket-engine-not-found-error?
	  websocket-error-engine
	  websocket-error-reason
	  ;; re-export from (rfc websocket engine)
	  websocket-engine-scheme-error
	  websocket-engine-scheme-error?
	  websocket-error-scheme
	  websocket-engine-connection-error?
	  websocket-error-host
	  websocket-error-port
	  ;; internal
	  websocket-connection-port
	  websocket-connection-socket
	  websocket-connection-state ;; not really needed
	  websocket-connection-state-set!
	  )
  (import (rnrs)
	  (rnrs eval)
	  (rfc websocket engine)
	  (rfc websocket conditions)
	  (sagittarius io) ;; for buffered-port
	  (sagittarius socket)
	  ;; underlying socket might be a TLS socket
	  ;; so import this after (sagittarius socket)
	  (rfc tls))

(define-condition-type &websocket-engine-not-found &websocket-engine
  make-websocket-engine-not-found-error websocket-engine-not-found-error?
  (engine websocket-error-engine)
  (reason websocket-error-reason))
(define (websocket-engine-not-found-error engine e)
  (raise (condition (make-websocket-engine-not-found-error engine e)
		    (make-who-condition 'websocket-connection)
		    (make-message-condition "Handshake engine not found"))))

(define (websocket-connection-protocol p)
  (lambda (uri :optional (engine 'http))
    (guard (e ((websocket-engine-error? e) (raise e))
	      (else (websocket-engine-not-found-error engine e)))
      (let ((make-engine (eval 'make-websocket-engine
			       (environment `(rfc websocket engine ,engine)))))
	(p (make-engine uri) uri #f 'created)))))
  
(define-record-type websocket-connection
  (fields engine uri (mutable socket-port) (mutable state))
  (protocol websocket-connection-protocol))

(define (websocket-connection-handshake! c . opt)
  (define engine (websocket-connection-engine c))
  (apply (websocket-engine-handshake engine) engine opt)
  (websocket-connection-state-set! c 'open)
  c)

(define (websocket-connection-close! c)
  (define socket (websocket-engine-socket (websocket-connection-engine c)))
  (define (close-socket-port p)
    (close-port p)
    (websocket-connection-socket-port-set! c #f))
  ;; closing buffered port
  (cond ((websocket-connection-socket-port c) => close-socket-port))
  (socket-shutdown socket SHUT_RDWR)
  (socket-close socket)
  (websocket-connection-state-set! c 'closed))

(define (websocket-connection-closing? c)
  (eq? (websocket-connection-state c) 'closing))


(define (websocket-connection-closed? c)
  (memq (websocket-connection-state c) '(created closed)))

(define (websocket-connection-port c)
  (or (websocket-connection-socket-port c)
      (let ((p (buffered-port (socket-port (websocket-connection-socket c) #f)
			      (buffer-mode block))))
	(websocket-connection-socket-port-set! c p)
	p)))
(define (websocket-connection-socket c)
  (websocket-engine-socket (websocket-connection-engine c)))


  )
