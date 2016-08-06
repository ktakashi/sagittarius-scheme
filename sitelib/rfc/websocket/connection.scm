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
	  websocket-reconnectable-connection?
	  server-socket->websocket-connection
	  websocket-connection-handshake!
	  websocket-connection-accept-handshake!
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
	  ;; low APIs
	  websocket-connection-protocol
	  websocket-connection-extensions
	  websocket-connection-pong-queue
	  ;; should be internal
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
	  (rfc tls)
	  (util concurrent shared-queue))

(define-condition-type &websocket-engine-not-found &websocket-engine
  make-websocket-engine-not-found-error websocket-engine-not-found-error?
  (engine websocket-error-engine)
  (reason websocket-error-reason))
(define (websocket-engine-not-found-error engine e)
  (raise (condition (make-websocket-engine-not-found-error engine e)
		    (make-who-condition 'websocket-connection)
		    (make-message-condition "Handshake engine not found"))))

(define-record-type (websocket-connection make-websocket-base-connection
					  websocket-connection?)
  (fields engine
	  (mutable socket)
	  (mutable port)
	  (mutable protocol) ;; subprotocol but we know this is websocket
	  (mutable extensions)
	  (mutable raw-headers)
	  (mutable state)
	  pong-queue)
  (protocol (lambda (p)
	      (lambda (engine)
		(p engine #f #f #f #f '() 'created (make-shared-queue))))))

(define-record-type websocket-reconnectable-connection
  (parent websocket-connection)
  (fields uri)
  (protocol (lambda (n) (lambda (uri engine) ((n engine) uri)))))

(define (make-websocket-connection uri :optional (engine 'http))
  (guard (e ((websocket-engine-error? e) (raise e))
	    (else (websocket-engine-not-found-error engine e)))
    (let* ((env (environment `(rfc websocket engine ,engine)))
	   (make-engine (eval 'make-websocket-engine env)))
      (make-websocket-reconnectable-connection uri (make-engine uri)))))

;; if the socket has already handshaked, then we just need
;; to convertion. means, we also need to set port and so.
(define (server-socket->websocket-connection socket :optional (engine 'http))
  (define (set-socket&port conn)
    (websocket-connection-socket-set! conn socket)
    (websocket-connection-port-set! conn
     (buffered-port (socket-port socket #f) (buffer-mode block)))
    conn)
  (guard (e ((websocket-engine-error? e) (raise e))
	    (else (websocket-engine-not-found-error engine e)))
    (let* ((env (environment `(rfc websocket engine ,engine)))
	   (make-engine (eval 'make-websocket-server-engine env)))
      ;; it's not reconnectable
      (set-socket&port (make-websocket-base-connection (make-engine socket))))))

(define (do-handshake c opt)
  (define engine (websocket-connection-engine c))
  (let-values (((socket port protocol extensions raw-headers)
		(apply (websocket-engine-handshake engine) engine opt)))
    (websocket-connection-socket-set! c socket)
    ;; if the port is already set and engine returned a port
    ;; then we need to close the previous port.
    (cond ((and port (websocket-connection-port c)) => close-port))
    (websocket-connection-port-set! c
     (buffered-port (or port (socket-port socket #f)) (buffer-mode block)))
    (websocket-connection-protocol-set! c protocol)
    (websocket-connection-extensions-set! c extensions)
    (websocket-connection-raw-headers-set! c raw-headers)
    (websocket-connection-state-set! c 'open)
    c))

;; TODO should we raise an error if it's not a reconnectable connection?
(define (websocket-connection-handshake! c . opt)
  ;; sends only connection is closed
  (if (websocket-connection-closed? c) 
      (do-handshake c opt)
      c))

(define (websocket-connection-accept-handshake! c . opt)
  (define engine (websocket-connection-engine c))
  (when (websocket-reconnectable-connection? c)
    (assertion-violation 'websocket-connection-accept-handshake!
			 "reconnectable connection can't wait handshake" c))
  (unless (eq? (websocket-connection-state c) 'created)
    (assertion-violation 'websocket-connection-accept-handshake!
			 "invalid state of connection" 
			 (websocket-connection-state c) c))
  (do-handshake c opt))

(define (websocket-connection-close! c)
  (define socket (websocket-connection-socket c))
  (define port   (websocket-connection-port c))
  ;; closing buffered port
  (when port
    (close-port port)
    (websocket-connection-port-set! c #f))
  ;; closing socket
  (when socket
    (socket-shutdown socket SHUT_RDWR)
    (socket-close socket)
    (websocket-connection-socket-set! c #f))
  (websocket-connection-state-set! c 'closed))

(define (websocket-connection-closing? c)
  (eq? (websocket-connection-state c) 'closing))


(define (websocket-connection-closed? c)
  (cond ((memq (websocket-connection-state c) '(created closed)) #t)
	(else #f)))

  )
