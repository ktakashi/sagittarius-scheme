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

	  websocket-engine-error?
	  websocket-engine-not-found-error?
	  websocket-error-engine
	  websocket-error-reason
	  ;; re-export from (rfc websocket engine)
	  websocket-scheme-error
	  websocket-scheme-error?
	  websocket-error-scheme
	  
	  websocket-connection-error?
	  websocket-error-host
	  websocket-error-port
	  ;; low APIs
	  websocket-connection-protocol
	  websocket-connection-extensions
	  websocket-connection-pong-queue

	  websocket-validate-uri
	  
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
	  (srfi :2 and-let*)
	  (sagittarius io) ;; for buffered-port
	  (sagittarius socket)
	  ;; underlying socket might be a TLS socket
	  ;; so import this after (sagittarius socket)
	  (rfc tls)
	  (rfc uri)
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

;; conditions related to connection
(define-condition-type &websocket-scheme &websocket
  make-websocket-scheme-error websocket-scheme-error?
  (scheme websocket-error-scheme))

(define (websocket-scheme-error who scheme uri)
  (raise (condition (make-websocket-scheme-error scheme)
		    (make-who-condition who)
		    (make-message-condition "unknown URI scheme")
		    (make-irritants-condition uri))))

(define-condition-type &websocket-connection &websocket
  make-websocket-connection-error websocket-connection-error?
  (host websocket-error-host)
  (port websocket-error-port))
(define (websocket-connection-error who host port)
  (raise (condition (make-websocket-connection-error host port)
		    (make-who-condition who)
		    (make-message-condition "Failed to connect"))))

(define (websocket-validate-uri uri)
    ;; TODO maybe we should use regular expression instead of parsing
  (let-values (((scheme ui host port path query frag) (uri-parse uri)))
    (or (and scheme (or (string=? scheme "ws") (string=? scheme "wss")))
	(websocket-scheme-error 'make-websocket-connection scheme uri))))

(define (make-websocket-connection uri :optional (engine 'http))
  ;; inittial check
  (websocket-validate-uri uri)
  (guard (e ((websocket-engine-error? e) (raise e))
	    (else (websocket-engine-not-found-error engine e)))
    (let* ((env (environment `(rfc websocket engine ,engine)))
	   (make-engine (eval 'make-websocket-client-engine env)))
      (make-websocket-reconnectable-connection uri (make-engine)))))
  
;; if the socket has already handshaked, then we just need
;; to convertion. means, we also need to set port and so.
(define (server-socket->websocket-connection socket :optional (engine 'http))
  (define (set-socket conn)
    (websocket-connection-socket-set! conn socket)
    conn)
  (guard (e ((websocket-engine-error? e) (raise e))
	    (else (websocket-engine-not-found-error engine e)))
    (let* ((env (environment `(rfc websocket engine ,engine)))
	   (make-engine (eval 'make-websocket-server-engine env)))
      ;; it's not reconnectable
      (set-socket (make-websocket-base-connection (make-engine))))))

(define (do-handshake c opt close?)
  (define engine (websocket-connection-engine c))

  (define (make-socket scheme host port)
    (define (rec ai-family)
      (guard (e (else #f))
	(if (string=? scheme "wss")
	    (make-client-tls-socket host port)
	    (make-client-socket host port))))
    ;; default IPv6, IPv4 is fallback
    ;; NB: some platforms do fallback automatically and some are not
    ;;     so keep it like this.
    (or (rec AF_INET6)
	(rec AF_INET)
	(websocket-connection-error 'http-websocket-handshake host port)))

  (define (retrieve-socket c)
    (or (websocket-connection-socket c)
	(and-let* (( (websocket-reconnectable-connection? c) )
		   (uri (websocket-reconnectable-connection-uri c)))
	  (let-values (((scheme ui host port path query frag) (uri-parse uri)))
	    (let* ((default-port 
		     (or (and scheme (string=? scheme "ws") "80")
			 (and scheme (string=? scheme "wss") "443")
			 (websocket-scheme-error 'make-websocket-engine
						 scheme uri)))
		   (s (make-socket scheme host
				   (or (and port (number->string port))
				       default-port))))
	      (websocket-connection-socket-set! c s)
	      s)))))
  (define (retrieve-port c)
    (or (websocket-connection-port c)
	(and-let* ((s (retrieve-socket c))
		   (p (buffered-port (socket-port s #f) (buffer-mode block))))
	  (websocket-connection-port-set! c p)
	  p)))

  (guard (e (else
	     (when close? (websocket-connection-close! c))
	     (raise e)))
    (let ((in/out (retrieve-port c))
	  (uri (and (websocket-reconnectable-connection? c)
		    (websocket-reconnectable-connection-uri c))))
      (let-values (((protocol extensions raw-headers)
		    (apply (websocket-engine-handshake engine)
			   engine in/out uri opt)))
	(websocket-connection-protocol-set! c protocol)
	(websocket-connection-extensions-set! c extensions)
	(websocket-connection-raw-headers-set! c raw-headers)
	(websocket-connection-state-set! c 'open)
	c))))

;; TODO should we raise an error if it's not a reconnectable connection?
(define (websocket-connection-handshake! c . opt)
  ;; sends only connection is closed
  (if (websocket-connection-closed? c) 
      (do-handshake c opt #t)
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
  ;; to let server cleanup, we don't close connection when
  ;; handshake failed
  (do-handshake c opt #f))

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
