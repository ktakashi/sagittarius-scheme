;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/websocket/engine - RFC 6455 Websocket connection engine
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

;; this is an interface library
;; the actual implementation must be under this library and
;; implementation libraries must export 'make-websocket-engine
;; procedure which accepts one argument, URI
;;
;; NB: the purpose of this library is handling multiple protocols
;;     other than HTTP1.1 (e.g. HTTP2)
(library (rfc websocket engine)
  (export websocket-engine
	  websocket-engine?
	  websocket-engine-socket
	  websocket-engine-handshake
	  websocket-engine-extensions ;; for validation?
	  websocket-engine-port

	  &websocket-engine-scheme
	  make-websocket-engine-scheme-error
	  websocket-engine-scheme-error
	  websocket-engine-scheme-error?
	  websocket-error-scheme

	  &websocket-engine-connection
	  make-websocket-engine-connection-error
	  websocket-engine-connection-error
	  websocket-engine-connection-error?
	  websocket-error-host
	  websocket-error-port
	  ;; internal for underlying engine implementation
	  websocket-engine-socket-set!
	  websocket-engine-port-set!
	  websocket-engine-extensions-set!)
  (import (rnrs)
	  (rfc websocket conditions))
  
(define-record-type websocket-engine
  (fields (mutable socket)
	  handshake
	  (mutable extensions)
	  ;; engine can set
	  (mutable port))
  (protocol 
   (lambda (p) (lambda (socket handshake) (p socket handshake #f #f)))))

(define-condition-type &websocket-engine-scheme &websocket-engine
  make-websocket-engine-scheme-error websocket-engine-scheme-error?
  (scheme websocket-error-scheme))
(define (websocket-engine-scheme-error who scheme uri)
  (raise (condition (make-websocket-engine-scheme-error scheme)
		    (make-who-condition who)
		    (make-message-condition "unknown URI scheme")
		    (make-irritants-condition uri))))

(define-condition-type &websocket-engine-connection &websocket-engine
  make-websocket-engine-connection-error websocket-engine-connection-error?
  (host websocket-error-host)
  (port websocket-error-port))
(define (websocket-engine-connection-error who host port)
  (raise (condition (make-websocket-engine-connection-error host port)
		    (make-who-condition who)
		    (make-message-condition "Failed to connect"))))
)
