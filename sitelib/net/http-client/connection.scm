;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/connection.scm - Base connection for HTTP client
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

#!nounbound
(library (net http-client connection)
    (export http-connection? (rename http-connection <http-connection>)
	    http-connection-node http-connection-service
	    http-connection-socket-options
	    http-connection-socket
	    http-connection-input http-connection-output
	    http-connection-user-agent

	    http-connection-open?
	    http-connection-open! http-connection-close!
	    http-connection-send-request! http-connection-receive-response!

	    *http-client-user-agent*
	    )
    (import (rnrs)
	    (net socket)
	    (sagittarius) ;; for sagittarius-version
	    (srfi :39 parameters))

(define *http-client-user-agent*
  (make-parameter
   (string-append "sagittarius-" (sagittarius-version) "/http-client")))

(define-record-type http-connection
  (fields node
	  service
	  socket-options
	  request-sender
	  response-receiver
	  (mutable socket)
	  (mutable input)
	  (mutable output)
	  user-agent)
  (protocol (lambda (p)
	      (lambda (node service option socket request response)
		(unless (or (not socket)
			    (or (socket? socket) (tls-socket? socket)))
		  (assertion-violation 'make-http-connection
				       "Socket or #f is required" socket))
		(unless (socket-options? option)
		  (assertion-violation 'make-http-connection
				       "<socket-options> is required" option))
		(p node service option request response socket
		   (and socket (socket-input-port socket))
		   (and socket (socket-output-port socket))
		   (*http-client-user-agent*))))))

(define (http-connection-open? conn)
  (and (http-connection-socket conn) #t))

;; Maybe for reconnect?
(define (http-connection-open! conn)
  (unless (http-connection-open? conn)
    (let ((socket (socket-options->client-socket
		   (http-connection-socket-options conn)
		   (http-connection-node conn)
		   (http-connection-service conn))))
      (http-connection-socket-set! conn socket)
      (http-connection-input-set! conn (socket-input-port socket))
      (http-connection-output-set! conn (socket-output-port socket))
      ))
  conn)

(define (http-connection-close! conn)
  (when (http-connection-open? conn)
    (let ((socket (http-connection-socket conn))
	  (input (http-connection-input conn))
	  (output (http-connection-output conn)))
      (close-port input)
      (close-port output)
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket)
      (http-connection-socket-set! conn #f)
      (http-connection-input-set! conn #f)
      (http-connection-output-set! conn #f)))
  conn)

(define (http-connection-send-request! conn request header-handler body-handler)
  ((http-connection-request-sender conn) conn request
   header-handler body-handler))

(define (http-connection-receive-response! conn)
  ((http-connection-response-receiver conn) conn))

)