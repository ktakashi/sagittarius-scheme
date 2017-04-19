;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/http-connections - HTTP1.1 and HTTP2 absorber
;;;  
;;;   Copyright (c) 2017  Takashi Kato  <ktakashi@ymail.com>
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

(library (rfc http-connections)
    (export http-connection? http-connection-secure?
	    http-connection-server http-connection-server&port
	    open-http-connection! close-http-connection!
	    http-request
	    
	    http1-connection? make-http1-connection
	    http2-connection? make-http2-connection

	    ;; utilities
	    http-null-receiver http-oport-receiver
	    http-blob-sender http-string-sender http-null-sender
	    )
    (import (rnrs)
	    (prefix (rfc http) rfc:)
	    (prefix (rfc http2 client) rfc:)
	    (srfi :13)
	    (clos user))
    
  (define-record-type http-connection
    (fields server secure?
	    open close
	    http-request))

  (define (open-http-connection! conn)
    ((http-connection-open conn) conn)
    conn)
  (define (close-http-connection! conn)
    ((http-connection-close conn) conn)
    conn)

  (define (http-request conn method path . opt)
    (apply (http-connection-http-request conn) conn method path opt))
  
  (define-record-type http1-connection
    (parent http-connection)
    (protocol (lambda (n)
		(lambda (server secure?)
		  ((n server secure?
		      values values
		      http1-request))))))
  (define (http:make-gzip-receiver)
    (rfc:http-gzip-receiver (rfc:http-binary-receiver)))
  (define (http1-request conn method path
			:key (sender #f) (receiver (http:make-gzip-receiver))
			:allow-other-keys headers)
    (rfc:http-request method (http-connection-server conn) path
		  :sender sender
		  :receiver receiver
		  :secure (http-connection-secure? conn)
		  :extra-headers (rfc:list->request-headers headers)))
  
  (define-record-type http2-connection
    (parent http-connection)
    (fields (mutable http2-connection))
    (protocol (lambda (n)
		(lambda (server secure?)
		  ((n server secure?
		      open-http2-connection!
		      close-http2-connection!
		      http2-request)
		   #f)))))

  (define (http-connection-server&port conn)
    (define (parse-port server)
      (cond ((string-index-right server #\:) =>
	     (lambda (p)
	       (values (string-copy server 0 p)
		       (string-copy server (+ p 1)))))
	    (else (values server #f))))
    (define secure? (http-connection-secure? conn))
    (define server (http-connection-server conn))
    
    (let-values (((server port) (parse-port server)))
      (values server (or port (if secure? "443" "80")))))
  
  (define (open-http2-connection! conn)
    (define secure? (http-connection-secure? conn))
    (let-values (((server port) (http-connection-server&port conn)))
      (let ((h2conn (rfc:make-http2-client-connection
		     server port :secure? secure?)))
	(http2-connection-http2-connection-set! conn h2conn)
	conn)))

  (define (close-http2-connection! conn)
    (rfc:close-http2-client-connection!
     (http2-connection-http2-connection conn))
    (http2-connection-http2-connection-set! conn #f)
    conn)

  (define (http2-response->http1-compatible h&b)
    (define header (car h&b))
    (define body (cadr h&b))
    (define (bv-header->string-header n&b)
      (list (utf8->string (car n&b)) (utf8->string (cadr n&b))))
    (let ((headers (map bv-header->string-header header)))
      (values (cond ((assoc ":status" headers) => cadr))
	      headers
	      body)))

  (define (http2-request conn method path
			 :key (sender #f) (receiver (rfc:make-gzip-receiver))
			 :allow-other-keys headers)
    (http2-response->http1-compatible
     (car (rfc:http2-request (http2-connection-http2-connection conn)
			     method path
			     (if sender
				 (rfc:http2-composite-sender
				  (apply rfc:http2-headers-sender headers)
				  sender)
				 (apply rfc:http2-headers-sender headers))
			     receiver))))

  ;; receivers
  (define-generic http-null-receiver)
  (define-method http-null-receiver ((conn http1-connection))
    (rfc:http-null-receiver))
  (define-method http-null-receiver ((conn http2-connection))
    (rfc:http2-null-receiver))
  (define-generic http-oport-receiver)
  (define-method http-oport-receiver ((conn http1-connection) sink flusher)
    (rfc:http-oport-receiver sink flusher))
  (define-method http-oport-receiver ((conn http2-connection) sink flusher)
    (rfc:http2-data-receiver sink flusher))
  
  ;; senders
  (define-generic http-blob-sender)
  (define-method http-blob-sender ((conn http1-connection) blob)
    (rfc:http-blob-sender blob))
  (define-method http-blob-sender ((conn http2-connection) blob)
    (rfc:http2-data-sender blob))

  (define-generic http-string-sender)
  (define-method http-string-sender ((conn http1-connection) string)
    (rfc:http-string-sender string))
  (define-method http-string-sender ((conn http2-connection) string)
    (rfc:http2-data-sender (string->utf8 string)))

  (define-generic http-null-sender)
  (define-method http-null-sender ((conn http1-connection))
    (rfc:http-null-sender))
  (define-method http-null-sender ((conn http2-connection)) #f)
)
