;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client.scm - Modern HTTP client
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

;; (rfc http) or (rfc http2) are not good as a modern HTTP cliente
;; as they don't have any connection, session or other things
;; Or even using HTTP2 requires preliminary knowledge.
#!nounbound
(library (net http-client)
    (export http:request? http:request-builder <http:request>
	    http:response?
	    http:response-status http:response-headers
	    http:response-body
	    http:headers?
	    http:headers-names http:headers-ref* http:headers-ref
	    http:method
	    http-method-set
	    
	    http:client? http:client-builder
	    (rename (http:client <http:client>))
	    http:version
	    http:redirect
	    
	    http:client-send
	    http:client-send-async)
    (import (rnrs)
	    (net http-client request)
	    (net http-client connection)
	    (net http-client http1)
	    (net http-client http2)
	    (record builder)
	    (net socket)
	    (net uri)
	    (binary io)
	    (rfc zlib)
	    (rfc gzip)
	    (rfc :5322)
	    ;; for now
	    ;; TODO implement completable future...
	    (scheme lazy))

(define-record-type http:client
  (fields connection-timeout
	  read-timeout
	  follow-redirects
	  cookie-handler     ;; reserved...
	  connection-manager ;; reserved...
	  version	     ;;
	  executor	     ;; TBD
	  ))
(define-syntax http:client-builder
  (make-record-builder http:client
		       (;; by default we don't follow
			(follow-redirects (http:redirect never))
			(version (http:version http/2)))))

(define-enumeration http:version
  (http/1.1 http/2)
  http-version)
(define-enumeration http:redirect
  (never always normal)
  http-redirect)

(define (http:client-send client request)
  ;; TODO convert a future (or something similar) to a response
  (http:client-send-async client request))

(define (http:client-send-async client request)
  (define (adjust-request conn request)
    (let* ((copy (http:request-builder (from request)))
	   (headers (http:request-headers copy)))
      (unless (http:headers-contains? headers "User-Agent")
	(http:headers-set! headers "User-Agent"
			   (http-connection-user-agent conn)))
      (unless (http:headers-contains? headers "Accept")
	(http:headers-set! headers "Accept" "*/*"))
      (http:headers-set! headers "Accept-Encoding" "gzip, deflate")
      copy))
  
  (let ((conn (get-http-connection client request)))
    (let-values (((header-handler body-handler response-retriever)
		  (make-handlers)))
      ;; TODO make it async here
      (http-connection-send-request! conn (adjust-request conn request)
				     header-handler body-handler)
      (delay-force
       (begin (http-connection-receive-response! conn)
	      ;; TODO redirect
	      (response-retriever))))))

;;; helper
(define-record-type mutable-response
  (fields (mutable status)
	  (mutable headers)
	  (mutable body)))

(define (make-handlers)
  (define response (make-mutable-response #f #f #f))
  (define (decompress headers body)
    (define (get-input headers body)
      (case (string->symbol
	     (rfc5322-header-ref headers "content-encoding" "none"))
	((gzip) (open-gzip-input-port body :owner? #t))
	((deflate) (open-inflating-input-port body :owner? #t))
	((none) body)
	;; TODO proper error
	(else
	 => (lambda (v)
	      (error 'http-client
		     "Content-Encoding contains unsupported value" v)))))
    (call-with-port (get-input headers body)
      (lambda (in) (get-bytevector-all in))))
		    
  (let ((in/out (open-chunked-binary-input/output-port)))
    (define (header-handler status headers)
      (mutable-response-status-set! response status)
      (mutable-response-headers-set! response headers))
    (define (body-handler data end?)
      (put-bytevector in/out data)
      (when end?
	(set-port-position! in/out 0)
	(let* ((headers (mutable-response-headers response))
	       (body (decompress headers in/out)))
	  (mutable-response-body-set! response body))))
    (define (response-retriever)
      ;; TODO build cookies here
      (http:response-builder (status (mutable-response-status response))
			     (headers (mutable-response-headers response))
			     (body (mutable-response-body response))))
    (mutable-response-body-set! response in/out)
    (values header-handler body-handler response-retriever)))
      
(define (get-http-connection client request)
  (define uri (http:request-uri request))
  (let-values (((socket host service option) (uri->socket uri client)))
    (if (http2? socket)
	(socket->http2-connection socket option host service)
	(socket->http1-connection socket option host service))))

(define (http2? socket)
  (and (tls-socket? socket)
       (equal? (tls-socket-selected-alpn socket) "h2")))
  
(define (uri->socket uri client)
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))
  (define port (uri-port uri))
  (define (get-option scheme host client)
    (define connection-timeout (http:client-connection-timeout client))
    (define read-timeout (http:client-read-timeout client))
    (define alpn
      (if (eq? (http:client-version client) 'http/2)
	  '("h2")
	  '()))
    (if (string=? scheme "https")
	(tls-socket-options (connection-timeout connection-timeout)
			    (read-timeout read-timeout)
			    (sni* (list host))
			    (alpn* alpn))
	(socket-options (connection-timeout connection-timeout)
			(read-timeout read-timeout))))
  (let ((option (get-option scheme host client))
	(service (or port scheme)))
    (values (socket-options->client-socket option host service)
	    host service option)))

)
