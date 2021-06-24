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
	    http:response-cookies http:response-body
	    http:headers?
	    http:headers-names http:headers-ref* http:headers-ref
	    http:method
	    http-method-set
	    
	    http:client? http:client-builder
	    (rename (http:client <http:client>))
	    http:version
	    http:redirect

	    http:make-default-cookie-handler

	    make-http-default-connection-manager
	    
	    make-http-ephemeral-connection-manager
	    http-ephemeral-connection-manager?
	    
	    make-http-pooling-connection-manager
	    http-pooling-connection-manager?
	    http-connection-pooling-config?
	    http-connection-pooling-config-builder
	    build-http-pooling-connection-manager

	    list->key-manager make-key-manager key-manager
	    key-manager?
	    socket-parameter-socket-hostname
	    socket-parameter-socket-ip-address
	    socket-parameter-socket-node
	    socket-parameter-socket-service

	    key-provider? <key-provider>
	    make-keystore-key-provider keystore-key-provider?
	    keystore-key-proider-add-key-retriever!
	    
	    http:client-shutdown!
	    http:client-send
	    http:client-send-async)
    (import (rnrs)
	    (net http-client request)
	    (net http-client connection)
	    (net http-client connection-manager)
	    (net http-client key-manager)
	    (net http-client http1)
	    (net http-client http2)
	    (record builder)
	    (net socket)
	    (net uri)
	    (binary io)
	    (rfc cookie)
	    (rfc zlib)
	    (rfc gzip)
	    (rfc :5322)
	    (rfc uri)
	    (util concurrent)
	    (scheme lazy))

(define *http-client:default-executor*
  ;; Don't create during the library loading
  (delay-force
   (make-thread-pool-executor 5 push-future-handler)))

(define-record-type http:client
  (fields connection-timeout
	  read-timeout
	  follow-redirects
	  cookie-handler
	  connection-manager
	  key-manager
	  version
	  executor
	  ))
(define-syntax http:client-builder
  (make-record-builder http:client
		       (;; by default we don't follow
			(follow-redirects (http:redirect never))
			(connection-manager
			 (make-http-default-connection-manager))
			(version (http:version http/2))
			(executor (force *http-client:default-executor*)))))

;; for now
(define (http:make-default-cookie-handler) (make-cookie-jar))

(define-enumeration http:version
  (http/1.1 http/2)
  http-version)
(define-enumeration http:redirect
  (never always normal)
  http-redirect)

(define http:client-shutdown!
  (case-lambda
   ((client)
    (http:client-shutdown! client #t))
   ((client shutdown-executor?)
    (http-connection-manager-shutdown! (http:client-connection-manager client))
    (when (and shutdown-executor? (not (default-executor? client)))
      (shutdown-executor! (http:client-executor client))))))

(define (http:client-send client request)
  (future-get (http:client-send-async client request)))

(define (http:client-send-async client request)
  (thunk->future
   (lambda () (send-request/response client request))
   (http:client-executor client)))

;;; helper
(define (default-executor? client)
  (eq? (http:client-executor client) (force *http-client:default-executor*)))
(define (send-request/response client request)
  (define (handle-redirect client request response)
    (define (get-location response)
      (http:headers-ref (http:response-headers response) "Location"))
    (define (check-scheme request response)
      (let ((uri (string->uri (get-location response)))
	    (request-scheme (uri-scheme (http:request-uri request))))
	(or (not (uri-scheme uri))
	    (equal? (uri-scheme uri) request-scheme)
	    ;; http -> https: ok
	    ;; https -> http: not ok
	    (equal? "http" request-scheme))))
    (define (do-redirect client request response)
      (define request-uri (http:request-uri request))
      (define (get-next-uri)
	(let ((uri (string->uri (get-location response))))
	  (string->uri
	   (uri-compose :scheme (or (uri-scheme uri) (uri-scheme request-uri))
			:authority (or (uri-authority uri)
				       (uri-authority request-uri))
			:path (uri-path uri)
			:query (uri-query uri)))))
      (let ((next (get-next-uri)))
	(send-request/response client (http:request-builder
				       (from request)
				       (uri next)))))
    (case (http:client-follow-redirects client)
      ((never) response)
      ((always) (do-redirect client request response))
      ((normal) (and (check-scheme request response)
		     (do-redirect client request response)))
      ;; well, just return...
      (else response)))
       
  (let ((conn (lease-http-connection client request)))
    (let-values (((header-handler body-handler response-retriever)
		  (make-handlers)))
      (http-connection-send-request! conn (adjust-request client conn request)
				     header-handler body-handler)
      (let ((reuse? (http-connection-receive-response! conn)))
	(release-http-connection client conn reuse?))
      (let* ((response (response-retriever))
	     (status (http:response-status response)))
	(when (http:client-cookie-handler client)
	  (add-cookie! client (http:response-cookies response)))
	(if (and status (char=? #\3 (string-ref status 0)))
	    (handle-redirect client request response)
	    response)))))
  
(define (adjust-request client conn request)
  (let* ((copy (http:request-builder (from request)))
	 (headers (http:request-headers copy)))
    (unless (http:headers-contains? headers "User-Agent")
      (http:headers-set! headers "User-Agent"
			 (http-connection-user-agent conn)))
    (unless (http:headers-contains? headers "Accept")
      (http:headers-set! headers "Accept" "*/*"))
    (http:headers-set! headers "Accept-Encoding" "gzip, deflate")
    (cond ((http:client-cookie-handler client) =>
	   (lambda (jar)
	     (define uri (http:request-uri request))
	     ;; okay add cookie here
	     (let ((cookies (cookie-jar->cookies jar (cookie-jar-selector uri))))
	       (unless (null? cookies)
		 (http:headers-add! headers "Cookie"
				    (cookies->string cookies)))))))
    copy))

(define (add-cookie! client cookies)
  (unless (null? cookies)
    (apply cookie-jar-add-cookie! (http:client-cookie-handler client) cookies)))

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
      (define headers (http:make-headers))
      ;; stored headers are RFC 5322 alist, so conver it here
      (for-each (lambda (kv)
		  (for-each (lambda (v)
			      (http:headers-add! headers (car kv) v))
			    (cdr kv)))
		(mutable-response-headers response))
      (let ((cookies (map parse-cookie-string
			  (http:headers-ref* headers "Set-Cookie"))))
	(http:response-builder (status (mutable-response-status response))
			       (headers headers)
			       (cookies cookies)
			       (body (mutable-response-body response)))))
    (mutable-response-body-set! response in/out)
    (values header-handler body-handler response-retriever)))
      
(define (lease-http-connection client request)
  (define uri (http:request-uri request))
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))
  (define (get-option scheme host client)
    (define (milli->micro v) (* v 1000))
    (define connection-timeout
      (cond ((http:client-connection-timeout client) => milli->micro)
	    (else #f)))
    (define read-timeout
      (cond ((http:client-read-timeout client)=> milli->micro)
	    (else #f)))
    (define alpn
      (if (eq? (http:client-version client) 'http/2)
	  '("h2")
	  '()))
    (define km (http:client-key-manager client))
    (if (string=? scheme "https")
	(tls-socket-options (connection-timeout connection-timeout)
			    (read-timeout read-timeout)
			    (client-certificate-provider
			     (and km (key-manager->certificate-callback km)))
			    (sni* (list host))
			    (alpn* alpn))
	(socket-options (connection-timeout connection-timeout)
			(read-timeout read-timeout))))
  (http-connection-manager-lease-connection
   (http:client-connection-manager client)
   request (get-option scheme host client)))
(define (release-http-connection client connection reuse?)
  (http-connection-manager-release-connection
   (http:client-connection-manager client)
   connection reuse?))
)
