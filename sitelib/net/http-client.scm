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
	    http:response-time
	    http:headers? http:make-headers
	    http:headers-names http:headers-ref* http:headers-ref
	    http:headers->alist
	    http:method
	    http-method-set

	    http:request-basic-auth
	    http:request-bearer-auth
	    
	    http:client? http:client-builder
	    (rename (http:client <http:client>))
	    *http-client:user-agent*
	    
	    http:version
	    http:redirect

	    http:make-default-cookie-handler

	    make-http-default-connection-manager
	    
	    make-http-ephemeral-connection-manager
	    http-ephemeral-connection-manager?

	    make-http-logging-connection-manager
	    http-logging-connection-manager?

	    http-connection-config?
	    http-connection-config-builder
	    
	    ;; delegate connection manager provider
	    default-delegate-connection-manager-provider
	    make-logging-delegate-connection-provider
	    
	    make-http-pooling-connection-manager
	    http-pooling-connection-manager?
	    http-pooling-connection-config?
	    http-pooling-connection-config-builder

	    ;; connection manager related conditions
	    dns-timeout-error? dns-timeout-node dns-timeout-service
	    connection-request-timeout-error?
	    
	    ;; executor parameter for DNS lookup timeout
	    *http-connection-manager:default-executor* 
	    
	    http-client-logger?
	    http-client-logger-builder
	    http-connection-logger?
	    http-connection-logger-builder
	    http-wire-logger?
	    http-wire-logger-builder
	    
	    list->key-manager make-key-manager key-manager
	    key-manager?

	    socket-parameter?
	    socket-parameter-socket-hostname
	    socket-parameter-socket-ip-address
	    socket-parameter-socket-node
	    socket-parameter-socket-service

	    key-provider? make-key-provider
	    <key-provider> key-provider-key-retrievers

	    make-keystore-key-provider keystore-key-provider?
	    keystore-key-provider-add-key-retriever!

	    ;; data handlers
	    http:bytevector-data-handler
	    
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
	    (net http-client logging)
	    (net socket)
	    (net uri)
	    (binary io)
	    (record builder)
	    (rfc cookie)
	    (rfc zlib)
	    (rfc gzip)
	    (rfc :5322)
	    (rfc uri)
	    (util concurrent)
	    (sagittarius)
	    (scheme lazy)
	    (srfi :19 time)
	    (srfi :39 parameters))

(define *http-client:default-executor*  
  (delay (make-fork-join-executor
	  (cpu-count)
	  (fork-join-pool-parameters-builder
	   (thread-name-prefix "default-http-client")))))

(define *http-client:user-agent*
  (make-parameter
   (string-append "sagittarius-" (sagittarius-version) "/http-client")))

(define-record-type http:client
  (fields follow-redirects
	  cookie-handler
	  connection-manager
	  version
	  executor
	  user-agent
	  ;; internal 
	  lease-option)
  (protocol (lambda (p)
	      (lambda (fr ch cm v e ua #:_)
		(p fr ch cm v e ua
		   (http-connection-lease-option-builder
		    (alpn (if (eq? v 'http/2) '("h2" "http/1.1") '()))
		    (executor
		     (force (*http-connection-manager:default-executor*)))))))))

(define-syntax http:client-builder
  (make-record-builder http:client
   (;; by default we don't follow
    (follow-redirects (http:redirect never))
    (connection-manager (make-http-default-connection-manager))
    (version (http:version http/2))
    (executor (force *http-client:default-executor*))
    (user-agent (*http-client:user-agent*)))))

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
   ((client) (http:client-shutdown! client #t))
   ((client shutdown-executor?)
    (http-connection-manager-shutdown! (http:client-connection-manager client))
    (when (and shutdown-executor? (not (default-executor? client)))
      (shutdown-executor! (http:client-executor client))))))

(define (http:client-send client request . opts)
  (future-get (apply http:client-send-async client request opts)))

(define (http:bytevector-data-handler) 
  (let-values (((out e) (open-bytevector-output-port)))
    (values out (lambda (#:_ #:_) (e)))))
(define (http:oport-data-handler sink flusher)
  (values sink (lambda (status hdrs) (flusher sink status hdrs))))

(define (http:client-send-async client request 
	 :key (data-handler http:bytevector-data-handler))
  (let-values (((f success failure) (make-piped-future)))
    (request/response client request data-handler success failure)
    f))

;;; helpers
(define (request/response client request data-handler success failure)
  (define ((release/fail conn) e)
    (release-http-connection client conn #f)
    (failure e))
  (define (submit-on-read conn handler fail)
    (http-connection-manager-register-on-readable
     (http:client-connection-manager client) conn
     handler fail
     (http:request-timeout request)))
  (lease-http-connection client request
   (lambda (conn)
     (executor-submit! (http:client-executor client)
      (lambda ()
	(let ((fail (release/fail conn)))
	  (guard (e (else (fail e) #f))
	    (let* ((resp-handler (send-request client conn request))
		   (handler (resp-handler client data-handler success fail)))
	      ;; if it's already ready, just start reading it
	      (if (http-connection-data-ready? conn)
		  (handler conn (lambda () (submit-on-read conn handler fail)))
		  (submit-on-read conn handler fail))))))))
   failure))

(define (default-executor? client)
  (eq? (http:client-executor client) (force *http-client:default-executor*)))

(define (handle-redirect client data-handler request response success failure)
  (define (get-location response)
    (cond ((http:headers-ref (http:response-headers response) "Location") =>
	   string->uri)
	  (else #f)))
  (define (check-scheme request response)
    (cond ((get-location response) =>
	   (lambda (uri)
	     (let ((request-scheme (uri-scheme (http:request-uri request))))
	       (or (not (uri-scheme uri))
		   (equal? (uri-scheme uri) request-scheme)
		   ;; http -> https: ok
		   ;; https -> http: not ok
		   (equal? "http" request-scheme)))))
	  ;; well, Location header doesn't exist
	  (else #f)))
  (define (do-redirect client request response)
    (define request-uri (http:request-uri request))
    (define (get-next-uri)
      (cond ((get-location response) =>
	     (lambda (uri)
	       (string->uri
		(uri-compose :scheme (or (uri-scheme uri)
					 (uri-scheme request-uri))
			     :authority (or (uri-authority uri)
					    (uri-authority request-uri))
			     :path (uri-path uri)
			     :query (uri-query uri)))))
	    (else #f)))
    (cond ((get-next-uri) =>
	   (lambda (next)
	     (let ((new-req (http:request-builder
			     (from request) (method 'GET) (uri next))))
	       (request/response client new-req data-handler success failure))))
	  (else (success response))))
  (case (http:client-follow-redirects client)
    ((never) (success response))
    ((always) (do-redirect client request response))
    ((normal) (or (and (check-scheme request response)
		       (do-redirect client request response))
		  (success response)))
    ;; well, just return...
    (else (success response))))

(define-record-type response-context
  (parent <http:response-context>)
  (fields (mutable status)
	  (mutable headers)
	  (mutable has-data?)
	  retriever
	  (mutable sink))
  (protocol (lambda (n)
	      (lambda (request header-handler data-handler)
		(let-values (((sink retriever) (data-handler)))
		  ((n request header-handler body-data-handler)
		   #f '() #f retriever sink))))))

(define (header-handler ctx status headers has-data?)
  (response-context-status-set! ctx status)
  (response-context-headers-set! ctx headers)
  (response-context-has-data?-set! ctx has-data?)

  (let ((sink (response-context-sink ctx))
	(encoding (rfc5322-header-ref headers "content-encoding" "none")))
    (response-context-sink-set! ctx
     (case (string->symbol encoding)
       ;; TODO implement below...
       ((gzip) (open-inflating-output-port sink :owner? #f :window-bits 31))
       ((defalte) (open-inflating-output-port sink :owner? #f))
       ((none) (open-forwarding-output-port sink))
       (else (error 'http-client "Unsupported Content-Encoding" encoding))))))

(define (body-data-handler ctx data end?)
  (define sink (response-context-sink ctx))
  (put-bytevector sink data))

(define (response-context->response ctx start)
  (define headers (http:make-headers))
  ;; stored headers are RFC 5322 alist, so convert it here
  (for-each (lambda (kv)
	      (for-each (lambda (v) (http:headers-add! headers (car kv) v))
			(cdr kv)))
	    (response-context-headers ctx))
  (let ((cookies (map parse-cookie-string
		      (http:headers-ref* headers "Set-Cookie")))
	(retriever (response-context-retriever ctx))
	(status (response-context-status ctx))
	(sink (response-context-sink ctx)))
    ;; To finish decompression
    (close-port sink)
    (http:response-builder (status status)
			   (headers headers)
			   (cookies cookies)
			   (body (retriever status headers))
			   (time (time-difference (current-time) start)))))

(define ((response-handler request start) client data-handler success failure)
  (define response-context
    (make-response-context request header-handler data-handler))
  (define executor (http:client-executor client))
  (define manager (http:client-connection-manager client))

  (define (receive-data conn response-context retry)
    (define (finish r)
      (let ((status (http:response-status r)))
	(if (and status (char=? #\3 (string-ref status 0)))
	    (handle-redirect client data-handler request r success failure)
	    (success r))))
    ;; TODO for SSE, we put separate event handling here
    (case (http-connection-receive-data! conn response-context)
      ((continue) (retry))
      (else =>
       (lambda (state)
	 (release-http-connection client conn (eq? state 'done))
	 (let ((response (response-context->response response-context start)))
	   (when (http:client-cookie-handler client)
	     (add-cookie! client (http:response-cookies response)))
	   (finish response))))))

  (define (receive-header conn retry)
    (define (delay-data-receive)
      (http-connection-manager-register-on-readable manager conn
       (lambda (conn retry) 
	 (executor-submit! executor
	  (lambda () (receive-data conn response-context retry))))
       failure (http:request-timeout request)))
    (guard (e (else (failure e)))
      (let loop ()
	(http-connection-receive-header! conn response-context)
	(let ((status (response-context-status response-context))
	      (has-data? (response-context-has-data? response-context)))
	  ;; TODO extra handler for 1xx status, esp 103?
	  (cond ((not has-data?) (response-context->response response-context))
		((eqv? (string-ref status 0) #\1) (loop))
		((http-connection-data-ready? conn)
		 (receive-data conn response-context delay-data-receive))
		(else (delay-data-receive)))))))

  (lambda (conn retry)
    (executor-submit! executor (lambda () (receive-header conn retry)))))

(define (send-request client conn request)
  (define now (current-time))
  (let ((req (adjust-request client request)))
    (http-connection-send-header! conn req)
    (http-connection-send-data! conn req)
    (response-handler req now)))

(define (adjust-request client request)
  (let* ((copy (http:request-builder (from request)))
	 (headers (http:request-headers copy)))
    (unless (http:headers-contains? headers "User-Agent")
      (http:headers-set! headers "User-Agent" (http:client-user-agent client)))
    (unless (http:headers-contains? headers "Accept")
      (http:headers-set! headers "Accept" "*/*"))
    (http:headers-set! headers "Accept-Encoding" "gzip, deflate")
    (cond ((http:request-auth request) =>
	   (lambda (provider)
	     (when (procedure? provider)
	       (http:headers-set! headers "Authorization" (provider))))))
    (let ((request-cookies (http:request-cookies request)))
      (unless (null? request-cookies)
	(http:headers-add! headers "Cookie" (cookies->string request-cookies))))
    (cond ((http:client-cookie-handler client) =>
	   (lambda (jar)
	     (define uri (http:request-uri request))
	     (define selector (cookie-jar-selector uri))
	     ;; okay add cookie here
	     (let ((cookies (cookie-jar->cookies jar selector)))
	       (unless (null? cookies)
		 (http:headers-add! headers "Cookie"
				    (cookies->string cookies)))))))
    copy))

(define (add-cookie! client cookies)
  (unless (null? cookies)
    (apply cookie-jar-add-cookie! (http:client-cookie-handler client) cookies)))

(define (lease-http-connection client request success failure)
  (define manager (http:client-connection-manager client))
  (http-connection-manager-lease-connection manager request
   (http:client-lease-option client) success failure))

(define (release-http-connection client connection reuse?)
  (http-connection-manager-release-connection
   (http:client-connection-manager client)
   connection reuse?))

;; no compression, but we should be able to close it without
;; underlying port to be closed
(define (open-forwarding-output-port sink)
  (define (write! bv start count)
    (put-bytevector sink bv start count)
    count)
  (define (close) #t) ;; do nothing
  (make-custom-binary-output-port "forwarding-port" write! #f #f close))

)
