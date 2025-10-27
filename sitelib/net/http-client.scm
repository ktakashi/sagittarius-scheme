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
	    (sagittarius)
	    (scheme lazy)
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
	  (mutable lease-option)
	  )
  (protocol (lambda (p)
	      (lambda args
		(let ((hc (apply p args)))
		  ;; set lease option here
		  (http:client-lease-option-set! hc
		   (http-connection-lease-option-builder
		    (alpn (if (eq? (http:client-version hc) 'http/2)
			      '("h2" "http/1.1") '()))
		    (executor
		     (force (*http-connection-manager:default-executor*)))))
		  hc)))))
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
   ((client)
    (http:client-shutdown! client #t))
   ((client shutdown-executor?)
    (http-connection-manager-shutdown! (http:client-connection-manager client))
    (when (and shutdown-executor? (not (default-executor? client)))
      (shutdown-executor! (http:client-executor client))))))

(define (http:client-send client request)
  (future-get (http:client-send-async client request)))

(define (http:client-send-async client request)
  (let-values (((f success failure) (make-piped-future)))
    (request/response client request success failure)
    f))

;;; helpers
(define (request/response client request success failure)
  (define ((release/fail conn) e)
    (release-http-connection client conn #f)
    (failure e))
  (lease-http-connection client request
   (lambda (conn)
     (executor-submit! (http:client-executor client)
      (lambda ()
	(let ((fail (release/fail conn)))
	  (guard (e (else (fail e)))
	    (let ((response-handler (send-request client conn request)))
	      (http-connection-manager-register-on-readable
	       (http:client-connection-manager client) conn
	       (response-handler client success fail)
	       fail
	       (http:request-timeout request))))))))
   failure))

(define (default-executor? client)
  (eq? (http:client-executor client) (force *http-client:default-executor*)))

(define (handle-redirect client request response success failure)
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
	       (request/response client new-req success failure))))
	  (else (success response))))
  (case (http:client-follow-redirects client)
    ((never) (success response))
    ((always) (do-redirect client request response))
    ((normal) (or (and (check-scheme request response)
		       (do-redirect client request response))
		  (success response)))
    ;; well, just return...
    (else (success response))))

(define ((response-handler header-state retriever request)
	 client success failure)
  (lambda (conn retry)
    (define executor (http:client-executor client))
    (define (receive-data conn)
      (let ((reuse? (http-connection-receive-data! conn request)))
	(release-http-connection client conn reuse?)
	(let ((response (retriever)))
	  (when (http:client-cookie-handler client)
	    (add-cookie! client (http:response-cookies response)))
	  response)))
    (define (finish r)
      (let ((status (http:response-status r)))
	(if (and status (char=? #\3 (string-ref status 0)))
	    (handle-redirect client request r success failure)
	    (success r))))
    ;; The idea of splitting header and body receiving was, probably, good.
    ;; Though using socket (retry) wasn't, even HTTP/2 server may immediately
    ;; return data frame and the socket won't be readable by select(2) call.
    ;; I'm not entirely sure how much impact we would get if we don't split
    ;; header and body reading, but for now, keep it simple...
    (executor-submit! executor
     (lambda ()
       (guard (e (else (failure e)))
	 (let loop ()
	   (http-connection-receive-header! conn request)
	   (let-values (((data? status h*) (apply values (header-state))))
	     (cond ((eqv? (string-ref status 0) #\1)
		    ;; TODO extra handler for 1xx status, esp 103?
		    (loop))
		   (else (finish (receive-data conn)))))))))))

(define (send-request client conn request)
  (let-values (((header-handler body-handler header-state response-retriever)
		(make-handlers)))
    (let ((req (adjust-request client request header-handler body-handler)))
      (http-connection-send-header! conn req)
      (http-connection-send-data! conn req)
      (response-handler header-state response-retriever req))))

(define (adjust-request client request header-handler data-handler)
  (let* ((copy (http:request-builder
		(from request)
		(header-handler header-handler)
		(data-handler data-handler)))
	 (headers (http:request-headers copy)))
    (unless (http:headers-contains? headers "User-Agent")
      (http:headers-set! headers "User-Agent"
			 (http:client-user-agent client)))
    (unless (http:headers-contains? headers "Accept")
      (http:headers-set! headers "Accept" "*/*"))
    (http:headers-set! headers "Accept-Encoding" "gzip, deflate")
    (cond ((http:request-auth request) =>
	   (lambda (provider)
	     (when (procedure? provider)
	       (http:headers-set! headers "Authorization" (provider))))))
    (let ((request-cookies (http:request-cookies request)))
      (unless (null? request-cookies)
	(http:headers-add! headers "Cookie"
			   (cookies->string request-cookies))))
    (cond ((http:client-cookie-handler client) =>
	   (lambda (jar)
	     (define uri (http:request-uri request))
	     ;; okay add cookie here
	     (let ((cookies (cookie-jar->cookies jar
						 (cookie-jar-selector uri))))
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
      (lambda (in) 
	(let ((v (get-bytevector-all in)))
	  (cond ((eof-object? v) #vu8())
		(else v))))))
		    
  (let ((in/out (open-chunked-binary-input/output-port))
	(response (make-mutable-response #f #f #vu8())))
    (define header-status '(#f #f #f))
    (define (header-handler status headers has-data?)
      (mutable-response-status-set! response status)
      (mutable-response-headers-set! response headers)
      (set! header-status (list has-data? status headers)))
    (define (body-handler data end?)
      (put-bytevector in/out data)
      (when end?
	(set-port-position! in/out 0)
	(let* ((headers (mutable-response-headers response))
	       (body (decompress headers in/out)))
	  (set! in/out #f)
	  (mutable-response-body-set! response body))))
    (define (response-retriever)
      (define headers (http:make-headers))
      ;; stored headers are RFC 5322 alist, so convert it here
      (for-each (lambda (kv)
		  (for-each (lambda (v)
			      (http:headers-add! headers (car kv) v))
			    (cdr kv)))
		(mutable-response-headers response))
      (let ((cookies (map parse-cookie-string
			  (http:headers-ref* headers "Set-Cookie")))
	    (body (mutable-response-body response)))
	;; clear with hope of GC friendliness
	(mutable-response-headers-set! response #f)
	(mutable-response-body-set! response #f)
	(http:response-builder (status (mutable-response-status response))
			       (headers headers)
			       (cookies cookies)
			       (body body))))
    (values header-handler body-handler (lambda () header-status)
	    response-retriever)))
      
(define (lease-http-connection client request success failure)
  (define manager (http:client-connection-manager client))
  (http-connection-manager-lease-connection manager request
   (http:client-lease-option client) success failure))

(define (release-http-connection client connection reuse?)
  (http-connection-manager-release-connection
   (http:client-connection-manager client)
   connection reuse?))
)
