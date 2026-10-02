;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client.scm - Modern HTTP client
;;;  
;;;   Copyright (c) 2021-2025  Takashi Kato  <ktakashi@ymail.com>
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

	    http:response? http:response-builder
	    <http:response>
	    http:response-status http:response-headers
	    http:response-cookies http:response-body
	    http:response-time
	    <http:response-context>
	    make-http:response-context
	    http:response-context-request
	    http:response-context-header-handler
	    http:response-context-data-handler
	    http:response-context-takeover-requested?
	    http:response-context-takeover-kind
	    http:response-context-takeover-resource
	    http:response-context-takeover!
	    http:stream-response?
	    http:stream-response-socket
	    http:stream-response-close!

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

	    http-client-error?
	    http-connection-error?
	    http-protocol-error?
	    
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
	    http:operation?
	    http:operation-state
	    http:operation-cancel!
	    http:client-start
	    http:client-send
	    http:client-send-async)
    (import (rnrs)
	    (net http-client connection)
	    (net http-client conditions)
	    (net http-client connection-manager)
	    (net http-client encoding)
	    (net http-client operation)
	    (net http-client key-manager)
	    (net http-client logging)
	    (net http-client request)
	    (net http-client stream)
	    (net socket)
	    (net uri)
	    (record builder)
	    (rfc cookie)
	    (rfc zlib)
	    (rfc :5322)
	    (rfc uri)
	    (util concurrent)
	    (sagittarius)
	    (time)
	    (srfi :19 time)
	    (srfi :39 parameters))

(define *http-client:default-executor*  
  (make-fork-join-executor
   (fork-join-pool-parameters-builder
    (thread-name-prefix "default-http-client"))))

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
		    (executor (*http-connection-manager:default-executor*))))))))

(define-syntax http:client-builder
  (make-record-builder http:client
   (;; by default we don't follow
    (follow-redirects (http:redirect never))
    (connection-manager (make-http-default-connection-manager))
    (version (http:version http/2))
    (executor *http-client:default-executor*)
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

(define (make-default-on-init data-handler)
  (lambda (request header-handler data-handler*)
    (make-response-context request header-handler data-handler* data-handler)))

(define default-on-init
  (make-default-on-init http:bytevector-data-handler))

(define (default-on-finalize ctx . rest)
  (cond ((http:response-context-takeover-requested? ctx)
	 (or (http:response-context-takeover-resource ctx)
	     (assertion-violation 'default-on-finalize
	      "Takeover requested but no takeover resource is assigned"
	      ctx)))
	(else
	 (apply response-context->response ctx rest))))

(define (default-on-headers operation ctx status headers has-data?)
  (response-context-status-set! ctx status)
  (response-context-headers-set! ctx headers)
  (response-context-has-data?-set! ctx has-data?)
  (let ((sink (response-context-sink ctx))
	(encoding (rfc5322-header-ref headers "content-encoding" "none")))
    (when sink
      (response-context-sink-set! ctx (->decoding-output-port sink encoding))))
	(when (require-stream-response? headers)
		(http:response-context-takeover! ctx 'stream))
  #t)

(define (default-on-data operation ctx data end?)
  (define sink (response-context-sink ctx))
  (put-bytevector sink data)
  #t)

(define (default-on-complete operation response) #t)
(define (default-on-error operation e) #t)

(define (http:client-start client request
	:key (on-init default-on-init)
	     (on-headers default-on-headers)
	     (on-data default-on-data)
	     (on-complete default-on-complete)
	     (on-finalize default-on-finalize)
	     (on-error default-on-error))
  (define operation
    (make-http:operation request on-init on-headers on-data
			 on-complete on-finalize on-error))
  (request/response operation client request 0)
  operation)

(define (http:client-send-async client request 
	 :key (data-handler http:bytevector-data-handler))
  (let-values (((f success failure) (make-piped-future)))
    (guard (e (else (failure e)))
      (http:client-start client request
	 :on-init (make-default-on-init data-handler)
	 :on-complete (lambda (operation response) (success response))
	 :on-error (lambda (operation e) (failure e))))
    f))

;;; helpers
(define *http:idempotent-methods*
  '(GET HEAD PUT DELETE OPTIONS TRACE))

(define (condition-who* e)
  (and (who-condition? e) (condition-who e)))

(define (retryable-request-body? request)
  (let ((body (http:request-body request)))
    (or (not body) (bytevector? body))))

(define (retryable-connection-error? e)
  (and (http-connection-error? e)
       (memq (condition-who* e) '(parse-status-line))))

(define (retryable-failure? request e attempt)
  (and (= attempt 0)
       (memq (http:request-method request) *http:idempotent-methods*)
       (retryable-request-body? request)
       (retryable-connection-error? e)))

(define (request/response operation client request :optional (attempt 0))
  (define manager (http:client-connection-manager client))
  (define executor (http:client-executor client))
  (define current-connection #f)
  (define detached? #f)
  (define released? #f)
  
  (define (release-current reuse?)
    (when (and current-connection (not detached?) (not released?))
      (set! released? #t)
      (release-http-connection client current-connection reuse?)))
  
  (define (detach-current! conn)
    (unless detached?
      (set! detached? #t)
      (http-connection-manager-detach-connection! manager conn)))
  
  (define (operation-complete response)
    (unless (http:operation-notify-complete! operation response)
      (when (http:stream-response? response)
	(http:stream-response-close! response))))
  
  (define (operation-failed e)
    (release-current #f)
    (cond ((and (retryable-failure? request e attempt)
		(not (http:operation-cancelled? operation)))
	   (request/response operation client request
			     (+ attempt 1)))
	  (else
	   (http:operation-notify-error! operation e))))
  
  (define (submit-on-read conn handler)
    (http-connection-manager-register-on-readable manager conn
     (lambda (conn retry)
       (if (http:operation-cancelled? operation)
	   (release-current #f)
	   (handler conn retry)))
     operation-failed
     (http:request-timeout request)))
  
  (http:operation-set-cancel-handler! operation (lambda () (release-current #f)))
  (when (http:operation-transition! operation 'acquiring-connection)
    (lease-http-connection 
     client request
     (lambda (conn)
       (set! current-connection conn)
       (if (http:operation-cancelled? operation)
	   (release-current #f)
	   (executor-submit! executor
	    (lambda ()
	      (guard (e (else (operation-failed e) #f))
		(http:operation-transition! operation 'sending-request)
		(let* ((resp-handler
			(send-request operation client conn request
				      release-current detach-current!))
		       (handler (resp-handler
				 client
				 operation-complete
				 operation-failed)))
		  (if (http:operation-cancelled? operation)
		      (release-current #f)
		      (if (http-connection-data-ready? conn)
			  (handler conn
				   (lambda ()
				     (submit-on-read conn handler)))
			  (submit-on-read conn handler)))))))))
     operation-failed)))

(define (default-executor? client)
  (eq? (http:client-executor client) *http-client:default-executor*))

(define (handle-redirect operation client request response success failure)
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
	       (request/response operation client new-req))))
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
  (fields start
	  (mutable status)
	  (mutable headers)
	  (mutable has-data?)
	  retriever
	  (mutable sink))
  (protocol (lambda (n)
	      (lambda (request header-handler data-handler payload-handler)
		(let-values (((sink retriever) (payload-handler)))
		  ((n request header-handler data-handler)
		   (current-time) #f '() #f retriever sink))))))

(define (response-context->response ctx)
  (define headers (http:make-headers))
  (define start (response-context-start ctx))
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

(define (stream-response status source-headers connection request)
  (define headers (http:make-headers))
  ;; stored headers are RFC 5322 alist, so convert it here
  (for-each (lambda (kv)
	      (for-each (lambda (v) (http:headers-add! headers (car kv) v))
			(cdr kv)))
	    source-headers)
  (let ((cookies (map parse-cookie-string
		      (http:headers-ref* headers "Set-Cookie")))
	(status status))
    (make-http:stream-response request status headers cookies connection)))

(define ((response-handler operation request release detach)
	 client success failure)
  (define response-status #f)
  (define response-headers '())
  (define has-data? #f)
  (define (header-callback ctx status headers has-data)
    (set! response-status status)
    (set! response-headers headers)
    (set! has-data? has-data)
    (http:operation-notify-headers! operation ctx status headers has-data))
  (define (data-callback ctx data end?)
    (http:operation-notify-data! operation ctx data end?))
  (define response-context
    (http:operation-on-init! operation request header-callback data-callback))
  (define (finalizer context)
    (http:operation-on-finalize! operation context))
  (define executor (http:client-executor client))
  (define manager (http:client-connection-manager client))
  (define (handle-cookie! result)
    (cond ((http:response? result)
	   (when (http:client-cookie-handler client)
	     (add-cookie! client (http:response-cookies result)))
	   result)
	  (else result)))
  (define (finish-result result)
    (if (http:response? result)
	(let ((status (http:response-status result)))
	  (if (and status (char=? #\3 (string-ref status 0)))
	      (handle-redirect operation client request result success failure)
	      (success result)))
	(success result)))
  (define (prepare-takeover-response! conn status headers)
    (define requested (http:response-context-takeover-kind response-context))
    (let-values (((kind action)
		  (http-connection-resolve-response-takeover
		   conn requested)))
      (define takeover-response (stream-response status headers conn request))
      (http:response-context-takeover! response-context kind takeover-response)
      (case action
	((detach) (detach conn))
	((shared) (release #t))
	(else (assertion-violation 'response-handler
				   "Unsupported takeover action" action)))))
  (define (receive-data conn response-context retry)
    (if (http:operation-cancelled? operation)
	(release #f)
	(let ()
	  (http:operation-transition! operation 'receiving-body)
	  (case (http-connection-receive-data! conn response-context)
	    ((continue) (retry))
	    (else =>
	     (lambda (state)
	       (release (eq? state 'done))
	       (let ((result (finalizer response-context)))
		 (finish-result (handle-cookie! result)))))))))

  (define (receive-header conn retry)
    (define (delay-data-receive)
      (http-connection-manager-register-on-readable manager conn
       (lambda (conn retry) 
	 (executor-submit! executor
	  (lambda () (receive-data conn response-context retry))))
       failure (http:request-timeout request)))
    (if (http:operation-cancelled? operation)
	(release #f)
	(guard (e (else (failure e)))
	  (let loop ()
	    (http:operation-transition! operation 'receiving-headers)
	    (http-connection-receive-header! conn response-context)
	    (let ((status response-status)
		  (headers response-headers)
		  (has-data? has-data?))
	      ;; TODO extra handler for 1xx status, esp 103?
	      (cond ((eqv? (string-ref status 0) #\1) (loop))
		    ((http:response-context-takeover-requested?
		      response-context)
		     (prepare-takeover-response! conn status headers)
		     (finish-result
		      (handle-cookie! (finalizer response-context))))
		    ((or (not has-data?) (http-connection-data-ready? conn))
		     (receive-data conn response-context delay-data-receive))
		    (else (delay-data-receive))))))))
  
  (lambda (conn retry)
    (executor-submit! executor (lambda () (receive-header conn retry)))))

(define (send-request operation client conn request release detach)
  (let ((req (adjust-request client request)))
    (http-connection-send-header! conn req)
    (http-connection-send-data! conn req)
    (response-handler operation req release detach)))

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

)
