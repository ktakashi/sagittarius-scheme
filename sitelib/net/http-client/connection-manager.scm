;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/connection-manager.scm - Connection pool for HTTP client
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
(library (net http-client connection-manager)
    (export http-connection-manager-lease-connection
	    http-connection-manager-release-connection
	    http-connection-manager-shutdown!

	    http-connection-manager-register-on-readable

	    make-http-default-connection-manager

	    http-connection-config?
	    http-connection-config-builder

	    make-http-ephemeral-connection-manager
	    http-ephemeral-connection-manager?
	    
	    ;; for logging
	    make-http-logging-connection-manager
	    http-logging-connection-manager?
	    
	    make-http-pooling-connection-manager
	    http-pooling-connection-manager?
	    http-pooling-connection-config?
	    http-pooling-connection-config-builder

	    (rename (make-http-ephemeral-connection-manager
		     default-delegate-connection-manager-provider))
	    make-logging-delegate-connection-provider

	    *http-connection-manager:default-executor*

	    dns-timeout-error? dns-timeout-node dns-timeout-service
	    connection-request-timeout-error?
	    
	    ;; internal
	    http-connection-lease-option-builder
	    )
    (import (rnrs)
	    (net socket)
	    (net uri)
	    (net http-client logging)
	    (net http-client connection)
	    (net http-client key-manager)
	    (net http-client request)
	    (net http-client http1)
	    (net http-client http2)
	    (record builder)
	    (sagittarius)
	    (scheme lazy)
	    (srfi :18 multithreading)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (util concurrent)
	    (util duration))

(define-record-type connection-manager
  (fields lease
	  release
	  shutdown
	  key-manager
	  dns-timeout
	  read-timeout
	  connection-timeout
	  socket-selector
	  selector-terminator)
  (protocol
   (lambda (p)
     (case-lambda
      ((lease release shutdown config)
       (define km (http-connection-config-key-manager config))
       (define dns (http-connection-config-dns-timeout config))
       (define read (http-connection-config-read-timeout config))
       (define conn (http-connection-config-connection-timeout config))
       (define err (http-connection-config-selector-error-handler config))
       (let-values (((selector terminator) (make-socket-selector read err)))
	 (p lease release shutdown km dns read conn selector terminator)))
      ((lease release shutdown config selector terminator)
       (define km (http-connection-config-key-manager config))
       (define dns (http-connection-config-dns-timeout config))
       (define read (http-connection-config-read-timeout config))
       (define conn (http-connection-config-connection-timeout config))
       (define err (http-connection-config-selector-error-handler config))
       (p lease release shutdown km dns read conn selector terminator))))))

(define-record-type http-connection-config
  (fields key-manager
	  dns-timeout
	  read-timeout
	  connection-timeout
	  selector-error-handler
	  dns-lookup-executor))
(define-syntax http-connection-config-builder
  (make-record-builder http-connection-config))

(define-record-type http-connection-lease-option
  (fields alpn executor))
(define-syntax http-connection-lease-option-builder
  (make-record-builder http-connection-lease-option))

;; Don't create during the library loading
(define *http-connection-manager:default-executor*
  (make-parameter
   (delay (make-fork-join-executor
	   (fork-join-pool-parameters-builder
	    (thread-name-prefix "default-http-connection-manager"))))
   (lambda (v)
     (cond ((promise? v) v)
	   ((executor? v) (delay v))
	   (else (assertion-violation
		  '*http-connection-manager:default-executor*
		  "Promise or executor required" v))))))


(define (http-connection-manager-lease-connection manager request option
						  success failure)
  (define executor (http-connection-lease-option-executor option))
  (executor-submit! executor
   (lambda ()
     (cond ((guard (e (else (failure e) #f))
	      (internal-lease-connection manager request option))
	    => success)))))

(define (http-connection-manager-release-connection manager connection reuse?)
  ((connection-manager-release manager) manager connection reuse?))

(define (http-connection-manager-shutdown! manager)
  ((connection-manager-selector-terminator manager))
  ((connection-manager-shutdown manager) manager))

(define (http-connection-manager-register-on-readable manager
						      conn
						      on-read
						      on-error
						      timeout)
  (define selector (connection-manager-socket-selector manager))
  (define (wrap on-read)
    (lambda (socket e retry)
      (guard (e (else (on-error e)))
	(if e
	    (on-error e)
	    (on-read conn retry)))))
  (cond ((http-connection-socket conn) =>
	 (lambda (socket) (selector socket (wrap on-read) timeout)))
	(else
	 ;; TODO proper error
	 (error 'http-connection-manager-register-on-readable
		"Connection is already closed or not open yet" conn))))

(define-condition-type &timeout &i/o
  make-timeout-error timeout-error?)

(define-condition-type &connection-request-timeout &timeout
  make-connection-request-timeout-error connection-request-timeout-error?)

(define-condition-type &dns-timeout &timeout
  make-dns-timeout-error dns-timeout-error?
  (node dns-timeout-node)
  (service dns-timeout-service))

(define (internal-lease-connection manager request option)
  ((connection-manager-lease manager) manager request option))

(define (http-connection-manager->socket-option cm request option)
  (define uri (http:request-uri request))
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))

  (define (time/millis v)
    (cond ((integer? v) (duration:of-millis v))
	  ((time? v) v)
	  (else (assertion-violation 'http-connection-manager->socket-option
				     "timeout must be integer or time" v))))
  
  (define (timeout->expires-at v) (add-duration (current-time) (time/millis v)))
  
  (define connection-timeout
    (cond ((connection-manager-connection-timeout cm) => time/millis)
	  (else #f)))
  (define read-timeout
    (cond ((connection-manager-read-timeout cm) => time/millis)
	  (else #f)))
  (define dns-timeout
    (cond ((connection-manager-dns-timeout cm) => timeout->expires-at)
	  (else #f)))
  (define executor (delegatable-connection-manager-dns-lookup-executor cm))
  
  (define (make-dns-resolver)
    (define (dns-resolver node service options)
      (let ((f (executor-submit! executor
		(lambda () (default-dns-resolver node service options)))))
	(or (future-get f dns-timeout)
	    (raise (condition
		    (make-dns-timeout-error node service)
		    (make-who-condition 'http-connection-manager->socket-option)
		    (make-message-condition "DNS lookup timeout")
		    (make-irritants-condition (list node service)))))))
    (and dns-timeout dns-resolver))
  
  (define km (connection-manager-key-manager cm))
  (if (string=? scheme "https")
      (tls-socket-options (connection-timeout connection-timeout)
			  (read-timeout read-timeout)
			  (client-certificate-provider
			   (and km (key-manager->certificate-callback km)))
			  (sni* (list host))
			  (alpn* (http-connection-lease-option-alpn option))
			  (dns-resolver (make-dns-resolver)))
      (socket-options (connection-timeout connection-timeout)
		      (read-timeout read-timeout)
		      (dns-resolver (make-dns-resolver)))))

(define-record-type delegatable-connection-manager
  (parent connection-manager)
  (fields dns-lookup-executor)
  (protocol (lambda (n)
	      (define ((make-shutdown! shutdown) m)
		(shutdown-executor!
		 (delegatable-connection-manager-dns-lookup-executor m))
		(shutdown m))
	      (case-lambda
	       ((lease release shutdown config)
		((n lease release (make-shutdown! shutdown) config)
		 (or (http-connection-config-dns-lookup-executor config)
		     (make-fork-join-executor
		      (fork-join-pool-parameters-builder
		       (thread-name-prefix "dns-lookup"))))))
	       ((lease release shutdown config delegatee)
		((n lease release (make-shutdown! shutdown) config
		    (connection-manager-socket-selector delegatee)
		    (connection-manager-selector-terminator delegatee))
		 (or (http-connection-config-dns-lookup-executor config)
		     (make-fork-join-executor
		      (fork-join-pool-parameters-builder
		       (thread-name-prefix "dns-lookup"))))))))))

;;; ephemeral (no pooling)
(define-record-type http-ephemeral-connection-manager
  (parent delegatable-connection-manager)
  (protocol (lambda (n)
	      (case-lambda
	       ((config)
		((n ephemeral-lease-connection ephemeral-release-connection
		    (lambda (m) #t) config)))
	       ((config delegatee)
		((n ephemeral-lease-connection ephemeral-release-connection
		    (lambda (m) #t) config delegatee)))))))

(define (ephemeral-lease-connection manager request option)
  (define uri (http:request-uri request))
  (define (http2? socket)
    (and (tls-socket? socket)
	 (equal? (tls-socket-selected-alpn socket) "h2")))
  
  (let ((socket-option
	 (http-connection-manager->socket-option manager request option)))
    (let-values (((socket host service option) (uri->socket uri socket-option)))
      (if (http2? socket)
	  (socket->http2-connection socket option host service)
	  (socket->http1-connection socket option host service)))))

(define (ephemeral-release-connection manager connection reuseable?)
  (http-connection-close! connection))
(define (make-http-default-connection-manager)
  (make-http-ephemeral-connection-manager (http-connection-config-builder)))

(define (uri->socket uri option)
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))
  (define port (uri-port uri))
  (let ((service (or port scheme)))
    (values (socket-options->client-socket option host service)
	    host service option)))

(define-record-type http-logging-connection-manager
  (parent delegatable-connection-manager)
  (protocol (lambda (n)
	      (case-lambda
	       ((config logger)
		((n (make-logging-lease-connection logger)
		    (make-logging-release-connection logger)
		    (lambda (m) #t) config)))
	       ((config logger delegatee)
		((n (make-logging-lease-connection logger)
		    (make-logging-release-connection logger)
		    (lambda (m) #t) config) delegatee))))))

(define (make-logging-lease-connection logger)
  (lambda (manager request option)
    (define uri (http:request-uri request))
    (guard (e (else
	       (http-client-logger-write-log logger 'connection-manager
		 "[Lease Connection] Failed to acquire a connection: ~a"
		 (condition-message e)
		 e)
	       (raise e)))
      (let ((conn (ephemeral-lease-connection manager request option)))
	(http-client-logger-write-log logger 'connection-manager
	  "[Lease Connection] Connected to service: ~a, node: ~a, port: ~a"
	  (uri-scheme uri) (uri-host uri) (or (uri-port uri) "?"))
	(make-http-logging-connection conn logger)))))

(define (make-logging-release-connection logger)
  (define connection-logger
    (and logger (http-client-logger-connection-logger logger)))
  (lambda (manager connection reuseable?)
    (http-client-logger-write-log logger 'connection-manager
     "[Release Connection] Releasing a connection of node: ~a, service: ~a"
     (http-connection-node connection) (http-connection-service connection))
    (ephemeral-release-connection manager connection reuseable?)))

(define (make-logging-delegate-connection-provider logger)
  (lambda (config delegatee)
    (make-http-logging-connection-manager config logger delegatee)))

;;; pooling
(define-record-type http-pooling-connection-manager
  (parent connection-manager)
  (fields connection-request-timeout
	  max-connection-per-route
	  route-max-connections
	  time-to-live
	  ;; these are private
	  available
	  leasing
	  (mutable delegate)
	  lock
	  notifier)
  (protocol
   (lambda (n)
     (lambda (config)
       (let* ((me ((n pooling-lease-connection pooling-release-connection
		      pooling-shutdown config)
		   (http-pooling-connection-config-connection-request-timeout config)
		   (http-pooling-connection-config-max-connection-per-route config)
		   (http-pooling-connection-config-route-max-connections config)
		   (http-pooling-connection-config-time-to-live config)
		   (make-hashtable string-hash string=?)
		   (make-hashtable string-hash string=?)
		   #f
		   (make-mutex "pooling-connection-manager-lock")
		   (make-notifier)))
	      (delegate ((http-pooling-connection-config-delegate-provider config) config me)))
	 (http-pooling-connection-manager-delegate-set! me delegate)
	 me)))))

(define-record-type http-pooling-connection-config
  (parent http-connection-config)
  (fields connection-request-timeout
	  max-connection-per-route
	  route-max-connections
	  time-to-live
	  delegate-provider))
(define (route-max-connections-check v)
  (define (check e)
    (and (list? e)
	 (not (null? e))
	 (string? (car e))
	 (not (null? (cdr e)))
	 (integer? (cadr e))))
  (unless (and (list? v) (for-all check v))
    (assertion-violation 'http-pooling-connection-config-builder
     "route-mac-connections must be list of (string integer)" v))
  v)
(define-syntax http-pooling-connection-config-builder
  (make-record-builder http-pooling-connection-config
   ;; random numbers ;-)
   ((max-connection-per-route 5 (lambda (v) (or v 5)))
    (route-max-connections '() route-max-connections-check)
    (time-to-live 2 (lambda (v) (or v 2)))
    (delegate-provider make-http-ephemeral-connection-manager))))

(define (pooling-shutdown manager)
  (define delegate (http-pooling-connection-manager-delegate manager))
  (define available (http-pooling-connection-manager-available manager))
  (define leasing (http-pooling-connection-manager-leasing manager))
  (define (v0 e) (vector-ref e 0))
  (define (do-shutdown table unwrap)
    (let-values (((keys values) (hashtable-entries table)))
      (vector-for-each
       (lambda (sq)
	 (for-each (lambda (e)
		     (http-connection-manager-release-connection
		      delegate (pooling-entry-connection (unwrap e)) #f))
		   (shared-queue->list sq))
	 (shared-queue-clear! sq))
       values))
    (hashtable-clear! table))
  (do-shutdown available values)
  (do-shutdown leasing v0)
  (http-connection-manager-shutdown! delegate)
  #t)

(define-record-type pooling-entry
  (fields expires
	  connection)
  (protocol (lambda (p)
	      (lambda (ttl conn)
		(p (add-duration (current-time)
				 (make-time time-duration 0 ttl))
		   conn)))))
(define (pooling-entry-expired? entry)
  (time>=? (current-time) (pooling-entry-expires entry)))

(define (pooling-lease-connection manager request option)
  (define available (http-pooling-connection-manager-available manager))
  (define leasing (http-pooling-connection-manager-leasing manager))
  (define lock (http-pooling-connection-manager-lock manager))
  (define notifier (http-pooling-connection-manager-notifier manager))
  (define timeout
    (cond ((http-pooling-connection-manager-connection-request-timeout manager) 
	   => (lambda (t)
		(if (time? t)
		    t
		    (duration:of-millis t))))
	  (else #f)))
  (define max-per-route
    (http-pooling-connection-manager-max-connection-per-route manager))
  (define route-max-connections
    (http-pooling-connection-manager-route-max-connections manager))
  (define delegate
    (http-pooling-connection-manager-delegate manager))
  (define ttl
    (http-pooling-connection-manager-time-to-live manager))
  
  (define (ensure-queue table route max)
    (cond ((hashtable-ref table route #f))
	  (else
	   (mutex-lock! lock)
	   (let ((q (hashtable-ref table route #f)))
	     (cond (q (mutex-unlock! lock) q)
		   (else
		    (let ((q (make-shared-queue max)))
		      (hashtable-set! table route q)
		      (mutex-unlock! lock)
		      q)))))))
  (define (get-max-connection-per-route route)
    (cond ((assp (lambda (r) (equal? r route)) route-max-connections) => cadr)
	  (else max-per-route)))
  
  (define (try-new-connection leased avail max-conn)
    (define (lease) (internal-lease-connection delegate request option))
    (define mpe make-pooling-entry)

    (mutex-lock! lock)
    (let* ((total (+ (shared-queue-size leased) (shared-queue-size avail)))
	   (box (and (< total max-conn) (vector #f))))
      (when box (shared-queue-put! leased box))
      (mutex-unlock! lock)
      (and box
	   (let ((conn (lease)))
	     (vector-set! box 0 (mpe ttl conn))
	     conn))))

  (define (get-connection leased avail max-conn timeout)
    (define (handle-leased-connection entry leased avail max-conn timeout)
      ;; okay check if it's expired or not
      (cond ((pooling-entry-expired? entry)
	     (http-connection-manager-release-connection delegate
	       (pooling-entry-connection entry) #f)
	     (get-connection leased avail max-conn timeout))
	    (else
	     (shared-queue-put! leased (vector entry))
	     (pooling-entry-connection entry))))

    (define (wait)
      (let ((t (and timeout (add-duration (current-time) timeout))))
	(cond ((notifier-wait-notification! notifier t)
	       (or (not t) (time-difference t (current-time))))
	      (else #f))))

    (cond ((shared-queue-pop! avail 0) =>
	   (lambda (entry)
	     (handle-leased-connection entry leased avail max-conn timeout)))
	  ((try-new-connection leased avail max-conn))
	  ((wait) => (lambda (to) (get-connection leased avail max-conn to)))
	  (else (raise (condition
			(make-connection-request-timeout-error)
			(make-who-condition 'pooling-lease-connection)
			(make-message-condition
			 (format "Connection request timeout: ~a"
				 (get-route request)))
			(make-irritants-condition timeout))))))
  
  (define (queues lock route leasing available)
    ;; we don't make any limit for the queue to avoid overflowing dead lock
    (let ((leased (ensure-queue leasing route -1))
	  (avail (ensure-queue available route -1)))
      (values leased avail)))
  (let* ((route (get-route request))
	 (max-conn (get-max-connection-per-route route)))
    (let-values (((leased avail) (queues lock route leasing available)))
      (get-connection leased avail max-conn timeout))))

(define (pooling-release-connection manager connection reuse?)
  (define available (http-pooling-connection-manager-available manager))
  (define leasing (http-pooling-connection-manager-leasing manager))
  (define lock (http-pooling-connection-manager-lock manager))
  (define notifier (http-pooling-connection-manager-notifier manager))
  (define delegate (http-pooling-connection-manager-delegate manager))

  (define (->route connection)
    (define host (http-connection-node connection))
    (define service (http-connection-service connection))
    (define port (or (get-default-port service) service))
    (string-append host ":" port))

  (define (remove-entry sq conn)
    (define pec pooling-entry-connection)
    (define (v0 e) (vector-ref e 0))
    (define ((pred conn) e)
      (cond ((v0 e) => (lambda (e) (eq? conn (pec e))))
	    (else #f)))
    (let-values (((removed? v) (shared-queue-remp! sq (pred conn))))
      (and removed? (v0 v))))

  (define (release conn)
    (http-connection-manager-release-connection delegate conn #f))

  (let* ((route (->route connection))
	 (leased (hashtable-ref leasing route #f)))
    ;; must not be #f (if so it's a bug...)
    (cond ((and leased (remove-entry leased connection)) =>
	   (lambda (entry)
	     (cond ((and reuse? (hashtable-ref available route #f)) =>
		    (lambda (avail)
		      ;; due to the loose lock, we can't stricly manage
		      ;; the number of leasing and available connections
		      ;; to max connection. this means, the sum of these
		      ;; 2 queues might overflow the max connection, so
		      ;; we need to check it to avoid dead lock here.
		      (if (pooling-entry-expired? entry)
			  (release (pooling-entry-connection entry))
			  (shared-queue-put! avail entry))))
		   (else (release (pooling-entry-connection entry))))))
	  (else (release connection)))
    ;; only one is available now, so don't make mess :)
    (notifier-send-notification! notifier #f)))

(define (get-route request)
  (define uri (http:request-uri request))
  (define host (uri-host uri))
  (define port (or (uri-port uri) (get-default-port (uri-scheme uri))))
  (string-append host ":" port))

(define (get-default-port scheme)
  (or (and (string=? "http" scheme) "80")
      (and (string=? "https" scheme) "443")))

)
