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
     (lambda (lease release shutdown config)
       (define km (http-connection-config-key-manager config))
       (define dns (http-connection-config-dns-timeout config))
       (define read (http-connection-config-read-timeout config))
       (define conn (http-connection-config-connection-timeout config))
       (define err (http-connection-config-selector-error-handler config))
       (let-values (((selector terminator) (make-socket-selector read err)))
	 (p lease release shutdown km dns read conn selector terminator))))))

(define-record-type http-connection-config
  (fields key-manager
	  dns-timeout
	  read-timeout
	  connection-timeout
	  selector-error-handler))
(define-syntax http-connection-config-builder
  (make-record-builder http-connection-config))

(define-record-type http-connection-lease-option
  (fields alpn executor))
(define-syntax http-connection-lease-option-builder
  (make-record-builder http-connection-lease-option))

;; Don't create during the library loading
(define *http-connection-manager:default-executor*
  (make-parameter (delay (make-fork-join-executor))
		  (lambda (v)
		    (cond ((promise? v) v)
			  ((executor? v) (delay v))
			  (else (assertion-violation
				 '*http-connection-manager:default-executor*
				 "Promise or executor required" v))))))


(define (http-connection-manager-lease-connection manager request option)
  ((connection-manager-lease manager) manager request option))

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
      (when e (on-error e))
      (on-read conn retry)))
  (cond ((http-connection-socket conn) =>
	 (lambda (socket) (selector socket (wrap on-read) timeout)))
	(else
	 ;; TODO proper error
	 (error 'http-connection-manager-register-on-readable
		"Connection is already closed or not open yet" conn))))

(define-condition-type &dns-timeout &i/o
  make-dns-timeout-error dns-timeout-error?
  (node dns-timeout-node)
  (service dns-timeout-service))

(define (http-connection-manager->socket-option cm request option)
  (define uri (http:request-uri request))
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))

  (define (timeout->expires-at v)
    (let ((n (cond ((integer? v) (duration:of-millis v))
		   ((time? v) v)
		   (else (assertion-violation
			  'http-connection-manager->socket-option
			  "dns-timeout must be integer or time" v)))))
      (add-duration (current-time) n)))
  (define connection-timeout
    (cond ((connection-manager-connection-timeout cm) => duration:of-millis)
	  (else #f)))
  (define read-timeout
    (cond ((connection-manager-read-timeout cm) => duration:of-millis)
	  (else #f)))
  (define dns-timeout
    (cond ((connection-manager-dns-timeout cm) => timeout->expires-at)
	  (else #f)))
  (define executor (http-connection-lease-option-executor option))
  
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

;;; ephemeral (no pooling)
(define-record-type http-ephemeral-connection-manager
  (parent connection-manager)
  (protocol (lambda (n)
	      (lambda (config)
		((n ephemeral-lease-connection ephemeral-release-connection
		    (lambda (m) #t) config))))))

(define (ephemeral-lease-connection manager request alpn)
  (define uri (http:request-uri request))
  (define option (http-connection-manager->socket-option manager request alpn))
  (define (http2? socket)
    (and (tls-socket? socket)
	 (equal? (tls-socket-selected-alpn socket) "h2")))

  (let-values (((socket host service option) (uri->socket uri option)))
    (if (http2? socket)
	(socket->http2-connection socket option host service)
	(socket->http1-connection socket option host service))))

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
  (parent connection-manager)
  (protocol (lambda (n)
	      (lambda (config logger)
		((n (make-logging-lease-connection logger)
		    (make-logging-release-connection logger)
		    (lambda (m) #t) config))))))

(define (make-logging-lease-connection logger)
  (define connection-logger
    (and logger (http-client-logger-connection-logger logger)))
  (lambda (manager request option)
    (define uri (http:request-uri request))
    (guard (e (else
	       (http-connection-logger-write-log connection-logger
		(string-append 
		 "[Lease Connection] Failed to acquire a connection: "
		 (condition-message e))
		e)
	       (raise e)))
      (let ((conn (ephemeral-lease-connection manager request option)))
	(http-connection-logger-write-log connection-logger
	  "[Lease Connection] Connected to service: ~a, node: ~a, port: ~a"
	  (uri-scheme uri) (uri-host uri) (or (uri-port uri) "?"))
	(make-http-logging-connection conn logger)))))

(define (make-logging-release-connection logger)
  (define connection-logger
    (and logger (http-client-logger-connection-logger logger)))
  (lambda (manager connection reuseable?)
    (http-connection-logger-write-log connection-logger
     "[Release Connection] Releasing a connection of node: ~a, service: ~a"
     (http-connection-node connection) (http-connection-service connection))
    (ephemeral-release-connection manager connection reuseable?)))

(define (make-logging-delegate-connection-provider logger)
  (lambda (config)
    (make-http-logging-connection-manager config logger)))

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
	  delegate
	  lock)
  (protocol
   (lambda (n)
     (lambda (config)
       ((n pooling-lease-connection pooling-release-connection
	   pooling-shutdown config)
	(http-pooling-connection-config-connection-request-timeout config)
	(http-pooling-connection-config-max-connection-per-route config)
	(http-pooling-connection-config-route-max-connections config)
	(http-pooling-connection-config-time-to-live config)
	(make-hashtable string-hash string=?)
	(make-hashtable string-hash string=?)
	((http-pooling-connection-config-delegate-provider config) config)
	(make-mutex "pooling-connection-manager-lock"))))))

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
  (define (do-shutdown table)
    (let-values (((keys values) (hashtable-entries table)))
      (vector-for-each
       (lambda (sq)
	 (for-each (lambda (e)
		     (http-connection-close! (pooling-entry-connection e)))
		   (shared-queue->list sq))
	 (shared-queue-clear! sq))
       values))
    (hashtable-clear! table))
  (do-shutdown available)
  (do-shutdown leasing)
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
  (define timeout
    (http-pooling-connection-manager-connection-request-timeout manager))
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
	  (else (let ((q (make-shared-queue max)))
		  (hashtable-set! table route q)
		  q))))
  (define (get-max-connection-per-route route)
    (cond ((assp (lambda (r) (equal? r route)) route-max-connections) => cadr)
	  (else max-per-route)))
  (define (get-connection leased avail max-conn)
    (cond ((and (not (shared-queue-empty? avail))
		;; TODO maybe we need to do LIFO instead of FIFO for
		;;      better reusability
		(shared-queue-get! avail timeout))
	   => (lambda (entry)
		;; okay check if it's expired or not
		(cond ((pooling-entry-expired? entry)
		       (http-connection-manager-release-connection delegate
			(pooling-entry-connection entry) #f)
		       (get-connection leased avail max-conn))
		      (else
		       (shared-queue-put! leased entry)
		       (pooling-entry-connection entry)))))
	  ((and (shared-queue-empty? avail)
		(< (+ (shared-queue-size leased) (shared-queue-size avail))
		   max-conn))
	   (let ((conn (http-connection-manager-lease-connection delegate
								 request
								 option)))
	     (shared-queue-put! leased (make-pooling-entry ttl conn))
	     conn))
	  ;; okay, just create
	  (else (http-connection-manager-lease-connection delegate
							  request
							  option))))

  (let* ((route (get-route request))
	 (max-conn (get-max-connection-per-route route)))
    (mutex-lock! lock)
    (guard (e (else (mutex-unlock! lock) (raise e)))
      (let* ((leased (ensure-queue leasing route -1))
	     (avail (ensure-queue available route max-conn))
	     (conn (get-connection leased avail max-conn)))
	(mutex-unlock! lock)
	conn))))

(define (pooling-release-connection manager connection reuse?)
  (define available (http-pooling-connection-manager-available manager))
  (define leasing (http-pooling-connection-manager-leasing manager))
  (define lock (http-pooling-connection-manager-lock manager))
  (define delegate
    (http-pooling-connection-manager-delegate manager))
  (define (->route connection)
    (define host (http-connection-node connection))
    (define service (http-connection-service connection))
    (define port (cond ((get-default-port service))
		       (else service)))
    (string-append host ":" port))
  (define (remove-entry sq conn)
    (cond ((shared-queue-find sq (lambda (e)
				   (eq? (pooling-entry-connection e) conn)))
	   => (lambda (e) (shared-queue-remove! sq e) e))
	  (else #f)))
  
  (mutex-lock! lock)
  (guard (e (else (mutex-unlock! lock) (raise e)))
    (let* ((route (->route connection))
	   (leased (hashtable-ref leasing route #f)))
      ;; must not be #f (if so it's a bug...)
      (cond ((and leased (remove-entry leased connection)) =>
	     (lambda (entry)
	       (when reuse?
		 (let ((avail (hashtable-ref available route #f)))
		   (and avail (shared-queue-put! avail entry))))))
	    (else
	     (http-connection-manager-release-connection
	      delegate connection #f))))
    (mutex-unlock! lock)))

(define (get-route request)
  (define uri (http:request-uri request))
  (define host (uri-host uri))
  (define port (or (uri-port uri) (get-default-port (uri-scheme uri))))
  (string-append host ":" port))

(define (get-default-port scheme)
  (or (and (string=? "http" scheme) "80")
      (and (string=? "https" scheme) "443")))

)
