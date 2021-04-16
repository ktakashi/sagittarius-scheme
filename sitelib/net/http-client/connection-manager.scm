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

	    ;; default no pooling?
	    (rename (make-http-ephemeral-connection-mamager
		     make-http-default-connection-manager))
	    make-http-ephemeral-connection-mamager
	    http-ephemeral-connection-mamager?
	    
	    make-http-pooling-connection-mamager
	    http-pooling-connection-mamager?
	    )
    (import (rnrs)
	    (net socket)
	    (net uri)
	    (net http-client connection)
	    (net http-client request)
	    (net http-client http1)
	    (net http-client http2)
	    (srfi :18 multithreading)
	    (util concurrent))

(define-record-type connection-manager
  (fields lease
	  release
	  shutdown))

(define (http-connection-manager-lease-connection manager request option)
  ((connection-manager-lease manager) manager request option))

(define (http-connection-manager-release-connection manager connection reuse?)
  ((connection-manager-release manager) manager connection reuse?))

(define (http-connection-manager-shutdown! manager)
  ((connection-manager-shutdown manager) manager))

;;; ephemeral (no pooling)
(define-record-type http-ephemeral-connection-mamager
  (parent connection-manager)
  (protocol (lambda (n)
	      (lambda ()
		((n ephemeral-lease-connection ephemeral-release-connection
		    (lambda (m) #t)))))))

(define (ephemeral-lease-connection manager request option)
  (define uri (http:request-uri request))
  (define (http2? socket)
    (and (tls-socket? socket)
	 (equal? (tls-socket-selected-alpn socket) "h2")))

  (let-values (((socket host service option) (uri->socket uri option)))
    (if (http2? socket)
	(socket->http2-connection socket option host service)
	(socket->http1-connection socket option host service))))

(define (ephemeral-release-connection manager connection reuseable?)
  (http-connection-close! connection))

(define (uri->socket uri option)
  (define scheme (uri-scheme uri))
  (define host (uri-host uri))
  (define port (uri-port uri))
  (let ((service (or port scheme)))
    (values (socket-options->client-socket option host service)
	    host service option)))

;;; pooling
(define-record-type http-pooling-connection-mamager
  (parent connection-manager)
  (fields connection-request-timeout
	  max-connection-per-route
	  ;; these are private
	  available
	  leasing
	  ephemeral-manager
	  lock)
  (protocol (lambda (n)
	      (lambda (timeout max-connection-per-route)
		((n pooling-lease-connection pooling-release-connection
		    pooling-shutdown)
		 timeout
		 max-connection-per-route
		 (make-hashtable string-hash string=?)
		 (make-hashtable string-hash string=?)
		 (make-http-ephemeral-connection-mamager)
		 (make-mutex))))))

(define (pooling-shutdown manager)
  (define available (http-pooling-connection-mamager-available manager))
  (define leasing (http-pooling-connection-mamager-leasing manager))
  (define (do-shutdown table)
    (let-values (((keys values) (hashtable-entries table)))
      (vector-for-each
       (lambda (sq)
	 (for-each http-connection-close! (shared-queue->list sq))
	 (shared-queue-clear! sq))
       values))
    (hashtable-clear! table))
  (do-shutdown available)
  (do-shutdown leasing)
  #t)

(define (pooling-lease-connection manager request option)
  (define available (http-pooling-connection-mamager-available manager))
  (define leasing (http-pooling-connection-mamager-leasing manager))
  (define lock (http-pooling-connection-mamager-lock manager))
  (define timeout
    (http-pooling-connection-mamager-connection-request-timeout manager))
  (define max-per-route
    (http-pooling-connection-mamager-max-connection-per-route manager))
  (define delegate
    (http-pooling-connection-mamager-ephemeral-manager manager))
  (define (ensure-queue table route max)
    (cond ((hashtable-ref table route #f))
	  (else (let ((q (make-shared-queue max)))
		  (hashtable-set! table route q)
		  q))))
  (define (get-connection leased avail)
    (cond ((and (not (shared-queue-empty? avail))
		(shared-queue-get! avail timeout)))
	  ((and (shared-queue-empty? avail)
		(< (+ (shared-queue-size leased) (shared-queue-size avail))
		   max-per-route))
	   (let ((conn (http-connection-manager-lease-connection delegate
								 request
								 option)))
	     (shared-queue-put! leased conn)
	     conn))
	  ;; okay, just create
	  (else (http-connection-manager-lease-connection delegate))))
  (let ((route (get-route request)))
    (mutex-lock! lock)
    (let* ((leased (ensure-queue leasing route -1))
	   (avail (ensure-queue available route max-per-route))
	   (conn (get-connection leased avail)))
      (mutex-unlock! lock)
      conn)))

(define (pooling-release-connection manager connection reuse?)
  (define available (http-pooling-connection-mamager-available manager))
  (define leasing (http-pooling-connection-mamager-leasing manager))
  (define lock (http-pooling-connection-mamager-lock manager))
  (define delegate
    (http-pooling-connection-mamager-ephemeral-manager manager))
  (define (->route connection)
    (define host (http-connection-node connection))
    (define service (http-connection-service connection))
    (define port (cond ((get-default-port service))
		       (else service)))
    (string-append host ":" port))
  
  (mutex-lock! lock)
  (let* ((route (->route connection))
	 (leased (hashtable-ref leasing route #f)))
    ;; must not be #f (if so it's a bug...)
    (if (and leased (shared-queue-remove! leased connection) reuse?)
	(let ((avail (hashtable-ref available route #f)))
	  (and avail (shared-queue-put! avail connection)))
	(http-connection-manager-release-connection delegate connection #f)))
  (mutex-unlock! lock))

(define (get-route request)
  (define uri (http:request-uri request))
  (define host (uri-host uri))
  (define port (or (uri-port uri) (get-default-port (uri-scheme uri))))
  (string-append host ":" port))

(define (get-default-port scheme)
  (or (and (string=? "http" scheme) "80")
      (and (string=? "https" scheme) "443")))

)
