;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server.scm - HTTP server
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (net http-server)
    (export <http-server> make-http-server http-server?
	    http-server-upgrade-registry
	    http-server:connection-open?
	    http-server:register-upgrade!

	    <http-server-config> make-http-server-config http-server-config?
	    http-server-config-read-size
	    http-server-config-cache
	    http-server-config-upgrades

	    make-http-server:default-protocol-registry
	    http-server:register-protocol-driver!
	    http-server:deregister-protocol-driver!
	    http-server:protocol-registry-update-driver-config!

            http-server:upgrade-registry?
            make-http-server:upgrade-registry
	    http-server:upgrade-registry-register!
	    http-server:upgrade-registry-registered?

	    http-config?
	    http-config-max-header-bytes
	    http-config-max-body-bytes
	    <http1-config> http1-config? make-http1-config
	    http1-config-max-request-par-connection
	    http1-config-max-pipelined-requests	    
	    <http2-config> http2-config? make-http2-config
	    http2-config-max-concurrent-streams
	    http2-config-enable-push?
	    
	    http-server:request?
	    make-http-server:request
	    http-server:request-method
	    http-server:request-target
	    http-server:request-path
	    http-server:request-query
	    http-server:request-http-version
	    http-server:request-headers
	    http-server:request-body-bytevector
	    http-server:request-body-port
	    http-server:request-remote
	    http-server:request-attributes
	    http-server:request-header-ref
	    http-server:request-header-ref*
	    http-server:request-attribute-ref
	    http-server:request-attribute-set!

	    http-server:response?
	    make-http-server:response
	    http-server:response-status
	    http-server:response-status-set!
	    http-server:response-reason
	    http-server:response-reason-set!
	    http-server:response-headers
	    http-server:response-body
	    http-server:response-body-set!
	    http-server:response-pushes
	    http-server:response-cacheable?
	    http-server:response-cacheable?-set!
	    http-server:response-cache-ttl
	    http-server:response-cache-ttl-set!
	    http-server:response-header-ref
	    http-server:response-header-ref*
	    http-server:response-header-set!
	    http-server:response-header-add!
	    http-server:response-push!
	    http-server:response-text!
	    http-server:response-bytes!

	    http-server:router?
	    make-http-server:router
	    http-server:router-add-route!
	    http-server:make-router-handler

	    http-server:cache?
	    make-http-server:cache
	    http-server:cache-lookup
	    http-server:cache-store!
	    http-server:cache-invalidate!
	    http-server:cache-clear!
	    http-server:make-cache-middleware
	    make-http-server:memory-cache)
    (import (rnrs)
	    (clos user)
	    (sagittarius) ;; for get-keyword
	    (srfi :18)
	    (net socket)
	    (net server)
	    (net http-server types)
	    (net http-server request)
	    (net http-server response)
	    (net http-server router)
	    (net http-server cache)
	    (net http-server cache memory)
	    (net http-server protocol)
	    (net http-server upgrade)
	    (net http-server http1)
	    (net http-server http2)
	    (util bytevector))

(define-class <http-server-config> (<server-config>)
  ((read-size :init-keyword :read-size :init-value 8192
	      :reader http-server-config-read-size)
   (cache :init-keyword :cache :init-value #f
	  :reader http-server-config-cache)
   (upgrades :init-keyword :upgrades :init-value '()
	     :reader http-server-config-upgrades)
   (max-drain :init-keyword :max-drain :init-value 8)
   (select-delay :init-keyword :select-delay :init-value 1)))

(define (make-http-server-config . opts)
  (define http2? (get-keyword :http2? opts #t))
  (define opts*
    (if (memq :alpn opts)
        opts
        (append (list :alpn (if http2? '("h2" "http/1.1") '("http/1.1"))) opts)))
  (apply make <http-server-config>
	 :close-socket? #f
	 opts*))

(define (http-server-config? o) (is-a? o <http-server-config>))

(define-class <http-server> (<simple-server>)
  ((protocol-registry :init-keyword :protocol-registry)
   (upgrade-registry :init-keyword :upgrade-registry
		     :reader http-server-upgrade-registry)
   (app-handler :init-keyword :app-handler)
   (states :init-form (make-eq-hashtable))
   (lock :init-form (make-mutex))))
(define (http-server? o) (is-a? o <http-server>))

(define (http-server:connection-open? server socket)
  (define lock (slot-ref server 'lock))
  (define states (slot-ref server 'states))

  (mutex-lock! lock)
  (let ((alive (hashtable-ref states socket #f)))
    (mutex-unlock! lock)
    (and alive #t)))

(define (make-http-server:default-protocol-registry)
  (let ((r (make-http-server:protocol-registry *http-server:http1-driver*)))
    (http-server:register-protocol-driver! r
     "http/1.1" *http-server:http1-driver* (make-http1-config))
    (http-server:register-protocol-driver! r 
     "h2" *http-server:http2-driver* (make-http2-config))
    r))

(define (make-http-server port handler
	  :key (config (make-http-server-config))
	       (protocol-registry (make-http-server:default-protocol-registry))
	       (upgrade-registry (make-http-server:upgrade-registry)))
  (define app-handler
    (let ((cache (http-server-config-cache config)))
      (if (http-server:cache? cache)
          (http-server:make-cache-middleware cache handler)
          handler)))

  (define max-drain (slot-ref config 'max-drain))
  (define select-delay (slot-ref config 'select-delay))
  (define read-size (http-server-config-read-size config))
  (define (socket-handler server socket)
    (let ((conn (get-state server socket app-handler)))
      (let loop ((drain-count 0))
        (let ((chunk (socket-recv socket read-size)))
          (cond ((or (not chunk) (zero? (bytevector-length chunk)))
		 (http-server:close-connection! conn))
		((http-server:http-connection? conn)
                 (unless (serve-state! server socket conn chunk)
                   (when (and (http-server:connection-open? server socket)
                              (< drain-count max-drain)
			      (socket-ready? socket 'read select-delay))
                     (loop (+ drain-count 1)))))
		((http-server:custom-connection? conn)
                 (unless (http-server:connection-process! conn chunk)
                   (http-server:close-connection! conn)))
		(else (http-server:close-connection! conn)))))))
  (for-each (lambda (o)
	      (http-server:upgrade-registry-register! upgrade-registry o))
	    (http-server-config-upgrades config))
  (make-simple-server port socket-handler
		      :server-class <http-server>
		      :config config
		      :protocol-registry protocol-registry
		      :upgrade-registry upgrade-registry
		      :app-handler app-handler))

(define (http-server:register-upgrade! server upgrade)
  (http-server:upgrade-registry-register!
   (http-server-upgrade-registry server) upgrade))

;; internal
(define (make-server-http-connection server socket app-handler)
  (let* ((protocol-registry (slot-ref server 'protocol-registry))
         (driver (http-server:select-protocol-driver protocol-registry socket)))
    (http-server:protocol-driver-connect! driver server socket app-handler)))

(define (get-state server socket app-handler)
  (define lock (slot-ref server 'lock))
  (define states (slot-ref server 'states))

  (mutex-lock! lock)
  (let ((state (hashtable-ref states socket #f)))
    (if state
        (begin
          (mutex-unlock! lock)
          state)
        (let ((new-state (make-server-http-connection server socket app-handler)))
          (hashtable-set! states socket new-state)
          (mutex-unlock! lock)
          new-state))))

(define (set-state! server socket conn)
  (define lock (slot-ref server 'lock))
  (define states (slot-ref server 'states))

  (mutex-lock! lock)
  (hashtable-set! states socket conn)
  (mutex-unlock! lock))

(define (serve-state! server socket conn chunk)
  (let ((r (http-server:http-connection-feed! conn chunk)))
    ;; if the feed returned a new connection, then the
    ;; upgrade happened, so update the state.
    (when (and (http-server:connection? r) (not (eq? r conn)))
      (set-state! server socket r))
    (and (boolean? r)
	 r
	 (http-server:close-connection! conn)
	 #t)))

)



