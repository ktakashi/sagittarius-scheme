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
    (export make-http-server

	    http-server-config?
	    http-server:register-upgrade!
	    make-http-server-config
	    http-server-config-max-header-bytes
	    http-server-config-max-body-bytes
	    http-server-config-max-pipelined-requests
	    http-server-config-max-requests-per-connection
	    http-server-config-read-size
	    http-server-config-cache
	    http-server-config-http2?
	    http-server-config-http2-cleartext?
	    http-server-config-http2-enable-push?

            http-server:upgrade-registry?
            make-http-server:upgrade-registry
	    http-server:upgrade-registry-register!
	    http-server:upgrade-registry-registered?

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

(define (make-http-server port handler
	  :key (config (make-http-server-config))
	       (upgrade-registry (make-http-server:upgrade-registry)))
  (define app-handler
    (let ((cache (http-server-config-cache config)))
      (if (http-server:cache? cache)
          (http-server:make-cache-middleware cache handler)
          handler)))

  (define registry
    (let ((r (make-http-server:protocol-registry *http-server:http1-driver*)))
      (http-server:register-protocol-driver! r
	"http/1.1" *http-server:http1-driver*)
      (when (http-server-config-http2? config)
        (http-server:register-protocol-driver! r "h2" *http-server:http2-driver*))
      r))

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
		      :registry registry
		      :upgrade-registry upgrade-registry
		      :app-handler app-handler))

(define (http-server:register-upgrade! server upgrade)
  (http-server:upgrade-registry-register!
   (http-server-upgrade-registry server) upgrade))

;; internal
(define (make-server-http-connection server socket app-handler)
  (let* ((protocol-registry (slot-ref server 'registry))
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
  (define app-handler (slot-ref server 'app-handler))
  (define config (slot-ref server 'config))
  (define max-header-bytes (http-server-config-max-header-bytes config))
  (define max-body-bytes (http-server-config-max-body-bytes config))
  (define max-requests-per-connection
    (http-server-config-max-requests-per-connection config))
  (define max-pipelined-requests
    (http-server-config-max-pipelined-requests config))

  (let ((r (http-server:http-connection-feed!
            conn
            chunk
            :max-header-bytes max-header-bytes
            :max-body-bytes max-body-bytes
	    :max-requests-per-connection max-requests-per-connection
	    :max-pipelined-requests max-pipelined-requests)))
    ;; `http-server:http-connection-feed!` may return a new connection
    ;; (e.g. protocol upgrade) or a status boolean. Keep existing state
    ;; unless a connection object is returned.
    (when (http-server:connection? r)
      (set-state! server socket r))
    (and (boolean? r)
	 r
	 (http-server:close-connection! conn)
	 #t)))

)



