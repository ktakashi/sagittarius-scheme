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
          make-http-server-config
          http-server-config-max-header-bytes
          http-server-config-max-body-bytes
          http-server-config-max-pipelined-requests
          http-server-config-max-requests-per-connection
          http-server-config-read-size
          http-server-config-cache

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
          http-server:response-cacheable?
          http-server:response-cacheable?-set!
          http-server:response-cache-ttl
          http-server:response-cache-ttl-set!
          http-server:response-header-ref
          http-server:response-header-ref*
          http-server:response-header-set!
          http-server:response-header-add!
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
          (net http-server http1)
	  (util bytevector))

(define-class <http-server-config> (<server-config>)
  ((max-header-bytes :init-keyword :max-header-bytes :init-value 65536
		     :reader http-server-config-max-header-bytes)
   (max-body-bytes :init-keyword :max-body-bytes :init-value 1048576
		   :reader http-server-config-max-body-bytes)
   (max-pipelined-requests :init-keyword :max-pipelined-requests :init-value 16
			   :reader http-server-config-max-pipelined-requests)
   (max-requests-per-connection 
    :init-keyword :max-requests-per-connection
    :init-value 100
    :reader http-server-config-max-requests-per-connection)
   (read-size :init-keyword :read-size :init-value 8192
	      :reader http-server-config-read-size)
   (cache :init-keyword :cache :init-form (make-http-server:memory-cache)
	  :reader http-server-config-cache)))
(define (make-http-server-config . opts)
  (apply make <http-server-config> opts))
(define (http-server-config? o) (is-a? o <http-server-config>))

(define-record-type connection-state
  (fields (mutable buffer)
          (mutable request-count)
          driver))

(define (make-error-response code message)
  (let ((res (make-http-server:response code)))
    (http-server:response-text! res message)
    (http-server:response-header-set! res "content-type"
				      "text/plain; charset=utf-8")
    res))

(define (normalize-handler-result result fallback)
  (if (http-server:response? result) result fallback))

(define (make-http-server port handler :key (config (make-http-server-config)))
  (define app-handler
    (let ((cache (http-server-config-cache config)))
      (if (http-server:cache? cache)
          (http-server:make-cache-middleware cache handler)
          handler)))

  (define registry
    (let* ((http1-driver
            (make-http-server:protocol-driver
             "http/1.1"
             (lambda (socket req res)
               (http-server:http1-write-response! socket req res))))
           (r (make-http-server:protocol-registry http1-driver)))
      (http-server:register-protocol-driver! r "http/1.1" http1-driver)
      r))

  (define states (make-eq-hashtable))
  (define lock (make-mutex))

  (define (close-connection! server socket)
    (mutex-lock! lock)
    (hashtable-delete! states socket)
    (mutex-unlock! lock)
    (server-detach-socket! server socket)
    (socket-close socket))

  (define (connection-open? socket)
    (mutex-lock! lock)
    (let ((alive (hashtable-ref states socket #f)))
      (mutex-unlock! lock)
      (and alive #t)))

  (define (get-state server socket)
    (mutex-lock! lock)
    (let ((state (hashtable-ref states socket #f)))
      (if state
          (begin
            (mutex-unlock! lock)
            state)
          (let* ((driver (http-server:select-protocol-driver registry socket))
                 (new-state (make-connection-state #vu8() 0 driver)))
            (hashtable-set! states socket new-state)
            (mutex-unlock! lock)
            new-state))))

  (define (remote-info socket)
    (guard (e (else #f))
      (socket-info socket)))

  (define (as-request req socket)
    (make-http-server:request
     (http-server:http1-request-method req)
     (http-server:http1-request-target req)
     (http-server:http1-request-path req)
     (http-server:http1-request-query req)
     (http-server:http1-request-version req)
     (http-server:http1-request-headers req)
     (http-server:http1-request-body req)
     (remote-info socket)
     '()))

  (define (serve-state! server socket state)
    (let loop ((served 0))
      (let-values (((kind a b remainder)
                    (http-server:http1-consume
                     (connection-state-buffer state)
                     :max-header-bytes (http-server-config-max-header-bytes config)
                     :max-body-bytes (http-server-config-max-body-bytes config))))
        (cond ((eq? kind 'need-more)
               (connection-state-buffer-set! state remainder)
               #f)
              ((eq? kind 'error)
               (let* ((code a)
                      (message b)
                      (dummy-req (make-http-server:http1-request
                                  'GET "/" "/" #f "HTTP/1.1"
                                  (make-http-server:headers)
                                  #vu8()))
                      (res (make-error-response code message)))
                 (http-server:http1-write-response! socket dummy-req res)
                 (close-connection! server socket)
                 #t))
              (else
               (connection-state-buffer-set! state remainder)
               (let* ((req (as-request a socket))
                      (res (make-http-server:response))
                      (result
                       (guard (e (else
                                  (let ((er (make-http-server:response 500)))
                                    (http-server:response-text!
                                     er
                                     "Unhandled application error")
                                    er)))
                         (normalize-handler-result (app-handler req res) res)))
                      (close? ((http-server:protocol-driver-serve!
                                (connection-state-driver state))
                               socket a result)))
                 (connection-state-request-count-set!
                  state
                  (+ 1 (connection-state-request-count state)))
                 (if (or close?
                         (>= (connection-state-request-count state)
                             (http-server-config-max-requests-per-connection config)))
                     (begin
                       (close-connection! server socket)
                       #t)
                     (if (and (< served (http-server-config-max-pipelined-requests config))
                              (> (bytevector-length (connection-state-buffer state)) 0))
                         (loop (+ served 1))
                         #f))))))))

  (define (socket-handler server socket)
    (let ((state (get-state server socket)))
      (let loop ((drain-count 0))
        (let ((chunk (socket-recv socket (http-server-config-read-size config))))
          (if (or (not chunk) (zero? (bytevector-length chunk)))
              (close-connection! server socket)
              (begin
                (connection-state-buffer-set!
                 state
                 (bytevector-append (connection-state-buffer state) chunk))
                (unless (serve-state! server socket state)
                  (when (and (connection-open? socket)
                             (< drain-count 8)
                             (pair? (socket-read-select 20 socket)))
                    (loop (+ drain-count 1))))))))))

  (make-simple-server port socket-handler :config config))
)
