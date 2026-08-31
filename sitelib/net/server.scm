;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/server.scm - Simple server framework.
;;;  
;;;   Copyright (c) 2010-2017  Takashi Kato  <ktakashi@ymail.com>
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

;; super simple server framework
#!nounbound
(library (net server)
    (export make-simple-server
	    make-server-config
	    server? server-port server-shutdown-port
	    server-config? server-config server-context
	    server-start! on-server-start!
	    ;; well for multithreading?
	    server-stop!  on-server-stop! 
	    
	    server-stopped? wait-server-stop!

	    ;; for socket detaching
	    server-detach-socket!
	    
	    server-status
	    server-status?
	    report-server-status
	    server-status-target-server
	    server-status-thread-count
	    server-status-thread-statuses
	    thread-status-thread-info
	    thread-status-thread-id
	    thread-status-active-socket-count
	    ;; for extension
	    <simple-server>
	    <server-config>)
    (import (rnrs)
	    (util concurrent)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius socket)
	    (sagittarius object)
	    (sagittarius threads) ;; need thread-interrupt!
	    (rename (srfi :1) (alist-cons acons))
	    (srfi :26)
	    (srfi :39)
	    (srfi :117)
	    (net socket)
	    (net server monitor))

  (define (close-socket socket)
    ;; we don't care if socket sending failed or not...
    (socket-shutdown socket SHUT_RDWR)
    (socket-close socket))

  ;; connecting this would shut it donw
  (define (default-shutdown-handler server socket) #t)

  (define-class <server-config> ()
    ((shutdown-port :init-keyword :shutdown-port :init-value #f)
     (shutdown-handler :init-keyword :shutdown-handler
		       :init-value default-shutdown-handler)
     (exception-handler :init-keyword :exception-handler
			:init-value #f)
     (max-thread    :init-keyword :max-thread    :init-value 1)
     ;; not used
     (max-retry     :init-keyword :max-retry     :init-value 10)
     ;; enabling this creates 2 server socket for both IPv4 and IPv6
     ;; if EADDRINUSE is raised then only IPv6 is created.
     ;; NOTE: I don't have such environment yet...
     (use-ipv6?     :init-keyword :use-ipv6?     :init-value #f)
     ;; For TLS socket
     (secure?       :init-keyword :secure?       :init-value #f)
     (certificates  :init-keyword :certificates  :init-value '())
     (private-key   :init-keyword :private-key   :init-value #f)
     ;; non blocking (not used)
     (non-blocking? :init-keyword :non-blocking? :init-value #f)
     ;; default give 100ms for client socket to finish when server
     ;; stop is called
     (grace-period :init-keyword :grace-period :init-value 100)))
  (define (server-config? o) (is-a? o <server-config>))
  
  (define (default-server-monitor)
    (error 'server-monitor "not supported"))
  (define-class <simple-server> ()
    ((server-sockets :init-keyword :server-sockets :init-value #f)
     (handler        :init-keyword :handler :init-keyword #f)
     (fork-join-pool :init-keyword :fork-join-pool)
     (stopper-socket :init-keyword :stopper-socket :init-value #f)
     (socket-selector :init-keyword :socket-selector :init-value #f)
     (selector-terminate :init-keyword :selector-terminate :init-value #f)
     (config         :init-keyword :config :reader server-config)
     ;; private slot not to use thread-terminate!
     (server-stopped :init-keyword :server-stopped)
     (server-stopped-put :init-keyword :server-stopped-put)
     (stop-request   :init-value #f)
     (port           :init-keyword :port)
     (running-port   :init-keyword :running-port :reader server-port)
     (shutdown-port  :init-value #f :reader server-shutdown-port)
     (context        :init-keyword :context :init-value #f
		     :reader server-context)
     (monitor        :reader server-monitor
		     :init-value default-server-monitor)))

  (define (server-status server) ((server-monitor server)))
  (define (server? o) (is-a? o <simple-server>))
  (define (server-stopped? server) (future-done? (~ server 'server-stopped)))

  (define (make-server-config . opt) (apply make <server-config> opt))

  (define (server-detach-socket! server socket)
    )
  
  (define (stop-server server)
    (define terminate (~ server 'selector-terminate))
    (set! (~ server 'stop-request) #t)
    (terminate)
    (fork-join-pool-wait-all! (~ server 'fork-join-pool)
			      (~ server 'config 'grace-period))
    (let ((socks (socket-selector-clear! (~ server 'socket-selector))))
      (for-each close-socket socks))
    (set! (~ server 'server-sockets) #f)
    ((~ server 'server-stopped-put) #t))
    
  (define (make-simple-server port handler
			      :key (server-class <simple-server>)
				   ;; must have default config
			           (config (make-server-config))
			      :allow-other-keys rest)
    (define-values (selector terminate) (make-socket-selector))
    (define-values (future put! cancel!) (make-piped-future))
    (define num-threads (~ config 'max-thread))
    (define fork-join-pool (make-fork-join-pool num-threads))

    (apply make server-class
	   :config config :port port
	   :handler handler
	   :fork-join-pool fork-join-pool
	   :socket-selector selector
	   :selector-terminate terminate
	   :running-port port
	   :server-stopped future
	   :server-stopped-put put!
	   rest))

  (define (initialise-server! server)
    (define selector (~ server 'socket-selector))
    (define config (~ server 'config))
    (define port (~ server 'port))
    (define pool (~ server 'fork-join-pool))
    (define option (config->socket-option config))
    (define handler (~ server 'handler))
    (define (handle-exception e socket)
      (cond ((~ config 'exception-handler) =>
	     (lambda (eh) (eh server socket e)))
	    (else (close-socket socket))))
    ;; accepted socket task
    (define (socket-task socket e retry)
      (if e
	  (handle-exception e socket)
	  (fork-join-pool-push-task! pool
	    (lambda ()
	      (guard (e (else (handle-exception e socket)))
		(handler server socket)
		(retry))))))

    (define (socket-dispatch sock e retry)
      (unless (~ server 'stop-request)
	(unless e
	  (let ((client-socket (socket-accept sock)))
	    (selector client-socket socket-task)))
	(retry)))
    (define (stop-process sock e retry)
      (cond ((socket-accept sock) =>
	     (lambda (client-sock)
	       (guard (e (else #t))
		 (cond (((~ config 'shutdown-handler) server client-sock)
			(stop-server server)
			(close-socket client-sock)
			(close-socket sock))
		       (else (close-socket client-sock)
			     (retry))))))
	    (else (retry))))

    (let ((sockets (if (tls-socket-options? option)
		       (make-server-tls-socket* port option)
		       (make-server-socket* port option))))
      (when (null? sockets)
	(error 'make-simple-server "failed to create server sockets" port))
      (when (or (not port) (equal? port "0"))
	(let ((si (socket-info (car sockets))))
	  (set! (~ server 'running-port)
		(number->string (socket-info-port si)))))
      (for-each (lambda (sock) (selector sock socket-dispatch)) sockets)
      (set! (~ server 'server-sockets) sockets)
      (when (~ config 'shutdown-port)
	(let* ((shutdown-port (~ config 'shutdown-port))
	       (stop-socket (make-server-socket shutdown-port)))
	  (set! (~ server 'stopper-socket) stop-socket)
	  (set! (~ server 'shutdown-port)
		(if (or (not shutdown-port) (equal? shutdown-port "0"))
		    (number->string
		     (socket-info-port (socket-info stop-socket)))
		    shutdown-port))
	  (selector stop-socket stop-process)))

      (set! (~ server 'monitor)
	    (make-non-blocking-server-monitor server pool selector))
      server))
  
  (define (config->socket-option config)
    (let ((ai-family (if (~ config 'use-ipv6?) AF_UNSPEC AF_INET)))
      (if (and (~ config 'secure?) (not (null? (~ config 'certificates))))
	  (server-tls-socket-options
	   (ai-family ai-family)
	   (certificates (~ config 'certificates))
	   (private-key (~ config 'private-key)))
	  (socket-options
	   (ai-family ai-family)))))

  ;; default do nothing
  (define-generic on-server-start!)
  (define-generic on-server-stop!)
  (define-method on-server-start! ((s <simple-server>) . ignore))
  (define-method on-server-stop! ((s <simple-server>) . ignore))

  (define (server-start! server :key (background #f)
			 :rest opts)
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))

    (if (~ server 'server-sockets) 
	(assertion-violation 'start-server! "server is already started" server)
	(initialise-server! server))
    ;; pass all keyword arguments
    (apply on-server-start! server opts)
    (guard (e ((terminated-thread-exception? e) #t)
	      (else (raise e)))
      (unless background
	;; for backward compatibility
	(thread-join! (current-thread)))))

  (define (server-stop! server . opt)
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))
    (unless (server-stopped? server)
      (let ((ohandler (~ server 'config 'shutdown-handler)))
	(set! (~ server 'config 'shutdown-handler) default-shutdown-handler)
	(stop-server server)
	(set! (~ server 'config 'shutdown-handler) ohandler))
      ;; should this be here?
      (apply on-server-stop! server opt)))

  (define (wait-server-stop! server :optional (timeout #f))
    (or (server-stopped? server)
	(future-get (~ server 'server-stopped) timeout)))
)
