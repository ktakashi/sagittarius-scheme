;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/server.scm - Simple server framework.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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
(library (net server)
    (export make-simple-server
	    make-server-config
	    server?
	    server-config? server-config
	    server-start! on-server-start!
	    ;; well for multithreading?
	    server-stop!  on-server-stop! 
	    
	    server-stopped? wait-server-stop!

	    ;; for extension
	    <simple-server>
	    <server-config>)
    (import (rnrs)
	    (util concurrent)
	    (clos user)
	    (sagittarius control)
	    (sagittarius socket)
	    (sagittarius object)
	    (srfi :18)
	    (srfi :26)
	    (rfc tls))

  ;; connecting this would shut it donw
  (define (default-shutdown-handler server socket) #t)

  (define-class <server-config> ()
    ((shutdown-port :init-keyword :shutdown-port :init-value #f)
     (shutdown-handler :init-keyword :shutdown-handler
		       :init-value default-shutdown-handler)
     (exception-handler :init-keyword :exception-handler
			:init-value #f)
     (max-thread    :init-keyword :max-thread    :init-value 1)
     (max-retry     :init-keyword :max-retry     :init-value 10)
     ;; enabling this creates 2 server socket for both IPv4 and IPv6
     ;; if EADDRINUSE is raised then only IPv6 is created.
     ;; NOTE: I don't have such environment yet...
     (use-ipv6?     :init-keyword :use-ipv6?     :init-value #f)
     ;; For TLS socket
     (secure?       :init-keyword :secure?       :init-value #f)
     (certificates  :init-keyword :certificates  :init-value '())))
  (define (server-config? o) (is-a? o <server-config>))

  (define-class <simple-server> ()
    ((server-sockets :init-keyword :server-sockets)
     (server-threads :init-keyword :server-threads)
     (stopper-socket :init-keyword :stopper-socket :init-value #f)
     (stopper-thread :init-keyword :stopper-thread :init-value #f)
     (stopped?       :init-value #f :reader server-stopped?)
     (config         :init-keyword :config
		     :reader server-config)
     (stop-lock      :init-form (make-mutex))
     (stop-waiter    :init-form (make-condition-variable))
     ;; private slot not to use thread-terminate!
     (stop-request   :init-value #f)
     ;; will be set
     server-stopper))
  (define (server? o) (is-a? o <simple-server>))

  (define (make-server-config . opt) (apply make <server-config> opt))
  (define (make-simple-server port handler
			      :key (server-class <simple-server>)
				   ;; must have default config
			           (config (make-server-config))
			      :allow-other-keys rest)
    (define dispatch
      (let ((executor (and (> (~ config 'max-thread) 1)
			   (make-thread-pool-executor (~ config 'max-thread)
			    (wait-finishing-handler (~ config 'max-retry))))))
	(lambda (server socket)
	  (define (handle socket)
	    (guard (e (else 
		       (when (~ config 'exception-handler)
			 ((~ config 'exception-handler) server socket e))
		       (socket-close socket)
		       #t))
	      (call-with-socket socket 
		(lambda (sock) (handler server sock)))))
	  ;; ignore error
	  (if executor
	      (let ((f (future (class <executor-future>) (handle socket))))
		(execute-future! executor f))
	      (handle socket)))))
    (define stop-socket (and (~ config 'shutdown-port)
			     (make-server-socket (~ config 'shutdown-port))))
    (define (make-socket ai-family)
      (if (and (~ config 'secure?)
	       (not (null? (~ config 'certificates))))
	  ;; For now no private key, it's simple server
	  ;; anyway
	  (make-server-tls-socket port (~ config 'certificates) ai-family)
	  (make-server-socket port ai-family)))
    (let* ((ai-families (if (~ config 'use-ipv6?) 
			    `(,AF_INET6 ,AF_INET)
			    `(,AF_INET)))
	   ;; hope all platform accepts dual when IPv6 is enabled
	   (sockets (fold-right (lambda (ai-family s)
				  (guard (e (else s))
				    (cons (make-socket ai-family) s)))
				'() ai-families)))
      (when (null? sockets)
	(error 'make-simple-server "failed to create server sockets" port))
      (let ((server (apply make server-class
		      :server-sockets sockets :stopper-socket stop-socket
		      :config config
		      rest)))
	(define server-threads
	  (map (lambda (socket)
		 (make-thread
		  (lambda ()
		    (let loop ((client-socket (socket-accept socket)))
		      (cond ((~ server 'stop-request) (socket-close socket))
			    (else
			     (dispatch server client-socket)
			     (loop (socket-accept socket)))))))) sockets))
	(define (stop-server)
	  (set! (~ server 'stop-request) #t)
	  (for-each (lambda (ai-family)
		      (if (and (~ config 'secure?)
			       (not (null? (~ config 'certificates))))
			  (make-client-tls-socket "localhost" port ai-family)
			  (make-client-socket "localhost" port ai-family)))
		    ai-families))
	(set! (~ server 'server-threads) server-threads)
	(set! (~ server 'server-stopper) stop-server)
	(when stop-socket
	  (set! (~ server 'stopper-thread)
		(make-thread 
		 (lambda ()
		   ;; lock it here
		   (mutex-lock! (~ server 'stop-lock))
		   (let loop ((sock (socket-accept stop-socket)))
		     ;; ignore all errors
		     (guard (e (else #t))
		       (when ((~ config 'shutdown-handler) server sock)
			 ;; access to stop
			 ;; this works because accepting thread is only one
			 ;; so once it's accepted, then the server socket
			 ;; won't call accept.
			 ;; FIXME ugly...
			 (stop-server)
			 (for-each thread-join! server-threads)
			 (set! (~ server 'stopped?) #t)
			 (condition-variable-broadcast! (~ server 'stop-waiter))
			 (mutex-unlock! (~ server 'stop-lock))))
		     (socket-close sock)
		     (if (~ server 'stopped?)
			 (socket-close stop-socket)
			 (loop (socket-accept stop-socket))))))))
	server)))
  
  ;; default do nothing
  (define-method on-server-start! ((s <simple-server>) . ignore))
  (define-method on-server-stop! ((s <simple-server>) . ignore))

  (define (server-start! server :key (background #f)
			 :rest opts)
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))
    (when (~ server 'stopper-thread)
      (thread-start! (~ server 'stopper-thread)))
    ;; pass all keyword arguments
    (apply on-server-start! server opts)
    (guard (e ((terminated-thread-exception? e) #t)
	      (else (raise e)))
      (let ((threads (map thread-start! (~ server 'server-threads))))
	(unless background
	  (map thread-join! threads)))))

  (define (server-stop! server . opt)
    (define (close-socket socket)
      ;; we don't care if socket sending failed or not...
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket))
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))
    (unless (~ server 'stopped?)
      (set! (~ server 'config 'shutdown-handler) default-shutdown-handler)
      (if (~ server 'stopper-thread)
	  ;; we need to stop the shutdown thread as well
	  (make-client-socket "localhost" (~ server 'config 'shutdown-port))
	  ((~ server 'server-stopper)))
      (map thread-join! (~ server 'server-threads))
      (map close-socket (~ server 'server-sockets))
      ;; should this be here?
      (apply on-server-stop! server opt)
      (set! (~ server 'stopped?) #t)))

  (define (wait-server-stop! server :optional (timeout #f))
    (or (~ server 'stopped?)
	;; we don't have to lock again
	(mutex-unlock! (~ server 'stop-lock) (~ server 'stop-waiter)
		       timeout)))
)
      
       
