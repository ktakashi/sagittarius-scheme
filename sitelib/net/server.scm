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
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius socket)
	    (sagittarius object)
	    (rename (srfi :1) (alist-cons acons))
	    ;; (srfi :18)
	    (sagittarius threads) ;; need thread-interrupt!
	    (srfi :26)
	    (rfc tls))

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
     (max-retry     :init-keyword :max-retry     :init-value 10)
     ;; enabling this creates 2 server socket for both IPv4 and IPv6
     ;; if EADDRINUSE is raised then only IPv6 is created.
     ;; NOTE: I don't have such environment yet...
     (use-ipv6?     :init-keyword :use-ipv6?     :init-value #f)
     ;; For TLS socket
     (secure?       :init-keyword :secure?       :init-value #f)
     (certificates  :init-keyword :certificates  :init-value '())
     (private-key   :init-keyword :private-key   :init-value #f)
     ;; non blocking (default #f for backward compatibility)
     (non-blocking? :init-keyword :non-blocking? :init-value #f)))
  (define (server-config? o) (is-a? o <server-config>))

  (define-class <simple-server> ()
    ((server-sockets :init-keyword :server-sockets :init-value #f)
     (server-threads :init-keyword :server-threads)
     (stopper-socket :init-keyword :stopper-socket :init-value #f)
     (stopper-thread :init-keyword :stopper-thread :init-value #f)
     (config         :init-keyword :config
		     :reader server-config)
     (lock           :init-form (make-mutex))
     (stop-lock      :init-form (make-mutex))
     (stop-waiter    :init-form (make-condition-variable))
     ;; private slot not to use thread-terminate!
     (stop-request   :init-value #f)
     ;; will be set
     server-stopper
     (port           :init-keyword :port)
     (dispatch       :init-keyword :dispatch)))
  (define (server? o) (is-a? o <simple-server>))
  (define (server-stopped? server) (not (~ server 'server-sockets)))

  (define (make-server-config . opt) (apply make <server-config> opt))
  (define (make-simple-server port handler
			      :key (server-class <simple-server>)
				   ;; must have default config
			           (config (make-server-config))
			      :allow-other-keys rest)
    (define dispatch
      (if (~ config 'non-blocking?)
	  (let* ((num-threads (~ config 'max-thread))
		 (thread-pool (make-thread-pool num-threads))
		 (socket-pool (make-vector num-threads))
		 (mutexes (make-vector num-threads))
		 ;; silly
		 (servers (make-vector num-threads))
		 (thread-ids (make-vector num-threads))
		 (cvs (make-vector num-threads)))
	    ;; prepare executor
	    (do ((i 0 (+ i 1)))
		((= i num-threads))
	      (vector-set! socket-pool i '())
	      (let ((mutex (make-mutex))
		    (cv (make-condition-variable)))
		(vector-set! mutexes i mutex)
		(vector-set! cvs i cv)
		;; keep id
		(vector-set! thread-ids i
		 (thread-pool-push-task! thread-pool
		  (lambda ()
		    (let loop ()
		      ;; unfortunately this is needed for Cygwin.
		      (mutex-lock! mutex)
		      (if (null? (vector-ref socket-pool i))
			  (mutex-unlock! mutex cv)
			  (mutex-unlock! mutex))
		      (let ((sockets 
			     (apply socket-read-select #f
				    (vector-ref socket-pool i)))
			    (server (vector-ref servers i)))
			;; we don't want to get thread-interrupt! here
			(mutex-lock! mutex)
			(for-each 
			 (lambda (socket) 
			   (guard (e ((~ config 'exception-handler)
				      ;; let them handle it
				      ((~ config 'exception-handler)
				       server socket e))
				     ;; if exception-handler is not there
				     ;; close the socket.
				     (else (socket-shutdown socket SHUT_RDWR)
					   (socket-close socket)))
			     (handler server socket)))
			 sockets)
			;; remove closed sockets
			(let ((closed (filter socket-closed? 
					      (vector-ref socket-pool i)))
			      (inactive (apply socket-write-select 0
					       (filter 
						(lambda (s)
						  (not (socket-closed? s)))
						(vector-ref socket-pool i)))))
			  (unless (null? closed)
			    (vector-set! socket-pool i
					 (lset-intersection eq?
					   (lset-difference eq? 
					    (vector-ref socket-pool i)
					    closed)
					   inactive)))
			  (mutex-unlock! mutex)
			  (loop)))))))))
	    ;; non blocking requires special coding rule.
	    ;;  1. handler must always return even the socket is still
	    ;;     active
	    ;;  2. handler must close the socket when it's not needed.
	    (lambda (server socket)
	      (define (find-min)
		(if (null? (vector-ref socket-pool 0))
		    0 ;; shortcut
		    (let loop ((i 1) 
			       (index 0)
			       (pb-len (length (vector-ref socket-pool 0))))
		      (let ((pool-a (vector-ref socket-pool i)))
			(cond ((= i num-threads) index)
			      ((null? pool-a) i) ;; shortcut
			      ((< (length pool-a) pb-len)
			       (loop (+ i 1) i (length pool-a)))
			      (else (loop (+ i 1) index pb-len)))))))
	      (let ((index (find-min)))
		(mutex-lock! (vector-ref mutexes index))
		(let ((sockets (vector-ref socket-pool index)))
		  (vector-set! socket-pool index (cons socket sockets))
		  (vector-set! servers index server) ;; it's always the same!
		  ;; notify it
		  (condition-variable-broadcast! (vector-ref cvs index))
		  (thread-interrupt!
		   (thread-pool-thread thread-pool 
				       (vector-ref thread-ids index)))
		  (mutex-unlock! (vector-ref mutexes index))))))
	  ;; normal one. 
	  (let ((executor (and (> (~ config 'max-thread) 1)
			       (make-thread-pool-executor 
				(~ config 'max-thread)
				(wait-finishing-handler 
				 (~ config 'max-retry))))))
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
		  (handle socket))))))
    (apply make server-class :dispatch dispatch :config config :port port rest))

  (define (initialise-server! server)
    (define config (~ server 'config))
    (define dispatch (~ server 'dispatch))
    (define port (~ server 'port))

    (define (make-socket ai-family)
      (if (and (~ config 'secure?)
	       (not (null? (~ config 'certificates))))
	  ;; For now no private key, it's simple server
	  ;; anyway
	  (make-server-tls-socket port (~ config 'certificates) 
				  :private-key (~ config 'private-key)
				  ai-family)
	  (make-server-socket port ai-family)))
    (let* ((ai-families (if (~ config 'use-ipv6?) 
			    `(,AF_INET6 ,AF_INET)
			    `(,AF_INET)))
	   ;; hope all platform accepts dual when IPv6 is enabled
	   (socket&ais (fold-left (lambda (s ai-family)
				    (guard (e (else s))
				      (acons (make-socket ai-family) ai-family
					     s)))
				  '() ai-families))
	   (sockets (map car socket&ais)))
      (define server-threads
	(map (lambda (socket)
	       (make-thread
		(lambda ()
		  (define stop? #f)
		  (thread-specific-set! (current-thread) 'done)
		  (let loop ()
		    (guard (e (else
			       (when (~ config 'exception-handler)
				 ((~ config 'exception-handler) server #f e))))
		      (let ((client-socket (socket-accept socket)))
			(cond ((~ server 'stop-request)
			       (close-socket client-socket)
			       (close-socket socket)
			       (set! stop? #t))
			      (else
			       (dispatch server client-socket)))))
		    (unless stop? (loop))))))
	     sockets))
      (define (stop-server)
	(set! (~ server 'stop-request) #t)
	(for-each (lambda (sock&ai)
		    (define (try ai-family)
		      (close-socket
		       (if (and (~ config 'secure?)
				(not (null? (~ config 'certificates))))
			   (make-client-tls-socket "localhost" port ai-family)
			   (make-client-socket "localhost" port ai-family))))
		    ;; At least on Linux, AF_INET6 can create a server
		    ;; socket but client is not allowed. To avoid waiting
		    ;; forever, we need to try both IPv6 and IPv4
		    (guard (e (else (try AF_INET))) ;; IPv4
		      (try (cdr sock&ai))))
		  socket&ais))
      (when (null? sockets)
	(error 'make-simple-server "failed to create server sockets" port))

      (set! (~ server 'server-sockets) sockets)
      (set! (~ server 'server-threads) server-threads)
      (set! (~ server 'server-stopper) stop-server)
      (when (~ config 'shutdown-port)
	(set! (~ server 'stopper-thread)
	      (make-thread 
	       (lambda ()
		 (define stop-socket (make-server-socket 
				      (~ config 'shutdown-port)))
		 (set! (~ server 'stopper-socket) stop-socket)
		 ;; lock it here
		 (mutex-lock! (~ server 'stop-lock))
		 (let loop ((sock (socket-accept stop-socket)))
		   ;; to avoid passing #f to socket close
		   (mutex-lock! (~ server 'lock))
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
		       (set! (~ server 'server-sockets) #f)
		       (condition-variable-broadcast! (~ server 'stop-waiter))
		       (mutex-unlock! (~ server 'stop-lock))))
		   (mutex-unlock! (~ server 'lock))
		   (socket-shutdown sock SHUT_RDWR)
		   (socket-close sock)
		   (if (server-stopped? server)
		       (socket-close stop-socket)
		       (loop (socket-accept stop-socket))))))))
      server))


  ;; wait until the server started
  (define (server-wait! server)
    (let loop ()
      ;; kinda awkward...
      (unless (for-all (lambda (t) (eq? (thread-specific t) 'done))
		       (~ server 'server-threads))
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))

  ;; default do nothing
  (define-method on-server-start! ((s <simple-server>) . ignore))
  (define-method on-server-stop! ((s <simple-server>) . ignore))

  (define (server-start! server :key (background #f)
			 :rest opts)
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))

    (if (~ server 'server-sockets) 
	(assertion-violation 'start-server! "server is already started" server)
	(initialise-server! server))
    (when (~ server 'stopper-thread)
      (thread-start! (~ server 'stopper-thread)))
    ;; pass all keyword arguments
    (apply on-server-start! server opts)
    (guard (e ((terminated-thread-exception? e) #t)
	      (else (raise e)))
      (let ((threads (map thread-start! (~ server 'server-threads))))
	(if background
	    (server-wait! server)
	    (map thread-join! threads)))))

  (define (server-stop! server . opt)
    (unless (server? server)
      (assertion-violation 'start-server! "server object required" server))
    (unless (server-stopped? server)
      (let ((ohandler (~ server 'config 'shutdown-handler)))
	(set! (~ server 'config 'shutdown-handler) default-shutdown-handler)
	(if (~ server 'stopper-thread)
	    ;; we need to stop the shutdown thread as well
	    (close-socket
	     (make-client-socket "localhost" (~ server 'config 'shutdown-port)))
	    ((~ server 'server-stopper)))
	(set! (~ server 'config 'shutdown-handler) ohandler))
      (map thread-join! (~ server 'server-threads))
      (mutex-lock! (~ server 'lock))
      (when (~ server 'server-sockets)
	(map close-socket (~ server 'server-sockets)))
      ;; should this be here?
      (apply on-server-stop! server opt)
      (set! (~ server 'server-sockets) #f)
      (mutex-unlock! (~ server 'lock))))

  (define (wait-server-stop! server :optional (timeout #f))
    (or (server-stopped? server)
	;; we don't have to lock again
	(mutex-unlock! (~ server 'stop-lock) (~ server 'stop-waiter)
		       timeout)))
)
      
       
