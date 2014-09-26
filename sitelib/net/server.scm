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
	    start-server!
	    stop-server! ;; well for multithreading?
	    <server-config>)
    (import (rnrs)
	    (util concurrent)
	    (clos user)
	    (sagittarius socket)
	    (sagittarius object)
	    (srfi :18)
	    (rfc tls))

  ;; connecting this would shut it donw
  (define (default-shutdown-handler socket) #t)

  (define-class <server-config> ()
    ((shutdown-port :init-keyword :shutdown-port :init-value #f)
     (shutdown-handler :init-keyword :shutdown-handler
		       :init-value default-shutdown-handler)
     (exception-handler :init-keyword :exception-handler
			:init-value #f)
     (max-thread    :init-keyword :max-thread    :init-value 1)
     ;; For TLS socket
     (secure?       :init-keyword :secure?       :init-value #f)
     (certificates  :init-keyword :certificates  :init-value '())))

  (define-class <simple-server> ()
    ((server-thread :init-keyword :server-thread)
     (stopper-thread :init-keyword :stopper-thread)))

  (define (make-server-config . opt) (apply make <server-config> opt))
  (define (make-simple-server port handler 
			      :optional (config (make-server-config)))
    (define stop? #f)
    (define dispatch
      (let ((executor (and (> (~ config 'max-thread) 1)
			   (make-executor (~ config 'max-thread)
					  (waiting-next-handler 10)))))
	(lambda (socket)
	  (define (handle socket)
	    (guard (e (else 
		       (when (~ config 'exception-handler)
			 ((~ config 'exception-handler) e socket))
		       (socket-close socket)
		       #t))
	      (call-with-socket socket handler)))
	  ;; ignore error
	  (if executor
	      (let ((f (future (handle socket))))
		(execute-future! executor f))
	      (handle socket)))))
    (define stop-socket (and (~ config 'shutdown-port)
			     (make-server-socket (~ config 'shutdown-port))))
    (let ((socket (if (and (~ config 'secure?)
			   (not (null? (~ config 'certificates))))
		      ;; For now no private key, it's simple server
		      ;; anyway
		      (make-server-tls-socket port (~ config 'certificates))
		      (make-server-socket port))))
      (define server-thread 
	(make-thread
	 (lambda ()
	   (let loop ((client-socket (socket-accept socket)))
	     (dispatch client-socket)
	     (loop (socket-accept socket))))))
      
      (make <simple-server> :server-thread server-thread
	    :stopper-thread
	    (if stop-socket
		(make-thread 
		 (lambda ()
		   (let ((sock (socket-accept stop-socket)))
		     ;; ignore all errors
		     (guard (e (else #t))
		       (set! stop? ((~ config 'shutdown-handler) sock))
		       (when stop? (thread-terminate! server-thread)))
		     (socket-close sock))))
		#f))))
  
  (define (start-server! server)
    (when (~ server 'stopper-thread)
      (thread-start! (~ server 'stopper-thread)))
    (guard (e ((terminated-thread-exception? e) #t)
	      (else (raise e)))
      (thread-join! (thread-start! (~ server 'server-thread)))))

  (define (stop-server! server)
    (thread-terminate! (~ server 'server-thread))
    (when (~ server 'stopper-thread)
      (thread-terminate! (~ server 'stopper-thread))))

)
      
       