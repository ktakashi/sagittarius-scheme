;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/websocket.scm - RFC 6455 Websocket
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

;; Websocket client library. Server library may come in (rfc websocket server)
;; but not promised.
(library (rfc websocket)
  (export ;; User level APIs
	  make-websocket
	  websocket?
	  websocket-open
	  websocket-close
	  websocket-send
	  websocket-ping
	  websocket-on-text-message
	  websocket-on-binary-message
	  websocket-on-open
	  websocket-on-error
	  websocket-on-close
	  
	  ;; Low APIs
	  make-websocket-connection
	  websocket-connection?
	  websocket-connection-handshake!
	  
	  websocket-send-text
	  websocket-send-binary
	  websocket-send-close
	  websocket-send-ping
	  websocket-send-pong

	  websocket-receive
	  websocket-compose-close-status
	  websocket-parse-close-status
	  
	  +websocket-text-frame+
	  +websocket-binary-frame+
	  +websocket-close-frame+
	  +websocket-ping-frame+
	  +websocket-pong-frame+
	  
	  ;; conditions
	  websocket-error?
	  websocket-engine-error?
	  websocket-engine-not-found-error?
	  websocket-error-engine
	  websocket-error-reason
	  websocket-closed-error?
	  websocket-error-status
	  websocket-error-message

	  websocket-pong-error?
	  websocket-error-pong-data

	  websocket-engine-scheme-error
	  websocket-engine-scheme-error?
	  websocket-error-scheme
	  websocket-engine-connection-error?
	  websocket-error-host
	  websocket-error-port
	  
	  *websocket-mask-data?*
	  )
  (import (rnrs)
	  (srfi :18)
	  (rfc websocket conditions)
	  (rfc websocket connection)
	  (rfc websocket messages)
	  (util concurrent shared-queue))

(define-record-type websocket
  (fields protocols
	  extensions
	  connection
	  dispatchers
	  (mutable thread)
	  mutex
	  pong-channel)
  (protocol
   (lambda (p)
     (lambda (uri :key (protocols '()) (extensions '()) (engine 'http))
       (p protocols extensions
	  (make-websocket-connection uri engine)
	  (make-eq-hashtable)
	  #f
	  (make-mutex)
	  (make-shared-queue 1))))))
(define-condition-type &websocket-pong &websocket
  make-websocket-pong-error websocket-pong-error?
  (pong-data websocket-error-pong-data))
(define-condition-type &websocket-close-timeout &websocket
  make-websocket-close-timeout-error websocket-close-timeout-error)

(define (invoke-event websocket event . opt)
  (define dispatchers (websocket-dispatchers websocket))
  (cond ((hashtable-ref dispatchers event #f) =>
	 (lambda (handler) (apply handler websocket opt))))
  websocket)

;; if it's &websocket, then let on-error handle it.
;; others just go though.
(define-syntax with-error-handling
  (syntax-rules ()
    ((_ websocket exprs ...)
     ;; engine error can not be recoverable so re-raise.
     (guard (e ((websocket-engine-error? e)
		(invoke-event websocket 'error e)
		(raise e))
	       ((websocket-error? e)
		(invoke-event websocket 'error e)
		websocket))
       exprs ...))))

(define-syntax define-websocket
  (syntax-rules ()
    ((_ (name websocket . args) exprs ...)
     (define (name websocket . args)
       (with-error-handling websocket exprs ...)))))

(define (start-dispatch-thread websocket)
  (define (dispatch)
    (define conn (websocket-connection websocket))
    (define finish? #f)
    (let restart ()
      (guard (e ((websocket-closed-error? e) (raise e))
		((websocket-error? e) (invoke-event websocket 'error e)))
	(let loop ()
	  (let-values (((opcode data) (websocket-receive conn)))
	    (cond ((eqv? opcode +websocket-close-frame+)
		   (set! finish? #t))
		  ((eqv? opcode +websocket-text-frame+)
		   (invoke-event websocket 'text data)
		   (loop))
		  ((eqv? opcode +websocket-binary-frame+)
		   (invoke-event websocket 'binary data)
		   (loop))
		  ((eqv? opcode +websocket-pong-frame+)
		   (shared-queue-put!
		    (websocket-pong-channel websocket) data)
		   (loop))
		  (else
		   ;; TODO should we raise an error?
		   (websocket-send-close conn
		    (websocket-compose-close-status 1002) #f)
		   (loop))))))
      (unless finish? (restart))))
  (let ((t (make-thread dispatch)))
    (websocket-thread-set! websocket t)
    (thread-start! t)))

(define-websocket (websocket-open websocket)
  (websocket-connection-handshake! (websocket-connection websocket)
				   (websocket-protocols websocket)
				   (websocket-extensions websocket))
  (start-dispatch-thread websocket)
  (invoke-event websocket 'open))

(define-websocket (websocket-close websocket
				   :key (status #f) (message "") (timeout #f))
  (define conn (websocket-connection websocket))
  (let ((data (cond (status (websocket-compose-close-status status message))
		    (else #vu8()))))
    ;; we don't wait, let dispatch thread handle it
    (websocket-send-close conn data #f)
    (guard (e ((uncaught-exception? e)
	       ;; make sure it's closed... should we?
	       (websocket-connection-close! conn)
	       (websocket-thread-set! websocket #f)
	       (raise (uncaught-exception-reason e)))
	      ((join-timeout-exception? e)
	       ;; close it first, then terminate thread
	       (websocket-connection-close! conn)
	       ;; sorry then die
	       (thread-terminate! (websocket-thread websocket))
	       (websocket-thread-set! websocket #f)
	       (raise (condition (make-websocket-close-timeout-error)
				 (make-who-condition 'websocket-close)
				 (make-message-condition "timeout!")))))
      (thread-join! (websocket-thread websocket) timeout)
      (websocket-thread-set! websocket #f)
      ;; close connection. NB: dispatcher thread doesn't close
      (websocket-connection-close! conn)))
  (invoke-event websocket 'close))

(define-websocket (websocket-send websocket data . opt)
  (define conn (websocket-connection websocket))
  (cond ((string? data) (apply websocket-send-text conn data opt))
	((bytevector? data) (apply websocket-send-binary conn data opt))
	(else (assertion-violation 'websocket-send "invalid data" data)))
  websocket)

(define-websocket (websocket-ping websocket data . opt)
  (define mutex (websocket-mutex websocket))
  (let try ()
    (mutex-lock! mutex)
    (websocket-send-ping (websocket-connection websocket) data)
    (let ((r (apply shared-queue-get! (websocket-pong-channel websocket) opt)))
      (mutex-unlock! mutex)
      ;; Should we raise an exception?
      ;; if so, what kind of exception? &websocket would be caught
      (unless (and (bytevector? r) (bytevector=? data r))
	(raise (condition (make-websocket-pong-error r)
			  (make-who-condition 'websocket-ping)
			  (make-message-condition "unknown pong"))))
      websocket)))

(define (set-event-handler websocket event handler)
  (define dispatchers (websocket-dispatchers websocket))
  (hashtable-set! dispatchers event handler)
  websocket)

(define (websocket-on-text-message websocket handler)
  (set-event-handler websocket 'text handler))
(define (websocket-on-binary-message websocket handler)
  (set-event-handler websocket 'binary handler))
(define (websocket-on-open websocket handler)
  (set-event-handler websocket 'open handler))
(define (websocket-on-error websocket handler)
  (set-event-handler websocket 'error handler))
(define (websocket-on-close websocket handler)
  (set-event-handler websocket 'close handler))

)				   
