;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/http2/state.scm - HTTP/2 server driver state
;;;
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
;;;

#!nounbound
(library (net http-server http2 state)
    (export <http2-config> http2-config? make-http2-config
	    http2-config-max-concurrent-streams
	    http2-config-enable-push?

	    http-server:http2-request?
	    make-http-server:http2-request
	    http-server:http2-request-stream-id
	    
	    http2-server-stream? make-http2-server-stream
	    http2-server-stream-id
	    http2-server-stream-method
	    http2-server-stream-target
	    http2-server-stream-path
	    http2-server-stream-query
	    http2-server-stream-headers
	    http2-server-stream-body-chunks http2-server-stream-body-chunks-set!
	    http2-server-stream-body-size http2-server-stream-body-size-set!
	    http2-server-stream-send-window http2-server-stream-send-window-set!
	    http2-server-stream-recv-window http2-server-stream-recv-window-set!
	    http2-server-stream-recv-consumed 
	    http2-server-stream-recv-consumed-set!
	    http2-server-stream-pending-output
	    http2-server-stream-pending-output-set!
	    http2-server-stream-pending-input-port
	    http2-server-stream-pending-input-port-set!
	    http2-server-stream-pending-end-stream?
	    http2-server-stream-pending-end-stream?-set!
	    http2-server-stream-priority-dependency
	    http2-server-stream-priority-dependency-set!
	    http2-server-stream-priority-weight
	    http2-server-stream-priority-weight-set!
	    http2-server-stream-exchange

	    http2-server-connection-state? make-http2-server-connection-state
	    http2-server-connection-state-socket
	    http2-server-connection-state-config
	    http2-server-connection-state-connection-info
	    http2-server-connection-state-app-handler
	    http2-server-connection-state-streams
	    http2-server-connection-state-stream-manager
	    http2-server-connection-state-decoder-context
	    http2-server-connection-state-encoder-context
	    http2-server-connection-state-read-buffer
	    http2-server-connection-state-write-buffer
	    http2-server-connection-state-mutex
	    http2-server-connection-state-pending
	    http2-server-connection-state-pending-set!
	    http2-server-connection-state-stage
	    http2-server-connection-state-stage-set!
	    http2-server-connection-state-last-stream-id
	    http2-server-connection-state-last-stream-id-set!
	    http2-server-connection-state-next-push-stream-id
	    http2-server-connection-state-next-push-stream-id-set!
	    http2-server-connection-state-local-max-frame-size
	    http2-server-connection-state-local-max-frame-size-set!
	    http2-server-connection-state-remote-max-frame-size
	    http2-server-connection-state-remote-max-frame-size-set!
	    http2-server-connection-state-remote-enable-push?
	    http2-server-connection-state-remote-enable-push?-set!
	    http2-server-connection-state-remote-initial-window-size
	    http2-server-connection-state-remote-initial-window-size-set!

	    http2-server-connection-state-connection-send-window
	    http2-server-connection-state-connection-send-window-set!
	    http2-server-connection-state-connection-recv-window
	    http2-server-connection-state-connection-recv-window-set!
	    http2-server-connection-state-connection-recv-consumed
	    http2-server-connection-state-connection-recv-consumed-set!
	    http2-server-connection-state-priority-tree

	    ->connection-state
	    connection-closed?
	    close-connection!

	    find-stream
	    register-stream!
	    drop-stream!
	    for-each-stream

	    invoke-app-handler
	    )
    (import (rnrs)
	    (clos user)
	    (net http-server exchange)
	    (net http-server types)
	    (net http-server protocol)
	    (net http-server request)
	    (net http-server response)
	    (net http-server http2 const)
	    (rfc http2 stream-manager)
	    (rfc http2 priority))

(define-class <http2-config> (<http-config>)
  ((max-concurrent-streams :init-keyword :max-concurrent-streams
			   :init-value +default-max-concurrent-streams+
			   :reader http2-config-max-concurrent-streams)
   (enable-push? :init-keyword :enable-push? :init-value #f
		 :reader http2-config-enable-push?)))

(define (http2-config? o) (is-a? o <http2-config>))
(define (make-http2-config . rest) (apply make <http2-config> rest))


(define-record-type http-server:http2-request
  (parent http-server:request)
  (fields stream-id)
  (protocol
   (lambda (p)
     (lambda (method target path query headers body stream-id
		     :optional (connection-info #f) (exchange #f))
       (let* ((info (or (and exchange
			(http-server:exchange-connection-info exchange))
			connection-info
			(make-http-server:connection-info #f #f #f "h2")))
	      (exchange* (or exchange (make-http-server:exchange info)))
	      (remote (http-server:connection-info-remote info)))
	 ((p method target path query "HTTP/2" headers body remote '() exchange)
	  stream-id))))))

(define-record-type http2-server-stream
  (fields id
	  method
	  target
	  path
	  query
	  headers
	  (mutable body-chunks)
	  (mutable body-size)
	  (mutable send-window)
	  (mutable recv-window)
	  (mutable recv-consumed)
	  (mutable pending-output)
	  (mutable pending-input-port)
	  (mutable pending-end-stream?)
	  (mutable priority-dependency)
	  (mutable priority-weight)
	  exchange))

(define-record-type http2-server-connection-state
  (fields socket
	  config
	  connection-info
	  app-handler
	  streams
	  stream-manager
	  decoder-context
	  encoder-context
	  read-buffer
	  write-buffer
	  mutex
	  (mutable pending)
	  (mutable stage)
	  (mutable last-stream-id)
	  (mutable next-push-stream-id)
	  (mutable local-max-frame-size)
	  (mutable remote-max-frame-size)
	  (mutable remote-enable-push?)
	  (mutable remote-initial-window-size)
	  (mutable connection-send-window)
	  (mutable connection-recv-window)
	  (mutable connection-recv-consumed)
	  priority-tree))

(define (connection-closed? state)
  (eq? (http2-server-connection-state-stage state) 'closed))

(define (->connection-state conn-or-state)
  (if (http-server:http-connection? conn-or-state)
      (http-server:http-connection-parse-state conn-or-state)
      conn-or-state))

(define (close-connection! conn)
  (define state (->connection-state conn))
  (unless (connection-closed? state)
    (let ((sid* '()))
      (for-each-stream state
       (lambda (stream)
	 (set! sid* (cons (http2-server-stream-id stream) sid*))))
      (for-each
       (lambda (sid)
	 (let ((stream (find-stream state sid)))
	   (when stream
	     (%http-server:exchange-mark-aborted!
	      (http2-server-stream-exchange stream))))
	 (drop-stream! state sid))
       sid*))
    (http2-server-connection-state-stage-set! state 'closed)))

;; stream

(define (find-stream conn sid)
  (http2-stream-manager-find-stream
   (http2-server-connection-state-stream-manager conn)
   sid))

(define (drop-stream! conn sid)
  (let ((stream (find-stream conn sid)))
    (when (and stream
               (http2-server-stream-pending-input-port stream))
      (guard (e (else #f))
        (close-port (http2-server-stream-pending-input-port stream)))
      (http2-server-stream-pending-input-port-set! stream #f))
    (http2-stream-manager-unregister-stream!
     (http2-server-connection-state-stream-manager conn)
     sid)
    (http2-priority-tree-remove!
     (http2-server-connection-state-priority-tree conn)
     sid)))

(define (for-each-stream conn proc)
  (http2-stream-manager-for-each-stream
   (http2-server-connection-state-stream-manager conn)
   proc))

(define (register-stream! conn stream)
  (if (http2-stream-manager-register-stream!
       (http2-server-connection-state-stream-manager conn)
       (http2-server-stream-id stream)
       stream)
      (begin
	(when (odd? (http2-server-stream-id stream))
	  (http2-server-connection-state-last-stream-id-set!
	   conn
	   (http2-server-stream-id stream)))
	stream)
      #f))

(define (invoke-app-handler state stream req)
  (define exchange (http-server:request-exchange req))
  (define app-handler (http2-server-connection-state-app-handler state))
  (define res (make-http-server:response 200 exchange))
  (define (make-application-error-response)
    (let ((er (make-http-server:response 500 exchange)))
      (http-server:response-text! er "Unhandled application error")
      er))
  (%http-server:exchange-mark-handling! exchange)
  (guard (e (else (make-application-error-response)))
    (let ((response (%http-server:normalize-handler-response
		     (app-handler req res) res exchange)))
      (let-values (((ok? code message)
		    (http-server:validate-final-response
		     (http-server:request-method req)
		     (http-server:response-status response)
		     'http2)))
	(if ok?
	    response
	    (let ((err (make-http-server:response code exchange)))
	      (http-server:response-text! err message)
	      err))))))

)
