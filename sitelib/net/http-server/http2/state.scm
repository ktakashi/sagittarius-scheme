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
	    http2-server-stream-pending-end-stream?
	    http2-server-stream-pending-end-stream?-set!
	    http2-server-stream-priority-dependency
	    http2-server-stream-priority-dependency-set!
	    http2-server-stream-priority-weight
	    http2-server-stream-priority-weight-set!

	    http2-server-connection-state? make-http2-server-connection-state
	    http2-server-connection-state-socket
	    http2-server-connection-state-config
	    http2-server-connection-state-app-handler
	    http2-server-connection-state-streams
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
	    for-each-stream)
    (import (rnrs)
	    (clos user)
	    (net http-server types)
	    (net http-server protocol)
	    (net http-server request)
	    (net http-server http2 const)
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
     (lambda (method target path query headers body stream-id)
       ((p method target path query "HTTP/2" headers body #f '()) stream-id)))))

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
          (mutable pending-end-stream?)
          (mutable priority-dependency)
          (mutable priority-weight)))

(define-record-type http2-server-connection-state
  (fields socket
          config
          app-handler
          streams
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
    (http2-server-connection-state-stage-set! state 'closed)))

;; stream

(define (find-stream conn sid)
  (hashtable-ref (http2-server-connection-state-streams conn) sid #f))

(define (drop-stream! conn sid)
  (hashtable-delete! (http2-server-connection-state-streams conn) sid)
  (http2-priority-tree-remove! (http2-server-connection-state-priority-tree conn)
                               sid))

(define (for-each-stream conn proc)
  (let-values (((keys values)
                (hashtable-entries (http2-server-connection-state-streams conn))))
    (let ((size (vector-length keys)))
      (let loop ((i 0))
        (unless (= i size)
          (let ((stream (vector-ref values i)))
            (when stream
              (proc stream)))
          (loop (+ i 1)))))))

(define (register-stream! conn stream)
  (hashtable-set! (http2-server-connection-state-streams conn)
                  (http2-server-stream-id stream)
                  stream)
  (when (odd? (http2-server-stream-id stream))
    (http2-server-connection-state-last-stream-id-set!
     conn
     (http2-server-stream-id stream)))
  stream)
)
