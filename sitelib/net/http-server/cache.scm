;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/cache.scm - cache abstraction for http-server
;;;

#!nounbound
(library (net http-server cache)
  (export http-server:cache?
          make-http-server:cache
          http-server:cache-lookup
          http-server:cache-store!
          http-server:cache-invalidate!
          http-server:cache-clear!
          http-server:cache-key
          http-server:make-cache-middleware)
  (import (rnrs)
          (net http-server request)
          (net http-server response)
          (net http-server types))

(define-record-type http-server:cache
  (fields (immutable %lookup cache-lookup-proc)
          (immutable %store! cache-store-proc)
          (immutable %invalidate! cache-invalidate-proc)
          (immutable %clear! cache-clear-proc)
          (immutable %key-maker cache-key-maker-proc)))

(define (http-server:cache-lookup cache req)
  ((cache-lookup-proc cache)
   (http-server:cache-key cache req)
   req))

(define (http-server:cache-store! cache req res)
  ((cache-store-proc cache)
   (http-server:cache-key cache req)
   req
   res))

(define (http-server:cache-invalidate! cache key)
  ((cache-invalidate-proc cache) key))

(define (http-server:cache-clear! cache)
  ((cache-clear-proc cache)))

(define (http-server:cache-key cache req)
  ((cache-key-maker-proc cache) req))

(define (default-cache-key req)
  (string-append (symbol->string (http-server:request-method req))
                 " "
                 (http-server:request-target req)))

(define (http-server:make-cache-middleware cache next)
  (lambda (req res)
    (if (not (eq? (http-server:request-method req) 'GET))
        (next req res)
        (cond ((http-server:cache-lookup cache req) =>
               (lambda (cached)
                 (http-server:response-status-set! res
		  (http-server:response-status cached))
                 (http-server:response-reason-set! res
		  (http-server:response-reason cached))
                 (for-each (lambda (kv)
                             (for-each (lambda (v)
                                         (http-server:response-header-add! res
					  (car kv) v))
                                       (cdr kv)))
                           (http-server:headers->alist
			    (http-server:response-headers cached)))
                 (http-server:response-body-set! res
		  (http-server:response-body cached))
                 res))
              (else
               (let ((result (next req res)))
                 (when (and (http-server:response-cacheable? result)
                            (or (bytevector? (http-server:response-body result))
                                (string? (http-server:response-body result))))
                   (http-server:cache-store! cache req result))
                 result))))))
)
