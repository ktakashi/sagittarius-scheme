;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/request.scm - HTTP server request representation
;;;

#!nounbound
(library (net http-server request)
    (export http-server:request?
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
            http-server:request-attribute-set!)
    (import (rnrs)
            (net http-server types))

(define-record-type http-server:request
  (fields method
          target
          path
          query
          http-version
          headers
          body-bytevector
          remote
          (mutable attributes)))

(define (http-server:request-body-port req)
  (open-bytevector-input-port
   (http-server:request-body-bytevector req)))

(define (http-server:request-header-ref req name :optional (default #f))
  (http-server:headers-ref (http-server:request-headers req) name default))

(define (http-server:request-header-ref* req name :optional (default '()))
  (http-server:headers-ref* (http-server:request-headers req) name default))

(define (http-server:request-attribute-ref req key :optional (default #f))
  (let ((kv (assq key (http-server:request-attributes req))))
    (if kv (cdr kv) default)))

(define (http-server:request-attribute-set! req key value)
  (let loop ((rest (http-server:request-attributes req)) (out '()))
    (cond ((null? rest)
           (http-server:request-attributes-set!
            req
            (reverse (cons (cons key value) out))))
          ((eq? (caar rest) key)
           (http-server:request-attributes-set!
            req
            (append (reverse out)
                    (cons (cons key value) (cdr rest)))))
          (else (loop (cdr rest) (cons (car rest) out))))))
)
