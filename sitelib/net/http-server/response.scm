;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/response.scm - HTTP server response representation
;;;

#!nounbound
(library (net http-server response)
    (export http-server:response?
	    make-http-server:response
	    http-server:response-status
	    http-server:response-status-set!
	    http-server:response-reason
	    http-server:response-reason-set!
	    http-server:response-headers
	    http-server:response-body
	    http-server:response-body-set!
	    http-server:response-pushes
	    http-server:response-cacheable?
	    http-server:response-cacheable?-set!
	    http-server:response-cache-ttl
	    http-server:response-cache-ttl-set!

	    http-server:response-header-ref
	    http-server:response-header-ref*
	    http-server:response-header-set!
	    http-server:response-header-add!

	    http-server:response-push!
	    http-server:response-text!
	    http-server:response-bytes!
	    http-server:response-copy)
    (import (rnrs)
	    (net http-server types))
  
(define-record-type (http-server:response
		     %make-http-server:response
		     http-server:response?)
  (fields (mutable status http-server:response-status %response-status-set!)
          (mutable reason)
          headers
          (mutable body)
	  (mutable pushes)
          (mutable cacheable?)
          (mutable cache-ttl)))

(define (make-http-server:response :optional (status 200))
  (%make-http-server:response status
                              (http-server:reason-phrase status)
                              (make-http-server:headers)
                              #vu8()
                              '()
                              #f
                              #f))

(define (http-server:response-header-ref res name :optional (default #f))
  (http-server:headers-ref (http-server:response-headers res) name default))

(define (http-server:response-header-ref* res name :optional (default '()))
  (http-server:headers-ref* (http-server:response-headers res) name default))

(define (http-server:response-header-set! res name value)
  (http-server:headers-set! (http-server:response-headers res) name value))

(define (http-server:response-header-add! res name value)
  (http-server:headers-add! (http-server:response-headers res) name value))

(define (http-server:response-status-set! res status)
  (%response-status-set! res status)
  (http-server:response-reason-set! res (http-server:reason-phrase status)))

(define (http-server:response-text! res text :optional (content-type "text/plain; charset=utf-8"))
  (http-server:response-body-set! res (string->utf8 text))
  (http-server:response-header-set! res "content-type" content-type)
  res)

(define (http-server:response-bytes! res body :optional (content-type "application/octet-stream"))
  (http-server:response-body-set! res body)
  (http-server:response-header-set! res "content-type" content-type)
  res)

(define (ensure-push-method method)
  (cond ((symbol? method) method)
        ((string? method) (string->symbol (string-upcase method)))
        (else
         (assertion-violation 'http-server:response-push!
                              "method must be symbol or string"
                              method))))

(define (ensure-push-path path)
  (if (string? path)
      path
      (assertion-violation 'http-server:response-push!
                           "path must be string"
                           path)))

(define (ensure-push-headers headers)
  (cond ((http-server:headers? headers) (headers-copy headers))
        ((null? headers) (make-http-server:headers))
        ((list? headers) (make-http-server:headers headers))
        (else
         (assertion-violation 'http-server:response-push!
                              "headers must be http-server:headers or alist"
                              headers))))

(define (http-server:response-push! res method path :optional (headers '()))
  (let ((m (ensure-push-method method))
        (p (ensure-push-path path))
        (h (ensure-push-headers headers)))
    (http-server:response-pushes-set!
     res
     (append (http-server:response-pushes res)
             (list (list m p h)))))
  res)

(define (headers-copy headers)
  (make-http-server:headers (http-server:headers->alist headers)))

(define (http-server:response-copy res)
  (define (push-entry-copy e)
    (list (car e)
          (cadr e)
          (headers-copy (caddr e))))
  (let ((r (%make-http-server:response
            (http-server:response-status res)
            (http-server:response-reason res)
            (headers-copy (http-server:response-headers res))
            (http-server:response-body res)
            (map push-entry-copy (http-server:response-pushes res))
            (http-server:response-cacheable? res)
            (http-server:response-cache-ttl res))))
    r))
)
