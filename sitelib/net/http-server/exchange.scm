;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/exchange.scm - HTTP exchange and connection context
;;;

#!nounbound
(library (net http-server exchange)
    (export http-server:connection-info?
	    make-http-server:connection-info
	    http-server:connection-info-remote
	    http-server:connection-info-peer-certificate
	    http-server:connection-info-peer-certificate-set!
	    http-server:connection-info-selected-alpn
	    http-server:connection-info-protocol

	    http-server:exchange?
	    make-http-server:exchange
	    http-server:exchange-attributes
	    http-server:exchange-attribute-ref
	    http-server:exchange-attribute-set!
	    http-server:exchange-connection-info
	    http-server:exchange-state

	    %http-server:exchange-mark-handling!
	    %http-server:exchange-mark-response-started!
	    %http-server:exchange-mark-completed!
	    %http-server:exchange-mark-aborted!)
    (import (rnrs))

(define-record-type http-server:connection-info
  (fields remote
	  (mutable peer-certificate)
	  selected-alpn
	  protocol)
  (protocol
   (lambda (p)
     (lambda (remote :optional (peer-certificate #f) (alpn #f) (proto #f))
       (p remote peer-certificate alpn proto)))))

(define-record-type http-server:exchange
  (fields (mutable attributes)
	  connection-info
	  (mutable state))
  (protocol (lambda (p)
	      (lambda (connection-info :optional (attributes '()))
		(p attributes connection-info 'created)))))

(define (http-server:exchange-attribute-ref exchange key :optional (default #f))
  (let ((kv (assq key (http-server:exchange-attributes exchange))))
    (if kv (cdr kv) default)))

(define (http-server:exchange-attribute-set! exchange key value)
  (let loop ((rest (http-server:exchange-attributes exchange)) (out '()))
    (cond ((null? rest)
	   (http-server:exchange-attributes-set!
	    exchange
	    (reverse (cons (cons key value) out))))
	  ((eq? (caar rest) key)
	   (http-server:exchange-attributes-set!
	    exchange
	    (append (reverse out)
		    (cons (cons key value) (cdr rest)))))
	  (else (loop (cdr rest) (cons (car rest) out))))))

(define (valid-transition? from to)
  (case from
    ((created) (or (eq? to 'handling) (eq? to 'aborted)))
    ((handling) (or (eq? to 'response-started) (eq? to 'aborted)))
    ((response-started) (or (eq? to 'completed) (eq? to 'aborted)))
    ((completed aborted) #f)
    (else #f)))

(define (transition! exchange state)
  (let ((current (http-server:exchange-state exchange)))
    (when (and (not (eq? current state))
	       (valid-transition? current state))
      (http-server:exchange-state-set! exchange state)))
  exchange)

(define (%http-server:exchange-mark-handling! exchange)
  (transition! exchange 'handling))

(define (%http-server:exchange-mark-response-started! exchange)
  (transition! exchange 'response-started))

(define (%http-server:exchange-mark-completed! exchange)
  (transition! exchange 'completed))

(define (%http-server:exchange-mark-aborted! exchange)
  (transition! exchange 'aborted))
)
