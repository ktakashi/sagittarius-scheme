;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/protocol.scm - protocol driver registry
;;;

#!nounbound
(library (net http-server protocol)
    (export http-server:connection
	    http-server:connection?
	    make-http-server:connection
	    http-server:connection-server
	    http-server:connection-socket
	    http-server:connection-process
	    http-server:connection-close
	    http-server:connection-process!
	    http-server:connection-close!

	    http-server:http-connection
	    http-server:http-connection?
	    make-http-server:http-connection
	    http-server:http-connection-buffer
	    http-server:http-connection-buffer-set!
	    http-server:http-connection-request-count
	    http-server:http-connection-request-count-set!
	    http-server:http-connection-parse-state
	    http-server:http-connection-parse-state-set!
	    http-server:http-connection-driver
	    http-server:http-connection-upgrade-registry


	    http-server:protocol-driver?
	    make-http-server:protocol-driver
	    http-server:protocol-driver-name
	    http-server:protocol-driver-connect
	    http-server:protocol-driver-consume!
	    http-server:protocol-driver-serve!
	    http-server:connection-oriented-driver?
	    http-server:protocol-driver-connect!

	    make-http-server:protocol-registry
	    http-server:register-protocol-driver!
	    http-server:select-protocol-driver)
    (import (rnrs)
            (net socket))

(define-record-type (http-server:connection
		     %make-http-server:connection
		     http-server:connection?)
  (fields server socket process close))

(define-record-type http-server:http-connection
  (parent http-server:connection)
  (fields (mutable buffer)
          (mutable request-count)
          (mutable parse-state)
          driver
          upgrade-registry))

(define make-http-server:connection
  (case-lambda
   ((process close)
    (%make-http-server:connection #f #f process close))
   ((server socket process close)
    (%make-http-server:connection server socket process close))))

(define (http-server:connection-process! conn chunk)
  ((http-server:connection-process conn) chunk))

(define (http-server:connection-close! conn)
  (guard (e (else #f))
    ((http-server:connection-close conn))))

(define-record-type http-server:protocol-driver
  (fields name consume serve connect)
  (protocol
   (lambda (p)
     (case-lambda
      ((name consume serve)
	(p name consume serve #f))
      ((name consume serve connect)
	(p name consume serve connect))))))

(define-record-type http-server:protocol-registry
  (fields (mutable drivers)
          (mutable default-driver))
  (protocol (lambda (p)
	      (lambda (driver)
		(p '() driver)))))

;; consume returns: status, req-or-code, extra, remainder, next-state (#f => fresh)
(define (http-server:protocol-driver-consume! driver conn . rest)
  (apply (http-server:protocol-driver-consume driver) conn rest))

;; req = #f, error response
(define (http-server:protocol-driver-serve! driver conn req result)
  ((http-server:protocol-driver-serve driver) conn req result))

(define (http-server:connection-oriented-driver? driver)
  (and (http-server:protocol-driver-connect driver) #t))

(define (http-server:protocol-driver-connect! driver server socket app-handler . rest)
  (let ((connect (http-server:protocol-driver-connect driver)))
    (if connect
	(apply connect server socket app-handler rest)
	(assertion-violation 'http-server:protocol-driver-connect!
			     "Connection oriented protocol driver required"
			     driver))))


(define (http-server:register-protocol-driver! registry alpn-name driver)
  (let ((name (and alpn-name (string-downcase alpn-name))))
    (http-server:protocol-registry-drivers-set!
     registry
     (cons (cons name driver)
           (http-server:protocol-registry-drivers registry)))))

(define (assoc-string key alist)
  (let loop ((rest alist))
    (and (pair? rest)
         (if (string=? key (caar rest))
             (car rest)
             (loop (cdr rest))))))

(define (http-server:select-protocol-driver registry socket)
  (if (and (tls-socket? socket) (tls-socket-selected-alpn socket))
      (let* ((alpn (string-downcase (tls-socket-selected-alpn socket)))
             (kv (assoc-string alpn (http-server:protocol-registry-drivers registry))))
        (if kv
            (cdr kv)
            (http-server:protocol-registry-default-driver registry)))
      (http-server:protocol-registry-default-driver registry)))
)
