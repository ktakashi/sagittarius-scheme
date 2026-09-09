;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; net/http-server/protocol.scm - protocol driver registry
;;;

#!nounbound
(library (net http-server protocol)
    (export http-server:protocol-driver?
            make-http-server:protocol-driver
            http-server:protocol-driver-name
	    http-server:protocol-driver-consume!
            http-server:protocol-driver-serve!

            make-http-server:protocol-registry
            http-server:register-protocol-driver!
            http-server:select-protocol-driver)
    (import (rnrs)
            (net socket))

(define-record-type http-server:protocol-driver
  (fields name consume serve))

(define-record-type (http-server:protocol-registry
                     %make-http-server:protocol-registry
                     http-server:protocol-registry?)
  (fields (mutable drivers)
          (mutable default-driver)))

(define (http-server:protocol-driver-consume! driver buffer . rest)
  (apply (http-server:protocol-driver-consume driver) buffer rest))

;; req = #f, error response
(define (http-server:protocol-driver-serve! driver socket req result)
  ((http-server:protocol-driver-serve driver) socket req result))


(define (make-http-server:protocol-registry default-driver)
  (%make-http-server:protocol-registry '() default-driver))

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
