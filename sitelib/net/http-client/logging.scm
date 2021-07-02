;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/logging.scm - Logging facility for HTTP client
;;;  
;;;   Copyright (c) 2021  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (net http-client logging)
    (export http-client-logger?
	    http-client-logger-builder
	    http-client-logger-connection-logger
	    http-client-logger-wire-logger

	    http-connection-logger?
	    http-connection-logger-builder
	    http-connection-logger-write-log
	    
	    http-wire-logger?
	    http-wire-logger-builder
	    http-wire-logger-write-log)
    (import (rnrs)
	    (sagittarius)
	    (record builder)
	    (util logging))

(define-record-type http-client-logger
  (fields connection-logger
	  wire-logger)
  (protocol (lambda (p)
	      (lambda (logger wire-logger)
		(p logger wire-logger)))))

(define-syntax http-client-logger-builder
  (make-record-builder http-client-logger))

(define-record-type http-connection-logger
  (fields logger))
(define-syntax http-connection-logger-builder
  (make-record-builder http-connection-logger))
(define-syntax http-connection-logger-write-log
  (syntax-rules ()
    ((_ connection-logger msg exp ...)
     (when connection-logger
       (let ((logger (http-connection-logger-logger connection-logger)))
	 (when (logger-debug? logger)
	   (debug-log logger msg exp ...)))))))

(define-record-type http-wire-logger
  (fields logger
	  data-formatter))
(define-syntax http-wire-logger-builder
  (make-record-builder http-wire-logger
		       ((data-formatter values))))

(define-syntax http-wire-logger-write-log
  (syntax-rules ()
    ((_ wire-logger message bv-exp)
     (when wire-logger 
       (let ((logger (http-wire-logger-logger wire-logger))
	     (formatter (http-wire-logger-data-formatter wire-logger)))
	 (when (logger-debug? logger)
	   (let ((bv bv-exp))
	     (debug-log logger 
			(format "~a (~a)" message (bytevector-length bv))
			(formatter bv)))))))))
)
