;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/http1.scm - HTTP/1.1 engine for HTTP client
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
(library (net http-client http1)
    (export http1-connection?
	    socket->http1-connection)
    (import (rnrs)
	    (net http-client connection)
	    (net http-client request))

(define-record-type http1-connection
  (parent <http-connection>)
  (fields requests)
  (protocol (lambda (n)
	      (lambda (socket option node service)
		((n node service option socket http1-request http1-response)
		 (make-eq-hashtable))))))

(define (socket->http1-connection socket socket-option node service)
  (make-http1-connection socket socket-option node service))

(define (http1-request connection request header-handler data-handler)
  ;; here we only push the request
  (hashtable-set! (http1-connection-requests connection)
		  request (list header-handler data-handler)))

(define (http1-response connection)
  (define requests (http1-connection-requests connection))
  (define (handle-requests connection requests)
    (let-values (((keys values) (hashtable-entries requests)))
      (vector-map (lambda (request handlers)
		    (apply http1-resquest/response connection request handlers))
		  keys values)))
  (let ((results (handle-requests connection requests)))
    (hashtable-clear! requests)
    ;; TODO check reconnectability
    #f))

(define (http1-resquest/response connection request header-handler data-handler)
  )

)
