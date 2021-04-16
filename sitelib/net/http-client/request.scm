;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/request.scm - HTTP request/response of HTTP client
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
(library (net http-client request)
    (export http:request? http:request-builder
	    (rename (http:request <http:request>))
	    http:request-uri http:request-method
	    http:request-content-type
	    http:request-headers
	    http:request-body
	    
	    ;; helper
	    http:request->request-uri

	    http:response? http:response-builder
	    (rename (http:response <http:response>))
	    http:response-status http:response-headers
	    http:response-cookies http:response-body

	    +http:managed-headers+
	    http:make-headers http:headers?
	    http:headers-ref* http:headers-ref http:headers-contains?
	    http:headers-set! http:headers-add!
	    http:headers-names

	    http:method
	    http-method-set
	    http:no-body-method?
	    )
    (import (rnrs)
	    (record builder)
	    (net uri))

;;; TODO maybe should make a record for this
(define (->headers l)
  (if (http:headers? l)
      (hashtable-copy l #t)
      (let ((ht (http:make-headers)))
	(for-each (lambda (kv) (http:headers-add! ht (car kv) (cdr kv))) l)
	ht)))

(define (http:make-headers) (make-hashtable string-ci-hash string-ci=?))
(define http:headers? hashtable?)
(define (http:headers-ref* h k) (hashtable-ref h k '()))
(define (http:headers-ref h k) (cond ((hashtable-ref h k #f) => car)
				     (else #f)))
(define (http:headers-contains? h k) (hashtable-contains? h k))
(define (http:headers-set! h k v) (hashtable-set! h k (list v)))
(define (http:headers-add! h k v)
  (hashtable-update! h k (lambda (v*) (cons v v*)) '()))
(define (http:headers-names header) (vector->list (hashtable-keys header)))

(define-enumeration http:method
  (CONNECT DELETE GET HEAD OPTIONS PATCH POST PUT TRACE)
  http-method-set)
(define *http:no-body-methods*
  (http-method-set CONNECT GET HEAD OPTIONS TRACE))
(define (http:no-body-method? method)
  (enum-set-member? method *http:no-body-methods*))

(define-record-type http:request
  (fields uri
	  method
	  content-type
	  headers
	  body))
(define (->uri uri)
  (if (uri? uri)
      uri
      (string->uri uri)))
(define-syntax http:request-builder
  (make-record-builder http:request
		       ((uri #f ->uri)
			(content-type "application/octet-stream")
			(body #f)
			(headers '() ->headers))))

(define (http:request->request-uri request)
  (define uri (http:request-uri request))
  (let ((path (or (uri-path uri) "/"))
	(query (uri-query uri)))
    ;; encode?
    (if query
	(string-append path "?" query)
	path)))

(define-record-type http:response
  (fields status
	  headers
	  cookies
	  body))
(define-syntax http:response-builder
  (make-record-builder http:response
		       ((body #f)
			;; let it fail if no header is provided...
			(headers #f ->headers)
			(cookies '()))))

;; Managed headers (these headers are ignored if user set)
;; Host is not listed here deliberately
(define +http:managed-headers+
  '("host"	 ;; this is handled separately but user can stil specify ;)
    "content-length"
    "content-type"
    "transfer-encoding"
    "connection"))

)
