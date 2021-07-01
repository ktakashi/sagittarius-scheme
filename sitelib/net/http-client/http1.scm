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
#!read-macro=sagittarius/bv-string
#!read-macro=sagittarius/regex
(library (net http-client http1)
    (export http1-connection-context?
	    socket->http1-connection)
    (import (rnrs)
	    (net http-client connection)
	    (net http-client request)
	    (net uri)
	    (sagittarius regex)
	    (rfc :5322)
	    (srfi :13 strings)
	    (prefix (binary io) binary:)
	    (util bytevector))

(define-record-type http1-connection-context
  (parent <http-connection-context>)
  (fields requests
	  buffer)
  (protocol (lambda (n)
	      (lambda ()
		((n) (make-eq-hashtable) (make-bytevector 4096))))))

(define (make-http1-connection socket socket-option node service)
  (make-http-connection node service socket-option socket
			http1-request http1-response
			(make-http1-connection-context)))

(define (socket->http1-connection socket socket-option node service)
  (make-http1-connection socket socket-option node service))

(define (http1-request connection request header-handler data-handler)
  (define context (http-connection-context-data connection))
  ;; here we only push the request
  (hashtable-set! (http1-connection-context-requests context)
		  request (list header-handler data-handler)))

(define (http1-response connection)
  (define context (http-connection-context-data connection))
  (define requests (http1-connection-context-requests context))
  (define (handle-requests connection requests)
    (let-values (((keys values) (hashtable-entries requests)))
      (vector-map (lambda (request handlers)
		    (apply http1-resquest/response connection request handlers))
		  keys values)))
  (let ((results (handle-requests connection requests)))
    (hashtable-clear! requests)
    ;; TODO check reconnectability
    (fold-left (lambda (acc conn) (and acc conn)) #t (vector->list results))))

(define (http1-resquest/response connection request header-handler data-handler)
  ;; 1. ensure connection (some bad server may not allow us to reuse the
  ;;    connection (i.e. no content-length or no transfer-encoding, or keep-alive
  ;;    closed specified)
  ;; 2. send request
  ;; 3. receive response
  ;; 4. close connection if needed
  (http-connection-open! connection)
  (send-request! connection request)
  (let ((keep? (receive-response! connection request
				  header-handler data-handler)))
    (cond (keep? connection)
	  (else (http-connection-close! connection) #f))))

(define (read-one-line in)
  ;; \r\n would be \r for binary:get-line so trim it
  (bytevector-trim-right (binary:get-line in) '(#x0d)))
(define (parse-status-line line)
  (cond ((eof-object? line)
	 ;; TODO proper condition
	 (error 'parse-status-line "http reply contains no data"))
	((#/[\w\/.]+\s+(\d\d\d)\s+(.*)/ line)
	 => (lambda (m) (values (m 1) (m 2))))
	(else (error 'parse-status-line "bad reply from server" line))))

(define (ensure-read in size)
  (define buf (make-bytevector size))
  (let loop ((s 0) (size size))
    (let ((r (get-bytevector-n! in buf s size)))
      (if (= r size)
	  buf
	  (loop (+ s r) (- size r))))))
(define (receive-response! connection request header-handler data-handler)
  (define in (http-connection-input connection))
  (define (check-connection headers)
    ;; TODO we need to tell connection manager how long we can
    ;; let it alive...
    (cond ((rfc5322-header-ref headers "connection") =>
	   (lambda (v) (string-contains v "keep-alive")))
	  (else #f)))
  (let-values (((code reason) (parse-status-line
			       (utf8->string (read-one-line in)))))
    (let ((headers (rfc5322-read-headers in)))
      (header-handler code headers)
      (cond ((rfc5322-header-ref headers "content-length") =>
	     (lambda (len)
	       (data-handler (ensure-read in (string->number len)) #t)
	       (check-connection headers)))
	    ((rfc5322-header-ref headers "transfer-encoding") =>
	     (lambda (v)
	       (cond ((string-contains v "chunked")
		      (read-chunked data-handler in)
		      (check-connection headers))
		     (else
		      (data-handler (get-bytevector-all in) #t)
		      #f))))
	    ;; no body, so it's okay
	    ((or (eq? 'HEAD (http:request-method request))
		 ;; 204 (no content) 304 (not modified)
		 (and code (memq (string->number code) '(204 304)))))
	    ;; very bad behaving server...
	    (else (data-handler (get-bytevector-all in) #t) #f)))))

(define (read-chunked data-handler in)
  (let ((line (read-one-line in)))
    (when (eof-object? line)
      ;; TODO proper condition
      (error 'read-chunked "Chunked body ended prematurely"))
    (cond ((#/^([0-9a-fA-F]+)/ line) =>
	   (lambda (m)
	     (let ((size (string->number (utf8->string (m 1)) 16)))
	       (if (zero? size)
		   (data-handler #vu8() #t)
		   (data-handler (ensure-read in size) #f))
	       (ensure-read in 2) ;; drop \r\n
	       (unless (zero? size)
		 (read-chunked data-handler in)))))
	  (else (error 'read-chunked "bad line in chunked data" line)))))

(define (send-request! connection request)
  (define context (http-connection-context-data connection))
  (define out (http-connection-output connection))
  (define method (http:request-method request))
  (define body (http:request-body request))
  (define (send-first-line out request)
    (put-bytevector out (string->utf8 (symbol->string method)))
    (put-bytevector out #*" ")
    (put-bytevector out (string->utf8 (http:request->request-uri request)))
    (put-bytevector out #*" HTTP/1.1\r\n"))
  
  (define (send-headers out request)
    (define headers (http:request-headers request))
    (define uri (http:request-uri request))
    (define (write-header out name value)
      (put-bytevector out (string->utf8 name))
      (put-bytevector out #*": ")
      (put-bytevector out (string->utf8 value))
      (put-bytevector out #*"\r\n"))

    (unless (http:no-body-method? method)
      (when body
	(write-header out "Content-Type"
		      (http:request-content-type request)))
      (cond ((bytevector? body)
	     (write-header out
			   "Content-Length"
			   (number->string (bytevector-length body))))
	    ((port? body)
	     (write-header out "Transfer-Encoding" "chunked"))))
    
    (write-header out "Host"
		  (or (http:headers-ref headers "Host")
		      (uri-host uri)
		      (http-connection-node connection)))
    (write-header out "Connection" "keep-alive")
    
    (for-each (lambda (name)
		(let ((small (string-downcase name)))
		  (unless (memp (lambda (n) (string=? small n))
				+http:managed-headers+)
		    (for-each (lambda (value) (write-header out name value))
			      (http:headers-ref* headers name)))))
	      (http:headers-names headers))
    (put-bytevector out #*"\r\n")
    (flush-output-port out))

  (define (send-body out request)
    (define (send-chunked out in)
      (define buffer (http1-connection-context-buffer context))
      (define buffer-size (bytevector-length buffer))
      (let loop ((n (get-bytevector-n! in buffer 0 buffer-size)))
	(cond ((zero? n)
	       (put-bytevector out #*"0\r\n\r\n"))
	      (else
	       (put-bytevector out (string->utf8 (number->string n)))
	       (put-bytevector out #*"\r\n")
	       (put-bytevector out buffer 0 n)
	       (put-bytevector out #*"\r\n")
	       (if (< n buffer-size)
		   (put-bytevector out #*"0\r\n\r\n")
		   (loop (get-bytevector-n! in buffer 0 buffer-size)))))))
    
    (unless (http:no-body-method? method)
      (cond ((bytevector? body) (put-bytevector out body))
	    ((port? body) (send-chunked out body)))
      (flush-output-port out)))
  
  (send-first-line out request)
  (send-headers out request)
  (send-body out request)
  )
)
