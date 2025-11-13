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
	    (srfi :18 multithreading)
	    (prefix (binary io) binary:)
	    (util bytevector))

(define-record-type http1-connection-context
  (parent <http-connection-context>)
  (fields buffer (mutable state))
  (protocol (lambda (n)
	      (lambda ()
		((n) (make-bytevector 4096) #f)))))

(define (make-http1-connection socket socket-option node service)
  (make-http-connection node service socket-option socket
			http1-send-header http1-send-data
			http1-receive-header http1-receive-data 
			(make-http1-connection-context)))

(define (socket->http1-connection socket socket-option node service . ignore)
  (make-http1-connection socket socket-option node service))

(define (http1-send-header connection request)
  (send-header! connection request))

(define (http1-send-data connection request)
  ;; 1. ensure connection (some bad server may not allow us to reuse
  ;;    the connection (i.e. no content-length or no
  ;;    transfer-encoding, or keep-alive closed specified)
  (unless (http-connection-open? connection)
    (assertion-violation 'http1-send-data "HTTP connection is already closed"
			 connection))
  ;; 2. send request
  (send-data! connection request))

(define (http1-receive-header connection response-context)
  (define header-handler (http:response-context-header-handler response-context))
  (define context (http-connection-context-data connection))
  (define in (http-connection-input connection))
  (define (check-data headers)
    (cond ((rfc5322-header-ref headers "content-length" #f) =>
	   (lambda (n) (not (string=? "0" n))))
	  ((rfc5322-header-ref headers "transfer-encoding" #f) =>
	   (lambda (n) (string=? "chunked" n)))
	  (else 'unknown)))
  (let*-values (((status-line) (read-one-line in))
		((code reason) (parse-status-line status-line)))
    (let ((headers (rfc5322-read-headers in)))
      (when (http-logging-connection? connection)
	(http-connection-write-log connection "[Response] ~a"
				   (utf8->string status-line))
	(for-each (lambda (h)
		    (for-each (lambda (v)
				(http-connection-write-log connection
				  "[Response header] ~a: ~a"
				  (car h) v)) (cdr h))) headers))
      (http1-connection-context-state-set! context (cons code headers))
      ;; If the server sends header and body in one go, then select
      ;; may not return for some reason (I was expecting if the socket
      ;; buffer contains data to read, it'd return, but that's not the
      ;; case at least on macOS). To avoid infinite wait, we return
      ;; 'unknown here so that header and body will be read at the same
      ;; time.
      (let ((has-data? (check-data headers)))
	(header-handler response-context code headers has-data?)
	has-data?))))

(define (http1-receive-data connection response-context)
  (define request (http:response-context-request response-context))
  (define data-handler (http:response-context-data-handler response-context))
  (define context (http-connection-context-data connection))
  (define state (http1-connection-context-state context))
  (define status (car state))
  (define headers (cdr state))
  (define in (http-connection-input connection))
  (define ((wrap handler) data end?) (data-handler response-context data end?))

  (cond ((rfc5322-header-ref headers "content-length") =>
	 (lambda (len)
	   (data-handler response-context
			 (ensure-read in (string->number len)) #t)
	   (check-connection headers)))
	((rfc5322-header-ref headers "transfer-encoding") =>
	 (lambda (v)
	   (cond ((string-contains v "chunked")
		  (read-chunked connection (wrap data-handler) headers in))
		 (else
		  (data-handler response-context (get-bytevector-all in) #t)
		  (http:response-body-state closed)))))
	;; no body, so it's okay
	((or (eq? 'HEAD (http:request-method request))
	     ;; 204 (no content) 304 (not modified)
	     (and status (memq (string->number status) '(204 304)))))
	;; very bad behaving server...
	(else (data-handler response-context (get-bytevector-all in) #t)
	      (http:response-body-state closed))))

(define (check-connection headers)
  ;; TODO we need to tell connection manager how long we can
  ;; let it alive...
  (cond ((rfc5322-header-ref headers "connection") =>
	 (lambda (v)
	   (if (string-contains v "keep-alive")
	       (http:response-body-state done)
	       (http:response-body-state closed))))
	(else (http:response-body-state closed))))

(define (read-one-line in)
  (let ((v (binary:get-line in)))
    (if (eof-object? v)
	v
	;; \r\n would be \r for binary:get-line so trim it
	(bytevector-trim-right v '(#x0d)))))
(define (parse-status-line line)
  (cond ((eof-object? line)
	 ;; TODO proper condition
	 (error 'parse-status-line "http reply contains no data"))
	((#/[\w\/.]+\s+(\d\d\d)\s+(.*)/ line)
	 => (lambda (m) (values (utf8->string (m 1)) (utf8->string (m 2)))))
	(else (error 'parse-status-line "bad reply from server" line))))

(define (ensure-read in size)
  (define buf (make-bytevector size))
  (let loop ((s 0) (size size))
    (let ((r (get-bytevector-n! in buf s size)))
      (if (= r size)
	  buf
	  (loop (+ s r) (- size r))))))

(define (read-chunked connection data-handler headers in)
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
	       (cond ((zero? size) (check-connection headers))
		     ((http-connection-data-ready? connection)
		      (read-chunked connection data-handler headers in))
		     (else (http:response-body-state continue))))))
	  (else (error 'read-chunked "bad line in chunked data" line)))))

(define (send-header! connection request)
  (define (send-first-line out request)
    (define method (http:request-method request))
    (http-connection-write-log connection
			       "[Request-Line] ~a ~a HTTP/1.1"
			       (symbol->string method)
			       (http:request->request-uri request))
    (put-bytevector out (string->utf8 (symbol->string method)))
    (put-bytevector out #*" ")
    (put-bytevector out (string->utf8 (http:request->request-uri request)))
    (put-bytevector out #*" HTTP/1.1\r\n"))

  (define (send-headers out request)
    (define method (http:request-method request))
    (define body (http:request-body request))
    (define headers (http:request-headers request))
    (define uri (http:request-uri request))
    (define (write-header out name value)
      (http-connection-write-log connection "[Request header] ~a: ~a" name value)
      (put-bytevector out (string->utf8 name))
      (put-bytevector out #*": ")
      (put-bytevector out (string->utf8 value))
      (put-bytevector out #*"\r\n"))
    
    (unless (http:no-body-method? method)
      (when body
	(write-header out "Content-Type" (http:request-content-type request))
	(cond ((bytevector? body)
	       (write-header out "Content-Length"
			     (number->string (bytevector-length body))))
	      ((port? body)
	       (write-header out "Transfer-Encoding" "chunked")))))
    
    (write-header out "Host"
		  (or (http:headers-ref headers "Host")
		      (uri-host uri)
		      (http-connection-node connection)))
    (unless (http:headers-ref headers "Connection")
      (write-header out "Connection" "keep-alive"))
    
    (for-each (lambda (name)
		(let ((small (string-downcase name)))
		  (unless (memp (lambda (n) (string=? small n))
				+http:managed-headers+)
		    (for-each (lambda (value) (write-header out name value))
			      (http:headers-ref* headers name)))))
	      (http:headers-names headers))
    (put-bytevector out #*"\r\n"))
  (let-values (((out e) (open-bytevector-output-port)))
    (send-first-line out request)
    (send-headers out request)
    (let ((out (http-connection-output connection)))
      (put-bytevector out (e))
      (flush-output-port out))))

(define (send-data! connection request)
  (define context (http-connection-context-data connection))
  (define out (http-connection-output connection))
  (define method (http:request-method request))
  (define body (http:request-body request))

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
)
