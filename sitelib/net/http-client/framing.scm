;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/framing.scm - HTTP/1.x framing helpers for HTTP client
;;;  
;;;   Copyright (c) 2026  Takashi Kato  <ktakashi@ymail.com>
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
(library (net http-client framing)
    (export http:header-tokens
	    http:header-has-token?
	    http:last-transfer-coding
	    http:response-framing
	    http:connection-reusable-after?
	    http:make-chunk-reader)
    (import (rnrs)
	    (net socket)
	    (sagittarius)
	    (sagittarius regex)
	    (rfc :5322)
	    (rename (srfi :1 lists) (last last-item))
	    (srfi :13 strings)
	    (prefix (binary io) binary:)
	    (util bytevector)
	    (net http-client conditions))

(define (normalize-token token)
  (string-downcase (string-trim-both token)))

;; Split one header field into normalized comma separated tokens.
(define (http:header-tokens value)
  (cond ((not value) '())
	((string? value)
	 (filter (lambda (token) (not (string-null? token)))
		 (map normalize-token (string-split value #/\s*,\s*/))))
	(else
	 (raise-http-protocol-error 'http:header-tokens
				    "Header value must be string"
				    value))))

;; Check if the normalized header value has the given token.
(define (http:header-has-token? value token)
  (let* ((needle (string-downcase token))
	 (tokens (http:header-tokens value)))
    (and (memp (lambda (e) (string=? e needle)) tokens) #t)))

;; Return the last normalized transfer coding token.
(define (http:last-transfer-coding value)
  (last-item (http:header-tokens value)))

(define (header->tokens headers field-name)
  (append-map http:header-tokens (rfc5322-header-ref* headers field-name)))

(define (headers-has-token? headers field-name token)
  (let ((needle (string-downcase token)))
    (and (memp (lambda (e) (string=? e needle))
	       (header->tokens headers field-name))
	 #t)))

(define (status-code status)
  (and status (string->number status)))

(define (no-body-response? status method)
  (define code (status-code status))
  (or (eq? method 'HEAD)
      (and code (<= 100 code 199))
      (and code (memq code '(204 304)))))

(define (validate-content-length values)
  (define (parse-length token)
    (unless (#/^[0-9]+$/ token)
      (raise-http-protocol-error 'http:response-framing
				 "Invalid Content-Length"
				 token values))
    (string->number token))
  (let loop ((tokens values) (size #f))
    (cond ((null? tokens)
	   (or size
	       (raise-http-protocol-error 'http:response-framing
		 "Invalid Content-Length"
		 values)))
	  (else
	   (let ((n (parse-length (car tokens))))
	     (cond ((not size) (loop (cdr tokens) n))
		   ((= size n) (loop (cdr tokens) size))
		   (else
		    (raise-http-protocol-error 'http:response-framing
		      "Mismatched Content-Length values"
		      values))))))))

(define (http:response-framing status method headers version)
  (define te-values (rfc5322-header-ref* headers "transfer-encoding"))
  (define te-tokens (header->tokens headers "transfer-encoding"))
  (define cl-values (header->tokens headers "content-length"))
  (if (no-body-response? status method)
      (values 'none #f)
      (cond ((pair? te-values)
	     (let ((last-coding (last-item te-tokens)))
	       (unless (and last-coding (string=? last-coding "chunked"))
		 (raise-http-protocol-error 'http:response-framing
		   "Transfer-Encoding must end with chunked"
		   te-values))
	       (values 'chunked #f)))
	    ((pair? cl-values)
	     (values 'length (validate-content-length cl-values)))
	    (else (values 'until-close #f)))))

(define (http:connection-reusable-after? headers version)
  (if (string=? version "1.0")
      (headers-has-token? headers "connection" "keep-alive")
      (not (headers-has-token? headers "connection" "close"))))

(define (read-one-line in)
  (let ((v (binary:get-line in)))
    (if (eof-object? v)
	v
	(bytevector-trim-right v '(#x0d)))))

(define (ensure-read in size who)
  (define buf (make-bytevector size))
  (let loop ((s 0) (remaining size))
    (let ((r (get-bytevector-n! in buf s remaining)))
      (cond ((eof-object? r)
	     (raise-http-connection-error who "Unexpected EOF from the server"
					  size))
	    ((= r remaining) buf)
	    (else (loop (+ s r) (- remaining r)))))))

(define (ensure-crlf in who)
  (let ((crlf (ensure-read in 2 who)))
    (unless (bytevector=? crlf #*"\r\n")
      (raise-http-protocol-error who "Missing CRLF after chunk data" crlf))))

;; Return a resumable chunk reader procedure.
(define (http:make-chunk-reader)
  (let ((state 'size)
	(size 0)
	(done? #f))
    (lambda (in data-handler)
      (let loop ()
	(cond (done? 'done)
	      ((eq? state 'size)
	       (let ((line (read-one-line in)))
		 (when (eof-object? line)
		   (raise-http-connection-error 'read-chunked
		     "Chunked body ended prematurely"))
		 (cond ((#/^([0-9a-fA-F]+)(;.*)?$/ line) =>
			(lambda (m)
			  (set! size (string->number (utf8->string (m 1)) 16))
			  (set! state (if (zero? size) 'trailer 'data))
			  (loop)))
		       (else
			(raise-http-protocol-error 'read-chunked
						   "Bad line in chunked data"
						   line)))))
	      ((eq? state 'data)
	       (data-handler (ensure-read in size 'read-chunked) #f)
	       (set! state 'chunk-crlf)
	       (loop))
	      ((eq? state 'chunk-crlf)
	       (ensure-crlf in 'read-chunked)
	       (set! state 'size)
	       (if (port-ready? in)
		   (loop)
		   'continue))
	      ((eq? state 'trailer)
	       (rfc5322-read-headers in)
	       (data-handler #vu8() #t)
	       (set! done? #t)
	       'done)
	      (else
	       (raise-http-protocol-error 'http:make-chunk-reader
					  "Unknown chunk reader state"
					  state)))))))

)
