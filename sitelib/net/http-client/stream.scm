;;; -*- mode:scheme;coding:utf-8 -*-
;;;
;;; net/http-client/stream.scm - HTTP stream response
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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
(library (net http-client stream)
    (export http:stream-response? make-http:stream-response
	    http:stream-response-socket ;; for socket selector
	    http:stream-response-close!

	    require-stream-response?)
    (import (rnrs)
	    (binary io)
	    (record builder)
	    (rfc :5322)
	    (net http-client connection)
	    (net http-client encoding)
	    (net http-client request))
(define-record-type http:stream-response
  (parent <http:response>)
  (fields connection)
  (protocol (lambda (n)
	      (lambda (request status headers cookies time conn)
		(define input (connection->input-port request headers conn))
		((n status headers cookies input time) conn)))))

(define (http:stream-response-socket (response (http:stream-response?)))
  (http-connection-socket (http:stream-response-connection response)))

(define (http:stream-response-close! (response (http:stream-response?)))
  (http-connection-close! (http:stream-response-connection response)))

;; HTTP SSE
(define (require-stream-response? headers)
  (equal? (rfc5322-header-ref headers "content-type" "application/octet-stream")
	  "text/event-stream"))

(define (connection->input-port request headers connection)
  ;; we don't use header-handler
  (define (header-handler #:_ #:_ #:_ #:_) #t)
  (define buffer (open-chunked-binary-input/output-port))
  (define encoding (or (http:headers-ref headers "content-encoding") "none"))
  (define sink (->decoding-output-port buffer encoding))
  (define data-end? #f)
  (define (data-handler ctx data end?)
    (put-bytevector sink data)
    (when end? (set! data-end? end?) (close-port sink)))
  (define response-context
    (make-http:response-context request header-handler data-handler))

  (define (fill!)
    (chunked-binary-input/output-port-clear-buffer! buffer)
    (http-connection-receive-data! connection response-context)
    (set-port-position! buffer 0))

  (define (read! bv start count)
    (let loop ((offset start) (n count) (read 0))
      (let ((r (get-bytevector-n! buffer bv offset n)))
	(cond ((eof-object? r)
	       (or (and data-end? read)
		   (and (fill!) (loop offset n read))))
	      ((< r n)
	       (cond (data-end? (+ read r))
		     (else (fill!) (loop (- n r) (+ offset r) (+ read r)))))
	      (else (+ read r))))))
  ;; the underlying connection must be closed by stream-response-close!
  (define (close) #t)
  (make-custom-binary-input-port "stream-response-input-port" read! #f #f close))


)
