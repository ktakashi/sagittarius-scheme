;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; example/websocket/chat-server.scm - WebSocket chat server
;;;  
;;;   Copyright (c) 2010-2016  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/bv-string
#!read-macro=sagittarius/regex
(import (rnrs)
	(sagittarius socket)
	(sagittarius regex)
	(rfc websocket messages)
	(net server)
	(prefix (binary io) binary:)
	(srfi :18)
	(rfc :5322)
	(rfc base64)
	(math hash)
	(getopt))

(define (put-bytevector* out bv . bvs)
  (put-bytevector out bv)
  (for-each (lambda (bv) (put-bytevector out bv)) bvs))

(define *uuid* #*"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(define (make-chat-server port)
  (define (calculate-key headers)
    (let ((key (rfc5322-header-ref headers "Sec-WebSocket-Key")))
      (base64-encode
       (hash SHA-1 (bytevector-append (string->utf8 key) *uuid*)))))

  (define (send-close out)
    (put-bytevector out #vu8(#x88 #x00))
    (flush-output-port out))

  ;; path : list
  (define chat-members (make-string-hashtable))
  ;; socket : path
  (define managed-sockets (make-eq-hashtable))

  (define (handshake socket in/out)
    (define (parse-request-line line)
      ;; use [[:graph:]] for my laziness
      (cond ((#/GET\s+([[:graph:]]+?)\s+HTTP\/1.1/ line) =>
	     (lambda (m) (utf8->string (m 1))))
	    (else #t)))
    (define (check-headers headers)
      (define (check-header field expected)
	(equal? (rfc5322-header-ref headers field) expected))
      (and (check-header "Connection" "Upgrade")
	   (check-header "Upgrade" "websocket")
	   (check-header "Sec-WebSocket-Version" "13")
	   (rfc5322-header-ref headers "Sec-WebSocket-Key")))
    
    (let* ((path (parse-request-line (binary:get-line in/out :eol #*"\r\n")))
	   (headers (rfc5322-read-headers in/out)))
      (if (and path (check-headers headers))
	  (let* ((key (calculate-key headers)))
	    (put-bytevector* in/out #*"HTTP/1.1 101 Switch protocol\r\n")
	    (put-bytevector* in/out #*"Upgrade: websocket\r\n")
	    (put-bytevector* in/out #*"Connection: Upgrade\r\n")
	    (put-bytevector* in/out #*"Sec-WebSocket-Accept: " key #*"\r\n")
	    (put-bytevector* in/out #*"\r\n")
	    (flush-output-port in/out)
	    (hashtable-set! managed-sockets socket path)
	    (hashtable-update! chat-members path
			       (lambda (v) (cons socket v))
			       '()))
	  (put-bytevector* in/out #*"HTTP/1.1 400 Bad request\r\n"))))

  (define (cleanup socket)
    (let ((p (hashtable-ref managed-sockets socket #f)))
      (hashtable-delete! managed-sockets socket)
      (when p
	(hashtable-update! chat-members p (lambda (v) (remq socket v)) '())))
    (socket-shutdown socket SHUT_RDWR)
    (socket-close socket))

  (define (broad-cast op data me sockets)
    (define (send-it socket)
      (unless (eq? socket me)
	(let ((out (buffered-port (socket-port socket #f) (buffer-mode block))))
	  (websocket-send-frame! out op #t data #t))))
    (for-each send-it sockets))

  (define (websocket-handler server socket)
    (define in/out (buffered-port (socket-port socket #f) (buffer-mode block)))
    (guard (e (else (cleanup socket)))
      (if (hashtable-contains? managed-sockets socket)
	  (let-values (((fin? op data) (websocket-recv-frame in/out)))
	    (cond ((= op +websocket-close-frame+)
		   (send-close in/out)
		   (cleanup socket))
		  ((or (= op +websocket-text-frame+)
		       (= op +websocket-binary-frame+))
		   (let* ((path (hashtable-ref managed-sockets socket))
			  (sockets (hashtable-ref chat-members path '())))
		     (broad-cast op data socket sockets)))
		  ((= op +websocket-ping-frame+)
		   (websocket-send-frame! in/out +websocket-pong-frame+ #f
					  data #t))
		  (else
		   (websocket-send-frame! in/out +websocket-close-frame+ #t
					  (websocket-compose-close-status 1002)
					  #t)
		   (cleanup socket))))
	  (handshake socket in/out)))
    ;; close the port. this is needed to prevent buffered-port flush
    ;; during error reporting.
    (close-port in/out))

  (define config (make-server-config :non-blocking? #t :use-ipv6? #t
				     :exception-handler print))
  (make-simple-server port websocket-handler :config config))

(define (main args)
  (with-args (cdr args)
      ((port (#\p "port") #t "80"))
    (let ((server (make-chat-server port)))
      (print "Chat server is starting on port: " port)
      (server-start! server))))
