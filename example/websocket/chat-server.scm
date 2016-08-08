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
	(sagittarius)
	(sagittarius socket)
	(sagittarius regex)
	(rfc websocket connection)
	(rfc websocket messages)
	(net server)
	(prefix (binary io) binary:)
	(srfi :18)
	(rfc :5322)
	(rfc base64)
	(math hash)
	(getopt))

(define (make-chat-server port)
  ;; path : list
  (define chat-members (make-string-hashtable))
  ;; socket : path
  (define managed-sockets (make-eq-hashtable))

  (define (handshake socket)
    (define (socket-read-line socket)
      (define buf (make-bytevector 1 0))
      (define (get-u8 sokcket) 
	(socket-recv! socket buf 0 1)
	(bytevector-u8-ref buf 0))
      (let-values (((out extract) (open-bytevector-output-port)))
	(let loop ((u8 (get-u8 socket)) (cr #f))
	  (cond ((= u8 #x0d) (loop (get-u8 socket) #t))
		((= u8 #x0a)
		 (cond (cr (extract))
		       (else
			(put-bytevector out #*"\r\n")
			(loop (get-u8 socket) #f))))
		(else (put-u8 out u8) (loop (get-u8 socket) #f))))))

    (define (parse-request-line line)
      ;; use [[:graph:]] for my laziness
      (cond ((#/GET\s+([[:graph:]]+?)\s+HTTP\/1.1/ line) =>
	     (lambda (m) (utf8->string (m 1))))
	    (else #t)))
    (define (close-socket socket response)
      (socket-send socket response)
      (socket-shutdown socket SHUT_RDWR)
      (socket-close socket))
    
    (guard (e ((websocket-engine-error? e)
	       (close-socket socket #*"HTTP/1.1 400 Bad request\r\n\r\n"))
	      (else
	       (close-socket socket
		#*"HTTP/1.1 500 Internal server error\r\n\r\n")))
      (let ((path (parse-request-line (socket-read-line socket))))
	(if path
	    (let ((conn (server-socket->websocket-connection socket)))
	      (websocket-connection-accept-handshake! conn)
	      (hashtable-set! managed-sockets socket (cons path conn))
	      (hashtable-update! chat-members path 
				 (lambda (v) (cons conn v)) '()))
	    (close-socket socket #*"HTTP/1.1 400 Bad request\r\n\r\n")))))

  (define (cleanup socket)
    (and-let* ((p&c (hashtable-ref managed-sockets socket #f)))
      (hashtable-delete! managed-sockets socket)
      (when p&c
	(hashtable-update! chat-members (car p&c)
			   (lambda (v) (remq (cdr p&c) v)) '()))
      (websocket-connection-close! (cdr p&c))))

  (define (broad-cast op data me conns)
    (define (send-it conn)
      (unless (eq? conn me)
	(if (string? data)
	    (websocket-send-text conn data)
	    (websocket-send-binary conn data))))
    (for-each send-it conns))

  (define (websocket-handler server socket)
    (guard (e (else (report-error e) (cleanup socket)))
      (cond ((hashtable-ref managed-sockets socket #f) =>
	     (lambda (p&c)
	       (let-values (((op data) (websocket-receive (cdr p&c))))
		 (cond ((= op +websocket-close-frame+) (cleanup socket))
		       ((or (= op +websocket-text-frame+)
			    (= op +websocket-binary-frame+))
			(let* ((path (car p&c))
			       (conns (hashtable-ref chat-members path '())))
			  (broad-cast op data (cdr p&c) conns)))
		       (else
			;; unknown
			(websocket-send-close (cdr p&c)
			  (websocket-compose-close-status 1002) #f)
			(cleanup socket))))))
	    (else (handshake socket)))))

  (define config (make-server-config :non-blocking? #t :use-ipv6? #t
				     :exception-handler (lambda (sr so e)
							  (report-error e))))
  (make-simple-server port websocket-handler :config config))

(define (main args)
  (with-args (cdr args)
      ((port (#\p "port") #t "80"))
    (let ((server (make-chat-server port)))
      (print "Chat server is starting on port: " port)
      (server-start! server))))
