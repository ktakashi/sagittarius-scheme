;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; example/websocket/chat-client.scm - WebSocket chat client
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

(import (rnrs)
	(sagittarius)
	(rfc websocket)
	(rfc uri)
	(getopt))

(define (connect-server host port path)
  (let* ((uri (uri-compose :scheme "ws" :host host :port port :path path)))
    (websocket-open
     (websocket-on-binary-message
      (websocket-on-text-message
       (websocket-on-error
	(websocket-on-open (make-websocket uri)
	 (lambda (ws) (print "CONNECTED!")))
	(lambda (ws e) (print "ERROR: " e)))
       (lambda (ws text) (newline) (print text) (format #t "> ~!")))
      (lambda (ws bin) (put-bytevector (standart-output-port) bin))))))

(define (chat ws)
  (let loop ()
    (format #t "> ~!")
    (let ((line (get-line (current-input-port))))
      (cond ((eof-object? line)
	     (websocket-close ws)
	     (display "Bye") (newline))
	    (else
	     (websocket-send ws line)
	     (loop))))))

(define (main args)
  (with-args (cdr args)
      ((port (#\p "port") #t "80")
       (host (#\h "host") #t "localhost")
       (path (#\r "room") #t "/chat"))
    (chat (connect-server host port path))))
