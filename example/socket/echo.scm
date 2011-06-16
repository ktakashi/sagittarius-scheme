;;; -*- scheme -*-
;;;
;;;   echo.scm: simple echo server for sample
;;;
;;;   Copyright (c) 2000-2011  Takashi Kato  <ktakashi@ymail.com>
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

;; import socket library
(import (sagittarius socket))

;; creates echo server socket with port number 5000
(define echo-server-socket (make-server-socket "5000"))

;; addr is client socket
(let loop ((addr (socket-accept echo-server-socket)))
  (call-with-socket
   addr
   (lambda (sock)
     ;; socket-port creates binary input/output port
     ;; make it transcoded port for convenience.
     (let ((p (transcoded-port (socket-port sock)
			       ;; on Sagittarius Scheme native-transcoder
			       ;; uses utf8 codec for ASCII compatibility.
			       ;; For socket programming it might be better
			       ;; to specify eol-style with crlf.
			       ;; But this sample just shows how it goes.
			       (native-transcoder))))
       (call-with-port
	p
	(lambda (p)
	  (put-string p "please type something\n\r")
	  (put-string p "> ")
	  ;; gets line from client.
	  (let lp2 ((r (get-line p)))
	    (unless (eof-object? r)
	      (print "received: " r)
	      ;; just returns message from client.
	      ;; NB: If client type nothing, it'll throw assertion-violation.
	      (put-string p r)
	      (put-string p "\r\n> ")
	      ;; waits for next input.
	      (lp2 (get-line p)))))))))
  ;; echo server waits next connection.
  (loop (socket-accept echo-server-socket)))