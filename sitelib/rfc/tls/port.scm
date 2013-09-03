;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; tls.scm - TLS 1.0 - 1.2 protocol library.
;;;  
;;;   Copyright (c) 2010-2013  Takashi Kato  <ktakashi@ymail.com>
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

;; Caution this library is not well tested and not secure yet.
(library (rfc tls port)
    (export tls-socket-port socket-port
	    tls-socket-input-port socket-input-port
	    tls-socket-output-port socket-output-port)
    (import (rnrs)
	    (rfc tls socket)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius object)
	    (sagittarius control)
	    (clos user))

  ;; make custom port
  (define (%tls-socket-port socket ctr)
    (define (read! bv start count)
      (let1 buf (tls-socket-recv socket count 0)
	(if (eof-object? buf)
	    0
	    (let1 len (bytevector-length buf)
	      (bytevector-copy! buf 0 bv start len)
	      len))))
    (define (write! bv start count)
      ;; for sagittarius implementation bv starts always 0, but just in case
      (let ((buf (bytevector-copy bv start (+ start count))))
	(tls-socket-send socket bv 0))
      count)
    (define (close) (tls-socket-close socket))
    (define (ready?) 
      (let1 raw-socket (~ socket 'raw-socket)
	(receive (r w e) (socket-select (list raw-socket) '() '() 0)
	  (not (null? r)))))
    (ctr read! write! close ready?))

  (define (tls-socket-port socket :optional (close? #t))
    (%tls-socket-port socket
		      (lambda (read! write! close ready?)
			(make-custom-binary-input/output-port
			 "tls-socket-port"
			 read! write! #f #f (and close? close) ready?))))

  (define (tls-socket-input-port socket)
    (%tls-socket-port socket
		      (lambda (read! write! close? ready?)
			(make-custom-binary-input-port
			 "tls-socket-input-port"
			 read! #f #f #f ready?))))

  (define (tls-socket-output-port socket)
    (%tls-socket-port socket
		      (lambda (read! write! close? ready?)
			(make-custom-binary-output-port
			 "tls-socket-output-port"
			 write! #f #f #f))))

  (define-method socket-port ((sock <tls-socket>) :optional (close? #t))
    (tls-socket-port sock close?))

  (define-method socket-input-port ((sock <tls-socket>))
    (tls-socket-input-port sock))

  (define-method socket-output-port ((sock <tls-socket>))
    (tls-socket-output-port sock))
  )