;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/authentications - SMTP authentication methods
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
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

;; the library contains actual authentication methods
(library (rfc smtp authentications)
    (export smtp-plain-authentication
	    smtp-login-authentication)
    (import (rnrs)
	    (sagittarius stty) 
	    (srfi :2)
	    (srfi :13)
	    (srfi :14)
	    (rfc base64))

;; SASL PLAIN thing
;; TODO make SASL library ant put it there
(define (smtp-plain-authentication userid password)
  (lambda ()
    (values "PLAIN"
	    (base64-encode-string (string-append userid "\x0;"
						 userid "\x0;"
						 password)))))

;; the data will be sent if in order if necessary
;; and consumed if it's sent
;; typical use case is passing username and password.
(define (smtp-login-authentication . data)
  (define next data)
  (define crlf (string->char-set "\r\n"))

  (define (read-input msg)
    (define (password? msg)
      (string-contains-ci msg "password"))
    (cond ((null? next)
	   (display msg (current-output-port))
	   (flush-output-port (current-output-port))
	   (if (password? msg)
	       (let ((r (with-stty '(not echo)
			   (lambda () (get-line (current-input-port))))))
		 ;; make it a bit nicier
		 (newline (current-output-port))
		 (string-trim-right r crlf))
	       (get-line (current-input-port))))
	  (else
	   (let ((r (car next)))
	     (set! next (cdr next))
	     r))))

  (define (prompt next?)
    (lambda (conn status resp)
      (or (and-let*  (( (= status 334) )
		      (msg (base64-decode-string resp)))
	    (values (base64-encode-string (read-input msg))
		    (and next? (prompt #f))))
	  (values #f #f))))

  (values (lambda () (values "LOGIN" #f)) (prompt #t)))

)
