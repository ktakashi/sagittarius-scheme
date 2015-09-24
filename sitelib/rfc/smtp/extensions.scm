;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/extensions.scm - SMTP extensions
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; This library only supports SMTP extensions what I need for now
(library (rfc smtp extensions)
    (export smtp-starttls
	    smtp-auth
	    smtp-plain-authentication)
    (import (rnrs)
	    (rfc tls)
	    (rfc smtp commands)
	    (rfc smtp conditions)
	    (rfc base64))

;; STARTTLS extension
;; reference 
;;   https://tools.ietf.org/html/rfc3207
;; this requires socket to convert...
;; assume the port is input-output-port
(define (smtp-starttls port socket)
  (put-string port "STARTTLS\r\n")
  (flush-output-port port)
  (let-values (((status content) (smtp-recv port)))
    (unless (= status 220)
      (raise (condition 
	      (make-smtp-error)
	      (make-who-condition 'smtp-starttls)
	      (make-message-condition content)
	      (make-irritants-condition status))))
    (tls-client-handshake (socket->tls-socket socket))))

;; AUTH extension
;; reference https://tools.ietf.org/html/rfc4954
(define (smtp-auth port initial-response-generator . maybe-next)
  (define (do-next next type status content)
    (let-values (((commend next) (next status content)))
      (put-string port commend)
      (put-string port "\r\n")
      (let-values (((status resp) (smtp-recv port)))
	(case status
	  ((235) (values status resp))
	  ((334) (if next (do-next next type status resp)))
	  (else (raise (condition
			(make-smtp-authentication-failure type)
			(make-who-condition 'smtp-auth)
			(make-message-condition resp)
			(make-irritants-condition status))))))))
  (let-values (((type initial-response) (initial-response-generator)))
    (put-string port "AUTH ")
    (put-string port type)
    ;; if the initial-response is #f then don't send
    (when initial-response
      (put-char port #\space)
      (put-string port initial-response))
    (put-string port "\r\n")
    (flush-output-port port)
    (let-values (((status content) (smtp-recv port)))
      (case status
	((235) (values status content))
	((334) (if (null? maybe-next)
		   (values status content)
		   (do-next (car maybe-next) type status content)))
	(else
	 (raise (condition
		 (make-smtp-authentication-failure type)
		 (make-who-condition 'smtp-auth)
		 (make-message-condition content)
		 (make-irritants-condition status))))))))

;; SASL PLAIN thing
;; TODO move to somewhere
(define (smtp-plain-authentication userid password)
  (lambda ()
    (values "PLAIN"
	    (base64-encode-string (string-append userid "\x0;"
						 userid "\x0;"
						 password)))))

)
