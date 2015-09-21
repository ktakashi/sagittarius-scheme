;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/smtp/client.scm - SMTP client
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

(library (rfc smtp client)
    (export make-smtp-connection smtp-connection?
	    make-smtp-mail smtp-mail?
	    make-smtp-recipient smtp-recipient?
	    
	    ;; basic procedures
	    smtp-connect!
	    smtp-send!
	    smtp-disconnect!

	    ;; mail modification
	    smtp-mail-add-recipent
	    smtp-mail-add-header

	    ;; low level
	    smtp-mail->string)
    (import (rnrs)
	    (rfc smtp commands)
	    (rfc smtp extensions)
	    (rfc smtp conditions)
	    (srfi :106)
	    (srfi :112)
	    (rfc tls) ;; need socket-port for TLS
	    )

(define-record-type (<smtp-connection> make-smtp-connection smtp-connection?)
  (fields (immutable host smtp-connection-host)
	  (immutable port smtp-connection-port)
	  (immutable domain smtp-connection-domain) ;; computer name?
	  (mutable socket smtp-connection-socket smtp-connection-socket-set!)
	  (mutable in/out smtp-connection-in/out smtp-connection-in/out-set!)
	  (mutable options smtp-connection-options 
		   smtp-connection-options-set!))
  (protocol (lambda (p)
	      (lambda (host port . maybe-domain)
		(let ((domain (if (null? maybe-domain)
				  (machine-name)
				  (car maybe-domain))))
		  ;; we don't connect it here
		  (p host port domain #f #f '()))))))

(define-record-type (<smtp-mail> make-smtp-mail smtp-mail?)
  (fields (immutable from smtp-mail-from)
	  (immutable recipents smtp-mail-recipents)
	  (mutable headers smtp-mail-headers smtp-mail-headers-set!)
	  (mutable contents smtp-mail-contents smtp-mail-contents-set!)
	  (mutable attachments smtp-mail-attachments
		   smtp-mail-attachments-set!))
  (protocol (lambda (p)
	      ;; TODO maybe use case-lambda?
	      (lambda (from contents . recipents)
		(p from recipents '() contents '())))))

(define-record-type (<smtp-recipent> make-smtp-recipient smtp-recipient?)
  (fields (immutable type smtp-recipient-type)
	  (immutable address smtp-recipient-address)
	  (immutable name smtp-recipient-name))
  (protocol (lambda (p)
	      (lambda (type address . maybe-name)
		(let ((name (if (null? maybe-name) #f (car maybe-name))))
		  (p type address name))))))

(define (raise-smtp-error smtp who msg . irr)
  (raise (condition (list smtp
			  (make-who-condition who)
			  (make-message-condition msg)
			  (make-irritants-condition irr)))))

(define tr (make-transcoder (utf-8-codec) (eol-style none)))

(define (smtp-connect! conn)
  (define (parse-options resp)
    (let ((in (open-string-input-port resp)))
      (let loop ((r '()))
	(let ((line (get-line in)))
	  (if (eof-object? line)
	      r
	      (loop (cons line r)))))))
  (define (send-hello port domain)
    (smtp-ehlo port domain)
    (let loop ((ehlo #t))
      (let-values (((status resp) (smtp-recv port)))
	(cond ((= status 502)
	       (smtp-helo port (smtp-connection-domain conn))
	       (loop #f))
	      ((= status 250)
	       (if ehlo (parse-options resp) '()))
	      (else
	       (raise-smtp-error (make-smtp-error) 
				 'smtp-connect! "HELO failed" status))))))
  (let* ((socket (or (smtp-connection-socket conn)
		     (make-client-socket (smtp-connection-host conn)
					 (smtp-connection-port conn))))
	 (port (transcoded-port (socket-port socket #f) tr)))
    (let-values (((status resp) (smtp-recv port)))
      (unless (= status 220)
	(raise-smtp-error (make-smtp-error) 
			  'smtp-connect! "Greeting failed" status)))
    (let ((options (send-hello port (smtp-connection-domain conn))))
      (smtp-connection-options-set! conn options)
      (if (member "STARTTLS" options string=?)
	  (let ((socket (smtp-starttls port socket)))
	    (smtp-connection-socket-set! conn socket)
	    (smtp-connection-in/out-set! 
	     conn (transcoded-port (socket-port socket #f) tr)))
	  (begin
	    (smtp-connection-socket-set! conn socket)
	    (smtp-connection-in/out-set! conn port)))
      conn)))

;; Disconnect SMTP connection
;; close socket
(define (smtp-disconnect! conn)
  (let ((in/out (smtp-connection-in/out conn))
	(socket (smtp-connection-socket conn)))
    (smtp-quit in/out)
    (smtp-recv in/out)
    (smtp-connection-socket-set! conn #f)
    (smtp-connection-in/out-set! conn #f)
    (socket-shutdown socket *shut-rdwr*)
    (socket-close socket)
    conn))


)