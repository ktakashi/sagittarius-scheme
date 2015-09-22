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
	    smtp-connection-options
	    smtp-connection-authentication-methods
	    smtp-authentication-required?

	    make-smtp-mail smtp-mail?
	    smtp-recipient?
	    make-smtp-from smtp-from?
	    make-smtp-to smtp-to?
	    make-smtp-cc smtp-cc?
	    make-smtp-bcc smtp-bcc?

	    ;; basic procedures
	    smtp-connect!
	    smtp-authenticate!
	    smtp-send!
	    smtp-disconnect!

	    ;; high level mail constructor
	    smtp:mail
	    ;; these are auxiliary syntax
	    smtp:subject
	    smtp:from
	    smtp:to
	    smtp:cc
	    smtp:bcc
	    smtp:attachment

	    ;; re-export
	    smtp-plain-authentication

	    ;; mail modification
	    smtp-mail-add-recipent!
	    smtp-mail-add-header!
	    smtp-mail-add-attachment!

	    make-smtp-mail-attachment
	    ;; low level
	    smtp-mail->string)
    (import (rnrs)
	    (rfc smtp commands)
	    (rfc smtp extensions)
	    (rfc smtp conditions)
	    (srfi :13)
	    (srfi :106)
	    (srfi :112)
	    (rfc tls) ;; need socket-port for TLS
	    (rfc mime) ;; for attachments
	    (rfc quoted-printable)
	    )

(define-record-type (<smtp-connection> make-smtp-connection smtp-connection?)
  (fields (immutable host smtp-connection-host)
	  (immutable port smtp-connection-port)
	  (immutable domain smtp-connection-domain) ;; computer name?
	  (mutable socket smtp-connection-socket smtp-connection-socket-set!)
	  (mutable in/out smtp-connection-in/out smtp-connection-in/out-set!)
	  (mutable options smtp-connection-options 
		   smtp-connection-options-set!)
	  (mutable authentication-methods
		   smtp-connection-authentication-methods
		   smtp-connection-authentication-methods-set!))
  (protocol (lambda (p)
	      (lambda (host port . maybe-domain)
		(let ((domain (if (null? maybe-domain)
				  (machine-name)
				  (car maybe-domain))))
		  ;; we don't connect it here
		  (p host port domain #f #f '() '()))))))

(define-record-type (<smtp-mail> make-smtp-mail smtp-mail?)
  (fields (immutable from smtp-mail-from)
	  (mutable recipents smtp-mail-recipents smtp-mail-recipents-set!)
	  (mutable headers smtp-mail-headers smtp-mail-headers-set!)
	  (mutable subject smtp-mail-subject smtp-mail-subject-set!)
	  (mutable contents smtp-mail-contents smtp-mail-contents-set!)
	  (mutable attachments smtp-mail-attachments
		   smtp-mail-attachments-set!))
  (protocol (lambda (p)
	      ;; TODO maybe use case-lambda?
	      (lambda (from subject contents . recipients)
		(unless (smtp-from? from)
		  (assertion-violation 'make-smtp-mail
				       "<smtp-from> is required" from))
		(unless (for-all smtp-recipient? recipients)
		  (assertion-violation 'make-smtp-mail
				       "<smtp-recipient>s are required"
				       recipients))
		(p from recipients '() subject contents '())))))

(define-record-type (<smtp-address> make-smtp-address smtp-address?)
  (fields (immutable type smtp-address-type)
	  (immutable email smtp-address-email)
	  (immutable name smtp-address-name))
  (protocol (lambda (p)
	      (lambda (type email name)
		(p type email name)))))

(define-record-type (<smtp-recipipent> make-smtp-recipient smtp-recipient?)
  (parent <smtp-address>)
    (protocol (lambda (p) (lambda (t e m) ((p t e m))))))

(define-syntax define-recipient
  (lambda (x)
    (define (make-record-definitions k type)
      (let* ((s (symbol->string (syntax->datum type)))
	     (r (string->symbol (string-append "<smtp-" s ">")))
	     (c (string->symbol (string-append "make-smtp-" s)))
	     (p (string->symbol (string-append "smtp-" s "?"))))
	(datum->syntax k (list r c p))))
    (define (make-header k type)
      (let* ((s (symbol->string (syntax->datum type)))
	     (h (string-append (string-titlecase s) ": ")))
	(datum->syntax k h)))
    (syntax-case x ()
      ((k type p)
       (with-syntax (((record ctr pred) (make-record-definitions #'k #'type))
		     (head (make-header #'k #'type)))
	 #'(define-record-type (record ctr pred)
	     (parent p)
	     (protocol 
	      (lambda (p)
		(case-lambda
		 ((address) ((p head address #f)))
		 ((name address) ((p head address name))))))))))))

(define-recipient from <smtp-address>)
(define-recipient to <smtp-recipipent>)
(define-recipient cc <smtp-recipipent>)
(define-recipient bcc <smtp-recipipent>)

(define (smtp-mail-add-recipent! mail recipient)
  (unless (smtp-recipient? recipient)
    (assertion-violation 'smtp-mail-add-recipent 
			 "<smtp-recipient> is required" recipient))
  (let ((old (smtp-mail-recipents mail)))
    (smtp-mail-recipents-set! mail (cons recipient old))
    mail))

(define (smtp-mail-add-header! mail name value)
  (define reject-headers '("from" "to" "cc" "bcc" "subject"))
  (unless (and (string? name) (string? value))
    (assertion-violation 'smtp-mail-add-header!
			 "name and value must be strings" name value))
  ;; reject some of headers
  (when (member name reject-headers string-ci=?)
    (assertion-violation 'smtp-mail-add-header!
			 "specified header must be set by other procedures"
			 name))
  (let ((old (smtp-mail-headers mail)))
    (smtp-mail-headers-set! mail (cons (cons name value) old))
    mail))

(define (smtp-mail-add-attachment! mail mime)
  (unless (mime-part? mime)
    (assertion-violation 'smtp-mail-add-header!
			 "attachment must be mime-part" mime))

  (let ((old (smtp-mail-attachments mail)))
    (smtp-mail-attachments-set! mail (cons mime old))
    mail))

(define (raise-smtp-error smtp who msg . irr)
  (raise (condition (list smtp
			  (make-who-condition who)
			  (make-message-condition msg)
			  (make-irritants-condition irr)))))

(define tr (make-transcoder (utf-8-codec) (eol-style none)))

(define (smtp-authentication-required? conn)
  (not (null? (smtp-connection-authentication-methods conn))))

(define (smtp-connect! conn)

  (define (auth-methods? line)
    (define (auth? s) (string-prefix? "AUTH" s))
    (cond ((auth? line) (map string->symbol (cdr (string-tokenize line))))
	  (else #f)))

  (define (parse-options conn resp)
    (let ((in (open-string-input-port resp)))
      (let loop ((r '()))
	(let ((line (get-line in)))
	  (cond ((eof-object? line) r)
		((auth-methods? line) =>
		 (lambda (methods)
		   (smtp-connection-authentication-methods-set! conn methods)
		   (loop r)))
		(else (loop (cons line r))))))))
  (define (send-hello conn port domain)
    (smtp-ehlo port domain)
    (let loop ((ehlo #t))
      (let-values (((status resp) (smtp-recv port)))
	(cond ((= status 502)
	       (smtp-helo port (smtp-connection-domain conn))
	       (loop #f))
	      ((= status 250)
	       (if ehlo (parse-options conn resp) '()))
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
    (let* ((domain (smtp-connection-domain conn))
	   (options (send-hello conn port domain)))
      (if (member "STARTTLS" options string=?)
	  (let* ((socket (smtp-starttls port socket))
		 (port (transcoded-port (socket-port socket #f) tr)))
	    (smtp-connection-socket-set! conn socket)
	    (smtp-connection-in/out-set! conn port)
	    ;; ok do ehlo again
	    (smtp-connection-options-set! conn (send-hello conn port domain)))
	  (begin
	    (smtp-connection-socket-set! conn socket)
	    (smtp-connection-in/out-set! conn port)
	    (smtp-connection-options-set! conn options)))
      conn)))

(define (smtp-authenticate! conn gen-initial-response . maybe-next)
  (let-values (((status resp)
		(smtp-auth (smtp-connection-in/out conn) gen-initial-response)))
    (or (= status 235)
	(and (not (null? maybe-next))
	     ((car maybe-next) conn resp)))))

(define (smtp-send! conn mail)
  (define (has-8bitmime? options)
    ;; some SMTP server return "8 BITMIME", don't know why
    (find (lambda (s) (or (string=? s "8BITMIME") (string=? s "8 BITMIME")))
	  options))
  (define (address->postmaster a)
    (string-append "<" (smtp-address-email a) ">"))
  (define (check-status port status)
    (let-values (((rstatus resp) (smtp-recv port)))
      (unless (= rstatus status)
	(raise-smtp-error (make-smtp-error) 'smtp-send!
			  resp rstatus))))
  (let* ((data (smtp-mail->string mail)) ;; let it check here before we send
	 (8bitmime? (has-8bitmime? (smtp-connection-options conn)))
	 (port (smtp-connection-in/out conn))
	 (from (smtp-mail-from mail))
	 (recipents (smtp-mail-recipents mail)))
    ;; TODO PIPELINING
    (apply smtp-mail port (address->postmaster from) 
	   (if 8bitmime? '("BODY=8BITMIME") '()))
    (check-status port 250)
    (for-each (lambda (r)
		(smtp-rcpt port (address->postmaster r))
		(check-status port 250)) recipents)
    (smtp-data port)
    (check-status port 354)
    (smtp-send-data port (open-string-input-port data))
    (check-status port 250)
    #t))

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

(define (smtp-address->string address)
  (let-values (((out extract) (open-string-output-port)))
    (put-string out (smtp-address-type address))
    (let ((name (smtp-address-name address))
	  (email (smtp-address-email address)))
      (when name (put-string out name))
      (put-char out #\<)
      (put-string out email)
      (put-char out #\>))
    (extract)))

(define (smtp-mail->string mail)
  (define qpes quoted-printable-encode-string)
  (let ((from (smtp-mail-from mail))
	(recipents (smtp-mail-recipents mail))
	(headers (smtp-mail-headers mail))
	(subject (smtp-mail-subject mail))
	(content (smtp-mail-contents mail))
	(attachs (smtp-mail-attachments mail)))
    (when (null? recipents)
      (assertion-violation 'smtp-mail->string
			   "At least one recipient is required" mail))
    (unless (for-all mime-part? attachs)
      (assertion-violation 'smtp-mail->string
			   "Attachments must be mime-part" attachs))
    (let-values (((out extract) (open-string-output-port)))
      (put-string out (smtp-address->string from)) (put-string out "\r\n")
      (for-each (lambda (r)
		  (put-string out (smtp-address->string r))
		  (put-string out "\r\n")) recipents)
      ;; TODO handling multipart
      (for-each (lambda (h)
		  (put-string out (car h))
		  (put-string out ": ")
		  (put-string out (cdr h))
		  (put-string out "\r\n"))
		headers)
      ;; subject
      (put-string out "Subject: ")
      (put-string out  subject)
      (put-string out "\r\n")
      (cond ((null? attachs)
	     (put-string out "\r\n")
	     (put-string out content))
	    (else 
	     (let-values (((body boundary)
			   (mime-compose-message-string
			    (cons (make-mime-part
				   :content (qpes content)
				   :transfer-encoding "quoted-printable")
				  attachs))))
	       (put-string out "Mime-Version: 1.0\r\n")
	       (put-string out "Content-Type: multipart/mixed; boundary=\"")
	       (put-string out boundary)
	       (put-string out "\"\r\n")
	       (put-string out body))))
      (extract))))

(define (make-smtp-mail-attachment type subtype content . maybe-filename)
  (let ((filename (if (null? maybe-filename)
		      "attachment"
		      (string-append "attachment;filename=\""
				     (car maybe-filename) "\""))))
    (make-mime-part
     :type "text" :subtype "html"
     :transfer-encoding "base64"
     :content content
     :headers `(("content-disposition" ,filename)))))

;; High level API
(define-syntax smtp:subject    (syntax-rules ()))
(define-syntax smtp:to         (syntax-rules ()))
(define-syntax smtp:cc         (syntax-rules ()))
(define-syntax smtp:bcc        (syntax-rules ()))
(define-syntax smtp:from       (syntax-rules ()))
(define-syntax smtp:attachment (syntax-rules ()))

(define-syntax smtp:mail
  (syntax-rules (smtp:subject smtp:to smtp:cc smtp:bcc smtp:attachment)
    ;; parse subject
    ((_ "parse" from "no subject" content (recp ...) (attach ...)
	((smtp:subject sub) elements ...))
     (smtp:mail "parse" from sub content 
		(recp ...) (attach ...) (elements ...)))
    ;; TODO we want to generate these 3 somehow
    ;; parse to
    ((_ "parse" from subject content (recp ...) (attach ...)
	((smtp:to to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-to to ...))
		(attach ...) (elements ...)))
    ;; parse cc
    ((_ "parse" from subject content (recp ...) (attach ...)
	((smtp:cc to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-cc to ...))
		(attach ...) (elements ...)))
    ;; parse bcc
    ((_ "parse" from subject content (recp ...) (attach ...)
	((smtp:bcc to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-bcc to ...))
		(attach ...) (elements ...)))
    ;; parse attachment
    ((_ "parse" from subject content (recp ...) (attach ...)
	((smtp:attachment spec ...) elements ...))
     (smtp:mail "parse" from subject content (recp ...)
		((make-smtp-mail-attachment spec ...) attach ...)
		(elements ...)))
    ;; parse content
    ((_ "parse" from subject "" (recp ...) (attach ...)
	(e elements ...))
     (smtp:mail "parse" from subject e
		(recp ...)
		(attach ...) 
		(elements ...)))
    ((_ "parse" from subject content (recp ...) (attach ...)
	(e elements ...))
     (smtp:mail "parse" from subject (string-append content "\r\n" e)
		(recp ...)
		(attach ...) 
		(elements ...)))
    ;; finish
    ((_ "parse" from subject content (recp ...) (attach ...) ())
     (let ((mail (make-smtp-mail from subject content recp ...)))
       (smtp-mail-add-attachment! mail attach) ...
       mail))
    ;; entry point
    ((_ (smtp:from from ...) elements ...)
     (smtp:mail "parse" 
		(make-smtp-from from ...)
		"no subject" "" () () (elements ...)))))



)
