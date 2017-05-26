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

#!read-macro=sagittarius/regex
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
	    smtp:header
	    smtp:alternative

	    ;; re-export
	    smtp-plain-authentication
	    smtp-login-authentication

	    ;; mail modification
	    smtp-mail-add-recipent!
	    smtp-mail-add-header!
	    smtp-mail-add-attachment!

	    make-smtp-attachment
	    make-smtp-alternative-component
	    make-smtp-alternative
	    ;; low level
	    smtp-mail->string
	    smtp-address->string
	    string->smtp-address
	    )
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (rfc smtp commands)
	    (rfc smtp extensions)
	    (rfc smtp conditions)
	    (rfc smtp authentications)
	    (srfi :1) ;; for alist-delete
	    (srfi :13)
	    (srfi :14)
	    (srfi :106)
	    (srfi :112)
	    (rfc tls) ;; need socket-port for TLS
	    (rfc mime) ;; for attachments
	    (rfc quoted-printable)
	    (rfc :5322)
	    (sagittarius regex)
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
  (let ((old (smtp-mail-headers mail))
	(value (if (string-every char-set:ascii value)
		   value
		   (mime-encode-word value))))
    (cond ((assoc name old string-ci=?) =>
	   ;; Should we raise an error?
	   (lambda (slot) (set-cdr! slot (list value))))
	  (else
	   (smtp-mail-headers-set! mail (cons (list name value) old))))
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
  (define (wrap-next next)
    (lambda (status content)
      (let-values (((command next-next) (next conn status content)))
	(if next-next
	    (values command (wrap-next next-next))
	    (values command #f)))))
  
  (apply smtp-auth (smtp-connection-in/out conn) gen-initial-response
	 (if (null? maybe-next)
	     '()
	     (list (wrap-next (car maybe-next))))))

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
    conn))

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

(define (smtp-send-command conn s)
  (let ((in/out (smtp-connection-in/out conn)))
    (put-string in/out s)))
;; wrapper for smtp-recv
(define (smtp-recv-response conn)
  (let ((in/out (smtp-connection-in/out conn)))
    (smtp-recv in/out)))

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

(define (string->smtp-address ctr str)
  (cond ((#/([^<>]+)<(.+)>/ str) => (lambda (m) (ctr (m 1) (m 2))))
	(else (ctr str))))

(define (smtp-mail->string mail)
  (define qpes quoted-printable-encode-string)

  (define (handle-multipart out headers content attachs)
    (let*-values (((type subtype) 
		   (cond ((assoc "content-type" headers string-ci=?)
			  => (lambda (s)
			       (let ((ctype (mime-parse-content-type (cdr s))))
				 (values (car ctype) (cadr ctype)))))
			 (else (values "text" "plain"))))
		  ((body boundary)
		   (mime-compose-message-string
		    (if (string? content)
			(cons (make-mime-part
			       :type type :subtype subtype
			       :content (qpes content)
			       :transfer-encoding "quoted-printable")
			      attachs)
			attachs))))
      (put-string out "Mime-Version: 1.0\r\n")
      ;; To enable 'cid' reference defined in RFC 2387
      ;;  ref. https://tools.ietf.org/html/rfc2387
      ;; we check existance of 'content-id' in the given attachments
      ;; if there is, then we put multpart/related with the generated
      ;; boundary
      ;; TODO this is too naive and how can we support alternative 
      ;;      with this?
      (if (exists (lambda (m)
		    (assoc "content-id" (mime-part-headers m)
			   string-ci=?))
		  attachs)
	  (put-string out "Content-Type: multipart/related")
	  (put-string out "Content-Type: multipart/mixed"))
      (put-string out (mime-compose-parameters `((boundary . ,boundary)) #f))
      (put-string out "\r\n")
      (put-string out body)))

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
      (let ((headers (if (null? attachs)
			 headers
			 (alist-delete "content-type" headers string-ci=?))))
	(rfc5322-write-headers headers :output out :continue #t))
      ;; subject
      (put-string out "Subject: ")
      (if (string-every char-set:ascii subject)
	  (put-string out subject)
	  (put-string out  (mime-encode-word subject)))
      (put-string out "\r\n")
      (cond ((null? attachs)
	     (put-string out "\r\n")
	     ;; check no content
	     (when (string? content) (put-string out content)))
	    (else 
	     (handle-multipart out headers content attachs)))
      (extract))))

(define (make-smtp-attachment type subtype content . maybe-filename)
  (define (merge-header generated user-defined)
    ;; user-defined is always stronger so if it has content-disposition
    ;; then use that one
    (cond ((assoc "content-disposition" user-defined string-ci=?) user-defined)
	  (else (cons generated user-defined))))
	   
  (let* ((disposition-type (if (or (null? maybe-filename)
				   (null? (cdr maybe-filename)))
			       "attachment"
			       (cadr maybe-filename)))
	 (filename (if (null? maybe-filename)
		       disposition-type
		       (string-append disposition-type 
				      (mime-compose-parameters
				       `((filename . ,(car maybe-filename)))
				       #f))))
	 
	 (headers (if (or (null? maybe-filename)
			  (null? (cdr maybe-filename))
			  (null? (cddr maybe-filename)))
		      '()
		      (cddr maybe-filename))))
    (make-mime-part
     :type type :subtype subtype
     :transfer-encoding "base64"
     :content content
     :headers (merge-header `("content-disposition" ,filename)
			    headers))))

(define (make-smtp-alternative-component type subtype content . headers)
  (make-mime-part
     :type type :subtype subtype
     :transfer-encoding "base64"	; TODO printed-quotable?
     :content content
     :headers headers))

(define (make-smtp-alternative . alternatives)
  (make-mime-part
     :type "multipart" :subtype "alternative"
     :transfer-encoding #f
     :content alternatives))

;; High level API
(define-syntax smtp:subject    	(syntax-rules ()))
(define-syntax smtp:to         	(syntax-rules ()))
(define-syntax smtp:cc         	(syntax-rules ()))
(define-syntax smtp:bcc        	(syntax-rules ()))
(define-syntax smtp:from       	(syntax-rules ()))
(define-syntax smtp:attachment 	(syntax-rules ()))
(define-syntax smtp:header     	(syntax-rules ()))
(define-syntax smtp:alternative 
  (syntax-rules ()
    ((_ (spec1 ...) (spec2 ...) (spec* ...) ...)
     (make-smtp-alternative
      (make-smtp-alternative-component spec1 ...)
      (make-smtp-alternative-component spec2 ...)
      (make-smtp-alternative-component spec* ...) ...))))

(define-syntax smtp:mail
  (syntax-rules (smtp:from smtp:subject smtp:to smtp:cc 
		 smtp:bcc smtp:attachment smtp:header smtp:alternative)
    ;; parse subject
    ((_ "parse" from "no subject" content (recp ...) (attach ...) (head ...)
	((smtp:subject sub) elements ...))
     (smtp:mail "parse" from sub content 
		(recp ...) (attach ...) (head ...) (elements ...)))
    ;; TODO we want to generate these 3 somehow
    ;; parse to
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	((smtp:to to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-to to ...))
		(attach ...) (head ...) (elements ...)))
    ;; parse cc
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	((smtp:cc to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-cc to ...))
		(attach ...) (head ...) (elements ...)))
    ;; parse bcc
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	((smtp:bcc to ...) elements ...))
     (smtp:mail "parse" from subject content (recp ... (make-smtp-bcc to ...))
		(attach ...) (head ...) (elements ...)))
    ;; parse attachment
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	((smtp:attachment spec ...) elements ...))
     (smtp:mail "parse" from subject content (recp ...)
		((make-smtp-attachment spec ...) attach ...)
		(head ...)
		(elements ...)))
    ;; parse header
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	((smtp:header n v) elements ...))
     (smtp:mail "parse" from subject content (recp ...) (attach ...)
		(head ... (n v))
		(elements ...)))
    ;; parse alternative (content must be #f)
    ;; NB alternative is mere attachment
    ((_ "parse" from subject #f (recp ...) (attach ...) (head ...)
	((smtp:alternative (spec* ...) ...) elements ...))
     ;; mark content #t so that alternative can only exist once
     (smtp:mail "parse" from subject #t (recp ...)
		((smtp:alternative (spec* ...) ...) attach ...)
		(head ...)
		(elements ...)))
    ;; parse content
    ((_ "parse" from subject #f (recp ...) (attach ...) (head ...)
	(e elements ...))
     (smtp:mail "parse" from subject e
		(recp ...)
		(attach ...) 
		(head ...)
		(elements ...)))
    ((_ "parse" from subject content (recp ...) (attach ...) (head ...)
	(e elements ...))
     (smtp:mail "parse" from subject (string-append content "\r\n" e)
		(recp ...)
		(attach ...) 
		(head ...)
		(elements ...)))
    ;; finish
    ((_ "parse" from subject content (recp ...) (attach ...) ((hn hv) ...) ())
     (let ((mail (make-smtp-mail from subject content recp ...)))
       (smtp-mail-add-attachment! mail attach) ...
       (smtp-mail-add-header! mail hn hv) ...
       mail))
    ;; entry point
    ((_ (smtp:from from ...) elements ...)
     (smtp:mail "parse" 
		(make-smtp-from from ...)
		"no subject" #f () () () (elements ...)))))



)
