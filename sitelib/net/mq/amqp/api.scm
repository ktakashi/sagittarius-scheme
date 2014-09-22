;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; net/mq/amqp/api.scm - AMQP v1.0 API
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (net mq amqp api)
    (export (rename (amqp-make-client-connection open-amqp-connection))
	    close-amqp-connection!
	    begin-amqp-session!
	    end-amqp-session!

	    call-with-amqp-connection
	    call-with-amqp-session

	    create-amqp-sender
	    create-amqp-receiver
	    (rename (detach-amqp-link! destroy-amqp-sender)
		    (detach-amqp-link! destroy-amqp-receiver))

	    send-amqp-message
	    receive-amqp-message
	    ;; message
	    create-amqp-text-message
	    create-amqp-binary-message
	    create-amqp-mime-message
	    amqp-message-data
	    ;; amqp-message-header
	    ;; header
	    amqp-message-durable
	    amqp-message-durable-set!
	    amqp-message-priority
	    amqp-message-priority-set!
	    amqp-message-ttl
	    amqp-message-ttl-set!
	    amqp-message-first-aquirer
	    amqp-message-first-aquirer-set!
	    amqp-message-delivery-count
	    amqp-message-delivery-count-set!
	    ;; properties
	    amqp-message-message-id
	    amqp-message-message-id-set!
	    amqp-message-user-id
	    amqp-message-user-id-set!
	    amqp-message-to
	    amqp-message-to-set!
	    amqp-message-subject
	    amqp-message-subject-set!
	    amqp-message-reply-to
	    amqp-message-reply-to-set!
	    amqp-message-correlation-id
	    amqp-message-correlation-id-set!
	    amqp-message-content-type
	    amqp-message-content-type-set!
	    amqp-message-content-encoding
	    amqp-message-content-encoding-set!
	    amqp-message-absolute-expiry-time
	    amqp-message-absolute-expiry-time-set!
	    amqp-message-creation-time
	    amqp-message-creation-time-set!
	    amqp-message-group-id
	    amqp-message-group-id-set!
	    amqp-message-group-sequence
	    amqp-message-group-sequence-set!
	    amqp-message-reply-to-group-id
	    amqp-message-reply-to-group-id-set!
	    ;; application properties
	    amqp-message-delete-property!
	    amqp-message-ref-property
	    amqp-message-set-string-property!
	    )
    (import (rnrs)
	    (clos user)
	    (net mq amqp types)
	    (net mq amqp transport)
	    (net mq amqp messaging)
	    (rfc uuid)
	    (sagittarius)
	    (sagittarius socket)
	    (sagittarius object)
	    (sagittarius control))

  (define (call-with-amqp-connection host service proc . opts)
    (define conn (apply amqp-make-client-connection host service opts))
    (unwind-protect (proc conn) (close-amqp-connection! conn)))
  (define (call-with-amqp-session conn proc)
    (define s (begin-amqp-session! conn))
    (unwind-protect (proc s) (end-amqp-session! s)))

  (define-class <amqp-bare-message> ()
    ((properties :init-keyword :properties :init-form (make-amqp-properties))
     (application-properties :init-keyword :application-properties
			     :init-form (make-amqp-application-properties))
     (application-data :init-keyword :application-data)))

  (define-class <amqp-annotated-message> (<amqp-bare-message>)
    ((header :init-keyword :header :init-form (make-amqp-header))
     (delivery-annotations :init-keyword :delivery-annotations
			   :init-form (make-amqp-delivery-annotation))
     (message-annotations :init-keyword :message-annotations
			  :init-form (make-amqp-message-annotation))
     (footer :init-keyword :footer :init-form (make-amqp-footer))))

  (define (create-amqp-sender session destination)
    (let ((source (make-amqp-source :dynamic #t))
	  (target (make-amqp-target 
		   :address (->amqp-value address-string destination))))
      (attach-amqp-link! session (uuid->string (make-v4-uuid)) +amqp-sender+
			 source target)))

  (define (create-amqp-receiver session target)
    (let ((source (make-amqp-source 
		   :address (->amqp-value address-string target)))
	  (target (make-amqp-target :dynamic #t)))
      (attach-amqp-link! session (uuid->string (make-v4-uuid)) +amqp-receiver+
			 source target)))

  (define (send-amqp-message sender message)
    (send-transfer sender +message-format+ (amqp-message->bytevector message)
		   disposition-handler))

  (define (receive-amqp-message receiver :key (timeout #f))
    (define socket (~ receiver 'session 'connection 'raw-socket))
    (define (recv)
      (let* ((m (recv-transfer receiver disposition-handler))
	     (in (open-bytevector-input-port m)))
	;; message is just an bytevector so we need to parse it
	(let loop ((opts '()))
	  (let ((e (read-amqp-data in)))
	    ;; we don't check presense so the last come is used
	    (cond ((eof-object? e) 
		   (apply make <amqp-annotated-message> opts))
		  ((is-a? e <amqp-header>) 
		   (loop (cons* :header e opts)))
		  ((is-a? e <amqp-footer>) 
		   (loop (cons* :footer e opts)))
		  ((is-a? e <amqp-delivery-annotations>) 
		   (loop (cons* :delivery-annotations e opts)))
		  ((is-a? e <amqp-message-annotations>) 
		   (loop (cons* :message-annotations e opts)))
		  ((is-a? e <amqp-properties>) 
		   (loop (cons* :properties e opts)))
		  ((is-a? e <amqp-application-properties>)
		   (loop (cons* :application-properties e opts)))
		  ((or (is-a? e <amqp-amqp-value>)
		       (is-a? e <amqp-data>)
		       (is-a? e <amqp-amqp-sequence>))
		   (loop (cons* :application-data e opts)))
		  ;; ignore or raise an error?
		  (else (loop opts)))))))
    (let ((r (if timeout (socket-read-select timeout socket) '(1))))
      (if (null? r)
	  #f
	  (recv))))

  (define (amqp-message->bytevector message)
    (bytevector-append
     (amqp-value->bytevector (~ message 'header))
     (amqp-value->bytevector (~ message 'delivery-annotations))
     (amqp-value->bytevector (~ message 'message-annotations))
     (amqp-value->bytevector (~ message 'properties))
     (amqp-value->bytevector (~ message 'application-properties))
     (amqp-value->bytevector (~ message 'application-data))
     (amqp-value->bytevector (~ message 'footer))))

  (define (create-amqp-text-message text)
    (make <amqp-annotated-message> 
      :application-data (make-amqp-amqp-value (->amqp-value :string text))))

  (define (create-amqp-binary-message data)
    (make <amqp-annotated-message> 
      :application-data (make-amqp-amqp-value (->amqp-value :binary text))))

  (define (create-amqp-mime-message content-type data)
    (make <amqp-annotated-message> 
      :properties (make-amqp-properties
		   :content-type (string->symbol content-type))
      :application-data (make-amqp-data (->amqp-value :binary data))))

  (define (amqp-message-data message)
    (scheme-value (~ message 'application-data 'value)))

  ;; (define (amqp-message-header message) (~ message 'header))

  (define-syntax define-message-accessor
    (lambda (x)
      (syntax-case x ()
	((k path prop)
	 #'(define-message-accessor path prop (lambda (x) x) (lambda (x) x)))
	((k path prop type-val type-ctr)
	 (with-syntax ((ref (datum->syntax #'k
			     (string->symbol
			      (format "amqp-message-~a" 
				      (syntax->datum #'prop)))))
		       (set (datum->syntax #'k
			     (string->symbol
			      (format "amqp-message-~a-set!" 
				      (syntax->datum #'prop))))))
	   #'(begin
	       ;; should we make fallback mandatory?
	       (define (ref message :optional fallback)
		 (let ((p (~ message 'path)))
		   (cond ((undefined? fallback)
			  (type-val (~ p 'prop)))
			 ((slot-bound? p 'prop)
			  (type-val (~ p 'prop)))
			 (else fallback))))
	       (define (set message v) 
		 (set! (~ message 'path 'prop) (type-ctr v)))))))))

  ;; header types are all promitives so ignroe
  (define-syntax define-header-accessor
    (syntax-rules ()
      ((_ prop type)
       (define-message-accessor header prop))))
  
  (define-header-accessor durable :boolean)
  (define-header-accessor priority :ubyte)
  (define-header-accessor ttl milliseconds)
  (define-header-accessor first-aquirer :boolean)
  (define-header-accessor delivery-count :uint)

  ;; message properties accessor
  (define-syntax define-property-accessor
    (syntax-rules ()
      ((_ prop)
       (define-message-accessor properties prop))
      ((_ prop ref set)
       (define-message-accessor properties prop ref set))))

  ;; we can't see what message id type is.
  (define (make-amqp-address str) (->amqp-value address-string str))
  (define-property-accessor message-id scheme-value make-amqp-message-id)
  (define-property-accessor user-id)
  (define-property-accessor to scheme-value make-amqp-address)
  (define-property-accessor subject)
  (define-property-accessor reply-to scheme-value make-amqp-address)
  (define-property-accessor correlation-id scheme-value make-amqp-address)
  (define-property-accessor content-type symbol->string string->symbol)
  (define-property-accessor content-encoding)
  (define-property-accessor absolute-expiry-time)
  (define-property-accessor creation-time)
  (define-property-accessor group-id)
  (define-property-accessor group-sequence)
  (define-property-accessor reply-to-group-id)

  (define (amqp-message-delete-property! m key)
    (annotation-delete! (~ m 'application-properties) 
			(->amqp-value :string (->string key))))

  (define (amqp-message-set-property! m key type v)
    (annotation-set! (~ m 'application-properties)
		     (->amqp-value :string (->string key))
		     (->amqp-value type v)))
  (define (amqp-message-set-string-property! message key str)
    (amqp-message-set-property! message key :string str))

  (define (amqp-message-ref-property m key)
    (scheme-value (annotation-ref (~ m 'application-properties) 
				  (->amqp-value :string (->string key)))))
)
