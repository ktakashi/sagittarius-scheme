;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/service/auth.scm - SSH2 server authentication
;;;  
;;;   Copyright (c) 2025  Takashi Kato  <ktakashi@ymail.com>
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

#!nounbound
(library (rfc ssh server service connection)
    (export ssh-handle-service-request
	    ssh-handle-channel-open-packet
	    ssh-handle-channel-request-packet
	    ssh-handle-channel-data-packet
	    ssh-handle-channel-window-adjust-packet
	    ssh-handle-channel-close-packet

	    ssh-handle-connection-packet

	    ssh-handle-channel-open
	    ssh-handle-channel-request
	    
	    ssh-deliver-channel-data
	    
	    <ssh-server-connection> ssh-server-connection?
	    ssh-server-connection-auth-ticket
	    *ssh-accept-anonymous-connection-request*
	    ssh-send-channel-request-success
	    ssh-send-channel-request-failure
	    <ssh-server-channel>
	    <ssh-server-session-channel>

	    ssh-channel-data-handler
	    ssh-channel-data-handler-set!

	    ssh-channel-put-attribute!
	    ssh-channel-ref-attribute
	    ssh-channel-after-channel-request!

	    ssh-invalidate-auth-ticket
	    )
    (import (rnrs)
	    (clos core)
	    (clos user)
	    (srfi :18 multithreading)
	    (srfi :39 parameters)
	    (sagittarius)
	    (sagittarius object)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh connection)
	    (rfc ssh server transport)
	    (rfc ssh server service api))
(define-class <ssh-server-connection> (<ssh-server-transport> <ssh-connection>)
  ((auth-ticket :reader ssh-server-connection-auth-ticket)))
(define (ssh-server-connection? o) (is-a? o <ssh-server-connection>))
(define-generic ssh-invalidate-auth-ticket)
(define-method ssh-invalidate-auth-ticket (t) #t) ;; do nothing

(define-class <ssh-server-channel> (<ssh-channel>)
  ((data-handler :init-value #f :reader ssh-channel-data-handler
		 :writer ssh-channel-data-handler-set!)
   (attributs :init-form (make-eqv-hashtable)
	      :reader ssh-channel-attributes)))
(define-class <ssh-server-session-channel> (<ssh-server-channel>)
  ())

(define (ssh-channel-put-attribute! c (name symbol?) value)
  (hashtable-set! (ssh-channel-attributes c) name value))
(define (ssh-channel-ref-attribute c (name symbol?))
  (hashtable-ref (ssh-channel-attributes c) name #f))
(define after-channel-request (list 'after-channel-request))
(define (ssh-channel-after-channel-request! c thunk)
  (hashtable-set! (ssh-channel-attributes c) after-channel-request thunk))

(define *ssh-accept-anonymous-connection-request*
  (make-parameter #f))
(define-method ssh-handle-service-request ((m (equal +ssh-connection+))
					   transport)
  (cond ((*ssh-accept-anonymous-connection-request*)
	 (let ((msg (make <ssh-msg-service-accept> :service-name m)))
	   (ssh-write-ssh-message transport msg))
	 (internal-ssh-connection-handler transport #f))
	(else
	 (close-server-ssh-transport! transport 
	  +ssh-disconnect-service-not-available+
	  :description "anonymous ssh-connection is not supported"))))

(define-method ssh-handle-service-request ((m (equal +ssh-connection+))
					   transport ticket)
  (internal-ssh-connection-handler transport ticket))

(define-method ssh-server-on-disconnect ((t <ssh-server-connection>))
  (let ((ticket (slot-ref t 'auth-ticket)))
    (slot-set! t 'auth-ticket #f)
    (ssh-invalidate-auth-ticket ticket)))
      

(define (internal-ssh-connection-handler transport auth-ticket?)
  ;; convert server transport to server connection
  ;; by using CLOS's black magic :)
  (change-class transport <ssh-server-connection>)
  (slot-set! transport 'channels (make-eqv-hashtable))
  (slot-set! transport 'auth-ticket auth-ticket?)
  (slot-set! transport 'next-channel-id 0)
  (slot-set! transport 'lock (make-mutex))
  transport)

(define (ssh-handle-connection-packet connection packet)
  (define u8 (bytevector-u8-ref packet 0))
  (cond ((= u8 +ssh-msg-channel-open+)
	 (ssh-handle-channel-open-packet connection packet)
	 packet)
	((= u8 +ssh-msg-channel-request+)
	 (ssh-handle-channel-request-packet connection packet)
	 packet)
	((= u8 +ssh-msg-channel-data+)
	 (ssh-handle-channel-data-packet connection packet)
	 packet)
	((= u8 +ssh-msg-channel-eof+)
	 (ssh-handle-channel-eof-packet connection packet)
	 packet)
	((= u8 +ssh-msg-channel-window-adjust+)
	 (ssh-handle-channel-window-adjust-packet connection packet)
	 packet)
	((= u8 +ssh-msg-channel-close+)
	 (ssh-handle-channel-close-packet connection packet)
	 packet)
	(else #f)))

(define (ssh-send-channel-request-success channel)
  (ssh-write-ssh-message (~ channel 'connection)
			 (make <ssh-msg-channel-success>
			   :recipient-channel (~ channel 'sender-channel))))
(define (ssh-send-channel-request-failure channel)
  (ssh-write-ssh-message (~ channel 'connection)
			 (make <ssh-msg-channel-failure>
			   :recipient-channel (~ channel 'sender-channel))))

(define (ssh-handle-channel-open-packet connection packet)
  (define msg (bytevector->ssh-message <ssh-msg-channel-open> packet))
  (ssh-handle-channel-open (~ msg 'channel-type) connection msg packet))

(define (ssh-handle-channel-request-packet connection packet)
  (define msg (bytevector->ssh-message <ssh-msg-channel-request> packet))
  (define (send c succes?)
    (when (~ msg 'want-reply)
      (if succes?
	  (ssh-send-channel-request-success c)
	  (ssh-send-channel-request-failure c)))
    (cond ((hashtable-ref (ssh-channel-attributes c) after-channel-request #f) =>
	   (lambda (thunk)
	     (hashtable-delete! (ssh-channel-attributes c) after-channel-request)
	     (thunk)))))
  (cond ((search-channel connection (~ msg 'recipient-channel)) =>
	 (lambda (c)
	   (let ((t (~ msg 'request-type)))
	     (cond ((ssh-handle-channel-request t c msg packet) =>
		    (lambda (data-handler)
		      (when (procedure? data-handler)
			(ssh-channel-data-handler-set! c data-handler))
		      (send c #t)))
		   (else (send c #f))))))))

(define (ssh-handle-channel-data-packet connection packet)
  (let ((recipient (bytevector-u32-ref packet 1 (endianness big))))
    (cond ((search-channel connection recipient) =>
	   (lambda (c)
	     (cond ((ssh-channel-data-handler c) =>
		    (lambda (handler)
		      (let ((len (bytevector-u32-ref packet 5 (endianness big))))
			(handler c packet 9 len))))))))))
(define (ssh-handle-channel-eof-packet connection packet)
  (let ((recipient (bytevector-u32-ref packet 1 (endianness big))))
    (cond ((search-channel connection recipient) =>
	   (lambda (c)
	     (cond ((ssh-channel-data-handler c) =>
		    (lambda (handler) (handler c #f 0 0)))))))))

(define (ssh-handle-channel-window-adjust-packet connection packet)
  (let ((msg (bytevector->ssh-message <ssh-msg-channel-request> packet)))
    (cond ((search-channel connection (~ msg 'recipient-channel)) =>
	   (lambda (c)
	     (ssh-channel-update-peer-window-size! c (~ msg 'size)))))))

(define (ssh-handle-channel-close-packet connection packet)
  (let ((recipient (bytevector-u32-ref packet 1 (endianness big))))
    (cond ((search-channel connection recipient) => close-ssh-channel))))

;; different name than the client, sadly, we can't reuse it :(
(define (ssh-deliver-channel-data channel data
	  :optional (offset 0) (data-len (bytevector-length data)))
  (define data-offset +ssh-channel-data-data-offset+)
  (cond ((ssh-channel-check-peer-window-size channel data-len) =>
	 (lambda (len)
	   (let ((buffer (~ channel 'channel-buffer))
		 (recipient (~ channel 'sender-channel)))
	     (bytevector-u8-set! buffer 0 +ssh-msg-channel-data+)
	     (bytevector-u32-set! buffer 1 recipient (endianness big))
	     (bytevector-u32-set! buffer 5 len (endianness big))
	     (bytevector-copy! data offset buffer data-offset len)
	     (ssh-write-packet (~ channel 'connection)
			       buffer 0 (+ data-offset len))
	     (ssh-channel-update-peer-current-size! channel len)
	     (if (= data-len len)
		 len
		 (ssh-deliver-channel-data channel data (+ offset len)
					   (- data-len len))))))
	(else #f)))

(define (search-channel connection recipient)
  (hashtable-ref (~ connection 'channels) recipient))

(define-generic ssh-handle-channel-open
  :class <predicate-specializable-generic>)
(define-method ssh-handle-channel-open (m connection msg packet)
  (ssh-write-ssh-message connection
     (make <ssh-msg-channel-open-failure>
       :recipient-channel (~ msg 'sender-channel)
       :reason-code +ssh-open-unknown-channel-type+
       :description (string-append "unknown channel name: " m))))

(define-method ssh-handle-channel-open
  ((m (equal "session")) connection msg packet)
  (define (register-channel! id)
    (let ((ch (make <ssh-server-session-channel>
		:connection connection
		:sender-channel (~ msg 'sender-channel)
		:recipient-channel id
		:peer-window-size (~ msg 'initial-window)
		:peer-packet-size (~ msg 'maximum-packet)
		:host-window-size #x100000
		:host-packet-size #x4000)))
      (hashtable-set! (~ connection 'channels) id ch)
      ch))
  (let ((id (ssh-connection-next-channel-id! connection)))
    (ssh-write-ssh-message connection
     (make <ssh-msg-channel-open-confirmation>
       :recipient-channel (~ msg 'sender-channel)
       :sender-channel id
       :initial-window #x100000
       :maximum-packet #x4000))
    (register-channel! id)))

(define-generic ssh-handle-channel-request
  :class <predicate-specializable-generic>)

)
