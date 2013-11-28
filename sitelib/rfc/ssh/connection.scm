;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/connection.scm - SSH2 protocol connection protocol
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

(library (rfc ssh connection)
    (export open-client-ssh-channel
	    open-client-ssh-session-channel
	    ssh-chennel-eof
	    close-ssh-channel
	    call-with-ssh-channel

	    <ssh-msg-channel-open>
	    <ssh-msg-channel-open-confirmation>)
    (import (rnrs)
	    (clos user)
	    (clos core)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (sagittarius regex)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (binary pack)
	    (crypto))

  ;; base classes (session can use this)
  (define-ssh-message <ssh-msg-channel-open> (<ssh-message>)
    ((type :byte +ssh-msg-channel-open+)
     (channel-type :string)
     (sender-channel :uint32)
     (initial-window :uint32)
     (maximum-packet :uint32)))

  (define-ssh-message <ssh-msg-channel-open-confirmation> (<ssh-message>)
    ((type :byte +ssh-msg-channel-open-confirmation+)
     (sender-channel    :uint32)
     (recipient-channel :uint32)
     (initial-window    :uint32)
     (maximum-packet    :uint32)))

  (define-ssh-message <ssh-msg-channel-open-failure> (<ssh-message>)
    ((type :byte +ssh-msg-channel-open-confirmation+)
     (recipient-channel :uint32)
     (reason-code       :uint32)
     (description       :string)
     (language          :string)))

  (define (open-client-ssh-channel transport 
				   open-channel
				   handle-confirmation
				   :key (initial-window #x100000)
				   (maximum-packet #x4000))
    (define (compute-channel transport) (length (~ transport 'channels)))
    (let* ((sender-channel (compute-channel transport))
	   (channel-open (open-channel
			   :sender-channel sender-channel
			   :initial-window initial-window
			   :maximum-packet maximum-packet)))
      (write-packet transport (ssh-message->bytevector channel-open))
      (let* ((resp (read-packet transport))
	     (type (bytevector-u8-ref resp 0)))
	(cond ((= type +ssh-msg-channel-open-confirmation+)
	       (rlet1 channel (handle-confirmation channel-open resp)
		 (push! (~ transport 'channels) channel)))
	      ((= type +ssh-msg-channel-open-failure+)
	       (let1 f (read-message <ssh-msg-channel-open-failure>
				     (open-bytevector-input-port resp))
		 (error 'open-client-ssh-channel
			(utf8->string (~ f 'description))
			(~ f 'reason-code))))
	      (else
	       (error 'open-client-ssh-channel "unknown type" type))))))

  (define (open-client-ssh-session-channel transport . opts)
    (apply open-client-ssh-channel transport
	   (lambda channel-info
	     (apply make <ssh-msg-channel-open>
		    :channel-type "session" channel-info))
	   (lambda (req packet)
	     (let1 c (read-message <ssh-msg-channel-open-confirmation>
				     (open-bytevector-input-port packet))
	       (make <ssh-channel>
		 :transport transport
		 ;; use server responded one
		 :sender-channel (~ c 'sender-channel)
		 :recipient-channel (~ c 'recipient-channel)
		 :client-window-size (~ req 'initial-window)
		 :client-packet-size (~ req 'maximum-packet)
		 :server-window-size (~ c 'initial-window)
		 :server-packet-size (~ c 'maximum-packet))))
	   opts))

  (define-ssh-message <ssh-msg-channel-eof> (<ssh-message>)
    ((type :byte +ssh-msg-channel-eof+)
     (recipient-channel :uint32)))

  (define (ssh-channel-eof channel)
    (let1 e (make <ssh-msg-channel-eof>
	      :recipient-channel (~ channel 'recipient-channel))
      (write-packet (~ channel 'transport) 
		    (ssh-message->bytevector e))))

  (define-ssh-message <ssh-msg-channel-close> (<ssh-message>)
    ((type :byte +ssh-msg-channel-close+)
     (recipient-channel :uint32)))

  (define (close-ssh-channel channel)
    (ssh-channel-eof channel)
    (let* ((e (make <ssh-msg-channel-close>
		:recipient-channel (~ channel 'recipient-channel)))
	   (transport (~ channel 'transport))
	   (channels (~ transport 'channels)))
      (write-packet transport (ssh-message->bytevector e))
      ;; remove this from transport
      (set! (~ transport 'channels) (remq channel channels))))

  (define (call-with-ssh-channel channel proc)
    (unwind-protect (proc channel)
      (close-ssh-channel channel)))

)