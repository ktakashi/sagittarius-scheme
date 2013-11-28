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

	    ssh-request-pseudo-terminal
	    ssh-request-shell
	    ssh-request-exec

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
    ;; close-ssh-channel might raise an error so catch it
    ;; and ignore
    (unwind-protect (proc channel)
     (guard (e (else #f)) (close-ssh-channel channel))))

  ;; base
  (define-ssh-message <ssh-msg-channel-request> (<ssh-message>)
    ((type :byte +ssh-msg-channel-request+)
     (recipient-channel :uint32)
     (request-type      :string)
     (want-reply        :boolean #t)))

  (define-ssh-message <ssh-msg-channel-pty-request> (<ssh-msg-channel-request>)
    ((term :string (or (getenv "TERM") "vt100"))
     (width  :uint32 80)
     (height :uint32 24)
     (width-in-pixels  :uint32 640)
     (height-in-pixels :uint32 480)
     (mode :string #vu8(0))))

  ;; exit status
  (define-ssh-message <ssh-msg-exit-status> (<ssh-message>)
    ((exit-status :uint32)))
  (define-ssh-message <ssh-msg-exit-signal> (<ssh-message>)
    ((signal-name  :string)
     (core-dumped? :boolean)
     (message      :string)
     (language     :string)))

  (define-ssh-message <ssh-msg-channel-success> (<ssh-message>)
    ((type :byte +ssh-msg-channel-success+)
     (recipient-channel :uint32)))
  (define-ssh-message <ssh-msg-channel-failure> (<ssh-message>)
    ((type :byte +ssh-msg-channel-failure+)
     (recipient-channel :uint32)))
  (define-ssh-message <ssh-msg-channel-data> (<ssh-message>)
    ((type :byte +ssh-msg-channel-failure+)
     (recipient-channel :uint32)
     (data :string)))

  (define-ssh-message <ssh-msg-channel-window-adjust> (<ssh-message>)
    ((type :byte +ssh-msg-channel-window-adjust+)
     (recipient-channel :uint32)
     (size :uint32)))

  (define (handle-channel-request-response channel response data?)
    (define transport (~ channel 'transport))
    (let loop ((response response) (r '()) (status #f))
      (define (read-it class response)
	(let1 m (read-message class (open-bytevector-input-port response))
	  (if data?
	      (loop (read-packet transport)
		    (if (is-a? m <ssh-msg-channel-data>)
			(cons (~ m 'data) r)
			r)
		    status)
	      m)))
      (let1 b (bytevector-u8-ref response 0)
	(cond ((= b +ssh-msg-channel-success+)
	       (read-it <ssh-msg-channel-success> response))
	      ((= b +ssh-msg-channel-failure+)
	       (read-it <ssh-msg-channel-failure> response))
	      ((= b +ssh-msg-channel-data+)
	       (read-it <ssh-msg-channel-data> response))
	      ((= b +ssh-msg-channel-eof+)
	       ;; next will be close so discard
	       (read-packet transport)
	       (values status (bytevector-concatenate r)))
	      ((= b +ssh-msg-channel-close+) 
	       (values status (bytevector-concatenate r)))
	      ((= b +ssh-msg-channel-window-adjust+)
	       (read-it <ssh-msg-channel-window-adjust> response))
	      ((= b +ssh-msg-channel-request+)
	       ;; must be exit status but first get the head
	       (let* ((in (open-bytevector-input-port response))
		      (m  (read-message <ssh-msg-channel-request> in)))
		 (case (string->symbol (utf8->string (~ m 'request-type)))
		   ((exit-status)
		    (let1 m (read-message <ssh-msg-exit-status> in)
		      ;;(values (~ m 'exit-status) (bytevector-concatenate r))))
		      (loop (read-packet transport) r (~ m 'exit-status))))
		   ((exit-signal)
		    ;; TODO should we return something instead of raising an
		    ;; error?
		    (let1 m (read-message <ssh-msg-exit-signal> in)
		      (error (~ m 'signal-name) (~ m 'message)
			     (~ m 'core-dumped?))))
		   (else 
		    => (lambda (n)
			 (error 'handle-channel-request-response
				"unknown exit status" n))))))
	      (else
	       (error 'handle-channel-request-response 
		      "unknown response" response))))))

  (define (ssh-request-pseudo-terminal channel)
    (let1 m (make <ssh-msg-channel-pty-request>
	      :recipient-channel (~ channel 'recipient-channel)
	      :request-type "pty-req")
      (write-packet (~ channel 'transport) (ssh-message->bytevector m))
      (let1 r (read-packet (~ channel 'transport))
	(handle-channel-request-response channel r #f))))

  (define (ssh-request-shell channel)
    (let1 m (make <ssh-msg-channel-request>
	      :recipient-channel (~ channel 'recipient-channel)
	      :request-type "shell")
      (write-packet (~ channel 'transport) (ssh-message->bytevector m))
      (let1 r (read-packet (~ channel 'transport))
	(handle-channel-request-response channel r #f))))

  (define-ssh-message <ssh-msg-channel-exec-request> (<ssh-msg-channel-request>)
    ((command :string)))

  (define (ssh-request-exec channel command)
    (let1 m (make <ssh-msg-channel-exec-request>
	      :recipient-channel (~ channel 'recipient-channel)
	      :request-type "exec" :command command)
      (write-packet (~ channel 'transport) (ssh-message->bytevector m))
      (let1 r (read-packet (~ channel 'transport))
	(handle-channel-request-response channel r #t))))

)