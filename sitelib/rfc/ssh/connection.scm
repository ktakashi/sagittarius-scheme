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
	    ssh-channel-eof
	    close-ssh-channel
	    call-with-ssh-channel

	    ssh-request-pseudo-terminal
	    ssh-request-shell
	    ssh-request-exec
	    ssh-request-subsystem
	    ssh-send-channel-data
	    ssh-recv-channel-data

	    ssh-binary-data-receiver
	    ssh-oport-receiver

	    <ssh-msg-channel-open>
	    <ssh-msg-channel-open-confirmation>
	    <ssh-msg-channel-open-failure>
	    <ssh-msg-channel-eof>
	    <ssh-msg-channel-close>
	    <ssh-msg-channel-request>
	    <ssh-msg-channel-pty-request>
	    <ssh-msg-channel-success>
	    <ssh-msg-channel-failure>
	    <ssh-msg-channel-data>
	    <ssh-msg-channel-window-adjust>
	    ;; misc
	    <ssh-msg-exit-status>
	    <ssh-msg-exit-signal>

	    ;; util
	    ssh-execute-command
	    )
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
	    (util bytevector)
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
      (ssh-write-ssh-message transport channel-open)
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
	       (error 'open-client-ssh-channel "unknown type" 
		      type resp))))))

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
      (ssh-write-ssh-message (~ channel 'transport) e)))

  (define-ssh-message <ssh-msg-channel-close> (<ssh-message>)
    ((type :byte +ssh-msg-channel-close+)
     (recipient-channel :uint32)))

  (define (close-ssh-channel channel :key (logical #f))
    (define transport (~ channel 'transport))
    (define channels (~ transport 'channels))
    (unless (or (channel-closed? channel) logical)
      (ssh-channel-eof channel)
      (let1 e (make <ssh-msg-channel-close>
		:recipient-channel (~ channel 'recipient-channel))
	(ssh-write-ssh-message transport e)))
    ;; remove this from transport
    (set! (~ channel 'open?) #f)
    (set! (~ transport 'channels) (remq channel channels)))

  (define (channel-closed? c) (not (~ c 'open?)))
  (define-syntax check-open
    (syntax-rules ()
      ((_ who c)
       (when (channel-closed? c) (error 'who "channel is closed")))))

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
    ((term   :string (or (getenv "TERM") "vt100"))
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
    ((type :byte +ssh-msg-channel-data+)
     (recipient-channel :uint32)
     (data :string)))

  (define-ssh-message <ssh-msg-channel-window-adjust> (<ssh-message>)
    ((type :byte +ssh-msg-channel-window-adjust+)
     (recipient-channel :uint32)
     (size :uint32)))

  (define (ssh-binary-data-receiver)
    (let ((r '()))
      (lambda (data)
	(if (eof-object? data)
	    (bytevector-concatenate (reverse! r))
	    (set! r (cons data r))))))

  (define (ssh-oport-receiver oport)
    (lambda (data)
      (if (eof-object? data)
	  #t
	  (put-bytevector oport data))))

  ;; FIXME not sure if this handles data properly...
  (define (handle-channel-request-response channel response receiver)
    (define transport (~ channel 'transport))
    (let loop ((response response) (status #f))
      (define (read-it class response)
	(let1 m (read-message class (open-bytevector-input-port response))
	  (let* ((size (if (is-a? m <ssh-msg-channel-data>)
			   (let1 data (~ m 'data)
			     (and receiver (receiver data))
			     (bytevector-length data))
			   0))
		 (client-size (+ (~ channel 'client-size) size)))
	    (set! (~ channel 'client-size) client-size)
	    (when (> (+ client-size (~ channel 'client-packet-size))
		     (~ channel 'client-window-size))
	      ;; send window adjust requst
	      ;; simply double it for now
	      (let* ((new-size (* (~ channel 'client-window-size) 2))
		     (m (make <ssh-msg-channel-window-adjust>
			  :recipient-channel (~ channel 'recipient-channel)
			  :size new-size)))
		(set! (~ channel 'client-window-size) new-size)
		(ssh-write-ssh-message transport m))))
	  (if receiver
	      (loop (read-packet transport) status)
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
	       (loop (read-packet transport) status))
	      ((= b +ssh-msg-channel-close+)
	       (close-ssh-channel channel :logical #t)
	       (if receiver
		   (values status (receiver (eof-object)))
		   (read-it <ssh-msg-channel-close> response)))
	      ((= b +ssh-msg-channel-window-adjust+)
	       (let1 m (read-message <ssh-msg-channel-window-adjust>
				     (open-bytevector-input-port response))
		 (set! (~ channel 'server-window-size) (~ m 'size))
		 (loop (read-packet transport) status)))
	      ((= b +ssh-msg-channel-request+)
	       ;; must be exit status but first get the head
	       (let* ((in (open-bytevector-input-port response))
		      (m  (read-message <ssh-msg-channel-request> in)))
		 (case (string->symbol (utf8->string (~ m 'request-type)))
		   ((exit-status)
		    (let1 m (read-message <ssh-msg-exit-status> in)
		      (loop (read-packet transport) (~ m 'exit-status))))
		   ((exit-signal)
		    ;; TODO should we return something instead of raising
		    ;; an error?
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

  (define (ssh-request-pseudo-terminal channel 
				       :key
				       (term (or (getenv "TERM") "vt100"))
				       (width  80)
				       (height 24)
				       (width-in-pixels 640)
				       (height-in-pixels 480)
				       (mode #vu8(0)))
    (check-open ssh-request-pseudo-terminal channel)
    (let1 m (make <ssh-msg-channel-pty-request>
	      :recipient-channel (~ channel 'recipient-channel)
	      :request-type "pty-req"
	      :term term
	      :width width
	      :height height
	      :width-in-pixels width-in-pixels
	      :height-in-pixels height-in-pixels
	      :mode mode)
      (ssh-write-ssh-message (~ channel 'transport) m)
      (let1 r (read-packet (~ channel 'transport))
	(handle-channel-request-response channel r #f))))

  (define (%ssh-request msg channel receiver)
    (define transport (~ channel 'transport))
    (check-open ssh-request-shell channel)
    (ssh-write-ssh-message transport msg)
    (let1 r (read-packet transport)
      (handle-channel-request-response channel r receiver)))

  (define (ssh-request-shell channel)
    (%ssh-request (make <ssh-msg-channel-request>
		    :recipient-channel (~ channel 'recipient-channel)
		    :request-type "shell")
		  channel #f))

  (define-ssh-message <ssh-msg-channel-exec-request> (<ssh-msg-channel-request>)
    ((command :string)))
  (define (ssh-request-exec channel command
			    :key (receiver (ssh-binary-data-receiver)))
    (%ssh-request (make <ssh-msg-channel-exec-request>
		    :recipient-channel (~ channel 'recipient-channel)
		    :request-type "exec" :command command)
		  channel
		  receiver))

  (define-ssh-message <ssh-msg-channel-subsystem-request> 
    (<ssh-msg-channel-request>)
    ((subsystem-name :string)))
  (define (ssh-request-subsystem channel subsystem-name)
    (%ssh-request (make <ssh-msg-channel-subsystem-request>
		    :recipient-channel (~ channel 'recipient-channel)
		    :request-type "subsystem"
		    :subsystem-name subsystem-name)
		  channel #f))

  (define (ssh-send-channel-data channel data)
    (define transport (~ channel 'transport))
    (define (do-bv data)
      (let loop ((data data))
	(let1 size (bytevector-length data)
	  (let-values (((sending rest)
			(if (> size (~ channel 'server-window-size))
			    (bytevector-split-at* data size)
			    (values data #vu8()))))
	    (let1 m (make <ssh-msg-channel-data>
		      :recipient-channel (~ channel 'recipient-channel)
		      :data sending)
	      (ssh-write-ssh-message transport m)
	      (unless (zero? (bytevector-length rest)) (loop rest)))))))
    (define (do-port port)
      (define buffer-size (min (~ channel 'server-window-size) 4096))
      (define buffer (make-bytevector buffer-size))
      (let loop ()
	(let1 n (get-bytevector-n! port buffer 0 buffer-size)
	  (unless (eof-object? n)
	    (let1 m (make <ssh-msg-channel-data>
		      :recipient-channel (~ channel 'recipient-channel)
		      :data (if (= n buffer-size)
				buffer
				(bytevector-copy buffer 0 n)))
	      (ssh-write-ssh-message transport m)
	      (when (= buffer-size n) (loop)))))))
    (check-open ssh-send-channel-data channel)
    (cond ((bytevector? data) (do-bv data))
	  ((port? data) (do-port data))
	  (else
	   (error 'ssh-send-channel-data 
		  "bytevector or binary input port required" data))))

  (define (ssh-recv-channel-data channel)
    (define transport (~ channel 'transport))
    (check-open ssh-recv-channel-data channel)
    (let1 m (handle-channel-request-response channel (read-packet transport) #f)
      (if (is-a? m <ssh-msg-channel-data>)
	  (~ m 'data)
	  (error 'ssh-recv-channel-data "unexpected message" m))))

  (define (ssh-execute-command transport command
			       :key (receiver (ssh-binary-data-receiver))
			       :allow-other-keys opts)
    (call-with-ssh-channel (open-client-ssh-session-channel transport)
      (lambda (c)
	(apply ssh-request-pseudo-terminal c opts)
	(ssh-request-exec c command :receiver receiver))))

)
