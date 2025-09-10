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

#!nounbound
(library (rfc ssh client connection)
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

	    ;; util
	    ssh-execute-command)
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
	    (rfc ssh connection)
	    (srfi :117 list-queues)
	    (util bytevector))

(define-class <ssh-client-channel> (<ssh-channel>)
  ((packet-queue :init-form (list-queue))))   

(define (open-client-ssh-channel transport 
				 open-channel
				 handle-confirmation
				 :key (initial-window #x100000)
				      (maximum-packet #x4000))
  (define (compute-channel transport)
    (ssh-connection-next-channel-id! transport))
  (let* ((sender-channel (compute-channel transport))
         (channel-open (open-channel
      			:sender-channel sender-channel
      			:initial-window initial-window
      			:maximum-packet maximum-packet)))
    (ssh-write-ssh-message transport channel-open)
    (let loop ()
      (let* ((resp (ssh-read-packet transport))
             (type (bytevector-u8-ref resp 0)))
        (cond ((= type +ssh-msg-channel-open-confirmation+)
      	       (rlet1 c (handle-confirmation channel-open resp)
     		 (hashtable-set! (~ transport 'channels) sender-channel c)))
      	      ((= type +ssh-msg-channel-open-failure+)
      	       (let1 f (ssh-read-message <ssh-msg-channel-open-failure>
      					 (open-bytevector-input-port resp))
      		 (error 'open-client-ssh-channel
      			(utf8->string (~ f 'description))
      			(~ f 'reason-code))))
      	      ((= type +ssh-msg-global-request+)
      	       ;; TODO handle need response
      	       ;; for now just ignore
      	       (loop))
      	      (else
      	       (error 'open-client-ssh-channel "unknown type" type resp)))))))

(define (open-client-ssh-session-channel transport . opts)
  (apply open-client-ssh-channel transport
         (lambda channel-info
           (apply make <ssh-msg-channel-open>
      		  :channel-type "session" channel-info))
         (lambda (req packet)
           (let1 c (ssh-read-message <ssh-msg-channel-open-confirmation>
      				     (open-bytevector-input-port packet))
             (make <ssh-client-channel>
      	       :connection transport
      	       ;; use server responded one
      	       :sender-channel (~ c 'sender-channel)
      	       :recipient-channel (~ c 'recipient-channel)
      	       :host-window-size (~ req 'initial-window)
      	       :host-packet-size (~ req 'maximum-packet)
      	       :peer-window-size (~ c 'initial-window)
      	       :peer-packet-size (~ c 'maximum-packet))))
         opts))

(define-syntax check-open
  (syntax-rules ()
    ((_ who c)
     (when (ssh-channel-closed? c) (error 'who "channel is closed")))))

(define (call-with-ssh-channel channel proc)
  ;; close-ssh-channel might raise an error so catch it
  ;; and ignore
  (unwind-protect (proc channel)
    (guard (e (else #f)) (close-ssh-channel channel))))

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
            :recipient-channel (~ channel 'sender-channel)
            :request-type "pty-req"
            :term term
            :width width
            :height height
            :width-in-pixels width-in-pixels
            :height-in-pixels height-in-pixels
            :mode mode)
    (ssh-write-ssh-message (~ channel 'connection) m)
    (handle-channel-request-response channel)))

(define (%ssh-request msg channel)
  (define transport (~ channel 'connection))
  (check-open ssh-request-shell channel)
  (ssh-write-ssh-message transport msg)
  (handle-channel-request-response channel))

(define (ssh-request-shell channel)
  (%ssh-request (make <ssh-msg-channel-request>
      		  :recipient-channel (~ channel 'sender-channel)
      		  :request-type "shell")
      		channel))

(define (ssh-request-exec channel command
      			  :key (receiver (ssh-binary-data-receiver)))
  (%ssh-request (make <ssh-msg-channel-exec-request>
      		  :recipient-channel (~ channel 'sender-channel)
      		  :request-type "exec" :command command)
      		channel)
  (let loop ()
    (cond ((%recv-channel-data channel) =>
	   (lambda (data) (receiver data) (loop)))
	  (else (receiver (eof-object))))))

(define (ssh-request-subsystem channel subsystem-name)
  (%ssh-request (make <ssh-msg-channel-subsystem-request>
      		  :recipient-channel (~ channel 'sender-channel)
      		  :request-type "subsystem"
      		  :subsystem-name subsystem-name)
      		channel))

(define (ssh-send-channel-data channel data)
  (define transport (~ channel 'connection))
  (define buffer (~ channel 'channel-buffer))
  (define buffer-size (bytevector-length buffer))
  (define size-offset +ssh-channel-data-size-offset+)
  (define data-offset +ssh-channel-data-data-offset+)
  (define (check-peer-window-size channel data-len)
    (cond ((ssh-channel-check-peer-window-size channel data-len) =>
	   (lambda (len)
	     (ssh-channel-update-peer-current-size! channel len)
	     len))
	  (else
	   (wait-for-window-adjust channel)
	   (check-peer-window-size channel data-len))))

  (define (do-bv data)
    (define len (bytevector-length data))
    (let loop ((offset 0) (rest len))
      ;; TODO do we need to add extra 5?
      (unless (zero? rest)
	(let ((sending (check-peer-window-size channel rest)))
	  (bytevector-u32-set! buffer size-offset sending (endianness big))
	  (bytevector-copy! data offset buffer data-offset sending)
	  (ssh-write-packet transport  buffer 0 (+ sending data-offset))
	  (loop (+ offset sending) (- rest sending))))))

  (define (do-port port)
    (let loop ()
      (let* ((capacity (check-peer-window-size channel buffer-size))
	     (n (get-bytevector-n! port buffer data-offset capacity)))
        (unless (eof-object? n)
	  (bytevector-u32-set! buffer size-offset n (endianness big))
	  (ssh-write-packet transport buffer 0 (+ n data-offset))
	  (when (= capacity n) (loop))))))

  (check-open ssh-send-channel-data channel)
  ;; in any case, this is needed :)
  (bytevector-u8-set! buffer 0 +ssh-msg-channel-data+)
  (bytevector-u32-set! buffer 1 (~ channel 'sender-channel) (endianness big))
  (cond ((bytevector? data) (do-bv data))
        ((port? data) (do-port data))
        (else
         (error 'ssh-send-channel-data 
      		"bytevector or binary input port required" data))))

(define (ssh-recv-channel-data channel)
  (define transport (~ channel 'connection))
  (check-open ssh-recv-channel-data channel)
  (%recv-channel-data channel))

(define (ssh-execute-command transport command
			     :key (receiver (ssh-binary-data-receiver))
			     :allow-other-keys opts)
  (call-with-ssh-channel (open-client-ssh-session-channel transport)
    (lambda (c)
      (apply ssh-request-pseudo-terminal c opts)
      (ssh-request-exec c command :receiver receiver))))


;; internal APIs
(define (read-channel-packet channel)
  (define transport (~ channel 'connection))
  (define recipient (~ channel 'recipient-channel))
  (let loop ()
    ;; in this API, we always send want-reply #t, so we can check the result
    (let* ((packet (ssh-read-packet transport))
	   (b (bytevector-u8-ref packet 0)))
      (cond ((memv b *ssh-recipent-messages*)
	     (if (= (bytevector-u32-ref packet 1 (endianness big)) recipient)
		 (if (= b +ssh-msg-channel-window-adjust+)
		     (let1 m (bytevector->ssh-message
			      <ssh-msg-channel-window-adjust> packet)
      		       (set! (~ channel 'peer-window-size)
			     (+ (~ channel 'peer-window-size) (~ m 'size)))
		       (loop))
		     packet)
		 (begin 
		   (push-channel-packet! channel packet)
		   (loop))))
	    ;; global must be handled by transport io
	    (else (loop))))))
(define (handle-channel-request-response channel)
  (let* ((packet (read-channel-packet channel))
	 (b (bytevector-u8-ref packet 0)))
    (cond ((= b +ssh-msg-channel-success+) #t)
	  ((= b +ssh-msg-channel-failure+) #f)
	  ;; there shouldn't be any channel data related
	  ;; message as this is the start of the channel
	  (else (error 'handle-channel-request-response
		       "Invalid response from the server" b)))))

;; for multiple channels in one transport
(define *ssh-recipent-messages* (list +ssh-msg-channel-open-confirmation+
				      +ssh-msg-channel-open-failure+
				      +ssh-msg-channel-window-adjust+
				      +ssh-msg-channel-data+
				      +ssh-msg-channel-extended-data+
				      +ssh-msg-channel-eof+
				      +ssh-msg-channel-close+
				      +ssh-msg-channel-request+
				      +ssh-msg-channel-success+
				      +ssh-msg-channel-failure+))
(define (push-channel-packet! channel bv)
  (let ((recipient (bytevector-u32-ref bv 1 (endianness big))))
    ;; TODO
    )

  )

;; we don't assume that the server sends window adjust when
;; n percent of the window is used or so. we use all the window
;; then wait the window adjust, in case the data is big  
(define (wait-for-window-adjust channel)
  (define transport (~ channel 'connection))
  (define (adjust-size m)
    (ssh-channel-update-peer-window-size! channel (~ m 'size)))
  (let ((bv (read-channel-packet channel)))
    (when (= (bytevector-u8-ref bv 0) +ssh-msg-channel-window-adjust+)
      (let ((m (bytevector->ssh-message <ssh-msg-channel-window-adjust> bv)))
	(adjust-size m)))))

;; FIXME not sure if this handles data properly...
(define (%recv-channel-data channel)
  (let loop ((response (read-channel-packet channel)) (status #f))
    (define (adjust-host-window channel-data)
      (let* ((size (bytevector-u32-ref channel-data 5 (endianness big)))
	     (host-size (+ (~ channel 'host-size) size)))
	(set! (~ channel 'host-size) host-size)
	(when (> (+ host-size (~ channel 'host-packet-size))
      		 (~ channel 'host-window-size))
            ;; send window adjust requst
            ;; simply double it for now
            (let* ((new-size (* (~ channel 'host-window-size) 2))
      		   (m (make <ssh-msg-channel-window-adjust>
      			:recipient-channel (~ channel 'sender-channel)
      			:size new-size)))
      	      (set! (~ channel 'host-window-size) new-size)
      	      (ssh-write-ssh-message (~ channel 'connection) m)))
        (bytevector-copy channel-data 9 (bytevector-length channel-data))))
    (let1 b (bytevector-u8-ref response 0)
      (cond ((= b +ssh-msg-channel-data+) (adjust-host-window response))
            ((= b +ssh-msg-channel-eof+) #f)
            ((= b +ssh-msg-channel-close+)
             (close-ssh-channel channel :logical #t)
	     #f)
	    ;; TODO what should we do?
            ((= b +ssh-msg-channel-request+)
             ;; must be exit status but first get the head
             (let* ((in (open-bytevector-input-port response))
      		    (m  (ssh-read-message <ssh-msg-channel-request> in)))
      	       (case (string->symbol (~ m 'request-type))
      		 ((exit-status)
      		  (let1 m (ssh-read-message <ssh-msg-exit-status> in)
      		    (loop (read-channel-packet channel) (~ m 'exit-status))))
      		 ((exit-signal)
      		  ;; TODO should we return something instead of raising
      		  ;; an error?
      		  (let1 m (ssh-read-message <ssh-msg-exit-signal> in)
      		    (error (~ m 'signal-name) (~ m 'message)
      			   (~ m 'core-dumped?))))
      		 (else 
      		  => (lambda (n)
      		       (error 'ssh-recv-channel-data
      			      "unknown exit status" n))))))
            (else
             (error 'ssh-recv-channel-data "unknown response" response))))))
)
