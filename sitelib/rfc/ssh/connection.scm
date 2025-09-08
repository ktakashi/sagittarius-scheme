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
(library (rfc ssh connection)
    (export <ssh-msg-channel-open>
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

	    <ssh-msg-channel-exec-request>
	    <ssh-msg-channel-subsystem-request>

	    ;; misc
	    <ssh-msg-exit-status>
	    <ssh-msg-exit-signal>

	    ssh-connection-next-channel-id!

	    +ssh-channel-data-recipient-offset+
	    +ssh-channel-data-size-offset+
	    +ssh-channel-data-data-offset+

	    ssh-channel-closed?
	    ssh-channel-check-peer-window-size
	    ssh-channel-update-peer-current-size!
	    ssh-channel-update-host-current-size!
	    ssh-channel-update-peer-window-size!
	    ssh-channel-update-host-window-size!
	    ssh-channel-eof
	    close-ssh-channel
	    )
    (import (rnrs)
	    (srfi :18 multithreading)
	    (clos user)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport io))
;; base classes (session can use this)
(define-ssh-message <ssh-msg-channel-open> (<ssh-message>)
  ((type :byte +ssh-msg-channel-open+)
   (channel-type :utf8-string)
   (sender-channel :uint32)
   (initial-window :uint32)
   (maximum-packet :uint32)))

(define-ssh-message <ssh-msg-channel-open-confirmation> (<ssh-message>)
  ((type :byte +ssh-msg-channel-open-confirmation+)
   (recipient-channel :uint32)
   (sender-channel    :uint32)
   (initial-window    :uint32)
   (maximum-packet    :uint32)))

(define-ssh-message <ssh-msg-channel-open-failure> (<ssh-message>)
  ((type :byte +ssh-msg-channel-open-failure+)
   (recipient-channel :uint32)
   (reason-code       :uint32)
   (description       :string)
   (language          :string #vu8())))

(define-ssh-message <ssh-msg-channel-eof> (<ssh-message>)
  ((type :byte +ssh-msg-channel-eof+)
   (recipient-channel :uint32)))

(define-ssh-message <ssh-msg-channel-close> (<ssh-message>)
  ((type :byte +ssh-msg-channel-close+)
   (recipient-channel :uint32)))

;; base
(define-ssh-message <ssh-msg-channel-request> (<ssh-message>)
  ((type :byte +ssh-msg-channel-request+)
   (recipient-channel :uint32)
   (request-type      :utf8-string)
   (want-reply        :boolean #t)))

(define-ssh-message <ssh-msg-channel-pty-request> (<ssh-msg-channel-request>)
  ((term   :string)
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

(define-ssh-message <ssh-msg-channel-exec-request> (<ssh-msg-channel-request>)
  ((command :string)))

(define-ssh-message <ssh-msg-channel-subsystem-request> 
  (<ssh-msg-channel-request>)
  ((subsystem-name :utf8-string)))

(define (ssh-connection-next-channel-id! conn)
  (mutex-lock! (slot-ref conn 'lock))
  (let ((id (slot-ref conn 'next-channel-id)))
    (slot-set! conn 'next-channel-id (+ id 1))
    (mutex-unlock! (slot-ref conn 'lock))
    id))

(define-constant +ssh-channel-data-recipient-offset+ 1)
(define-constant +ssh-channel-data-size-offset+ 5)
(define-constant +ssh-channel-data-data-offset+ 9)

(define (ssh-channel-check-peer-window-size channel data-len)
  (define buffer (slot-ref channel 'channel-buffer))
  (define buffer-size (bytevector-length buffer))
  (define current-size (slot-ref channel 'peer-size))
  (define window-size (slot-ref channel 'peer-window-size))
  (define len (min data-len (- buffer-size +ssh-channel-data-data-offset+)))
  ;; I assume when the window size is zero, server won't do flow control
  (cond ((zero? window-size) len)
	((= window-size current-size) #f)
	((<= window-size (+ current-size len))
	 ;; diff should never be negative
	 (- window-size current-size))
	;; the length is within the window
	(else len)))
(define (ssh-channel-update-peer-current-size! channel size)
  (update-size channel 'peer-size size))
(define (ssh-channel-update-host-current-size! channel size)
  (update-size channel 'host-size size))
(define (ssh-channel-update-peer-window-size! channel size)
  (update-size channel 'peer-window-size size))
(define (ssh-channel-update-host-window-size! channel size)
  (update-size channel 'host-window-size size))
(define (update-size channel slot size)
  (slot-set! channel slot (+ (slot-ref channel slot) size)))

(define (ssh-channel-closed? c) (not (slot-ref c 'open?)))

(define (ssh-channel-eof channel)
  (let ((e (make <ssh-msg-channel-eof>
             :recipient-channel (slot-ref channel 'recipient-channel))))
    (ssh-write-ssh-message (slot-ref channel 'connection) e)))
(define (close-ssh-channel channel :key (logical #f))
  (define transport (slot-ref channel 'connection))
  (define channels (slot-ref transport 'channels))
  (unless (or (ssh-channel-closed? channel) logical)
    (ssh-channel-eof channel)
    (let ((e (make <ssh-msg-channel-close>
      	       :recipient-channel (slot-ref channel 'recipient-channel))))
      (ssh-write-ssh-message transport e)))
  (set! (slot-ref channel 'open?) #f)
  (hashtable-delete! channels (slot-ref channel 'sender-channel)))
)
