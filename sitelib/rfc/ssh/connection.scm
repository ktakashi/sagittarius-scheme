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
	    <ssh-msg-exit-signal>)
    (import (rfc ssh constants)
	    (rfc ssh types))
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
  ((type :byte +ssh-msg-channel-open-failure+)
   (recipient-channel :uint32)
   (reason-code       :uint32)
   (description       :string)
   (language          :string)))

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
   (request-type      :string)
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
  ((subsystem-name :string)))

)
