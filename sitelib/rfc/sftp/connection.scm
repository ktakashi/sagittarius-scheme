;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/sftp/connection.scm - SFTP protocol types.
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

;; reference
;;  http://tools.ietf.org/html/draft-ietf-secsh-filexfer-02
(library (rfc sftp connection)
    (export make-client-sftp-connection
	    sftp-close-connection
	    call-with-sftp-connection
	    
	    ;; operations
	    sftp-opendir
	    sftp-readdir
	    sftp-open
	    sftp-read!

	    ;; helper
	    sftp-binary-sink
	    sftp-file-sink

	    ;; low level
	    send-sftp-packet
	    recv-sftp-packet)
    (import (rnrs) 
	    (clos user) 
	    (clos core)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius control)
	    (rfc ssh) 
	    (rfc sftp types) 
	    (binary data)
	    (binary pack))

(define-class <sftp-connection> ()
  ((message-id :init-value 0)
   (transport :init-keyword :transport)
   (channel :init-keyword :channel)))

(define (read-sftp-packet-data type in)
  (ssh-read-message (sftp-class-lookup type) in))
(define (recv-sftp-packet connection)
  (let*-values (((bv) (ssh-recv-channel-data (~ connection 'channel)))
		((len type) (unpack "!LC" bv))
		((tmp) (bytevector-copy bv 5 (+ len 4)))) ;; len-1+5
    (let ((r (read-sftp-packet-data type (open-bytevector-input-port tmp))))
      (if (= (bytevector-length bv) (+ len 4))
	  (values r #vu8()) ;; only one packet
	  (values r (bytevector-copy bv (+ 4 len)))))))

(define (send-sftp-packet connection data)
  (let ((bv (ssh-message->bytevector data))
	(type (sftp-type-lookup (class-of data))))
    (unless type (error 'send-sftp-packet "unknown sftp packet type" data))
    (ssh-send-channel-data 
     (~ connection 'channel)
     (call-with-bytevector-output-port
      (lambda (out)
	(put-u32 out (+ (bytevector-length bv) 1) (endianness big))
	(put-u8 out type)
	(put-bytevector out bv))))))

(define (make-client-sftp-connection server port 
				     :key (username #f) (password #f))
  (let ((transport (make-client-ssh-transport server port)))
    ;; TODO supporting other authentication method 
    (when (and username password)
      (ssh-authenticate transport +ssh-auth-method-password+ username password))
    ;; TODO handle error to disconnect!
    (let* ((channel (open-client-ssh-session-channel transport))
	   (connection (make <sftp-connection> :transport transport 
			     :channel channel)))
      (ssh-request-subsystem channel "sftp")
      ;; no extension data for now
      (send-sftp-packet connection 
			(make <sftp-fxp-init> :version +sftp-version3+))
      (let-values (((r rest) (recv-sftp-packet connection)))
	;; assume rest is empty
	(unless (and (is-a? r <sftp-fxp-version>)
		     (= (~ r 'version) +sftp-version3+))
	  (error 'make-client-ssh-transport 
		 "server respond non supported version" r))
	connection))))
(define (sftp-close-connection connection)
  (close-ssh-channel (~ connection 'channel))
  (ssh-disconnect (~ connection 'transport)))

(define (call-with-sftp-connection server port proc . opts)
  (let ((conn (apply make-client-sftp-connection server port opts)))
    (receive r (proc conn)
      (sftp-close-connection conn)
      (apply values r))))

(define (sftp-message-id! conn)
  (rlet1 id (~ conn 'message-id)
    (set! (~ conn 'message-id) (+ id 1))))
(define-syntax check-status
  (syntax-rules ()
    ((_ v)
     (when (is-a? v <sftp-fxp-status>)
       ;; todo condition?
       (error 'who (format "~a code[~a]" (utf8->string (~ v 'message))
			   (~ v 'code)))))))
(define (recv-sftp-packet1 conn)
  (let-values (((r rest) (recv-sftp-packet conn)))
    (check-status r)
    r))

(define (sftp-opendir conn path)
  (let1 id (sftp-message-id! conn)
    (send-sftp-packet conn (make <sftp-fxp-opendir> :id id :path path))
    (recv-sftp-packet1 conn)))
(define (sftp-readdir conn handle/path)
  (let1 handle (if (is-a? handle/path <sftp-fxp-handle>) 
		   handle/path
		   (sftp-opendir conn handle/path))
    (send-sftp-packet conn (make <sftp-fxp-readdir>
			     :id (sftp-message-id! conn)
			     :handle (~ handle 'handle)))
    (recv-sftp-packet1 conn)))

(define (sftp-open conn filename pflags)
  (let1 id (sftp-message-id! conn)
    ;; for now don't consider attrs
    (send-sftp-packet conn (make <sftp-fxp-open> :id id :filename filename
				 :pflags pflags))
    (recv-sftp-packet1 conn)))
(define-constant +sftp-default-buffer-size+ 4096)
(define (sftp-read! conn handle/filename sink)
  (let1 handle (~ (if (is-a? handle/filename <sftp-fxp-handle>) 
		      handle/filename
		      (sftp-open conn handle/filename +ssh-fxf-read+))
		  'handle)
    ;; ok now read it
    ;; read may be multiple part so we need to read it until
    ;; server respond <sftp-fxp-status>
    ;; the buffer size is 4096 fixed for now.
    (let loop ((offset 0))
      (send-sftp-packet conn (make <sftp-fxp-read>
			       :id (sftp-message-id! conn)
			       :handle handle
			       :offset offset
			       :len +sftp-default-buffer-size+))
      (let-values (((r ignore) (recv-sftp-packet conn)))
	(if (is-a? r <sftp-fxp-status>)
	    (sink -1 (eof-object))
	    (begin
	      ;; must be <sftp-fxp-data>
	      (sink offset (~ r 'data))
	      (loop (+ offset +sftp-default-buffer-size+))))))))
(define (sftp-binary-sink)
  (let-values (((out extract) (open-bytevector-output-port)))
    (lambda (offset data)
      (if (eof-object? data)
	  (extract)
	  (put-bytevector out data)))))
(define (sftp-file-sink filename :key (options (file-options)))
  (let1 out (open-file-output-port filename options 'block #f)
    (lambda (offset data)
      (if (eof-object? data)
	  (close-port out)
	  (put-bytevector out data)))))
)