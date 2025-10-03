;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/transport.scm - SSH2 server transport
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
(library (rfc ssh server transport)
    (export socket->server-ssh-transport
	    close-server-ssh-transport!
	    ssh-server-transport?
	    *ssh-server-kex-list*
	    *ssh-server-encryption-list*
	    *ssh-server-mac-list*
	    *ssh-server-extension-creator*
	    key-pair->ssh-host-key

	    ssh-unimplemented

	    <ssh-server-transport>
	    ssh-initiate-key-exchange
	    ssh-handle-transport-packet)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (sagittarius socket)
	    (sagittarius crypto keys)
	    (srfi :39 parameters)
	    (rfc ssh types)
	    (rfc ssh crypto)
	    (rfc ssh constants)
	    (rfc ssh server types)
	    (rfc ssh server kex)
	    (rfc ssh transport version)
	    (rfc ssh transport io))

(define (ssh-server-transport? o) (is-a? <ssh-server-transport> o))

(define signature-list '())
(define (default-extension-creator transport)
  (list (make <ssh-msg-ext-info-extension>
	  :name "server-sig-algs"
	  :value (bytevector-copy
		  (ssh-message->bytevector 
		   (list->name-list 
		    (list +public-key-ssh-ed25519+
			  +public-key-ssh-ed448+
			  +public-key-ecdsa-sha2-nistp256+
			  +public-key-ecdsa-sha2-nistp384+
			  +public-key-ecdsa-sha2-nistp521+
			  +public-key-ecdsa-sha2-nistk163+
			  +public-key-ecdsa-sha2-nistp192+
			  +public-key-ecdsa-sha2-nistp224+
			  +public-key-ecdsa-sha2-nistk233+
			  +public-key-ecdsa-sha2-nistb233+
			  +public-key-ecdsa-sha2-nistk283+
			  +public-key-ecdsa-sha2-nistk409+
			  +public-key-ecdsa-sha2-nistb409+
			  +public-key-ecdsa-sha2-nistt571+
			  +public-key-rsa-sha2-512+
			  +public-key-rsa-sha2-256+)))
		  ;; skip length
		  4))
	;; not sure if this makes our life easier...
	#;(make <ssh-msg-ext-info-extension>
	  :name "no-flow-control"
	  ;; "p"
	  :value #vu8(0 0 0 1 112))))

(define *ssh-server-extension-creator*
  (make-parameter default-extension-creator
		  (lambda (v)
		    (unless (procedure? v)
		      (assertion-violation '*ssh-server-extension-creator*
					   "procedure is required" v))
		    v)))

(define-generic ssh-host-key-algorithm)

(define (key-pair->ssh-host-key kp)
  (let ((private (key-pair-private kp))
	(public (key-pair-public kp)))
    (make-ssh-host-key (ssh-host-key-algorithm private) private public)))

(define (socket->server-ssh-transport socket host-keys)
  (let ((transport (make <ssh-server-transport> :socket socket
			 :host-keys host-keys)))
    (guard (e (else (close-server-ssh-transport!
		     transport +ssh-disconnect-key-exchange-failed+
		     (condition-message e))
		    (raise e)))
      ;; this doesn't have packet, so do it here
      (ssh-version-exchange transport)
      (ssh-initiate-key-exchange transport)
      (let ((extensions ((*ssh-server-extension-creator*) transport)))
	(let-values (((out e) (open-bytevector-output-port)))
	  (ssh-write-message
	   (make <ssh-msg-ext-info> :count (length extensions)) out)
	  (for-each (lambda (e) (ssh-write-message e out)) extensions)
	  (ssh-write-packet transport (e))))
      transport)))

(define (close-server-ssh-transport! transport code
				     :optional (description #f))
  (unless (socket-closed? (~ transport 'socket))
    (let ((msg (make <ssh-msg-disconnect> :code code
		     :description (or description ""))))
      (ssh-write-ssh-message transport msg)
      (close-transport-socket! transport))))

(define (ssh-handle-transport-packet transport packet)
  (define type (bytevector-u8-ref packet 0))
  (cond ((= type +ssh-msg-disconnect+)
	 ;; the client must already be closed, so just close socket
	 (close-transport-socket! transport)
	 packet)
	((= type +ssh-msg-ignore+) packet)	  ;; ignore
	((= type +ssh-msg-unimplemented+) packet) ;; ignore
	((= type +ssh-msg-debug+) packet)	  ;; ignore
	((= type +ssh-msg-ext-info+)
	 ;; TODO handle ext info
	 packet)
	((= type +ssh-msg-kexinit+)
	 (ssh-handle-kexinit-packet transport packet)
	 packet)
	(else #f)))

(define (ssh-handle-kexinit-packet transport packet)
  (ssh-initiate-key-exchange transport :peer-packet packet))

(define (ssh-initiate-key-exchange transport . opts)
  (let ((names (map ssh-host-key-algorithm-name (~ transport 'host-keys)))
	(encryption-algorithms (*ssh-server-encryption-list*))
	(mac-algorithms (*ssh-server-mac-list*))
	(kex-algorithms (*ssh-server-kex-list*)))
    (apply ssh-server-key-exchange transport
     :server-host-key-algorithms (list->name-list names)
     :kex-algorithms kex-algorithms
     :encryption-algorithms-client-to-server encryption-algorithms
     :encryption-algorithms-server-to-client encryption-algorithms
     :mac-algorithms-client-to-server mac-algorithms
     :mac-algorithms-server-to-client mac-algorithms
     opts)))

(define (close-transport-socket! transport)
  (socket-shutdown (~ transport 'socket) SHUT_RDWR)
  (socket-close (~ transport 'socket)))
				     
(define-method ssh-server-exchange-kex-message
  (kex transport server-packet client-packet)
  (close-server-ssh-transport! transport +ssh-disconnect-key-exchange-failed+
			       "KEX not supported")
  (error 'ssh-server-key-exchange "KEX not supported" kex))


(define-method ssh-host-key-algorithm ((pk <dsa-private-key>))
  +public-key-ssh-dss+)
(define-method ssh-host-key-algorithm ((pk <rsa-private-key>))
  +public-key-rsa-sha2-512+)
(define-method ssh-host-key-algorithm ((pk <eddsa-private-key>))
  (if (ed25519-key? pk)
      +public-key-ssh-ed25519+
      +public-key-ssh-ed448+))
(define-method ssh-host-key-algorithm ((pk <ecdsa-private-key>))
  (define curve (ecdsa-key-parameter pk))
  (define id (ec-parameter->ssh-ecdsa-identifier curve))
  (string-append "ecdsa-sha2-" id))

(define (ssh-unimplemented transport)
  (let ((msg (make-bytevector 5 +ssh-msg-unimplemented+)))
    (bytevector-u32-set! msg 1 (~ transport 'peer-sequence) (endianness big))
    (ssh-write-ssh-message transport msg)))
  
)
