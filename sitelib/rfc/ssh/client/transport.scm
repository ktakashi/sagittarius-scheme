;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/client/transport.scm - SSH2 client transport
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
(library (rfc ssh client transport)
    (export make-client-ssh-transport
	    socket->client-ssh-transport
	    open-client-ssh-transport!
	    close-client-ssh-transport!
	    ssh-client-transport?
	    ssh-client-service-request

	    *ssh-client-kex-list*
	    *ssh-client-public-key-list*
	    *ssh-client-encryption-list*
	    *ssh-client-mac-list*)

    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius object)
	    (sagittarius socket)
	    (srfi :39 parameters)
	    (rfc ssh types)
	    (rfc ssh constants)
	    (rfc ssh transport)
	    (rfc ssh client kex))

(define *ssh-client-kex-list*
  ;; The keyword is from in RFC9142
  (make-parameter (name-list
		   +kex-curve25519-sha256+	       ;; SHOUD
		   +kex-curve448-sha512+	       ;; MAY
		   +kex-ecdh-sha2-nistp256+	       ;; SHOULD
		   +kex-ecdh-sha2-nistp384+	       ;; SHOULD
		   +kex-ecdh-sha2-nistp521+	       ;; SHOULD
		   +kex-diffie-hellman-group15-sha512+ ;; MAY
		   +kex-diffie-hellman-group16-sha512+ ;; SHOULD
		   +kex-diffie-hellman-group17-sha512+ ;; MAY
		   +kex-diffie-hellman-group18-sha512+ ;; MAY
      		   +kex-diffie-hellman-group-exchange-sha256+ ;; MAY
		   +kex-diffie-hellman-group14-sha256+ ;; MUST
		   +ext-info-c+			       ;; SHOULD

		   ;; Below are marked as SHOULD NOT in RFC9142 or using 
		   ;; less secure digest algorithm, i.e. SHA1
      		   ;; +kex-diffie-hellman-group-exchange-sha1+ ;; SHOULD NOT
      		   ;; +kex-diffie-hellman-group14-sha1+        ;; MAY
      		   ;; +kex-diffie-hellman-group1-sha1+         ;; SHOULD NOT
		   )
      		  list->name-list))

(define *ssh-client-public-key-list*
  (make-parameter (name-list
		   +public-key-ssh-ed448+
		   +public-key-ssh-ed25519+
		   +public-key-ecdsa-sha2-nistp256+
		   +public-key-ecdsa-sha2-nistp384+
		   +public-key-ecdsa-sha2-nistp521+
		   +public-key-rsa-sha2-256+
		   +public-key-rsa-sha2-512+
		   ;; let's not use SHA1 by default
      		   ;; +public-key-ssh-rsa+
      		   ;; +public-key-ssh-dss+
		   )
		  list->name-list))

(define *ssh-client-encryption-list*
  (make-parameter (name-list 
      		   ;; AEC CTR -> CBC
      		   +enc-aes256-ctr+
      		   +enc-aes128-ctr+
      		   +enc-aes256-cbc+
      		   +enc-aes128-cbc+
		   ;; Ok, blowfish should be better than 3DES
		   +enc-blowfish-ctr+
		   +enc-blowfish-cbc+
		   ;; We don't recommend to use broken cipher
		   ;; but for the legacy systems?
		   +enc-3des-ctr+ 
      		   +enc-3des-cbc+)
		  list->name-list))

(define *ssh-client-mac-list*
  (make-parameter (name-list +mac-hmac-sha2-256+ +mac-hmac-sha1+)
		  list->name-list))

(define-class <ssh-client-transport> (<ssh-transport>)
  ((server-signature-algorithms :init-value #f)
   (host-version :allocation :delegate :forwarding 'client-version)
   (peer-version :allocation :delegate :forwarding 'server-version)))

(define-method write-object ((o <ssh-client-transport>) out)
  (format out "#<ssh-client-transport ~a ~a ~a ~a ~a ~a>"
	  (slot-ref o 'server-version)
	  (slot-ref o 'client-enc)
	  (slot-ref o 'server-enc)
	  (slot-ref o 'client-mac)
	  (slot-ref o 'server-mac)
	  (slot-ref o 'server-signature-algorithms)))

;; must do until handshake but for now
(define (make-client-ssh-transport server port)
  (make <ssh-client-transport> :server server :port port))
;; should work but not tested
(define (socket->client-ssh-transport socket)
  (make <ssh-client-transport> :socket socket))
(define (ssh-client-transport? transport)
  (is-a? <ssh-client-transport> transport))

(define (open-client-ssh-transport! transport)
  (unless (~ transport 'socket)
    (let ((server (~ transport 'server))
	  (port (~ transport 'port)))
      (set! (~ transport 'socket) (make-client-socket server port))))
  ;; didn't make performance better
  ;; (socket-setsockopt! socket IPPROTO_TCP TCP_NODELAY 1)
  ;; (socket-setsockopt! socket SOL_SOCKET SO_RCVBUF #x40000)
  (let ((public-key-algorithms (*ssh-client-public-key-list*))
	(encryption-algorithms (*ssh-client-encryption-list*))
	(mac-algorithms (*ssh-client-mac-list*))
	(kex-algorithms (*ssh-client-kex-list*)))
    (ssh-version-exchange transport)
    (ssh-client-key-exchange transport
     :kex-algorithms kex-algorithms
     :server-host-key-algorithms public-key-algorithms
     :encryption-algorithms-client-to-server encryption-algorithms
     :encryption-algorithms-server-to-client encryption-algorithms
     :mac-algorithms-client-to-server mac-algorithms
     :mac-algorithms-server-to-client mac-algorithms)
    transport))


(define (close-client-ssh-transport! transport
				     :key (code +ssh-disconnect-by-application+)
				     (description "finish"))
  (let ((msg (make <ssh-msg-disconnect> :code code :description description)))
    (ssh-write-ssh-message transport msg)
    (socket-shutdown (~ transport 'socket) SHUT_RDWR)
    (socket-close (~ transport 'socket))))

(define (ssh-client-service-request transport name)
  (let ((msg (make <ssh-msg-service-request> :service-name name)))
    (ssh-write-ssh-message transport msg))
  (let ((packet (ssh-read-packet transport)))
    (bytevector->ssh-message <ssh-msg-service-accept> packet)))
)
