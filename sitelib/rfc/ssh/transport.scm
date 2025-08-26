;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/transport.scm - SSH2 protocol transport layer.
;;;  
;;;   Copyright (c) 2010-2025  Takashi Kato  <ktakashi@ymail.com>
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh transport)
    (export make-client-ssh-transport
	    open-client-ssh-transport!
	    socket->client-ssh-transport
	    ssh-transport? ssh-client-transport?
	    close-client-ssh-transport!
	    ;; parameter
	    *ssh-version-string*
	    ssh-data-ready?
	    
	    ssh-write-packet
	    ssh-read-packet
	    ssh-service-request

	    ;; given packet is binary input port
	    ssh-write-packet-port
	    ssh-write-ssh-message

	    *ssh:debug-package-handler*
	    *ssh:ignore-package-handler*
	    *ssh:ext-info-handler*
	    )
    (import (rnrs)
	    (sagittarius)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport io)
	    (rfc ssh transport client)
	    (clos user))
;; FIXME
;;   currently it's really bad way to read and write a packet
;;   (especially read).
;;   try read maximum number of packet and encrypt/decrypt if needed.
;; TODO
;;   * make buffer in transport and read&decrypt per cipher block size
(define-constant +max-packet-size+ 35000)

(define (ssh-transport? transport) (is-a? <ssh-transport> transport))

;; As the feature of block cipher, if the decryption is done by
;; in order then it can decrypt per block so that we don't have
;; to allocate huge bytevector buffer after decryption.

;; for my laziness
(define (ssh-service-request transport name)
  (let ((msg (make <ssh-msg-service-request>
	       :service-name (string->utf8 name))))
    (ssh-write-ssh-message transport msg))
  (ssh-read-message <ssh-msg-service-accept>
   (open-bytevector-input-port (ssh-read-packet transport))))
)
