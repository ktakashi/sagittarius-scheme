;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/server/kex.scm - SSH2 server KEX
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
(library (rfc ssh server kex)
    (export ssh-server-key-exchange
	    ssh-server-exchange-kex-message

	    *ssh-server-kex-list*
	    *ssh-server-encryption-list*
	    *ssh-server-mac-list*)
    (import (rnrs)
	    (clos user)
	    (sagittarius object)
	    (sagittarius socket)
	    (srfi :39 parameters)
	    (rfc ssh types)
	    (rfc ssh constants)
	    (rfc ssh transport kex)
	    (rfc ssh server types)
	    (rfc ssh server kex api)
	    (rfc ssh server kex dh)
	    (rfc ssh server kex ecdh))

(define *ssh-server-kex-list*
  ;; the same as client but ext-info-s :)
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
		   +ext-info-s+			       ;; SHOULD
		   )
      		  list->name-list))

(define *ssh-server-encryption-list*
  (make-parameter (name-list
		   ;; for server, default only AES
      		   +enc-aes256-ctr+
      		   +enc-aes128-ctr+)
		  list->name-list))

(define *ssh-server-mac-list*
  ;; no SHA1
  (make-parameter (name-list +mac-hmac-sha2-256+)
		  list->name-list))

(define ssh-server-key-exchange 
  (ssh-key-exchange ssh-server-exchange-kex-message))

)
