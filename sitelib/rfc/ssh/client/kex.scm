;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/client/kex.scm - SSH2 protocol client transport key exchange
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

#!read-macro=sagittarius/regex
#!nounbound
(library (rfc ssh client kex)
    (export ssh-client-key-exchange
	    *ssh-client-kex-list*)
    (import (rnrs)
	    (srfi :39 parameters)
	    (rfc ssh constants)
	    (rfc ssh types)
	    (rfc ssh transport)
	    (rfc ssh client kex api)
	    (rfc ssh client kex dh)
	    (rfc ssh client kex ecdh))

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
(define ssh-client-key-exchange
  (ssh-key-exchange *ssh-client-kex-list* ssh-client-exchange-kex-message))

)
