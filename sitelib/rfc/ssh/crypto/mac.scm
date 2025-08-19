;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/crypto/mac.scm - SSH2 cryptography
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
(library (rfc ssh crypto mac)
    (export make-ssh-mac)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (sagittarius)
	    (sagittarius crypto mac)
	    (sagittarius crypto digests)
	    (sagittarius crypto ciphers)) ;; for cmac if we support

(define (make-ssh-mac name key-retriever)
  (let-values (((desc size opts) (ssh-mac-descriptor (string->keyword name))))
    (apply make-mac desc (key-retriever size) opts)))

(define-generic ssh-mac-descriptor)
(define (hmac-descriptor digest)
  (let ((size (digest-descriptor-digest-size digest)))
    (values *mac:hmac* size (list :digest digest))))
(define-method ssh-mac-descriptor ((m (eql :hmac-sha1)))
  (hmac-descriptor *digest:sha-1*))
(define-method ssh-mac-descriptor ((m (eql :hmac-sha2-256)))
  (hmac-descriptor *digest:sha-256*))
(define-method ssh-mac-descriptor ((m (eql :hmac-sha2-512)))
  (hmac-descriptor *digest:sha-512*))
(define-method ssh-mac-descriptor ((m (eql :hmac-md5)))
  (hmac-descriptor *digest:md5*))

)
