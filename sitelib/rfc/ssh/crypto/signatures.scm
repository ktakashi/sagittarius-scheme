;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; rfc/ssh/crypto/signatures.scm - SSH2 cryptography
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
(library (rfc ssh crypto signatures)
    (export make-ssh-signer
	    make-ssh-verifier)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures))

(define-generic make-ssh-signer)
(define-method make-ssh-signer (ignore (pk <dsa-private-key>))
  (make-signer *signature:dsa* pk :der-encode #f))  

(define-method make-ssh-signer ((alg (eql :ssh-rsa)) (pk <rsa-private-key>))
  (apply make-signer *signature:rsa* pk
	 :encoder pkcs1-emsa-v1.5-encode
	 :digest *digest:sha-1*))
(define-method make-ssh-signer ((alg (eql :rsa-sha2-256)) (pk <rsa-private-key>))
  (make-signer *signature:rsa* pk :encoder pkcs1-emsa-v1.5-encode))
(define-method make-ssh-signer ((alg (eql :rsa-sha2-512)) (pk <rsa-private-key>))
  (make-signer *signature:rsa* pk
	       :encoder pkcs1-emsa-v1.5-encode
	       :digest *digest:sha-512*))

(define-method make-ssh-signer (ignore (pk <eddsa-private-key>))
  (if (ed25519-key? pk)
      (make-signer *signature:ed25519* pk)
      (make-signer *signature:ed448* pk)))

(define-generic make-ssh-verifier)
(define-method make-ssh-verifier ((alg (eql :ssh-rsa)) key)
  (make-verifier *signature:rsa* key :digest *digest:sha-1*
		 :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (eql :rsa-sha2-256)) key)
  (make-verifier *signature:rsa* key :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (eql :rsa-sha2-512)) key)
  (make-verifier *signature:rsa* key :digest *digest:sha-512*
		 :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (eql :ssh-dss)) key)
  (make-verifier *signature:dsa* key :der-encode #f))


)
