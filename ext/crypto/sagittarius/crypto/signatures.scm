;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures.scm - Signature signer / verifier
;;;  
;;;   Copyright (c) 2022  Takashi Kato  <ktakashi@ymail.com>
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
(library (sagittarius crypto signatures)
    (export signer? <signer> make-signer
	    signer-sign-message signer-init! signer-process! signer-sign!

	    verifier? <verifier> make-verifier
	    verifier-verify-signature verifier-init! verifier-process!
	    verifier-verify!
	    
	    *signature:rsa*
	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message

	    mgf-1
	    pkcs1-emsa-pss-encode pkcs1-emsa-pss-verify
	    pkcs1-emsa-v1.5-encode pkcs1-emsa-v1.5-verify

	    *signature:dsa*
	    *signature:ecdsa*
	    *signature:ed25519*
	    *signature:ed25519ctx*
	    *signature:ed25519ph*
	    *signature:ed448*
	    *signature:ed448ph*
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto signatures rsa)
	    (sagittarius crypto signatures dsa)
	    (sagittarius crypto signatures ecdsa)
	    (sagittarius crypto signatures eddsa)
	    (sagittarius crypto keys)
	    (sagittarius mop immutable))

(define-class <signature> (<immutable>) 
  ((state :init-keyword :state :reader signature-state)
   (processor :init-keyword :processor :reader signature-processor)))

(define-class <signer> (<signature>) ())
(define (signer? o) (is-a? o <signer>))
(define (make-signer scheme key . opts)
  (let ((state (apply make-signer-state scheme key opts)))
    (make <signer> :state state
	  :processor (signature-state-processor state))))

(define (signer-sign-message signer message)
  (signer-sign! (signer-process! (signer-init! signer) message)))

(define (signer-init! (signer signer?))
  (signature-state-init! (signature-state signer))
  signer)
(define (signer-process! (signer signer?) (msg bytevector?)
			 :optional (start 0) (end (bytevector-length msg)))
  ((signature-processor signer) (signature-state signer) msg start end)
  signer)
(define (signer-sign! (signer signer?))
  (signer-state->signature (signature-state signer)))

(define-class <verifier> (<signature>) ())
(define (verifier? o) (is-a? o <verifier>))

(define (make-verifier scheme key . opts)
  (let ((state (apply make-verifier-state scheme key opts)))
    (make <verifier> :state state
	  :processor (signature-state-processor state))))

(define (verifier-verify-signature verifier message signature)
  (verifier-verify! (verifier-process! (verifier-init! verifier) message)
		    signature))
(define (verifier-init! (verifier verifier?))
  (signature-state-init! (signature-state verifier))
  verifier)
(define (verifier-process! (verifier verifier?) (msg bytevector?)
			   :key (start 0) (end (bytevector-length msg)))
  ((signature-processor verifier) (signature-state verifier) msg start end)
  verifier)
(define (verifier-verify! (verifier verifier?) (signature bytevector?))
  (verifier-state-verify-message (signature-state verifier) signature))
)
