;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/types.scm - Signature types
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
(library (sagittarius crypto signatures types)
    (export signer-state? <signer-state>
	    verifier-state? <verifier-state>
	    signature-state? <signature-state>
	    signature-state-key signature-state-init!
	    signature-state-processor

	    digest-signature-state? <digest-sigature-state>
	    digest-signature-state-digest
	    digest-signature-state-init!
	    digest-signature-state-process!
	    digest-signature-state-signing-message!

	    make-signer-state
	    signer-state->signature
	    make-verifier-state
	    verifier-state-verify-message
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto digests)
	    (sagittarius mop immutable))

;; interface
(define-class <signer-state> () ())
(define (signer-state? o) (is-a? o <signer-state>))
(define-class <verifier-state> () ())
(define (verifier-state? o) (is-a? o <verifier-state>))

(define-class <signature-state> (<immutable>)
  ((key :init-keyword :key :reader signature-state-key)))
(define (signature-state? o) (is-a? o <signature-state>))

(define-generic signature-state-init!)
(define-generic signature-state-processor)

;; For RSA, ECDSA et.al
(define-class <digest-sigature-state> (<signature-state>)
  ((digest :init-keyword :digest :reader digest-signature-state-digest)
   (md  :reader digest-signature-state-md :mutable #t)))
(define-method initialize ((o <digest-sigature-state>) initargs)
  (call-next-method)
  (slot-set! o 'md (make-message-digest (slot-ref o 'digest)))
  o)
(define (digest-signature-state? o) (is-a? o <digest-sigature-state>))

(define (digest-signature-state-init! (state signature-state?))
  (message-digest-init! (digest-signature-state-md state)))
(define (digest-signature-state-process! (state signature-state?) bv start end)
  (message-digest-process! (digest-signature-state-md state)
			   bv start (- end start)))
(define (digest-signature-state-signing-message! (state signature-state?)
						 . opts)
  (apply message-digest-done (digest-signature-state-md state) opts))
(define-method signature-state-init! ((o <digest-sigature-state>))
  (digest-signature-state-init! o)
  o)
(define-method signature-state-processor ((o <digest-sigature-state>))
  digest-signature-state-process!)

;; For EdDSA
;; This means, EdDSA is not suitable for signatures of large files / data...
(define-class <buffered-signature-state> (<signature-state>)
  ((buffer :reader buffered-signature-state-buffer)
   (retriever :reader  buffered-signature-state-retriever)))
(define-method initialize ((o <buffered-signature-state>) initargs)
  (call-next-method)
  (let-values (((out e) (open-bytevector-output-port)))
    (slot-set! o 'buffer out)
    (slot-set! o 'retriever e))
  o)
(define (buffered-signature-state? o) (is-a? o <buffered-signature-state>))

(define-generic make-signer-state)
(define-generic signer-state->signature)

(define-generic make-verifier-state)
(define-generic verifier-state-verify-message)
)
