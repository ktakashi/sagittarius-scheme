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
	    make-ssh-verifier

	    ssh-ecdsa-digest-descriptor)
    (import (rnrs)
	    (clos user)
	    (rfc ssh types)
	    (rfc ssh constants)
	    (sagittarius)
	    (sagittarius crypto keys)
	    (sagittarius crypto digests)
	    (sagittarius crypto signatures)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto math ec)
	    (sagittarius crypto asn1))

(define-generic make-ssh-signer :class <predicate-specializable-generic>)
(define-method make-ssh-signer (ignore (pk <dsa-private-key>))
  (make-signer *signature:dsa* pk :der-encode #f))  

(define-method make-ssh-signer ((alg (equal +public-key-ssh-rsa+))
				(pk <rsa-private-key>))
  (make-signer *signature:rsa* pk
	       :encoder pkcs1-emsa-v1.5-encode
	       :digest *digest:sha-1*))
(define-method make-ssh-signer ((alg (equal +public-key-rsa-sha2-256+))
				(pk <rsa-private-key>))
  (make-signer *signature:rsa* pk :encoder pkcs1-emsa-v1.5-encode))
(define-method make-ssh-signer ((alg (equal +public-key-rsa-sha2-512+))
				(pk <rsa-private-key>))
  (make-signer *signature:rsa* pk
	       :encoder pkcs1-emsa-v1.5-encode
	       :digest *digest:sha-512*))

(define-method make-ssh-signer (ignore (pk <eddsa-private-key>))
  (if (ed25519-key? pk)
      (make-signer *signature:ed25519* pk)
      (make-signer *signature:ed448* pk)))

(define-method make-ssh-signer (ignore (pk <ecdsa-private-key>))
  (define (creator key)
    (make-signer *signature:ecdsa* key 
     :digest (ssh-ecdsa-digest-descriptor (ecdsa-key-parameter key))))
  (make-signer :ssh-ecdsa pk :creator creator))

(define-generic make-ssh-verifier :class <predicate-specializable-generic>)
(define-method make-ssh-verifier ((alg (equal +public-key-ssh-rsa+))
				  key)
  (make-verifier *signature:rsa* key :digest *digest:sha-1*
		 :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (equal +public-key-rsa-sha2-256+)) key)
  (make-verifier *signature:rsa* key :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (equal +public-key-rsa-sha2-512+)) key)
  (make-verifier *signature:rsa* key :digest *digest:sha-512*
		 :verifier pkcs1-emsa-v1.5-verify))
(define-method make-ssh-verifier ((alg (equal +public-key-ssh-dss+)) key)
  (make-verifier *signature:dsa* key :der-encode #f))
(define-method make-ssh-verifier ((alg (equal +public-key-ssh-ed25519+)) key)
  (make-verifier *signature:ed25519* key))
(define-method make-ssh-verifier ((alg (equal +public-key-ssh-ed448+)) key)
  (make-verifier *signature:ed448* key))
(define-method make-ssh-verifier (ignore (key <ecdsa-public-key>))
  (define (creator key)
    (make-verifier *signature:ecdsa* key
     :digest (ssh-ecdsa-digest-descriptor (ecdsa-key-parameter key))))
  (make-verifier :ssh-ecdsa key :creator creator))

(define (ssh-ecdsa-digest-descriptor ec-parameter)
  (let* ((field (elliptic-curve-field (ec-parameter-curve ec-parameter)))
	 (b (if (ec-field-fp? field)
		(bitwise-length (ec-field-fp-p field))
		(ec-field-f2m-m field))))
    (cond ((<= b 256)                 *digest:sha-256*)
	  ((and (< 256 b) (<= b 384)) *digest:sha-384*)
	  (else                       *digest:sha-512*))))

;; need wrapper for ECDSA signature, due to the different format
(define-class <ssh-ecdsa-delegate-state> (<signer-state> <verifier-state>)
  ((delegate :init-keyword :delegate :reader ssh-ecdsa-signer-state-delegate)))
(define-method make-signer-state ((m (eql :ssh-ecdsa))
				  (key <ecdsa-private-key>)
				  :key creator
				  :allow-other-keys ignore)
  (make <ssh-ecdsa-delegate-state> :delegate (creator key)))
(define-method signature-state-init! ((o <ssh-ecdsa-delegate-state>))
  (define delegete (ssh-ecdsa-signer-state-delegate o))
  (signature-state-init! (slot-ref delegete 'state))
  o)
(define (wrapper-signature-state-process! o bv start len)
  (let ((delegate (ssh-ecdsa-signer-state-delegate o)))
    ((slot-ref delegate 'processor) (slot-ref delegate 'state)  bv start len)))
(define-method signature-state-processor ((o <ssh-ecdsa-delegate-state>))
  wrapper-signature-state-process!)
(define-method signer-state->signature ((o <ssh-ecdsa-delegate-state>))
  (define delegate (ssh-ecdsa-signer-state-delegate o))
  (let* ((s (bytevector->asn1-object
	     (signer-state->signature (slot-ref delegate 'state))))
	 (r&s (map der-integer->integer (asn1-collection->list s))))
    (let-values (((out e) (open-bytevector-output-port)))
      (ssh-write-message :mpint (car r&s) out #f)
      (ssh-write-message :mpint (cadr r&s) out #f)
      (e))))

(define-method make-verifier-state ((m (eql :ssh-ecdsa))
				    (key <ecdsa-public-key>)
				    :key creator
				    :allow-other-keys ignore)
  (make <ssh-ecdsa-delegate-state> :delegate (creator key)))

(define-method verifier-state-verify-message ((o <ssh-ecdsa-delegate-state>)
					      signature)
  (define delegate (ssh-ecdsa-signer-state-delegate o))
  ;; decode signature
  (define bin (open-bytevector-input-port signature))
  (let* ((r (ssh-read-message :mpint bin #f))
	 (s (ssh-read-message :mpint bin #f)))
    (verifier-state-verify-message (slot-ref delegate 'state)
				   (asn1-encodable->bytevector
				    (der-sequence
				     (integer->der-integer r)
				     (integer->der-integer s))))))
)
