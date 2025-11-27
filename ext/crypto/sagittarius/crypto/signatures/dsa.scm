;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/dsa.scm - DSA signature
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
(library (sagittarius crypto signatures dsa)
    (export *signature:dsa*
	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message

	    ;; ECDSA can use this
	    <dsa-state> dsa-state-der-encode?
	    <dsa-signer-state> dsa-signer-state-k-generator
	    construct-dsa-signature deconstruct-dsa-signature
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto signatures k-generators)
	    (sagittarius crypto keys)
	    (sagittarius crypto math modular)
	    (sagittarius crypto math prime)
	    (sagittarius crypto digests)
	    (sagittarius crypto random)
	    (sagittarius crypto secure)
	    (util bytevector))

(define *signature:dsa* *key:dsa*)

(define-class <dsa-state> (<digest-signature-state>)
  ((der-encode? :init-keyword :der-encode :reader dsa-state-der-encode?)))
(define-class <dsa-signer-state> (<dsa-state> <signer-state>)
  ((k-generator :init-keyword :k-generator
		:reader dsa-signer-state-k-generator)))
(define-class <dsa-verifier-state> (<dsa-state> <verifier-state>) ())

(define (get-digest who param)
  (cond ((get-dsa-digest (dsa-key-parameter-p param)))
	(else
	 (assertion-violation who "Can't determine the digest algorithm"))))
(define-method make-signer-state ((m (eql *signature:dsa*))
				  (key <dsa-private-key>)
				  :key (k-generator default-k-generator)
				       (der-encode #t)
				       (digest #f)
				  :allow-other-keys opts)
  (make <dsa-signer-state> :k-generator k-generator :key key
	:digest (or digest
		    (get-digest 'make-signer-state (dsa-key-parameter key)))
	:der-encode der-encode))

(define (construct-dsa-signature state r s size)
  (if (dsa-state-der-encode? state)
      (asn1-encodable->bytevector
       (der-sequence
	(integer->der-integer r)
	(integer->der-integer s)))
      (bytevector-append (integer->bytevector r size)
			 (integer->bytevector s size))))

(define (deconstruct-dsa-signature state S size)
  (define (check-leading-zero bv)
    (and (>= (bytevector-length bv) 2)
	 (zero? (bytevector-u8-ref bv 0))
	 (zero? (bytevector-u8-ref bv 1))
	 (error 'deconstruct-dsa-signature
		"Signature value contains leading 0")))
  (if (dsa-state-der-encode? state)
      (let* ((in (open-bytevector-input-port S))
	     (seq (read-asn1-object in)))
	(unless (eof-object? (get-u8 in))
	  (error 'deconstruct-dsa-signature "Signature contains extra bytes"))
	(unless (der-sequence? seq)
	  (error 'deconstruct-dsa-signature "Signature is not a DER sequence"))
	(let-values (((r s) (deconstruct-asn1-collection seq)))
	  (check-leading-zero (asn1-simple-object-value r))
	  (check-leading-zero (asn1-simple-object-value s))
	  (values (der-integer->uinteger r) (der-integer->uinteger s))))
      (let-values (((r s) (bytevector-split-at* S size)))
	(values (bytevector->uinteger r) (bytevector->uinteger s)))))

(define-method signer-state->signature ((state <dsa-signer-state>))
  (define key (signature-state-key state))
  (define param (dsa-key-parameter key))
  (define size
    (let ((q (dsa-key-parameter-q param)))
      (div (+ (bitwise-length q) 7) 8)))
  (let* ((p (dsa-key-parameter-p param))
	 (q (dsa-key-parameter-q param))
	 (g (dsa-key-parameter-g param))
	 (X (dsa-private-key-X key))
	 (signing-message (digest-signature-state-signing-message! state))
	 (k ((dsa-signer-state-k-generator state) q X signing-message))
	 (r (mod (mod-expt g (+ k q) p) q))
	 (H (bytevector->integer signing-message))
	 (xr+h (mod (+ (* X r) H) q))
	 (kinv (mod-inverse k q))
	 (s (mod (* (if (> xr+h q) (- xr+h q) xr+h) kinv) q)))
    (construct-dsa-signature state r s size)))
    
(define-method make-verifier-state ((m (eql *signature:dsa*))
				    (key <dsa-public-key>)
				    :key (der-encode #t) (digest #f)
				    :allow-other-keys opts)
  (make <dsa-verifier-state> :der-encode der-encode
	:key key
	:digest (or digest
		    (get-digest 'make-verifier-state (dsa-key-parameter key)))))

(define-method verifier-state-verify-message ((state <dsa-verifier-state>)
					      (signature <bytevector>))
  (define key (signature-state-key state))
  (define param (dsa-key-parameter key))
  (define size
    (let ((q (dsa-key-parameter-q param)))
      (div (+ (bitwise-length q) 7) 8)))
  (define (compute-e q M)
    (if (>= (bitwise-length q) (* (bytevector-length M) 8))
	(bytevector->uinteger M)
	(bytevector->uinteger M 0 (div (bitwise-length q) 8))))
  (let* ((p (dsa-key-parameter-p param))
	 (q (dsa-key-parameter-q param))
	 (g (dsa-key-parameter-g param))
	 (Y (dsa-public-key-Y key))
	 (m (compute-e q (digest-signature-state-signing-message! state)))
	 (digest (digest-signature-state-digest state)))
    (let-values (((r s)
		  (deconstruct-dsa-signature state signature size)))
      (let* ((w (mod-inverse s q))
	     (u1 (mod (* m w) q))
	     (u2 (mod (* r w) q))
	     (u1d (mod-expt g u1 p))
	     (u2d (mod-expt Y u2 p))
	     (v (mod (mod (* u1d u2d) p) q)))
	(and (not (or (< r 1) (<= q r)))
	     (not (or (< s 1) (<= q s)))
	     (= v r))))))
)
