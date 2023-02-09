;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/signatures/ecdsa.scm - ECDSA signature
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
(library (sagittarius crypto signatures ecdsa)
    (export *signature:ecdsa*
	    make-signer-state signer-state->signature
	    make-verifier-state verifier-state-verify-message)
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius crypto asn1)
	    (sagittarius crypto signatures types)
	    (sagittarius crypto signatures k-generators)
	    (sagittarius crypto signatures dsa)
	    (sagittarius crypto keys)
	    (sagittarius crypto math modular)
	    (sagittarius crypto math ec)
	    (sagittarius crypto digests))

(define *signature:ecdsa* *key:ecdsa*)

(define-class <ecdsa-signer-state> (<dsa-signer-state>) ())
(define-class <ecdsa-verifier-state> (<dsa-state> <verifier-state>) ())

(define-method make-signer-state ((m (eql *signature:ecdsa*))
				  (key <ecdsa-private-key>)
				  :key (k-generator default-k-generator)
				       (der-encode #t)
				       (digest *digest:sha-256*)
				  :allow-other-keys opts)
  (make <ecdsa-signer-state> :k-generator k-generator :key key
	:digest digest :der-encode der-encode))

(define-method signer-state->signature ((state <ecdsa-signer-state>))
  (define key (signature-state-key state))
  (define k-generator (dsa-signer-state-k-generator state))
  (define ec-param (ecdsa-key-parameter key))
  (define M (digest-signature-state-signing-message! state))

  (define (compute-r ec k-generator n d M)
    (define G (ec-parameter-g ec))
    (define curve (ec-parameter-curve ec))
    (let loop ()
      (let* ((k (k-generator n d M))
	     (p (ec-point-mul curve G k))
	     (r (mod (ec-point-x p) n)))
	(if (zero? r)
	    (loop)
	    (values r k)))))
  (define (compute-s r k e d n) (mod (* (mod-inverse k n) (+ e (* d r))) n))
  (let* ((n (ec-parameter-n ec-param))
	 (e (compute-e n M))
	 (d (ecdsa-private-key-d key)))
    (let loop ()
      (let-values (((r k) (compute-r ec-param k-generator n d M)))
	(let ((s (compute-s r k e d n)))
	  (if (zero? s)
	      (loop)
	      (let ((size (ceiling (/ (bitwise-length n) 8))))
		(construct-dsa-signature state r s size))))))))
    
(define-method make-verifier-state ((m (eql *signature:ecdsa*))
				    (key <ecdsa-public-key>)
				    :key (der-encode #t)
					 (digest *digest:sha-256*)
				    :allow-other-keys opts)
  (make <ecdsa-verifier-state> :der-encode der-encode :key key :digest digest))

(define-method verifier-state-verify-message ((state <ecdsa-verifier-state>)
					      (signature <bytevector>))
  (define key (signature-state-key state))
  (define ec-param (ecdsa-key-parameter key))
  (define M (digest-signature-state-signing-message! state))
  (define (size n) (ceiling (/ (bitwise-length n) 8)))
  (let* ((n (ec-parameter-n ec-param))
	 (e (compute-e n M)))
    (let-values (((r s) (deconstruct-dsa-signature state signature (size n))))
      (let* ((w (mod-inverse s n))
	     (u1 (mod (* e w) n))
	     (u2 (mod (* r w) n))
	     (G (ec-parameter-g ec-param))
	     (Q (ecdsa-public-key-Q key))
	     (curve (ec-parameter-curve ec-param)))
	(and (not (or (< r 1) (< n r) (< s 1) (< n s)))
	     (let ((point (ec-point-add curve
					(ec-point-mul curve G u1)
					(ec-point-mul curve Q u2))))
	       (= (mod (ec-point-x point) n) (mod r n))))))))


(define (compute-e n M)
  (let ((len (bitwise-length n))
	(M-bits (* (bytevector-length M) 8)))
    (let ((e (bytevector->uinteger M)))
      (if (< len M-bits)
	  (bitwise-arithmetic-shift-right e (- M-bits len))
	  e))))
)
