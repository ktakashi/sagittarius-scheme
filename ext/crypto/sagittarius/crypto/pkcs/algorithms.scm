;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkcs/algorithms.scm - PKCS algorithms
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
(library (sagittarius crypto pkcs algorithms)
    (export pkcs-encrypt-data
	    pkcs-decrypt-data

	    x509-algorithm-identifier->kdf
	    x509-algorithm-identifier->cipher
	    oid->kdf
	    oid->cipher
	    oid->encryption-scheme)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto ciphers)
	    (sagittarius crypto keys)
	    (sagittarius crypto pkix algorithms))

(define (pkcs-encrypt-data aid key data . opts)
  (let* ((cipher (apply pkcs-make-cipher aid (cipher-direction encrypt)
			key opts))
	 (r (symmetric-cipher-encrypt-last-block cipher data)))
    (symmetric-cipher-done! cipher)
    r))

(define (pkcs-decrypt-data aid key data . opts)
  (let* ((cipher (apply pkcs-make-cipher aid (cipher-direction decrypt)
			key opts))
	 (r (symmetric-cipher-decrypt-last-block cipher data)))
    (symmetric-cipher-done! cipher)
    r))

(define (pkcs-make-cipher aid direction key . opts)
  (let ((kdf (x509-algorithm-identifier->kdf aid))
	(make-cipher (x509-algorithm-identifier->cipher aid)))
    (let-values (((cipher parameters) (make-cipher key)))
      (symmetric-cipher-init! cipher direction
			      (make-symmetric-key (apply kdf key opts))
			      parameters))))

(define (x509-algorithm-identifier->kdf x509-algorithm-identifier)
  (let ((oid (x509-algorithm-identifier-oid x509-algorithm-identifier))
	(param (x509-algorithm-identifier-parameters
		x509-algorithm-identifier)))
    (oid->kdf oid param)))

(define (x509-algorithm-identifier->cipher x509-algorithm-identifier)
  (let ((oid (x509-algorithm-identifier-oid x509-algorithm-identifier))
	(param (x509-algorithm-identifier-parameters
		x509-algorithm-identifier)))
    (oid->cipher oid param)))

(define-generic oid->kdf)
(define-generic oid->cipher)
(define-generic oid->encryption-scheme)
)
