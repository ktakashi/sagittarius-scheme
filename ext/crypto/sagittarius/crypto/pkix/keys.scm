;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/keys.scm - X.509 related key operations
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
(library (sagittarius crypto pkix keys)
    (export import-public-key export-public-key
	    subject-public-key-info->public-key
	    public-key->subject-public-key-info

	    import-private-key export-private-key
	    one-asymmetric-key->private-key
	    private-key->one-asymmetric-key
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto pkix modules x509)
	    (sagittarius crypto pkix modules akp)
	    (sagittarius crypto keys))

;; Subject public key info
(define-method import-public-key ((key <subject-public-key-info>))
  (subject-public-key-info->public-key key))
(define-method export-public-key ((key <subject-public-key-info>))
  (asn1-encodable->bytevector key))

(define (subject-public-key-info->public-key (spki subject-public-key-info?))
  (import-public-key (asn1-encodable->asn1-object spki)
		     (public-key-format subject-public-key-info)))
(define (public-key->subject-public-key-info (pk public-key?))
  (let ((bv (export-public-key pk (public-key-format subject-public-key-info))))
    (bytevector->asn1-encodable <subject-public-key-info> bv)))

(define-method import-private-key ((key <one-asymmetric-key>))
  (one-asymmetric-key->private-key key))
(define-method export-private-key ((key <one-asymmetric-key>))
  (asn1-encodable->bytevector key))

(define (one-asymmetric-key->private-key (oakp one-asymmetric-key?))
  (import-private-key (asn1-encodable->asn1-object oakp)
		      (private-key-format private-key-info)))

(define (private-key->one-asymmetric-key (private-key private-key?))
  (let ((bv (export-private-key private-key
				(private-key-format private-key-info))))
    (bytevector->asn1-encodable <one-asymmetric-key> bv)))
)
