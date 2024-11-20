;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/pkix/modules/scrypt.scm - scrypt module
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

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7914
#!nounbound
(library (sagittarius crypto pkcs modules scrypt)
    (export *pbes:scrypt*
	    scrypt-params? <scrypt-params>
	    scrypt-params-salt
	    scrypt-params-cost-parameter
	    scrypt-params-block-size
	    scrypt-params-parallelization-parameter
	    scrypt-params-key-length)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto asn1)
	    (sagittarius crypto asn1 modules))
(define oid oid-string->der-object-identifier)
(define *pbes:scrypt* (oid "1.3.6.1.4.1.11591.4.11"))

;; scrypt-params ::= SEQUENCE {
;;        salt OCTET STRING,
;;        costParameter INTEGER (1..MAX),
;;        blockSize INTEGER (1..MAX),
;;        parallelizationParameter INTEGER (1..MAX),
;;        keyLength INTEGER (1..MAX) OPTIONAL }
(define-asn1-encodable <scrypt-params>
  (asn1-sequence
   ((salt :type <der-octet-string> :reader scrypt-params-salt)
    (cost-parameter :type <der-integer> :reader scrypt-params-cost-parameter)
    (block-size :type <der-integer> :reader scrypt-params-block-size)
    (parallelization-parameter :type <der-integer>
			       :reader scrypt-params-parallelization-parameter)
    (key-length :type <der-integer> :optional #t
		:reader scrypt-params-key-length))))
(define (scrypt-params? o) (is-a? o <scrypt-params>))
)
