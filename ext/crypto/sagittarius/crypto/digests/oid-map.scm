;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/digests/oid-map.scm - Digest OID map
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
(library (sagittarius crypto digests oid-map)
    (export oid->digest-descriptor)
    (import (rnrs)
	    (clos user)
	    (sagittarius crypto digests descriptors)
	    (sagittarius crypto digests cshake))

(define *oid-map*
  (map (lambda (d) (cons (digest-descriptor-oid d) d))
       (list *digest:whirlpool*
	     *digest:ripemd-128* *digest:ripemd-160*
	     *digest:ripemd-256* *digest:ripemd-320*
	     *digest:sha-1*
	     *digest:sha-224* *digest:sha-256*
	     *digest:sha-384*
	     *digest:sha-512* *digest:sha-512/224* *digest:sha-512/256*
	     *digest:sha3-224* *digest:sha3-256* *digest:sha3-384*
	     *digest:sha3-512*
	     *digest:keccak-224* *digest:keccak-256* *digest:keccak-384*
	     *digest:keccak-512*
	     *digest:tiger-192*
	     *digest:md5* *digest:md4* *digest:md2*
	     *digest:blake2s-128* *digest:blake2s-160*
	     *digest:blake2s-224* *digest:blake2s-256*
	     *digest:blake2b-160* *digest:blake2b-256*
	     *digest:blake2b-384* *digest:blake2b-512*
	     *digest:shake-128* *digest:shake-256*
	     *digest:cshake-128* *digest:cshake-256*)))

(define-generic oid->digest-descriptor)
;; Default, if you need, implement the method
(define-method oid->digest-descriptor ((oid <string>))
  (cond ((assoc oid *oid-map*) => cdr)
	(else #f)))
)
