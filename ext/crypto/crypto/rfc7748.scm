;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; crypto/rfc7748.scm - X25519 and X448
;;;
;;;  Copyright (c) 2021 Takashi Kato. All rights reserved.
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; The library name is a bit off, but they don't have better name
;; such as X-ECDH or whatsoever. So just use the RFC number...
;; NOTE: user shouldn't use this library alone

;; ref
;; - https://datatracker.ietf.org/doc/html/rfc7748
#!deprecated
#!nounbound
(library (crypto rfc7748)
    (export X25519 X448

	    <x25519-private-key> x25519-private-key?
	    <x448-private-key> x448-private-key?
	    rfc7748-private-key?
	    rfc7748-private-key-random rfc7748-private-key-public-key

	    <x25519-public-key> x25519-public-key?
	    <x448-public-key> x448-public-key?
	    rfc7748-public-key? rfc7748-public-key-data
	    
	    
	    x25519-calculate-agreement
	    x448-calculate-agreement)
    (import (rnrs)
	    (sagittarius crypto keys))
(define X25519 *key:x25519*)
(define X448 *key:x448*)
)
