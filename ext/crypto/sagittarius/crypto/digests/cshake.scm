;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/digests/cshake.scm - cSHAKE descriptors 
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
(library (sagittarius crypto digests cshake)
    (export *digest:cshake-128* *digest:cshake-256*

	    ;; for KMAC
	    cshake-descriptor?
	    left-encode
	    right-encode
	    encode-string
	    bytepad)
    (import (rnrs)
	    (sagittarius)
	    (sagittarius crypto digests descriptors)
	    (prefix (sagittarius crypto tomcrypt) tc:))
;; cSHAKE
;; TODO these procedures can be optimised without allocating extra bytevectors
(define (left-encode x)
  (let ((n (if (zero? x) 1 (div (+ (bitwise-length x) 7) 8))))
    (bytevector-append (make-bytevector 1 n) (integer->bytevector x))))
(define (right-encode x)
  (let ((n (if (zero? x) 1 (div (+ (bitwise-length x) 7) 8))))
    (bytevector-append (integer->bytevector x) (make-bytevector 1 n))))
(define (encode-string (bv bytevector?))
  (define l (* (bytevector-length bv) 8))
  (unless (< l (expt 2 2040))
    (assertion-violation 'encode-string "Bytevector too large for cSHAKE" bv))
  (bytevector-append (left-encode l) bv))
(define (bytepad x l)
  (let* ((p (bytevector-append (left-encode l) x))
	 (np (mod (- l (mod (bytevector-length p) l)) l)))
    (bytevector-append p (make-bytevector np 0))))

;; KECCAK[256](bytepad(encode_string(N) || encode_string(S), 168) || X || 00, L)
(define ((cshake-initiate capacity rate) :key (name #f) (custom #f))
  (define (enc s) (encode-string (if s s #vu8())))
  (let ((s (tc:keccak-init capacity)))
    (if (or name custom)
	(let ((init (bytepad (bytevector-append (enc name) (enc custom)) rate)))
	  (tc:keccak-process! s init)
	  (cons #x04 s))
	;; just SHAKE, we use label here
	(cons #x1F s))))
(define (cshake-process! s bv . opts)
  (let ((st (cdr s)))
    (apply tc:keccak-process! st bv opts)
    s))
(define (cshake-done! s out . opts)
  (let ((st (cdr s)))
    (apply tc:keccak-done! st (car s) out opts)))

(define-record-type cshake-descriptor
  (parent <digest-descriptor>))

(define *digest:cshake-128* (make-cshake-descriptor "cshake-128"
						    (cshake-initiate 256 168)
						    cshake-process!
						    cshake-done!
						    168
						    #f
						    ;; does cSHAKE have OID?
						    #f))
(define *digest:cshake-256* (make-cshake-descriptor "cshake-256"
						    (cshake-initiate 512 136)
						    cshake-process!
						    cshake-done!
						    136
						    #f
						    ;; does cSHAKE have OID?
						    #f))

)
