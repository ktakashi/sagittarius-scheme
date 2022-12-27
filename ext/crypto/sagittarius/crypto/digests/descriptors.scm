;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/digests/descriptors.scm - Digest descriptors 
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
(library (sagittarius crypto digests descriptors)
    (export digest-descriptor? make-digest-descriptor
	    (rename (digest-descriptor <digest-descriptor>))
	    digest-descriptor-name
	    digest-descriptor-init
	    digest-descriptor-process
	    digest-descriptor-done
	    digest-descriptor-digest-size
	    digest-descriptor-block-size
	    digest-descriptor-oid

	    *digest:whirlpool*
	    
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
	    *digest:cshake-128* *digest:cshake-256*
	    
	    oid->digest-descriptor

	    ;; For HMAC
	    tc-digest-descriptor?
	    tc-digest-descriptor-digest
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (prefix (sagittarius crypto tomcrypt) tc:))

(define-record-type digest-descriptor
  (fields name
	  init ;; thunk to initiate
	  process
	  done
	  block-size
	  digest-size
	  oid))
(define-method write-object ((o digest-descriptor) p)
  (format p "#<digest-descriptor ~a>" (digest-descriptor-name o)))
(define-record-type tc-digest-descriptor
  (parent digest-descriptor)
  (fields digest))

(define ((make-digest-initiator digest) . ignore) (tc:digest-init digest))
(define (digest-done! ds out :optional (start 0) (ignore #f))
  (tc:digest-done! ds out start))
(define-syntax build-digest-descriptor
  (lambda (x)
    (define (->name k name)
      (define n (symbol->string (syntax->datum name)))
      (datum->syntax k(string->symbol (string-append "tc:" n))))
    (syntax-case x ()
      ((k name)
       (with-syntax ((tc:digest (->name #'k #'name)))
	 #'(let ((digest (tc:find-digest tc:digest)))
	     (make-tc-digest-descriptor (tc:digest-descriptor-name digest)
					(make-digest-initiator digest)
					tc:digest-process!
					digest-done!
					(tc:digest-descriptor-block-size digest)
					(tc:digest-descriptor-digest-size digest)
					(tc:digest-descriptor-oid digest)
					digest)))))))

(define *digest:whirlpool*   (build-digest-descriptor *digest:whirlpool*))
(define *digest:ripemd-128*  (build-digest-descriptor *digest:ripemd-128*))
(define *digest:ripemd-160*  (build-digest-descriptor *digest:ripemd-160*))
(define *digest:ripemd-256*  (build-digest-descriptor *digest:ripemd-256*))
(define *digest:ripemd-320*  (build-digest-descriptor *digest:ripemd-320*))
(define *digest:sha-1*	     (build-digest-descriptor *digest:sha-1*))
(define *digest:sha-224*     (build-digest-descriptor *digest:sha-224*))
(define *digest:sha-256*     (build-digest-descriptor *digest:sha-256*))
(define *digest:sha-384*     (build-digest-descriptor *digest:sha-384*))
(define *digest:sha-512*     (build-digest-descriptor *digest:sha-512*))
(define *digest:sha-512/224* (build-digest-descriptor *digest:sha-512/224*))
(define *digest:sha-512/256* (build-digest-descriptor *digest:sha-512/256*))
(define *digest:sha3-224*    (build-digest-descriptor *digest:sha3-224*))
(define *digest:sha3-256*    (build-digest-descriptor *digest:sha3-256*))
(define *digest:sha3-384*    (build-digest-descriptor *digest:sha3-384*))
(define *digest:sha3-512*    (build-digest-descriptor *digest:sha3-512*))
(define *digest:keccak-224*  (build-digest-descriptor *digest:keccak-224*))
(define *digest:keccak-256*  (build-digest-descriptor *digest:keccak-256*))
(define *digest:keccak-384*  (build-digest-descriptor *digest:keccak-384*))
(define *digest:keccak-512*  (build-digest-descriptor *digest:keccak-512*))
(define *digest:tiger-192*   (build-digest-descriptor *digest:tiger-192*))
(define *digest:md5*	     (build-digest-descriptor *digest:md5*))
(define *digest:md4*	     (build-digest-descriptor *digest:md4*))
(define *digest:md2*	     (build-digest-descriptor *digest:md2*))
(define *digest:blake2s-128* (build-digest-descriptor *digest:blake2s-128*))
(define *digest:blake2s-160* (build-digest-descriptor *digest:blake2s-160*))
(define *digest:blake2s-224* (build-digest-descriptor *digest:blake2s-224*))
(define *digest:blake2s-256* (build-digest-descriptor *digest:blake2s-256*))
(define *digest:blake2b-160* (build-digest-descriptor *digest:blake2b-160*))
(define *digest:blake2b-256* (build-digest-descriptor *digest:blake2b-256*))
(define *digest:blake2b-384* (build-digest-descriptor *digest:blake2b-384*))
(define *digest:blake2b-512* (build-digest-descriptor *digest:blake2b-512*))

;; SHAKE
(define ((shake-initiate num) . ignore) (tc:sha3-shake-init num))
(define *digest:shake-128* (make-digest-descriptor "shake-128"
						   (shake-initiate 128)
						   tc:sha3-shake-process!
						   tc:sha3-shake-done!
						   168 ;; in octet
						   #f ;; variable length
						   "2.16.840.1.101.3.4.2.11"))
(define *digest:shake-256* (make-digest-descriptor "shake-256"
						   (shake-initiate 256)
						   tc:sha3-shake-process!
						   tc:sha3-shake-done!
						   136 ;; in octed
						   #f ;; variable length
						   "2.16.840.1.101.3.4.2.12"))

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

(define *digest:cshake-128* (make-digest-descriptor "cshake-128"
						    (cshake-initiate 256 168)
						    cshake-process!
						    cshake-done!
						    168
						    #f
						    ;; does cSHAKE have OID?
						    #f))
(define *digest:cshake-256* (make-digest-descriptor "cshake-256"
						    (cshake-initiate 512 136)
						    cshake-process!
						    cshake-done!
						    136
						    #f
						    ;; does cSHAKE have OID?
						    #f))

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
	     *digest:shake-128* *digest:shake-256*)))

(define-generic oid->digest-descriptor)
;; Default, if you need, implement the method
(define-method oid->digest-descriptor ((oid <string>))
  (cond ((assoc oid *oid-map*) => cdr)
	(else #f)))
)
