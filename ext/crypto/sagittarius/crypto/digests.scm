;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/digests.scm - Digests calculator
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
(library (sagittarius crypto digests)
    (export message-digest? make-message-digest
	    digest-message digest-message!
	    message-digest-init! message-digest-process!
	    message-digest-done!
	    message-digest-done
	    message-digest-descriptor

	    digest-descriptor? <digest-descriptor> make-digest-descriptor
	    digest-descriptor-name
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

	    oid->digest-descriptor
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius)
	    (sagittarius mop immutable)
	    (sagittarius crypto digests descriptors))

(define-class <message-digest> (<immutable>)
  ((state :reader message-digest-state
	  :writer message-digest-state-set!
	  :init-value #f
	  :mutable #t)
   (descriptor :reader message-digest-descriptor :init-keyword :descriptor)))
(define (message-digest? o) (is-a? o <message-digest>))
(define (make-message-digest descriptor)
  (unless (digest-descriptor? descriptor)
    (assertion-violation 'make-message-digest "Digest descriptor is required"))
  (make <message-digest> :descriptor descriptor))

(define (digest-message md msg :optional (length #f))
  (define desc (message-digest-descriptor md))
  (let ((len (or (digest-descriptor-digest-size desc) length)))
    (unless len
      (assertion-violation 'digest-message
			   "Digest length must be specified for this digest"
			   (digest-descriptor-name desc)))
    (let ((out (make-bytevector len)))
      (digest-message! md msg out 0 len))))

(define (digest-message! md msg out :optional (start 0) (length #f))
  (message-digest-done! (message-digest-process! (message-digest-init! md) msg)
			out start (or length (bytevector-length out)))
  out)

(define (message-digest-init! (md message-digest?))
  (define desc (message-digest-descriptor md))
  (message-digest-state-set! md (make-digest-state desc))
  md)
(define (message-digest-process!
	 (md message-digest?)
	 (bv bytevector?)
	 :optional (start 0)
		   (length (- (bytevector-length bv) start)))
  (digest-state-process! (message-digest-state md) bv start length)
  md)

(define (message-digest-done!
	 (md message-digest?)
	 (out bytevector?)
	 :optional (start 0)
		   ;; length may not be used
		   (length (- (bytevector-length out) start)))
  (digest-state-done! (message-digest-state md) out start length))

(define (message-digest-done md :optional (length #f))
  (define desc (message-digest-descriptor md))
  (let ((len (or (digest-descriptor-digest-size desc) length)))
    (unless len
      (assertion-violation 'message-digest-done
			   "Digest length must be specified for this digest"
			   (digest-descriptor-name desc)))
    (let ((out (make-bytevector len)))
      (message-digest-done! md out)
      out)))

(define-record-type digest-state
  (fields descriptor state)
  (protocol (lambda (p)
	      (lambda (desc)
		(p desc ((digest-descriptor-init desc)))))))
(define (digest-state-process! ds bv start length)
  ((digest-descriptor-process (digest-state-descriptor ds))
   (digest-state-state ds)
   bv start length))
(define (digest-state-done! ds out start length)
  ((digest-descriptor-done (digest-state-descriptor ds))
   (digest-state-state ds) out start length))

)
