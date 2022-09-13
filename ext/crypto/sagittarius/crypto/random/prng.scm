;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/random/prng.scm - Pseudo random generator
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
(library (sagittarius crypto random prng)
    (export *prng:yarrow* *prng:fortuna* *prng:rc4* *prng:sober-128*
	    *prng:system* *prng:chacha20*

	    prng-descriptor? prng-descriptor-name

	    prng? prng-start prng-add-entropy! prng-read! prng-done!)
    (import (rnrs)
	    (prefix (sagittarius crypto tomcrypt) tc:))

(define-record-type prng-descriptor
  (fields prng ;; real prng descriptor, a fixnum
	  name))

(define-record-type prng
  (fields descriptor state))

(define (prng-start descriptor)
  (unless (prng-descriptor? descriptor)
    (assertion-violation 'prng-start "PRNG descriptor is required" descriptor))
  (make-prng descriptor (tc:prng-start (prng-descriptor-prng descriptor))))

(define (prng-add-entropy! prng bv . opts)
  (unless (prng? prng)
    (assertion-violation 'prng-add-entropy! "PRNG is required" prng))
  (apply tc:prng-add-entropy! (prng-state prng) bv opts))
(define (prng-read! prng bv . opts)
  (unless (prng? prng)
    (assertion-violation 'prng-read! "PRNG is required" prng))
  (apply tc:prng-read! (prng-state prng) bv opts))
(define (prng-done! prng)
  (unless (prng? prng)
    (assertion-violation 'prng-done! "PRNG is required" prng))
  (tc:prng-done! (prng-state prng)))

(define-syntax build-prng-descriptor
  (syntax-rules ()
    ((_ name)
     (let ((prng (tc:find-prng name)))
       (make-prng-descriptor prng (tc:prng-descriptor-name prng))))))

(define *prng:yarrow*    (build-prng-descriptor tc:*prng:yarrow*))
(define *prng:fortuna*	 (build-prng-descriptor tc:*prng:fortuna*))
(define *prng:rc4*	 (build-prng-descriptor tc:*prng:rc4*))
(define *prng:sober-128* (build-prng-descriptor tc:*prng:sober-128*))
(define *prng:system*	 (build-prng-descriptor tc:*prng:system*))
(define *prng:chacha20*  (build-prng-descriptor tc:*prng:chacha20*))

)
