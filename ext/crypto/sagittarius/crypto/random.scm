;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/random.scm - Random generator
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
(library (sagittarius crypto random)
    (export random-generator? secure-random-generator?
	    random-generator-read-random-bytes
	    random-generator-read-random-bytes!
	    random-generator-randomize!
	    random-generator-random-integer
	    

	    pseudo-random-generator
	    secure-random-generator
	    (rename (random-generator <random-generator>))
	    <secure-random-generator>
	    
	    *prng:yarrow* *prng:fortuna* *prng:rc4* *prng:sober-128*
	    *prng:system* *prng:chacha20*
	    prng-descriptor? prng-descriptor-name)
    (import (rnrs)
	    (sagittarius) ;; for bytevector->uinteger
	    (sagittarius crypto random prng))

(define-record-type random-generator
  (fields prng))

(define-record-type (<secure-random-generator> %make secure-random-generator?)
  (parent random-generator))

;; System random instance, use this to feed entropy
(define systam-prng (prng-start *prng:system*))

(define default-entropy (string->utf8 "default entropy"))
(define (pseudo-random-generator descriptor)
  (unless (prng-descriptor? descriptor)
    (assertion-violation 'pseudo-random-generator
			 "PRNG descriptor is required" descriptor))
  (let ((prng (prng-start descriptor)))
    ;; some PRNG requires initial entropy, i.e RC4 so add dummy here
    (prng-add-entropy! prng default-entropy)
    (make-random-generator (prng-ready! prng))))

(define (secure-random-generator descriptor :optional (seed-size 16))
  (unless (prng-descriptor? descriptor)
    (assertion-violation 'secure-random-generator
			 "PRNG descriptor is required" descriptor))
  (let ((prng (prng-start descriptor))
	(entropy (make-bytevector 16)))
    (prng-read! systam-prng entropy)
    (prng-add-entropy! prng entropy)
    (%make (prng-ready! prng))))

(define (random-generator-read-random-bytes random-generator size)
  (unless (random-generator? random-generator)
    (assertion-violation 'random-generator-read-random-bytes
			 "Random generator is required" random-generator))
  (let ((bv (make-bytevector size)))
    (random-generator-read-random-bytes! random-generator bv 0 size)
    ;; should we check the read amount?
    bv))

(define (random-generator-read-random-bytes! random-generator bv
	 :optional (start 0) (length (bytevector-length bv)))
  (unless (random-generator? random-generator)
    (assertion-violation 'random-generator-read-random-bytes!
			 "Random generator is required" random-generator))
  (prng-read! (random-generator-prng random-generator) bv start length))

(define (random-generator-randomize! random-generator seed . opts)
  (unless (random-generator? random-generator)
    (assertion-violation 'random-generator-randomize!
			 "Random generator is required" random-generator))
  (unless (secure-random-generator? random-generator)
    (apply prng-add-entropy! (random-generator-prng random-generator)
	   seed opts))
  random-generator)

(define (random-generator-random-integer random-generator size)
  (define rsize (ceiling (/ (bitwise-length size) 8)))
  (let* ((bv (random-generator-read-random-bytes random-generator rsize))
	 (i (bytevector->uinteger bv)))
    (if (>= i size)
	(mod i size)
	i)))

)
