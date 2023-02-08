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
    (export random-generator?
	    builtin-random-generator?
	    secure-random-generator?
	    custom-random-generator?
	    random-generator-read-random-bytes
	    random-generator-read-random-bytes!
	    random-generator-randomize!
	    random-generator-random-integer
	    random-generator-state

	    pseudo-random-generator
	    secure-random-generator
	    <random-generator>
	    <builtin-random-generator>
	    <custom-random-generator>
	    <secure-random-generator>
	    make-custom-random-generator
	    
	    *prng:yarrow* *prng:fortuna* *prng:rc4* *prng:sober-128*
	    *prng:system* *prng:chacha20*
	    prng-descriptor? prng-descriptor-name
	    )
    (import (rnrs)
	    (clos user)
	    (sagittarius) ;; for bytevector->uinteger
	    (sagittarius mop immutable)
	    (sagittarius crypto random prng))

(define-class <random-generator> () ())
(define (random-generator? o) (is-a? o <random-generator>))
(define-class <custom-random-generator> (<random-generator>)
  ((set-seed! :init-keyword :set-seed!
	      :reader custom-random-generator-set-seed!)
   (read-random! :init-keyword :read-random!
		 :reader custom-random-generator-read-random!)))
(define (custom-random-generator? o) (is-a? o <custom-random-generator>))
(define-generic make-custom-random-generator)

(define-class <builtin-random-generator> (<random-generator> <immutable>)
  ((prng :init-keyword :prng :reader builtin-random-generator-prng)))
(define (builtin-random-generator? o) (is-a? o <builtin-random-generator>))
(define (make-builtin-random-generator prng)
  (make <builtin-random-generator> :prng prng))

(define-class <secure-random-generator> (<random-generator>) ())
(define (secure-random-generator? o) (is-a? o <secure-random-generator>))


(define-class <builtin-secure-random-generator>
  (<builtin-random-generator> <secure-random-generator>) ())
(define (make-builtin-secure-random-generator prng)
  (make <builtin-secure-random-generator> :prng prng))

;; System random instance, use this to feed entropy
(define system-prng (prng-start *prng:system*))

(define default-entropy (string->utf8 "default entropy"))
(define (pseudo-random-generator descriptor . opts)
  (if (prng-descriptor? descriptor)
      (let ((prng (prng-start descriptor)))
	;; some PRNG requires initial entropy, i.e RC4 so add dummy here
	(prng-add-entropy! prng default-entropy)
	(make-builtin-random-generator (prng-ready! prng)))
      (apply make-custom-random-generator descriptor opts)))

(define (secure-random-generator descriptor :optional (seed-size 16) :rest opts)
  (if (prng-descriptor? descriptor)
      (let ((prng (prng-start descriptor))
	    (entropy (make-bytevector seed-size)))
	(prng-read! system-prng entropy)
	(prng-add-entropy! prng entropy)
	(make-builtin-secure-random-generator (prng-ready! prng)))
      (let ((prng (apply make-custom-random-generator descriptor
			 :secure-random #t opts))
	    (entropy (make-bytevector seed-size)))
	(prng-read! system-prng entropy)
	;; I'm not sure if this will be secure enough, but better than nothing
	(random-generator-randomize! prng entropy))))

(define (random-generator-read-random-bytes (random-generator random-generator?)
					    size)
  (let ((bv (make-bytevector size)))
    (random-generator-read-random-bytes! random-generator bv 0 size)
    ;; should we check the read amount?
    bv))

(define (random-generator-read-random-bytes!
	 (random-generator random-generator?)
	 (bv bytevector?)
	 :optional (start 0) (length (- (bytevector-length bv) start)))
  (if (builtin-random-generator? random-generator)
      (prng-read! (builtin-random-generator-prng random-generator)
		  bv start length)
      ((custom-random-generator-read-random! random-generator)
       random-generator bv 0 length))
  bv)

(define (random-generator-randomize! (random-generator random-generator?)
				     (seed bytevector?) . opts)
  (unless (secure-random-generator? random-generator)
    (if (builtin-random-generator? random-generator)
	(apply prng-add-entropy!
	       (builtin-random-generator-prng random-generator) seed opts)
	(apply (custom-random-generator-set-seed! random-generator)
	       random-generator seed opts)))
  random-generator)

(define (random-generator-random-integer random-generator size)
  (define rsize (ceiling (/ (+ (bitwise-length size) 7) 8)))
  (let* ((bv (random-generator-read-random-bytes random-generator rsize))
	 (i (bytevector->uinteger bv)))
    (if (>= i size)
	(mod i size)
	i)))

(define-generic random-generator-state)
(define-method random-generator-state ((o <random-generator>)) #f)
(define-method random-generator-state ((o <random-generator>) v) #f)
)
