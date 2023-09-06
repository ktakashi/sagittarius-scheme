;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; security/password/generator.scm - Password generator
;;;
;;;   Copyright (c) 2023  Takashi Kato  <ktakashi@ymail.com>
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
(library (security password generator)
    (export password-policy->generator
	    generate-password
	    *password-policy:default-length*
	    *password-policy:default-char-set*)
    (import (rnrs)
	    (security password policy)
	    (sagittarius generators)
	    (sagittarius crypto random)
	    (srfi :1 lists)
	    (srfi :14 char-sets)
	    (srfi :39 parameters))

(define default-prng (secure-random-generator *prng:chacha20*))
;; in case that given policy doesn't contain character-policy
(define char-set:graphic/ascii 
  (char-set-intersection char-set:ascii char-set:graphic))
(define *password-policy:default-length* (make-parameter 16))
(define *password-policy:default-char-set*
  (make-parameter char-set:graphic/ascii))

(define (generate-password policy :key (prng default-prng))
  (define (char-set->vector cs) (list->vector (char-set->list cs)))
  (define (pick cs prng)
    (define l (vector-length cs))
    (vector-ref cs (random-generator-random-integer prng l)))
  (define ((random-characters prng) policy)
    (let ((cs (char-set->vector (character-policy-char-set policy)))
	  (n (character-policy-at-least policy)))
      (do ((i 0 (+ i 1)) (r '() (cons (pick cs prng) r)))
	  ((= i n) r))))

  (define (fixup c* p* requried prng)
    (define l (length c*))
    (if (< l requried)
	(let ((cs (char-set->vector
		   (apply char-set-union (map character-policy-char-set p*)))))
	  (do ((n (- requried l)) (i 0 (+ i 1)) (r c* (cons (pick cs prng) r)))
	      ((= i n) r)))
	c*))

  (define (shuffle l prng)
    (define v (list->vector l))
    (let loop ((r '()) (i (vector-length v)))
      (if (zero? i)
	  r
	  (let* ((j (random-generator-random-integer prng i))
		 (c (vector-ref v j)))
	    (vector-set! v j (vector-ref v (- i 1)))
	    (loop (cons c r) (- i 1))))))
  
  (define (generate policy prng)
    (define (ensure-character-policy c*)
      (if (null? c*)
	  (list (make-character-policy (*password-policy:default-char-set*) 1))
	  c*))
    (define policies (compound-password-policy-policies policy))
    (let ((length (or (password-policy-length policy)
		      (*password-policy:default-length*)))
	  (chars (ensure-character-policy (filter character-policy? policies))))
      (fixup (append-map (random-characters prng) chars) chars length prng)))
  
  (if (single-password-policy? policy)
      (generate-password (password-policies policy) :prng prng)
      (list->string (shuffle (generate policy prng) prng))))

(define (password-policy->generator policy . args)
  (lambda () (apply generate-password policy args)))

)
