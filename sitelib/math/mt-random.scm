;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; mt-random.scm - Mersenne Twister random
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
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
#!compatible

;;; Scheme implementation of MT random
;;; based on mt19937-64.c
;;;  http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/mt64.html
(library (math mt-random)
    (export MT <mersenne-twister>)
    (import (rnrs)
	    (sagittarius)
	    (clos user)
	    (math))

  (define-constant NN       312)
  (define-constant MM       156)
  (define-constant MATRIX_A #xB5026F5AA96619E9)
  ;; Most significant 33 bits
  (define-constant UM       #xFFFFFFFF80000000)
  ;; Least significant 31 bits
  (define-constant LM       #x7FFFFFFF)

  ;; we need mask (64 bits)
  (define-constant *mask*   #xFFFFFFFFFFFFFFFF)
  (define (mask v) (bitwise-and *mask* v))
  ;; for convenient
  (define (*8 v) (* v 8))
  (define (mt-ref bv i)
    (bytevector-u64-native-ref bv (*8 i)))
  (define (mt-set! bv i value)
    (bytevector-u64-native-set! bv (*8 i) value))

  ;; this is ugly
  (define mag01 (make-bytevector 16))
  (begin
    (bytevector-u64-native-set! mag01 0 0)
    (bytevector-u64-native-set! mag01 8 MATRIX_A))

  (define (print-state prng)
    (do ((i 0 (+ i 1)))
	((= i NN))
      (format #t "~20' a " (mt-ref (slot-ref prng 'state) i))
      (when (= (mod i 3) 2)
	(newline)))
     (newline))

  (define (init-genrand prng seed)
    (let ((mt (slot-ref prng 'state)))
      (bytevector-u64-native-set! mt 0 seed)
      (do ((i 1 (+ i 1)))
	  ((= i NN) (slot-set! prng 'mti i))
	(let* ((v (mt-ref mt (- i 1)))
	       (t (bitwise-xor v (bitwise-arithmetic-shift-right v 62))))
	  (bytevector-u64-native-set! 
	   mt (*8 i) (mask (+ (* 6364136223846793005 t) i)))))))

  (define (init-by-array64 prng seed)
    (init-genrand prng 19650218)
    (let* ((key-len (/ (bytevector-length seed) 8))
	   (i 1) (j 0)
	   (mt (slot-ref prng 'state)))
      (do ((k (if (> NN key-len) NN key-len) (- k 1)))
	  ((zero? k))
	(let* ((v (mt-ref mt (- i 1)))
	       (t (* (bitwise-xor v (bitwise-arithmetic-shift-right v 62))
		     3935559000370003845))
	       (ik (mt-ref seed j)))
	  ;; non linear
	  (mt-set! mt i (mask (+ (bitwise-xor (mt-ref mt i) t) ik j)))
	  (set! i (+ i 1)) (set! j (+ j 1))
	  (when (>= i NN)
	    (mt-set! mt 0 (mt-ref mt (- NN 1)))
	    (set! i 1))
	  (when (>= j key-len) (set! j 0))))
      (do ((k (- NN 1) (- k 1)))
	  ((zero? k))
	(let* ((v (mt-ref mt (- i 1)))
	       (t (* (bitwise-xor v (bitwise-arithmetic-shift-right v 62))
		     2862933555777941757)))
	  ;; non linear
	  (mt-set! mt i (mask (- (bitwise-xor (mt-ref mt i) t) i)))
	  (set! i (+ i 1))
	  (when (>= i NN)
	    (mt-set! mt 0 (mt-ref mt (- NN 1)))
	    (set! i 1))))
      ;; MSB is 1; assuring non-zero initial array
      (mt-set! mt 0 (bitwise-arithmetic-shift 1 63))))

  (define (mt-set-seed prng seed)
    (define (64-bit-bytevector? seed)
      (zero? (mod (bytevector-length seed) 8)))
    (cond ((and (integer? seed)
		(positive? seed))
	   (init-genrand prng seed))
	  ((and (bytevector? seed)
		(64-bit-bytevector? seed))
	   (init-by-array64 prng seed))
	  (else
	   (assertion-violation 'set-seed
				"invalid seed" seed))))
  ;; generates a random number on [0, 2^64-1]-interval
  ;; I actually don't know how to treat this. for now, generate full number
  ;; and convert to bytevector.
  (define (mt-read-random prng bytes)
    (define (reset prng)
      (let ((mt (slot-ref prng 'state))
	    (mti (slot-ref prng 'mti))
	    (i 0) (x 0))
	(when (= mti (+ NN 1))
	  ;; if init-genrand has not been called,
	  ;; a default initial seed is used
	  (init-genrand prng 5489))	
	(let ((x 0))
	  (do ((j 0 (+ j 1)))
	      ((= j (- NN MM)) (set! i j))
	    (set! x (bitwise-ior
		     (bitwise-and (mt-ref mt j) UM)
		     (bitwise-and (mt-ref mt (+ j 1)) LM)))
	    (let ((v (bitwise-xor (mt-ref mt (+ j MM))
				  (bitwise-arithmetic-shift-right x 1)
				  (mt-ref mag01 (bitwise-and x 1)))))
	      (mt-set! mt j v)))
	  (do ((j i (+ j 1)))
	      ((= j (- NN 1)))
	    (set! x (bitwise-ior
		     (bitwise-and (mt-ref mt j) UM)
		     (bitwise-and (mt-ref mt (+ j 1)) LM)))
	    (let ((v (bitwise-xor (mt-ref mt (+ j (- MM NN)))
				  (bitwise-arithmetic-shift-right x 1)
				  (mt-ref mag01 (bitwise-and x 1)))))
	      (mt-set! mt j v)))
	  (set! x (bitwise-ior
		   (bitwise-and (mt-ref mt (- NN 1)) UM)
		   (bitwise-and (mt-ref mt 0) LM)))
	  (let ((v (bitwise-xor (mt-ref mt (- MM 1))
				(bitwise-arithmetic-shift-right x 1)
				(mt-ref mag01 (bitwise-and x 1)))))
	    (mt-set! mt (- NN 1) v))
	  (slot-set! prng 'mti 0))))

    (define (read-random bv count)	
      (let ((len (bytevector-length bv)))
	(let loop ((i 0) (offset 0))
	  (unless (= i count)
	    (let ((mt (slot-ref prng 'state)))
	      (when (>= (slot-ref prng 'mti) NN)
		(reset prng))
	      (let* ((mti (slot-ref prng 'mti))
		     (x (mt-ref mt mti)))
		(slot-set! prng 'mti (+ mti 1))
		(set! x (bitwise-xor x
			 (bitwise-and (bitwise-arithmetic-shift-right x 29)
				      #x5555555555555555)))
		(set! x (bitwise-xor x
			 (bitwise-and (bitwise-arithmetic-shift x 17)
				      #x71D67FFFEDA60000)))
		(set! x (bitwise-xor x
			 (bitwise-and (bitwise-arithmetic-shift x 37)
				      #xFFF7EEE000000000)))
		(set! x (bitwise-xor x (bitwise-arithmetic-shift-right x 43)))
		;; copy to result buffer
		;; src can be less than 8 bytes, so we need to calculate
		;; proper offset here.
		(let* ((src (integer->bytevector x))
		       (off (+ offset (- 8 (bytevector-length src)))))
		  (if (>= (- len off) 8)
		      (bytevector-copy! src 0 bv off (bytevector-length src))
		      (bytevector-copy! src 0 bv off (- len off)))
		  (loop (+ i 1) (+ offset 8)))))))))
    ;; check size the reading process read 8 byte in once.
    (let ((count (ceiling (/ bytes 8)))
	  (bv (make-bytevector bytes)))
      (read-random bv count)
      bv))

  ;; MT random is not secure random, so we do not implement <secure-random>
  (define-class <mersenne-twister> (<user-prng>)
    (;; The array for the state vector
     ;; using bytevector, it needs to be 64 bit aligned.
     (state :init-keyword :state :init-form (make-bytevector (* NN 8)))
     ;; mti==NN+1 means MT[NN] is not initialized
     (mti   :init-keyword :mti   :init-form (+ NN 1))))
  (define-method initialize ((o <mersenne-twister>) initargs)
    (call-next-method)
    (let ((seed (get-keyword :seed initargs #f)))
      (slot-set! o 'set-seed! mt-set-seed)
      (slot-set! o 'read-random mt-read-random)
      (when seed
	(mt-set-seed o seed))))

  (define-method prng-state ((prng <mersenne-twister>))
    (slot-ref prng 'state))

  (define-method prng-state ((prng <mersenne-twister>) state)
    (assertion-violation 'prng-state "state must be bytevector"
			 state))
  (define-method prng-state ((prng <mersenne-twister>)
			     (state <bytevector>))
    ;; assume state is proper state
    (unless (= (*8 (+ NN 1)) (bytevector-length state))
      (assertion-violation 
       'prng-state
       (format 
	"64 bit aligned bytevector of length ~a is required, but got length %d"
	(*8 (+ NN 1)) (bytevector-length state))))
    (slot-set! prng 'state state)
    (slot-set! prng 'mti (mt-ref state NN)))

  ;; register
  (define-class <mt> () ())
  (define MT (make <mt>))
  (register-prng MT <mersenne-twister>)
)