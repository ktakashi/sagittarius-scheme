;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; sagittarius/crypto/math/prime.scm - Prime check and generation
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
(library (sagittarius crypto math prime)
    (export probable-prime?
	    generate-random-prime)
    (import (rnrs)
	    (sagittarius crypto random))

(define *small-primes*
  '(3 5 7 11 13 17 19 23 29 31 37 41 43 
      47 53 59 61 67 71 73 79 83 89 97 101 
      103 107 109 113
      127 131 137 139 149 151 157 163 167 173
      179 181 191 193 197 199 211 223 227 229
      233 239 241 251 257 263 269 271 277 281
      283 293 307 311 313 317 331 337 347 349
      353 359 367 373 379 383 389 397 401 409
      419 421 431 433 439 443 449 457 461 463
      467 479 487 491 499 503 509 521 523 541
      547 557 563 569 571 577 587 593 599 601
      607 613 617 619 631 641 643 647 653 659
      661 673 677 683 691 701 709 719 727 733
      739 743 751 757 761 769 773 787 797 809
      811 821 823 827 829 839 853 857 859 863
      877 881 883 887 907 911 919 929 937 941
      947 953 967 971 977 983 991 997))

(define (jacobi-symbol p n)
  (define (discards-fact2 p u j)
    (define (shift-p p)
      (let loop ((p p))
	(if (zero? (fxand p 3))
	    (loop (fxarithmetic-shift-right p 2))
	    p)))
    (let ((p (shift-p p)))
      (if (even? p)
	  (let ((p (fxarithmetic-shift-right p 1)))
	    (values p (if (zero? (fxand 
				  (fxxor u (fxarithmetic-shift-right u 1))
				  2))
			  j
			  (- j))))
	  (values p j)))) ;; 3 or 5 mod 8
  (define (ensure-positive p u j)
    (if (< p 0)
	(let ((p (- p))
	      (n8 (fxand u 7)))
	  (values p (if (or (= n8 3) (= n8 7)) (- j) j)))
	(values p j)))
  (unless (fixnum? p)
    (error 'jacobi-symbol "p is too big" p))
  (if (or (zero? p) (even? n))
      0
      ;; get the least significant word
      (let ((u (bitwise-and n (greatest-fixnum))))
	(let*-values (((tp tj) (ensure-positive p u 1))
		      ((p j) (discards-fact2 tp u tj)))
	  (if (= p 1)
	      j
	      ;; apply quadratic reciprocity (p = u = 3 (mod 4)
	      (let ((j (if (zero? (fxand p u 2)) j (- j)))
		    ;; reduce u mod p
		    (u (mod n p)))
		;; compute jacobi(u, p), u < p
		(let loop ((u u) (p p) (j j))
		  (if (zero? u)
		      0
		      (let-values (((u j) (discards-fact2 u p j)))
			(if (= u 1)
			    j
			    ;; now both u and p are odd so use
			    ;; quadratic reciprocity
			    (let-values (((u p j)
					  (if (< u p)
					      (values p u (if (zero? 
							       (fxand u p 2))
							      j
							      (- j)))
					      (values u p j))))
			      (loop (mod u p) p j))))))))))))

;; Lucas-Lehmer prime test
(define (lucas-lehmer? n)
  (define (lucas-lehmer-sequence z k n)
    (define (compute-sub-shift t n)
      (bitwise-arithmetic-shift-right (if (odd? t) (- t n) t) 1))
    (let loop ((i (- (bitwise-length k) 2))
	       (u 1)
	       (v 1))
      (if (< i 0)
	  u
	  (let ((u2 (mod (* u v) n))
		(v2 (compute-sub-shift (mod (+ (* v v) (* z (* u u))) n) n)))
	    (if (bitwise-bit-set? k i)
		(loop (- i 1)
		      (compute-sub-shift (mod (+ u2 v2) n) n)
		      (compute-sub-shift (mod (+ v2 (* z u2)) n) n))
		(loop (- i 1) u2 v2))))))
  (define (step1 n)
    (do ((d 5 (if (< d 0) (+ (abs d) 2) (- (+ d 2)))))
	((= (jacobi-symbol d n) -1) d)))
  (let* ((d (step1 n))
	 (u (lucas-lehmer-sequence d (+ n 1) n)))
    (zero? (mod u n))))

(define (miller-rabin? q k rand)
  (define (check-small-prime q)
    (let loop ((p *small-primes*))
      (cond ((null? p) #f)
	    ((zero? (mod q (car p))) #t)
	    (else
	     (loop (cdr p))))))
  (let ((q (abs q)))
    (cond ((even? q) #f) ;; obvious
	  ((= q 1) #f)   ;; 1 is not prime
	  ((memv q *small-primes*) #t)
	  ((check-small-prime q) #f) ;; multiple of small-primes
	  (else
	   ;; Miller Rabin test
	   (let* ((t (- q 1))
		  (d (if (zero? (bitwise-and t 1))
			 (do ((d (bitwise-arithmetic-shift-right t 1)
				 (bitwise-arithmetic-shift-right d 1)))
			     ((not (zero? (bitwise-and d 1))) d))
			 t)))
	     (let loop ((i 0))
	       (if (= i k)
		   #t
		   (let ((a (+ (random rand (- q 2)) 1)))
		     (define (find-ty q t y)
		       ;; check 0, ..., q - 1
		       (let loop2 ((t t) (y y))
			 (if (and (not (= t (- q 1)))
				  (not (= y 1))
				  (not (= y (- q 1))))
			     (loop2 (bitwise-arithmetic-shift-left t 1)
				    (mod-expt y 2 q))
			     (values t y))))
		     (let-values (((t y) (find-ty q d (mod-expt a t q))))
		       (if (and (not (= y (- q 1)))
				(zero? (bitwise-and t 1)))
			   #f
			   (loop (+ i 1))))))))))))

;; Miller Rabin primality test
(define (probable-prime? q
	  :optional (k 50) (rand (secure-random-generator *prng:system*)))
  (define bit-size (bitwise-length q))
  (define try-count (cond ((< bit-size 100)  50)
			  ((< bit-size 256)  27)
			  ((< bit-size 512)  15)
			  ((< bit-size 768)  8)
			  ((< bit-size 1024) 4)
			  (else              2)))
  (let ((n (min try-count k)))
    (and (miller-rabin? q n rand)
	 (or (= try-count 50)
	     (lucas-lehmer? q)))))

(define (generate-random-prime size
	  :optional (prng (secure-random-generator *prng:chacha20*)))
  (let ((buf (make-bytevector size 0))
	(index (- size 1)))
    (let loop ()
      (let ((bv (random-generator-read-random-bytes! prng buf)))
	(bytevector-u8-set!
	 bv 0 (bitwise-ior (bytevector-u8-ref bv 0) #x80 #x40))
	(let ((b (bitwise-ior (bytevector-u8-ref bv index) #x01)))
	  (cond ((zero? (mod b 5)) (loop)) ;; ignore multiple number of 5
		(else
		 (bytevector-u8-set! bv index b)
		 (let ((ret (bytevector->integer bv)))
		   (if (prime? ret)
		       ret
		       (loop))))))))))
)
