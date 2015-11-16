;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; list.scm - list utility
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

;; The APIs name are from Gauche
(library (util list)
    (export intersperse
	    for-each-with-index
	    map-with-index
	    slices
	    split-at*
	    take*
	    drop*
	    combine
	    fold2
	    fold3
	    cond-list)
    (import (rnrs)
	    (core base)
	    (sagittarius)
	    (srfi :1 lists))
  (define (intersperse item lis)
    (define (rec l r)
      (if (null? l)
	  (reverse! r)
	  (rec (cdr l) (cons* (car l) item r))))
    (if (null? lis)
	'()
	(rec (cdr lis) (list (car lis)))))

  ;; is there a smart way not to copy&paste from scmlib.scm?
  ;; index start with 0
  (define (for-each-with-index proc lst1 . lst2)
    (if (null? lst2)
	(let loop ((index 0) (xs lst1))
	  (cond ((pair? xs) (proc index (car xs)) (loop (+ index 1) (cdr xs)))
		((null? xs) (undefined))
		(else
		 (assertion-violation 
		  'for-each-with-index
		  (wrong-type-argument-message "proper list" lst1 2)
		  (list proc lst1 lst2)))))
	(let loop ((index 0) (xs (apply list-transpose* lst1 lst2)))
	  (cond ((pair? xs)
		 (apply proc index (car xs))
		 (loop (+ index 1) (cdr xs)))
		((null? xs) (undefined))
		(else
		 (assertion-violation 
		  'for-each-with-index
		  (wrong-type-argument-message "proper list" lst1 2)
		  (list proc lst1 lst2)))))))

  (define (map-with-index proc lst1 . lst2)
    (if (null? lst2)
	(let loop ((index 0) (xs lst1) (r '()))
	  (cond ((pair? xs)
		 (loop (+ index 1) (cdr xs) (cons (proc index (car xs)) r)))
		((null? xs) (reverse! r))
		(else
		 (assertion-violation 
		  'map-with-index
		  (wrong-type-argument-message "proper list" lst1 2)
		  (list proc lst1 lst2)))))
	(let loop ((index 0) (xs (apply list-transpose* lst1 lst2)) (r '()))
	  (cond ((pair? xs)
		 (loop (+ index 1) (cdr xs)
		       (cons (apply proc index (car xs)) r)))
		((null? xs) (reverse! r))
		(else
		 (assertion-violation 
		  'map-with-index
		  (wrong-type-argument-message "proper list" lst1 2)
		  (list proc lst1 lst2)))))))

  ;; from Gauche
  (define (slices lis k . args)
    (unless (and (integer? k) (positive? k))
      (assertion-violation 'slices "index must be positive integer" k))
    (let loop ((lis lis) (r '()))
      (if (null? lis)
	  (reverse! r)
	  (receive (h t) (apply split-at* lis k args)
	    (loop t (cons h r))))))

  (define (split-at* lis k :optional (fill? #f) (padding #f))
    (unless (and (integer? k) (not (negative? k)))
      (assertion-violation 'split-at* "index must be non-negative integer" k))
    (let loop ((i 0) (lis lis) (r '()))
      (cond ((= i k) (values (reverse! r) lis))
	    ((null? lis)
	     (values (if fill?
			 (append! (reverse! r) (make-list (- k i) padding))
			 (reverse! r))
		     lis))
	    (else (loop (+ i 1) (cdr lis) (cons (car lis) r))))))

  (define (take* lis k . args)
    (receive (h t) (apply split-at* lis k args) h))

  (define (drop* lis k)
    (unless (and (integer? k) (not (negative? k)))
      (assertion-violation 'drop* "index must be non-negative integer" k))
    (let loop ((i 0) (lis lis))
      (cond ((= i k) lis)
	    ((null? lis) '())
	    (else (loop (+ i 1) (cdr lis))))))
  
  (define-syntax cond-list
    (syntax-rules (=> @)
      ((_) '())
      ((_ (test) . rest)
       (let* ((tmp test)
	      (r (cond-list . rest)))
	 (if tmp (cons tmp r) r)))
      ((_ (test => proc) . rest)
       (let* ((tmp test)
	      (r (cond-list . rest)))
	 (if tmp (cons (proc tmp) r) r)))
      ((_ (test => @ proc) . rest)
       (let* ((tmp test)
	      (r (cond-list . rest)))
	 (if tmp (append (proc tmp) r) r)))
      ((_ (test @ . expr) . rest)
       (let* ((tmp test)
	      (r (cond-list . rest)))
	 (if tmp (append (begin . expr) r) r)))
      ((_ (test . expr) . rest)
       (let* ((tmp test)
	      (r (cond-list . rest)))
	 (if tmp (cons (begin . expr) r) r)))
      ))

  (define-syntax define-fold-k
    (syntax-rules ()
      ((_ name (seed ...))
       (define (name proc seed ... lis . more)
	 (if (null? more)
	     (let loop ((lis lis) (seed seed) ...)
	       (if (null? lis)
		   (values seed ...)
		   (let-values (((seed ...) (proc (car lis) seed ...)))
		     (loop (cdr lis) seed ...))))
	     (let loop ((lis (apply list-transpose* lis more)) 
			(seed seed) ...)
	       (if (null? lis)
		   (values seed ...)
		   (let-values (((seed ...) 
				 (apply proc (append (car lis) 
						     (list seed ...)))))
		     (loop (cdr lis) seed ...)))))))))
  (define-fold-k fold2 (knil1 knil2))
  (define-fold-k fold3 (knil1 knil2 knil3))

  ;; map-accum in Gauche
  ;; the name is adjusted to SRFI-121 gcombine
  (define (combine proc knil lis . more)
    (if (null? more)
	(let-values (((res knil) 
		      (fold2 (lambda (elt lis knil)
			       (let-values (((res knil) (proc elt knil)))
				 (values (cons res lis) knil)))
			     '() knil lis)))
	  (values (reverse! res) knil))
	(let loop ((lis (apply list-transpose* lis more)) 
		   (res '())
		   (knil knil))
	  (if (null? lis)
	      (values (reverse! res) knil)
	      (let-values (((r knil) 
			    (apply proc (append (car lis) (list knil)))))
		(loop (cdr lis) (cons r res) knil))))))
)
