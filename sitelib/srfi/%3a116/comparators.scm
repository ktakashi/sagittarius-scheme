;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; comparators.scm - SRFI-116 immutable list comparators
;;;  
;;;   Copyright (c) 2014  Takashi Kato  <ktakashi@ymail.com>
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

;; the library name is taken from Vicare
;; See: https://groups.google.com/forum/#!topic/comp.lang.scheme/K3z5MGpHzg0

(library (srfi :116 comparators)
    (export ipair-comparator
	    ilist-comparator
	    make-ipair-comparator
	    make-ilist-comparator
	    make-improper-ilist-comparator
	    make-icar-comparator
	    make-icdr-comparator)
    (import (rnrs)
	    (srfi :114 comparators)
	    (srfi :116 ilists))


(define (make-ipair-comparison kar kdr)
  (let ((car-compare (comparator-comparison-procedure kar))
	(cdr-compare (comparator-comparison-procedure kdr)))
    (lambda (a b)
      (let ((r (car-compare (icar a) (icar b))))
	(if (zero? r)
	    (cdr-compare (icdr a) (icdr b))
	    r)))))

(define (make-ipair-hash kar kdr)
  (let ((car-hash (comparator-hash-function kar))
	(cdr-hash (comparator-hash-function kdr)))
    (lambda (obj)
      (+ (car-hash (icar obj)) (cdr-hash (icdr obj))))))

(define (make-ipair-comparator kar kdr)
  (define car-test-proc (comparator-type-test-procedure kar))
  (define cdr-test-proc (comparator-type-test-procedure kdr))
  (define (test-proc obj)
    (and (ipair? obj)
	 (car-test-proc (icar obj))
	 (cdr-test-proc (icdr obj))))
  (make-comparator test-proc
		   #t
		   (make-ipair-comparison kar kdr)
		   (make-ipair-hash kar kdr)))

(define ipair-comparator 
  (make-ipair-comparator default-comparator default-comparator))

(define (make-ilist-comparator comparator)
  (define element-test-proc (comparator-type-test-procedure comparator))
  (define (test-proc ilist)
    (if (ipair? ilist)
	(and (element-test-proc (icar ilist))
	     (test-proc (icdr ilist)))
	(null? ilist)))
  (make-listwise-comparator test-proc comparator null? icar icdr))

(define ilist-comparator (make-ilist-comparator default-comparator))

(define (make-icar-comparator comparator)
  (define car-test-proc (comparator-type-test-procedure comparator))
  (define (test-proc obj)
    (and (ipair? obj)
	 (car-test-proc (icar obj))))
  (make-comparator test-proc #t
		   (let ((compare (comparator-comparison-procedure comparator)))
		     (lambda (a b) (compare (icar a) (icar b))))
		   (let ((hash (comparator-hash-function comparator)))
		     (lambda (obj) (hash (icar obj))))))

(define (make-icdr-comparator comparator)
  (define cdr-test-proc (comparator-type-test-procedure comparator))
  (define (test-proc obj)
    (and (ipair? obj)
	 (cdr-test-proc (icdr obj))))
  (make-comparator test-proc #t
		   (let ((compare (comparator-comparison-procedure comparator)))
		     (lambda (a b) (compare (icdr a) (icdr b))))
		   (let ((hash (comparator-hash-function comparator)))
		     (lambda (obj) (hash (icdr obj))))))

(define (make-improper-ilist-comparison comparator)
  (define real-comparison (make-comparison< <))
  (let ((pair-comparison (make-ipair-comparison comparator comparator))
	(item-comparison (comparator-comparison-procedure comparator)))
    (lambda (a b)
      (define (type o)
	(cond ((null? o) 0)
	      ((ipair? o) 1)
	      (else 2)))
      (let ((result (real-comparison (type a) (type b))))
	(cond ((not (zero? result)) result)
	      ((null? a) 0)
	      ((ipair? a) (pair-comparison a b))
	      (else (item-comparison a b)))))))
(define (make-improper-ilist-hash comparator)
  (define hash (comparator-hash-function comparator))
  (lambda (obj) 
    (cond ((ipair? obj) (+ (hash (icar obj)) (hash (icdr obj))))
	  ((null? obj) 0)
	  (else (hash obj)))))

(define (make-improper-ilist-comparator comparator)
  (make-comparator #t #t 
		   (make-improper-ilist-comparison comparator)
		   (make-improper-ilist-hash comparator)))

  
)
