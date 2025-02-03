;;; -*- mode: scheme; coding: utf-8 -*-
;;;
;;; util/concurrent/atomic.scm - Atomic operations
;;;  
;;;   Copyright (c) 2024  Takashi Kato  <ktakashi@ymail.com>
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
(library (util concurrent atomic)
    (export memory-order?
	    *memory-order:relaxed*
	    *memory-order:consume*
	    *memory-order:acquire*
	    *memory-order:release*
	    *memory-order:acq-rel*
	    *memory-order:seq-cst*

	    make-atomic	        atomic?
	    make-atomic-flag    atomic-flag?
	    make-atomic-pair    atomic-pair?
	    make-atomic-fixnum  atomic-fixnum?

	    atomic-load atomic-store!
	    atomic-fixnum-load atomic-fixnum-store!

	    atomic-fixnum-add! atomic-fixnum-sub!
	    atomic-fixnum-ior! atomic-fixnum-xor! atomic-fixnum-and!

	    atomic-exchange! atomic-fixnum-exchange!

	    atomic-compare-and-swap! atomic-fetch-compare-and-swap!

	    atomic-flag-test-and-set! atomic-flag-clear!
	    
	    atomic-thread-fence

	    atomic-fixnum-inc!
	    atomic-fixnum-dec!

	    lock-free-queue? make-lock-free-queue
	    list->lock-free-queue lock-free-queue
	    lock-free-queue-size lock-free-queue-empty?

	    lock-free-queue-push!
	    lock-free-queue-pop!
	    lock-free-queue-get!

	    lock-free-queue->list)
    (import (rnrs)
	    (sagittarius atomic)
	    (srfi :1 lists))

;; lock-free queue
;; ref
;; - Even Better DCAS-Based Concurrent Deques
;;   DavidL.Detlefs, ChristineH.Flood, AlexanderT.Garthwaite, Paul A. Martin,
;;   Nir N. Shavit, and Guy L. Steele Jr.
;; - DCASis not a Silver Bullet for Nonblocking Algorithm
;;   Simon Doherty, Victor Luchangco, David L. Detlefs, Paul A. Martin,
;;   Lindsay Groves, Mark Moir, Christine H. Flood, Nir Shavit,
;;   Guy L. Steele Jr.
;;
;; Above papers discuss deque however we do linked list using pair.
;; We support the below operations
;; - push (= pushRight)
;; - pop  (= popRight)
;; - get  (= popLeft)
;; atomic-pair = { head, tail } = { LeftHat, RightHat }
(define-record-type lock-free-queue 
  (fields atomic-pair atomic-size)
  (protocol
   (lambda (p)
     (case-lambda
      (() (p (make-atomic-pair '() '()) (make-atomic-fixnum 0)))
      ((lis) (p (make-atomic-pair p (cdr p))
		(make-atomic-fixnum (length lis))))))))
(define *empty-value* (cons '() '()))
(define (empty-value? e) (equal? *empty-value* e))

(define (list->lock-free-queue lis) (make-lock-free-queue lis))
(define (lock-free-queue . lis) (list->lock-free-queue lis))
(define (lock-free-queue-size q)
  (atomic-fixnum-load (lock-free-queue-atomic-size q)))

(define (lock-free-queue->list q) 
  (let ((r (atomic-load (lock-free-queue-atomic-pair q))))
    (if (empty-value? r)
	'()
	r)))
(define (lock-free-queue-empty? q) (zero? (lock-free-queue-size q)))

(define (lock-free-queue-push! q e)
  (define atomic-pair (lock-free-queue-atomic-pair q))
  (define atomic-fx (lock-free-queue-atomic-size q))
  (define (try-push ap expected element)
    (define new-val (if (lock-free-queue-empty? q) 
			(list element)
			`(,@expected ,element)))
    (cond ((atomic-compare-and-swap! ap expected new-val)
	   (atomic-fixnum-inc! atomic-fx)
	   e)
	  (else (try-push ap (atomic-load ap) element))))
  (try-push atomic-pair (atomic-load atomic-pair) e))

(define (lock-free-queue-pop! q)
  (define atomic-pair (lock-free-queue-atomic-pair q))
  (define atomic-fx (lock-free-queue-atomic-size q))
  (define (try-get ap expected)
    (define new-val
      (if (null? (cdr expected))
	  *empty-value*
	  (cons (car expected) (drop-right (cdr expected) 1))))
    (cond ((atomic-compare-and-swap! ap expected new-val)
	   (atomic-fixnum-dec! atomic-fx)
	   (car (take-right expected 1)))
	  (else (try-get ap (atomic-load ap)))))
  (try-get atomic-pair (atomic-load atomic-pair)))

(define (lock-free-queue-get! q)
  (define atomic-pair (lock-free-queue-atomic-pair q))
  (define atomic-fx (lock-free-queue-atomic-size q))
  (define (try-get ap expected)
    (define new-val (if (null? (cdr expected))
			*empty-value*
			(cdr expected)))
    (cond ((atomic-compare-and-swap! ap expected new-val)
	   (atomic-fixnum-dec! atomic-fx)
	   (car expected))
	  (else (try-get ap (atomic-load ap)))))
  (try-get atomic-pair (atomic-load atomic-pair)))

)
