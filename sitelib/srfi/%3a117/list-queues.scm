;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; SRFI-117 - Mutable queues
;;;  
;;;   Copyright (c) 2010-2015  Takashi Kato  <ktakashi@ymail.com>
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

;; this is still draft state and probably list-queue-append and
;; list-queue-append! will be added later.

(library (srfi :117 list-queues)
    (export 
     make-list-queue list-queue 
     list-queue-copy list-queue-unfold list-queue-unfold-right

     list-queue? list-queue-empty?

     list-queue-front list-queue-back
     list-queue-add-front! list-queue-add-back! 
     list-queue-remove-front! list-queue-remove-back!
     list-queue-remove-all! list-queue-clear!

     list-queue-length 
     list-queue-append list-queue-append!
     list-queue-concatenate

     list-queue-map list-queue-map! list-queue-for-each
     list-queue-list list-queue-set-list!
     list-queue-first-last
     empty-list-queue-error?)
    (import (rnrs)
	    (rnrs mutable-pairs)
	    (sagittarius control)
	    (srfi :1 lists)
	    (clos user))

  (define-condition-type &empty-list-queue-error &error
    make-empty-list-queue-error empty-list-queue-error?)

  ;; to make this a bit consistence with (util queue)
  ;; we use CLOS to extend <sequence>
  (define-class <list-queue> (<sequence>)
    ((first :init-value '() :init-keyword :first 
	    :reader queue-first :writer queue-first-set!)
     (last  :init-value '() :init-keyword :last 
	    :reader queue-last  :writer queue-last-set!)))
  (define (list-queue? o) (is-a? o <list-queue>))

  (define make-list-queue
    (case-lambda
     ((lst) (if (null? lst)
		(make <list-queue>)
		(make <list-queue> :first lst :last (last-pair lst))))
     ;; no check as the SRFI mentioned
     ((first last)
      (make <list-queue> :first first :last last))))

  (define (list-queue . objs) (make-list-queue objs))
  (define (list-queue-copy q) (make-list-queue (list-copy (queue-first q))))

  (define (list-queue-empty? q) (null? (queue-first q)))

  (define (raise-lqe-error who q)
    (raise (apply condition 
		  (filter valies
			  (list (and who (make-who-condition who))
				(make-message-condition  "list-queue is empty")
				(make-empty-list-queue-error)
				(make-irritants-condition q))))))
  
  (define (list-queue-front q)
    (if (list-queue-empty? q)
	(raise-lqe-error 'list-queue-front q)
	(car (queue-first q))))
  (define (list-queue-back q)
    (if (list-queue-empty? q)
	(raise-lqe-error 'list-queue-front q)
	(car (queue-last q))))

  ;; mutators
  (define (list-queue-add-front! q e)
    (let ((new-first (cons e (queue-first q))))
      (when (list-queue-empty? q)
	  (queue-last-set! q new-first))
      (queue-first-set! q new-first)))

  (define (list-queue-add-back! q e)
    (let ((new-last (list e)))
      (if (list-queue-empty? q)
	  (queue-first-set! q new-last)
	  (set-cdr! (queue-last q) new-last))
      (queue-last-set! q new-last)))

  (define (list-queue-remove-front! q)
    (when (list-queue-empty? q)
      (raise-lqe-error 'list-queue-remove-front! q))
    (let* ((old-first (queue-first q))
	   (e (car old-first))
	   (new-first (cdr old-first)))
      (when (null? new-first)
	(queue-last-set! q '()))
      (queue-first-set! q new-first)
      e))

  (define (list-queue-remove-back! q)
    (define (penult-pair lis)
      (let lp ((lis lis))
	(cond ((null? (cdr lis)) '())
	      ((null? (cddr lis)) lis)
	      (else (lp (cdr lis))))))
    (when (list-queue-empty? q)
      (raise-lqe-error 'list-queue-remove-back! q))
    (let* ((old-last (queue-last q))
	   (e (car old-last))
	   (new-last (penult-pair (queue-first q))))
      (if (null? new-last)
	  (queue-first-set! q '())
	  (set-cdr! new-last '()))
      (queue-last-set! q new-last)
      e))

  (define (list-queue-remove-all! q)
    (let ((result (queue-first q)))
      (list-queue-clear! q)
      result))

  (define (list-queue-clear! q)
    (queue-first-set! q '())
    (queue-last-set! q '()))

  (define (list-queue-length q) (length (queue-first q)))

  (define (list-queue-append . qs) (list-queue-concatenate qs))
  (define (list-queue-concatenate qs)
    (rlet1 r (list-queue)
      (dolist (q qs)
	(dolist (e (queue-first q))
	  (list-queue-add-back! r e)))))
  (define (list-queue-append! dst . qs)
    (let loop ((qs qs) (first (queue-first dst)) (last (queue-last dst)))
      (if (null? qs)
	  dst
	  (let1 qf (queue-first (car qs))
	    (cond ((null? qf) (loop (cdr qs) first last)) ;; nothing to append
		  ((null? first) 
		   (let ((next (list-copy qf)))
		     (list-queue-set-list! dst next)
		     (loop (cdr qs) next (queue-last dst))))
		  (else 
		   (let ((next (list-copy qf)))
		     (set-cdr! last next)
		     (loop (cdr qs) first (last-pair next)))))))))

  (define (list-queue-map proc q)  (make-list-queue (map proc (queue-first q))))
  (define (list-queue-map! proc q) (map! proc (queue-first q)))

  (define (list-queue-for-each proc q) (for-each proc (queue-first q)))

  ;; should we make them inlined?
  (define (list-queue-unfold stop? mapper successor seed)
    (make-list-queue (unfold stop? mapper successor seed)))
  (define (list-queue-unfold-right stop? mapper successor seed)
    (make-list-queue (unfold-right stop? mapper successor seed)))

  (define list-queue-set-list!
    (case-lambda
     ((q first)
      (queue-first-set! q first)
      (if (null? first)
	  (queue-last-set! q '())
	  (queue-last-set! q (last-pair first))))
     ;; we don't check as the SRFI mentioned
     ((q first last)
      (queue-first-set! q first)
      (queue-last-set! q last))))

  ;; for optimisation of queue-first, we redefine like this
  (define (list-queue-list q)       (queue-first q))
  (define (list-queue-first-last q) (values (queue-first q) (queue-last q)))
  
)