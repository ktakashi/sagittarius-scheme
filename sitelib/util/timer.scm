;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/timer.scm - Timer
;;;  
;;;   Copyright (c) 2010-2014  Takashi Kato  <ktakashi@ymail.com>
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

(library (util timer)
    (export make-timer timer?
	    timer-start! timer-stop!
	    timer-schedule! timer-reschedule!
	    timer-remove! timer-exists?)
    (import (rnrs)
	    (sagittarius) ;; for compare
	    (sagittarius control) ;; unwind-protect
	    (srfi :18)
	    (srfi :19)
	    (util heap))

(define-record-type (<timer-task> %make-timer-task timer-task?)
  ;; thunk, next (time object), period (integer)
  (fields (immutable id timer-task-id)
	  (immutable thunk timer-task-thunk) 
	  (mutable next timer-task-next timer-task-next-set!) 
	  (mutable period timer-task-period timer-task-period-set!)
	  (mutable running timer-task-running? timer-task-running-set!))
  (protocol (lambda (p)
	      (lambda (id thunk next period)
		(p id thunk next period #f)))))

(define (task-compare a b)
  (let ((r (compare (timer-task-next a) (timer-task-next b))))
    (if (zero? r)
	(compare (timer-task-id a) (timer-task-id b))
	r)))

(define-record-type (<timer> %make-timer timer?)
  (fields (immutable queue timer-queue)
	  (mutable done? timer-done? timer-done-set!)
	  (immutable lock timer-lock)
	  (immutable waiter timer-waiter)
	  ;; worker thread
	  (mutable worker timer-worker %timer-worker-set!)
	  (mutable next-id timer-next-id timer-next-id-set!)
	  ;; tiemrs table
	  (immutable active timer-active)
	  )
  (protocol (lambda (p)
	      (lambda ()
		(p (make-heap task-compare) #f
		   (make-mutex)
		   (make-condition-variable)
		   #f 1 (make-eqv-hashtable))))))

;; hmmmm, how many times I've written this type of macro...
(define-syntax wait-cv
  (syntax-rules ()
    ((_ mutex cv)
     (wait-cv mutex cv #f))
    ((_ mutex cv timeout)
     (let ((m mutex)
	   (c cv)
	   (to timeout))
       (when (mutex-unlock! m c to)
	 (mutex-lock! mutex))))))

(define (milliseconds->sec&nano msec)
  (let ((sec (div msec 1000))
	(nsec (* (mod msec 1000) 1000000)))
    (values sec nsec)))

(define (make-timer :key (error-handler #f))
  (define (timer-start! t)
    (define (main-loop t)
      (unless (timer-done? t)
	;; we may want to <mt-heap> for this purpose...
	(let ((queue (timer-queue t)))
	  (if (heap-empty? queue)
	      (wait-cv (timer-lock t) (timer-waiter t))
	      (let* ((first (heap-entry-value (heap-min queue)))
		     (now   (current-time))
		     (next  (timer-task-next first)))
		(if (time>=? now next)
		    (let ((first (heap-entry-value (heap-extract-min! queue))))
		      ;; set running
		      (timer-task-running-set! first #t)
		      
		      (mutex-unlock! (timer-lock t))
		      (guard (e (error-handler (error-handler e))
				(else (raise e)))
			 ((timer-task-thunk first)))
		      (mutex-lock! (timer-lock t))
		      (if (timer-task-running? first)
			  (let ((p (timer-task-period first)))
			    (timer-task-running-set! first #f)
			    (if (and (time? p)
				     (or (positive? (time-nanosecond p))
					 (positive? (time-second p))))
				(let ((next2 (add-duration next p)))
				  (timer-task-next-set! first next2)
				  (heap-set! queue first first))
				(hashtable-delete! (timer-active t)
						   (timer-task-id first))))
			  (hashtable-delete! (timer-active t)
					     (timer-task-id first))))
		    (wait-cv (timer-lock t) (timer-waiter t)
			     (timer-task-next first))))))
	(main-loop t)))
    (lambda ()
      (dynamic-wind
	  (lambda () (mutex-lock! (timer-lock t)))
	  (lambda () (main-loop t))
	  (lambda () (mutex-unlock! (timer-lock t))))))
  (let ((t (%make-timer)))
    (%timer-worker-set! t (make-thread (timer-start! t)))
    t))

(define (timer-start! t)
  (thread-start! (timer-worker t))
  t)

(define (timer-stop! t)
  (mutex-lock! (timer-lock t))
  (timer-done-set! t #t)
  (condition-variable-broadcast! (timer-waiter t))
  (mutex-unlock! (timer-lock t))
  (thread-join! (timer-worker t)))

(define (check-positive who v msg)
  (when (negative? v) (error who msg v)))

(define (millisecond->time-duration msec)
  (let-values (((sec nsec) (milliseconds->sec&nano msec)))
    (make-time time-duration nsec sec)))
(define (current-time+millisecond msec)
  (let ((t (current-time)))
    (if (zero? msec)
	t
	(add-duration t (millisecond->time-duration msec)))))

(define (check-period who period)
  (or (and (number? period) (check-positive who period "negative period"))
      (and (time? period) (eq? (time-type period) time-duration))
      (error who "positive or time-duration is required" period)))

(define (timer-schedule! timer thunk first :optional (period 0))
  (define (allocate-timer-id timer)
    (let ((c (timer-next-id timer)))
      (timer-next-id-set! timer (+ c 1))
      c))
  (define (check v msg) (check-positive 'timer-schedule! v msg))
  (unless (time? first) (check first "negative delay"))
  (check-period 'timer-schedule! period)

  (mutex-lock! (timer-lock timer))
  (let* ((id (allocate-timer-id timer))
	 (first (if (time? first) first (current-time+millisecond first)))
	 (p    (cond ((time? period) period)
		     ((zero? period) period)
		     (else (millisecond->time-duration period))))
	 (task (%make-timer-task id thunk first p)))
    (hashtable-set! (timer-active timer) id task)
    (heap-set! (timer-queue timer) task task)
    (condition-variable-broadcast! (timer-waiter timer))
    (mutex-unlock! (timer-lock timer))
    id))

(define (timer-reschedule! timer id first :optional (period 0))
  (define (check v msg) (check-positive 'timer-reschedule! v msg))
  (unless (time? first) (check first "negative delay"))
  (check-period 'timer-reschedule! period)

  (let ((lock (timer-lock timer)))
    (mutex-lock! lock)
    (let ((task (hashtable-ref (timer-active timer) id)))
      ;; task has next
      (when task
	(let ((old (timer-task-next task))
	      (next (if (time? first) first (current-time+millisecond first)))
	      (p    (cond ((time? period) period)
			  ((zero? period) period)
			  (else (millisecond->time-duration period))))
	      (queue (timer-queue timer)))
	  ;; should be able to delete here...
	  (unless (timer-task-running? task)
	    (heap-delete! queue task))
	  ;; update period
	  (timer-task-period-set! task p)
	  (timer-task-next-set! task next)
	  ;; now reschedule it
	  (heap-set! queue task task)
	  ;; let them know
	  (condition-variable-broadcast! (timer-waiter timer))))
      (mutex-unlock! lock)))
  id)

(define (timer-remove! timer id)
  (let ((lock (timer-lock timer)))
    (mutex-lock! lock)
    (let ((task (hashtable-ref (timer-active timer) id)))
      (cond ((not task) (mutex-unlock! lock) #f)
	    (else
	     (if (timer-task-running? task)
		 (timer-task-running-set! task #f)
		 (heap-delete! (timer-queue timer) task))
	     (hashtable-delete! (timer-active timer) id)
	     (condition-variable-broadcast! (timer-waiter timer))
	     (mutex-unlock! lock)
	     #t)))))

(define (timer-exists? timer id)
  (let ((lock (timer-lock timer)))
    (mutex-lock! lock)
    (let ((r (hashtable-contains? (timer-active timer) id)))
      (mutex-unlock! lock)
      r)))

)
