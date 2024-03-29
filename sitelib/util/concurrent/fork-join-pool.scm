;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; util/concurrent/fork-join-pool.scm - Fork join pool
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
(library (util concurrent fork-join-pool)
    (export make-fork-join-pool fork-join-pool?
	    (rename (fork-join-pool <fork-join-pool>))
	    fork-join-pool-thread-count
	    fork-join-pool-max-threads
	    fork-join-pool-push-task!
	    fork-join-pool-wait-all!
	    fork-join-pool-shutdown!
	    fork-join-pool-available?

	    fork-join-pool-parameters-builder
	    fork-join-pool-parameters?
	    )
    (import (rnrs)
	    (record builder)
	    (srfi :1 lists)
	    (srfi :18 multithreading)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (util concurrent shared-queue)
	    (util concurrent notifier)
	    (util duration)
	    (util vector))

;; Fork join pool.
;; 
;; (util concurrent thread-pool) provides thread pool of 
;; fixed number of threads. This library provides a bit more flexible
;; pool and (hopefully) better scheduling.
;; 
;; Disclaimer.
;; I'm not sure if my implementation is actually a fork join pool, but
;; at least it doest work stealing and spawns new threads if needed.

;; maybe we want to use other implementation in the future
;; like lock free queue
(define worker-queue-get! shared-queue-get!)
(define worker-queue-pop! shared-queue-pop!)
(define worker-queue-put! shared-queue-put!)
(define worker-queue-empty? shared-queue-empty?)
(define worker-queue-size shared-queue-size)
(define make-worker-queue make-shared-queue)

;; thread local value on worker thread
(define *current-worker-thread* (make-parameter #f))

(define (1+ v) (and v (+ v 1)))
(define (1- v) (and v (- v 1)))

(define-record-type worker-thread
  (fields (mutable thread)
	  queue
	  notifier
	  (mutable busy? worker-thread-busy? worker-thread-busy!)
	  (mutable shutdown? worker-thread-shutdown? worker-thread-shutdown!)
	  lock)
  (protocol (lambda (p)
	      (lambda (queue notifier)
		(p #f queue notifier #f #f (make-mutex))))))
(define (worker-thread-free? wh)
  (and (worker-queue-empty? (worker-thread-queue wh))
       (not (worker-thread-busy? wh))))
(define (worker-thread-queue-push-w/o-notify! wh v)
  (worker-queue-put! (worker-thread-queue wh) v))
(define (worker-thread-queue-push! wh v)
  (worker-thread-queue-push-w/o-notify! wh v)
  (notifier-send-notification! (worker-thread-notifier wh)))
(define (worker-thread-queue-pop! wh timeout val)
  (worker-queue-pop! (worker-thread-queue wh) timeout val))
(define (worker-thread-name wh) (thread-name (worker-thread-thread wh)))

(define-record-type fork-join-pool-parameters
  (fields max-threads
	  keep-alive
	  max-queue-depth
	  thread-name-prefix))
(define-syntax fork-join-pool-parameters-builder
  (make-record-builder fork-join-pool-parameters
   ((max-threads #f)			   ;; will be computed by default
    (keep-alive (duration:of-millis 5000)) ;; default 5 sec
    (max-queue-depth 10))))

(define *default-parameter* (fork-join-pool-parameters-builder))

(define-record-type fork-join-pool
  (fields core-threads	         ;; vector of worker threads
	  max-threads	         ;; # of max worker thread, incl. core threads
	  (mutable scheduler)	 ;; scheduler thread
	  scheduler-queue	 ;; pool queue from input
	  (mutable thread-count) ;; number of worker threads
	  parameter
	  lock)
  (protocol (lambda (p)
	      (lambda (n . maybe-parameter)
		(define parameter (if (null? maybe-parameter)
				      *default-parameter*
				      (car maybe-parameter)))
		(define (max-threads n parameter)
		  (cond ((fork-join-pool-parameters-max-threads parameter))
			;; core threads times 5, for now
			;; Maybe we should calculate coefficient based on the
			;; given `n`.
			(else (* n 5))))
		(let* ((indices (iota n))
		       (notifier (make-notifier))
		       (core-threads (make-vector n))
		       (r (p core-threads
			     (max-threads n parameter)
			     #f
			     (make-shared-queue)
			     n
			     parameter
			     (make-mutex))))
		  (do ((i 0 (+ i 1))) ((= i n))
		    (let* ((wq (make-worker-queue))
			   (others (remv i indices))
			   (t (make-core-worker-thread r i wq others notifier)))
		      (vector-set! core-threads i t)))
		  (fork-join-pool-scheduler-set! r 
		   (make-scheduler-thread r notifier))
		  r)))))

(define (make-core-worker-thread pool i worker-queue other-queues notifier)
  (define worker-threads (fork-join-pool-core-threads pool))
  (define parameter (fork-join-pool-parameter pool))
  (define prefix
    (or (fork-join-pool-parameters-thread-name-prefix parameter)
	"fork-join-pool"))
  (define (select-other-task other-queues worker-threads)
    (let loop ((indices other-queues))
      (and (not (null? indices))
	   (let ((wh (vector-ref worker-threads (car indices))))
	     (or (and (worker-thread-busy? wh)
		      (cond ((worker-thread-queue-pop! wh 0 wh) =>
			     (lambda (v)
			       (and (not (eq? v wh)) v)))
			    (else
			     (worker-thread-queue-push! wh #f)
			     #f)))
		 (loop (cdr indices)))))))
  
  (let ((wh (make-worker-thread worker-queue notifier)))
    (worker-thread-thread-set! wh
     (thread-start!
      (make-thread
       (lambda ()
	 (*current-worker-thread* wh)
	 (let loop ((task (worker-queue-get! worker-queue)))
	   (when task
	     (worker-thread-busy! wh task)
	     (guard (e (else #f)) (task))
	     (worker-thread-busy! wh #f)
	     (let check ()
	       (cond ((worker-thread-shutdown? wh)) ;; done...
		     ((worker-queue-get! worker-queue 0 #f) => loop)
		     ;; help other thread if they are busy but not me
		     ;; I'm such a kind thread :D
		     ((select-other-task other-queues worker-threads) => loop)
		     (else
		      (notifier-wait-notification! notifier)
		      (check)))))))
       (string-append prefix "-core-worker-" (number->string i)))))
    wh))

(define (make-scheduler-thread pool notifier)
  (define worker-threads (fork-join-pool-core-threads pool))
  (define sq (fork-join-pool-scheduler-queue pool))
  (define parameter (fork-join-pool-parameter pool))

  (define wh* (vector->list worker-threads))
  ;; must be a copy to avoid memory leak
  ;; if we don't `wh*` will be shared between the process and `whq` while
  ;; `whq` will try to rotate, however, `wh*` keeps the head of the list
  ;; so the rotation doesn't really discard the list which causes memory
  ;; leak.
  (define whq (make-list-queue (vector->list worker-threads)))
  (define duration (fork-join-pool-parameters-keep-alive parameter))
  (define max-queue-depth (fork-join-pool-parameters-max-queue-depth parameter))
  (define tprefix (fork-join-pool-parameters-thread-name-prefix parameter))
  (define (small-enough wh)
    (define wq (worker-thread-queue wh))
    (<= (worker-queue-size wq) max-queue-depth))
  (define thread-number 0)
  ;; This tries to reuse the thread if possible
  (define (spawn task0)
    (set! thread-number (+ thread-number 1))
    (thread-start!
     (make-thread
      (lambda ()
	(define (search-task wh*)
	  (let loop ((wh* wh*))
	    (cond ((null? wh*) #f)
		  ((worker-thread-queue-pop! (car wh*) 0 wh*) =>
		   (lambda (v)
		     (if (eq? wh* v) ;; timeout
			 (loop (cdr wh*))
			 v)))
		  ;; #f = terminate
		  (else
		   ;; we put termination signatl back to the original
		   ;; worker queue to avoid the thread to be alive
		   (worker-thread-queue-push! (car wh*) #f)
		   #t))))
	(let loop ((task task0) (first? #t))
	  ;; (when first (set! task0 #f)) ;; maybe big task...
	  (when task
	    (guard (e (else #f)) (task))	    
	    (let waiter ((wait (add-duration (current-time) duration)))
	      (cond ((search-task wh*) =>
		     (lambda (v)
		       (if (boolean? v)
			   (loop (not v) #f)
			   (loop v #f))))
		    ((time<? (current-time) wait)
		     (notifier-wait-notification! notifier wait)
		     (waiter wait))))))
	(update-thread-count! pool 1-))
      (string-append (or tprefix "fork-join-pool")
		     "-child-thread-"
		     (number->string thread-number))))
    (update-thread-count! pool 1+))

  (define (process)
    (let loop ()
      (let* ((task (shared-queue-get! sq)))
	(when task
	  ;; If the worker is idling, just use it
	  ;; TODO maybe we should put idling check, empty worker queue doesn't
	  ;;      mean the worker is not busy...
	  (cond ((or (find worker-thread-free? wh*) (find small-enough wh*)) =>
		 (lambda (wh) (worker-thread-queue-push! wh task)))
		((< (fork-join-pool-thread-count pool)
		    (fork-join-pool-max-threads pool))
		 (spawn task))
		;; put it randomly, well it's just round robin...
		(else
		 (let ((wh (list-queue-remove-front! whq)))
		   (worker-thread-queue-push! wh task)
		   (list-queue-add-back! whq wh))))
	  (loop)))))
  (thread-start! (make-thread process
			      (string-append (or tprefix "fork-join")
					     "-scheduler-thread"))))

(define (fork-join-pool-push-task! pool task)
  (unless (fork-join-pool-scheduler pool)
    (assertion-violation 'fork-join-pool-push-task!
			 "The pool is shutdown" pool))
  (unless (procedure? task)
    (assertion-violation 'fork-join-pool-push-task! "Task must be a procedure"
			 task))
  (cond ((vector-find (lambda (e) (eq? e (*current-worker-thread*)))
		      (fork-join-pool-core-threads pool)) =>
	 (lambda (wh) (worker-thread-queue-push! wh task)))
	(else (shared-queue-put! (fork-join-pool-scheduler-queue pool) task))))

;; waits all worker queue to be empty
(define (fork-join-pool-wait-all! pool . maybe-timeout)
  (define (get-timeout v)
    (cond ((integer? v) (add-duration (current-time) (duration:of-millis v)))
	  ((time? v) v)
	  (else #f)))
  (define timeout (and (not (null? maybe-timeout))
		       (get-timeout (car maybe-timeout))))
  (define wh* (fork-join-pool-core-threads pool))
  (define l (vector-length wh*))
  (let loop ((i 0))
    (cond ((= i l))
	  ((worker-thread-free? (vector-ref wh* i)) (loop (+ i 1)))
	  ((and timeout (time<=? timeout (current-time))) #f)
	  (else
	   (if timeout
	       (thread-sleep! timeout)
	       (thread-yield!))
	   (loop i)))))

(define (fork-join-pool-shutdown! pool)
  (let* ((core-threads (fork-join-pool-core-threads pool))
	 (wh* (vector->list core-threads)))
    (for-each (lambda (wh)
		(worker-thread-shutdown! wh #t)
		(worker-thread-queue-push-w/o-notify! wh #f))
	      (cdr wh*))
    (worker-thread-shutdown! (car wh*) #t)
    (worker-thread-queue-push! (car wh*) #f)
    (do ((i 0 (+ i 1)))
	((= i (vector-length core-threads)))
      (vector-set! core-threads i #f)))
  (shared-queue-put! (fork-join-pool-scheduler-queue pool) #f)
  (fork-join-pool-scheduler-set! pool #f)
  (update-thread-count! pool (lambda (tc) #f)))

(define (fork-join-pool-available? pool)
  (number? (fork-join-pool-thread-count pool)))

(define ((make-counter-updator! get set) pool updator)
  (define lock (fork-join-pool-lock pool))
  (mutex-lock! lock)
  (set pool (updator (get pool)))
  (mutex-unlock! lock))

(define update-thread-count!
  (make-counter-updator! fork-join-pool-thread-count
			 fork-join-pool-thread-count-set!))
)
