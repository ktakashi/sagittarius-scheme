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
	    (util duration))

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
(define *current-work-queue* (make-parameter #f))

(define-record-type fork-join-pool-parameters
  (fields max-threads
	  keep-alive
	  max-queue-depth
	  thread-name-prefix))
(define-syntax fork-join-pool-parameters-builder
  (make-record-builder fork-join-pool-parameters
;; no reason other than Java's ForkJoinPool limit, but should be large enough
   ((max-threads #x7fff)
    (keep-alive (duration:of-millis 5000)) ;; default 5 sec
    (max-queue-depth 2))))

(define *default-parameter* (fork-join-pool-parameters-builder))

(define-record-type fork-join-pool
  (fields core-threads	         ;; vector of worker threads
	  max-threads	         ;; # of max worker thread, incl. core threads
	  worker-queues	         ;; vector of worker queues
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
		(let ((core-threads (make-vector n))
		      (tprefix
		       (fork-join-pool-parameters-thread-name-prefix parameter))
		      (worker-queues (make-vector n))
		      (indices (iota n))
		      (scheduler-queue (make-shared-queue)))
		  (do ((i 0 (+ i 1))) ((= i n))
		    (let* ((wq (make-worker-queue))
			   (t (make-core-worker-thread
			       i
			       (or tprefix "fork-join-pool")
			       scheduler-queue
			       wq worker-queues
			       (remv i indices))))
		      (vector-set! worker-queues i wq)
		      (vector-set! core-threads i t)))
		  (let ((r (p core-threads
			      (fork-join-pool-parameters-max-threads parameter)
			      worker-queues
			      #f
			      scheduler-queue
			      n
			      parameter
			      (make-mutex))))
		    (fork-join-pool-scheduler-set! r
		     (make-scheduler-thread r scheduler-queue worker-queues
					    parameter))
		    r))))))

(define (make-core-worker-thread i prefix
				 sq worker-queue worker-queues other-queues)
  (define (select-other-task)
    (let loop ((indices other-queues))
      (and (not (null? indices))
	   (let* ((index (car indices))
		  (wq (vector-ref worker-queues index)))
	     (or (and (not (worker-queue-empty? wq))
		      (worker-queue-pop! wq 0 #f))
		 (loop (cdr indices)))))))
  (thread-start!
   (make-thread
    (lambda ()
      (parameterize ((*current-work-queue* worker-queue)) ;; set worker queue
	(let loop ((task (worker-queue-get! worker-queue)))
	  (when task
	    (guard (e (else #f)) (task))
	    (cond ((worker-queue-get! worker-queue 0 #f) => loop)
		  ;; help other thread if they are busy but not me
		  ;; I'm such a kind thread :D
		  ((and (worker-queue-empty? worker-queue)
			(select-other-task)) => loop)
		  (else
		   ;; Nothing to do, so wait for a next task
		   (loop (worker-queue-get! worker-queue))))))))
    (string-append prefix "-core-worker-" (number->string i)))))

(define (make-scheduler-thread pool sq worker-queues parameter)
  (define wq* (vector->list worker-queues))
  (define wqq (make-list-queue wq*))
  (define duration (fork-join-pool-parameters-keep-alive parameter))
  (define max-queue-depth (fork-join-pool-parameters-max-queue-depth parameter))
  (define tprefix (fork-join-pool-parameters-thread-name-prefix parameter))
  (define (small-enough wq)
    (< (worker-queue-size wq) max-queue-depth))
  (define thread-number 0)
  ;; This tries to reuse the thread if possible
  (define (spawn task0)
    (set! thread-number (+ thread-number 1))
    (thread-start!
     (make-thread
      (lambda ()
	(let loop ((task task0))
	  (guard (e (else #f)) (task))
	  (let ((wait (add-duration (current-time) duration)))
	    (cond ((exists (lambda (wq) (worker-queue-pop! wq wait #f)) wq*)
		   => loop))))
	(update-thread-count! pool (lambda (tc) (and tc (- tc 1)))))
      (string-append (or tprefix "fork-join-pool")
		     "-child-thread-"
		     (number->string thread-number))))
    (update-thread-count! pool (lambda (tc) (and tc (+ tc 1)))))

  (define (process)
    (let loop ()
      (let ((task (shared-queue-get! sq)))
	(when task
	  ;; If the worker is idling, just use it
	  ;; TODO maybe we should put idling check, empty worker queue doesn't
	  ;;      mean the worker is not busy...
	  (cond ((find worker-queue-empty? wq*) =>
		 (lambda (wq) (worker-queue-put! wq task)))
		((find small-enough wq*) =>
		 (lambda (wq) (worker-queue-put! wq task)))
		((< (fork-join-pool-thread-count pool)
		    (fork-join-pool-max-threads pool))
		 (spawn task))
		;; put it randomly, well it's just round robin...
		(else
		 (let ((wq (list-queue-remove-front! wqq)))
		   (worker-queue-put! wq task)
		   (list-queue-add-back! wqq wq))))
	  (loop)))))
  (thread-start! (make-thread process "fork-join-scheduler-thread")))

(define (fork-join-pool-push-task! pool task)
  (unless (fork-join-pool-scheduler pool)
    (assertion-violation 'fork-join-pool-push-task!
			 "The pool is shutdown" pool))
  (unless (procedure? task)
    (assertion-violation 'fork-join-pool-push-task! "Task must be a procedure"
			 task))
  (shared-queue-put! (fork-join-pool-scheduler-queue pool) task))

;; waits all worker queue to be empty
(define (fork-join-pool-wait-all! pool . maybe-timeout)
  (define (get-timeout v)
    (cond ((integer? v) (add-duration (current-time) (duration:of-millis v)))
	  ((time? v) v)
	  (else #f)))
  (define timeout (and (not (null? maybe-timeout))
		       (get-timeout (car maybe-timeout))))
  (define wq* (fork-join-pool-worker-queues pool))
  (define l (vector-length wq*))
  (let loop ((i 0))
    (cond ((= i l))
	  ((worker-queue-empty? (vector-ref wq* i)) (loop (+ i 1)))
	  ((and timeout (time<=? timeout (current-time))) #f)
	  (else
	   (if timeout
	       (thread-sleep! timeout)
	       (thread-yield!))
	   (loop i)))))

(define (fork-join-pool-shutdown! pool)
  (vector-for-each (lambda (wq) (worker-queue-put! wq #f))
		   (fork-join-pool-worker-queues pool))
  (shared-queue-put! (fork-join-pool-scheduler-queue pool) #f)
  (fork-join-pool-scheduler-set! pool #f)
  (update-thread-count! pool (lambda (tc) #f)))

(define (fork-join-pool-available? pool)
  (number? (fork-join-pool-thread-count pool)))

(define (update-thread-count! pool updator)
  (define lock (fork-join-pool-lock pool))
  (mutex-lock! lock)
  (let ((tc (fork-join-pool-thread-count pool)))
    (fork-join-pool-thread-count-set! pool (updator tc)))
  (mutex-unlock! lock))
)
