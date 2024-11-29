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
	    (rename (fork-join-pool <fork-join-pool>)
		    (fork-join-pool-thread-count-export
		     fork-join-pool-thread-count)
		    (fork-join-pool-idle-count-export
		     fork-join-pool-idle-count))
	    fork-join-pool-max-threads
	    fork-join-pool-push-task!
	    fork-join-pool-wait-all!
	    fork-join-pool-shutdown!
	    fork-join-pool-available?

	    fork-join-pool-parameters-builder from ;; for convenience
	    fork-join-pool-parameters?
	    fork-join-pool-parameters-max-threads
	    fork-join-pool-parameters-keep-alive
	    fork-join-pool-parameters-thread-name-prefix
	    )
    (import (rnrs)
	    (record builder)
	    (srfi :1 lists)
	    (srfi :18 multithreading)
	    (srfi :19 time)
	    (srfi :39 parameters)
	    (srfi :117 list-queues)
	    (srfi :133 vectors)
	    (util concurrent atomic)
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
(define-record-type worker-queue
  (fields notifier
	  pool				;; #f means root queue
	  ;; for now
	  ;; TODO implement lock free queue
	  queue
	  shutdown)
  (protocol (lambda (p)
	      (lambda (pool)
		(p (make-notifier)
		   pool
		   (make-shared-queue)
		   (make-atomic #f))))))
(define (worker-queue-get! wq) (shared-queue-get! (worker-queue-queue wq)))
(define (worker-queue-pop! wq . opts) 
  (apply shared-queue-pop! (worker-queue-queue wq) opts))
(define (worker-queue-put! wq v) (shared-queue-put! (worker-queue-queue wq) v))
(define (worker-queue-empty? wq) (shared-queue-empty? (worker-queue-queue wq)))
(define (worker-queue-size wq) (shared-queue-size (worker-queue-queue wq)))
(define (worker-queue-notify! wq)
  (notifier-send-notification! (worker-queue-notifier wq)))
(define (worker-queue-wait! wq . opts)
  (apply notifier-wait-notification! (worker-queue-notifier wq) opts))
(define (worker-queue-shutdown! wq)
  (atomic-store! (worker-queue-shutdown wq) #t)
  (worker-queue-notify! wq))
(define (worker-queue-shutdown? wq)
  (atomic-load (worker-queue-shutdown wq)))

;; thread local value on worker thread
(define *current-worker* (make-parameter #f))

(define (1+ v) (and v (+ v 1)))
(define (1- v) (and v (- v 1)))

(define ((make-worker-thread-run pool wq))
  (*current-worker* (cons pool wq))
  (dynamic-wind
      (lambda () (fork-join-pool-register-worker pool wq))
      (lambda () (fork-join-pool-run-worker pool wq))
      (lambda () (fork-join-pool-deregister-worker pool wq))))

(define-record-type worker-thread
  (fields thread
	  pool
	  queue)
  (protocol (lambda (p)
	      (lambda (pool n)
		(define prefix (or (fork-join-pool-thread-prefix pool)
				   "fork-join-pool-"))
		(let* ((wq (make-worker-queue pool))
		       (run (make-worker-thread-run pool wq)))
		  (p (make-thread run
		      (string-append prefix "worker-thread-" (number->string n)))
		     pool wq))))))
(define (worker-thread-start! wh)
  (thread-start! (worker-thread-thread wh))
  wh)
(define (worker-thread-queue-push! wh v)
  (worker-queue-put! (worker-thread-queue wh) v))
(define (worker-thread-queue-pop! wh timeout val)
  (worker-queue-pop! (worker-thread-queue wh) timeout val))
(define (worker-thread-name wh) (thread-name (worker-thread-thread wh)))

(define-record-type fork-join-pool-parameters
  (fields max-threads
	  keep-alive
	  thread-name-prefix))
(define-syntax fork-join-pool-parameters-builder
  (make-record-builder fork-join-pool-parameters
   ((max-threads #f)			    ;; will be computed by default
    (keep-alive (duration:of-millis 60000)) ;; default 1 min
    )))

(define *default-parameter* (fork-join-pool-parameters-builder))

(define-record-type fork-join-pool
  (fields max-threads		  ;; # of max worker thread
	  worker-queue
	  (mutable worker-queues) ;; registered worker queues
	  thread-count		  ;; # of worker threads
	  idle-count		  ;; # of idle threads
	  parameter
	  notifier
	  lock)
  (protocol (lambda (p)
	      (lambda ((n (or integer? fork-join-pool-parameters?)))
		(define parameter
		  (cond ((integer? n)
			 (fork-join-pool-parameters-builder
			  (from *default-parameter*)
			  (max-threads (* n 5))))
			((fork-join-pool-parameters? n) n)))
		(p (fork-join-pool-parameters-max-threads parameter)
		   (make-worker-queue #f)
		   (make-vector 32 #f)
		   (make-atomic-fixnum 0)
		   (make-atomic-fixnum 0)
		   parameter
		   (make-notifier)
		   (make-mutex))))))

(define (fork-join-pool-keep-alive pool)
  (fork-join-pool-parameters-keep-alive (fork-join-pool-parameter pool)))
(define (fork-join-pool-thread-prefix pool)
  (fork-join-pool-parameters-thread-name-prefix (fork-join-pool-parameter pool)))
(define (fork-join-pool-thread-count-export pool)
  (atomic-load (fork-join-pool-thread-count pool)))
(define (fork-join-pool-idle-count-export pool)
  (atomic-load (fork-join-pool-idle-count pool)))

(define (fork-join-pool-thread-count-inc! pool)
  (atomic-fixnum-inc! (fork-join-pool-thread-count pool)))
(define (fork-join-pool-thread-count-dec! pool)
  (atomic-fixnum-dec! (fork-join-pool-thread-count pool)))
(define (fork-join-pool-idle-count-inc! pool)
  (atomic-fixnum-inc! (fork-join-pool-idle-count pool)))
(define (fork-join-pool-idle-count-dec! pool)
  (atomic-fixnum-dec! (fork-join-pool-idle-count pool)))

(define (fork-join-pool-push-task! pool task)
  (define (push-task! task)
    (worker-queue-put! (fork-join-pool-worker-queue pool) task)
    (fork-join-pool-signal-work! pool))
  (unless (fork-join-pool-available? pool)
    (assertion-violation 'fork-join-pool-push-task!
			 "The pool is shutdown" pool))
  (unless (procedure? task)
    (assertion-violation 'fork-join-pool-push-task! "Task must be a procedure"
			 task))
  (cond ((*current-worker*) =>
	 (lambda (worker)
	   (let ((worker-pool (car worker))
		 (wq (cdr worker)))
	     (if (eq? pool worker-pool)
		 (worker-queue-put! wq task)
		 (push-task! task)))))
	(else (push-task! task))))

(define (fork-join-pool-signal-work! pool)
  (define max-threads (fork-join-pool-max-threads pool))
  (if (and (zero? (fork-join-pool-idle-count-export pool))
	   (< (fork-join-pool-thread-count-export pool) max-threads))
      (fork-join-pool-create-worker pool)
      (fork-join-pool-notify-idling-workers pool)))

(define (fork-join-pool-create-worker pool)
  (let ((wt (make-worker-thread pool (fork-join-pool-thread-count-export pool))))
    (worker-thread-start! wt)
    (fork-join-pool-thread-count-inc! pool)
    pool))

(define (fork-join-pool-notify-idling-workers pool)
  (define queues
    (filter values (vector->list (fork-join-pool-worker-queues pool))))
  (map worker-queue-notify! queues))

;; worker thread procedures
(define (fork-join-pool-register-worker pool wq)
  (define lock (fork-join-pool-lock pool))
  (define (grow-queue! pool queue)
    (define new (make-vector (* (vector-length queue) 2) #f))
    (vector-copy! new 0 queue)
    (fork-join-pool-worker-queues-set! pool new)
    new)
  (define (search! pool queues)
    (let loop ((i 0))
    (cond ((= i (vector-length queues))
	   (search! pool (grow-queue! pool queues)))
	  ((not (vector-ref queues i))
	   (vector-set! queues i wq))
	  (else (loop (+ i 1))))))
  (mutex-lock! lock)
  (search! pool (fork-join-pool-worker-queues pool))
  (mutex-unlock! lock))

(define (fork-join-pool-deregister-worker pool wq)
  (define lock (fork-join-pool-lock pool))
  (define (search! pool queues)
    (let loop ((i 0))
    (cond ((= i (vector-length queues)) #f)
	  ((eq? (vector-ref queues i) wq)
	   ;; TODO shrink...
	   (vector-set! queues i #f))
	  (else (loop (+ i 1))))))
  (mutex-lock! lock)
  (search! pool (fork-join-pool-worker-queues pool))
  (mutex-unlock! lock)
  (fork-join-pool-thread-count-dec! pool))

(define (fork-join-pool-run-worker pool wq)
  (define (run-task task wq)
    (when task
      (task)
      (run-task (worker-queue-pop! wq 0 #f) wq)))
  (define (->timeout pool) 
    (add-duration (current-time) (fork-join-pool-keep-alive pool)))
  (run-task (worker-queue-pop! (fork-join-pool-worker-queue pool) 0 #f) wq)
  (fork-join-pool-idle-count-inc! pool)
  (cond ((and (worker-queue-wait! wq (->timeout pool))
	      (not (worker-queue-shutdown? wq)))
	 (fork-join-pool-idle-count-dec! pool)
	 (fork-join-pool-run-worker pool wq))
	(else (fork-join-pool-idle-count-dec! pool))))
  
;; waits all worker queue to be empty
(define (fork-join-pool-wait-all! pool . maybe-timeout)
  (define (get-timeout v)
    (cond ((integer? v) (add-duration (current-time) (duration:of-millis v)))
	  ((time? v) v)
	  (else #f)))
  (define timeout (and (not (null? maybe-timeout))
		       (get-timeout (car maybe-timeout))))
  (define queues
    (filter values (vector->list (fork-join-pool-worker-queues pool))))
  (define queue (fork-join-pool-worker-queue pool))
  
  (let loop ((l (cons queue queues)))
    (cond ((null? l))
	  ((zero? (worker-queue-size (car l))) (loop (cdr l)))
	  ((and timeout (time<=? timeout (current-time))) #f)
	  (else
	   (if timeout
	       (thread-sleep! timeout)
	       (thread-yield!))
	   (loop l)))))

(define (fork-join-pool-shutdown! pool)
  (worker-queue-shutdown! (fork-join-pool-worker-queue pool))
  (for-each worker-queue-shutdown!
	    (filter values (vector->list (fork-join-pool-worker-queues pool)))))

(define (fork-join-pool-available? pool)
  (not (worker-queue-shutdown? (fork-join-pool-worker-queue pool))))
)
