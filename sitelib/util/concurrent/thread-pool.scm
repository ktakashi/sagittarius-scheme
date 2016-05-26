;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/concurrent/thread-pool.scm - Thread pool
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

(library (util concurrent thread-pool)
  (export make-thread-pool thread-pool? <thread-pool>
	  thread-pool-size
	  thread-pool-idling-count
	  thread-pool-idling?
	  thread-pool-push-task!
	  thread-pool-wait-all!
	  thread-pool-release!

	  thread-pool-thread-terminate! ;; hmmmm
	  thread-pool-thread
	  thread-pool-thread-id
	  thread-pool-thread-task-running?
	  thread-pool-current-thread-id
	  )
  (import (rnrs)
	  (srfi :18)
	  (srfi :39)
	  (util concurrent shared-queue))

(define *thread-pool-current-thread-id* (make-parameter #f))
;; make it readonly
(define (thread-pool-current-thread-id) (*thread-pool-current-thread-id*))

(define (make-executor idlings i queue error-handler)
  (lambda ()
    (define (call-error-handler e) (guard (ex (else #t)) (error-handler e)))
    (*thread-pool-current-thread-id* i)
    (let loop ()
      (shared-queue-put! idlings i)
      ;; when task is pushed then this thread id is popped from the
      ;; idlings queue.
      (let loop2 ((task (shared-queue-get! queue)))
	;; if it's #f then must be called from
	;; thread-pool-release!
	;; in that case, we don't put this thread id to idlings
	;; queue since it's not even available
	(when task
	  (guard (e (else (call-error-handler e))) (task))
	  (if (shared-queue-empty? queue)
	      (loop)
	      (loop2 (shared-queue-get! queue))))))))

(define (default-error-handler e) #f)
(define-record-type (<thread-pool> make-thread-pool thread-pool?)
  (fields threads ;; to join
	  queues  ;; shared-queues
	  idlings ;; shared-queue contains idling thread id
	  error-handler
	  )
  (protocol
   (lambda (p)
     (lambda (n . maybe-error-handler)
       (let* ((threads (make-vector n))
	      (queues  (make-vector n))
	      (idlings  (make-shared-queue))
	      (error-handler (if (null? maybe-error-handler)
				 default-error-handler
				 (car maybe-error-handler)))
	      (tp (p threads queues idlings error-handler)))
	 (do ((i 0 (+ i 1)))
	     ((= i n) 
	      ;; make sure all threads are up and running
	      (let loop ()
		(if (= n (shared-queue-size idlings))
		    tp
		    (begin
		      (thread-yield!)
		      (thread-sleep! 0.1)
		      (loop)))))
	   (let ((q (make-shared-queue)))
	     (vector-set! queues i q)
	     (vector-set! threads i
			  (thread-start!
			   (make-thread (make-executor idlings i q
						       error-handler)))))))))))

;; returns actual thread associated with given thread id
(define (thread-pool-thread tp id) (vector-ref (<thread-pool>-threads tp) id))
(define (thread-pool-thread-id tp thread)
  (define threads (<thread-pool>-threads tp))
  (define size (vector-length threads))
  ;; FIXME this takes O(n) but we want it O(1). so use hashtable
  (let loop ((i 0))
    (cond ((= i size) 
	   ;; TODO should we just return #f since managed but terminated thread
	   ;; can also be a possibility
	   (error 'thread-pool-thread-id "not a managed thread" thread))
	  ((eq? (vector-ref threads i) thread) i)
	  (else (loop (+ i 1))))))
;; returns size of pool
(define (thread-pool-size tp) (vector-length (<thread-pool>-threads tp)))

;; returns approx number of idling thread count
;; NB: getting exact count requries lock and that's rather useless
;;     since idling count is usually used to determine if users
;;     should add task or not. so for now, assume approx number
;;     is sufficient.
;; NB: this is O(1) operation, yahoo!!
(define (thread-pool-idling-count tp) 
  (shared-queue-size (<thread-pool>-idlings tp)))

;; returns #t if one of the threads is idling
(define (thread-pool-idling? tp) (not (zero? (thread-pool-idling-count tp))))

;; Optional argument must be a procedure takes one argument.
;;  see default-handler
(define (thread-pool-push-task! tp task . opt)
  ;; if we didn't get any then first one, sorry.
  (define (default-handler n) (if (negative? n) 0 n))

  ;; if this is called then there is *no* idling thread. thus we need
  ;; to find which one has the least loaded one.
  ;; TODO: maybe we want to add some diagnosis mechanism to avoid
  ;;       pushing task to waiting for inifinite process.
  (define (find-available tp add-to-back?)
    (let* ((threads (<thread-pool>-threads tp))
	   (queue (<thread-pool>-queues tp))
	   (size (vector-length threads)))
      (let loop ((i 0) (maybe -1) (qsize +inf.0))
	(if (= i size)
	    (add-to-back? maybe)
	    (let ((t (vector-ref threads i))
		  (s (shared-queue-size (vector-ref queue i))))
	      (cond ((and (add-to-back? i) (< s qsize)) (loop (+ i 1) i s))
		    (else (loop (+ i 1) maybe qsize))))))))
  (let ((where (or (and (thread-pool-idling? tp)
			(shared-queue-get! (<thread-pool>-idlings tp)))
		   (find-available tp (if (null? opt) 
					  default-handler 
					  (car opt))))))
    (shared-queue-put! (vector-ref (<thread-pool>-queues tp) where) task)
    where))

(define (thread-pool-wait-all! tp)
  (define (wait-queue tp)
    (define queus (vector->list (<thread-pool>-queues tp)))
    (let loop ()
      (unless (for-all shared-queue-empty? queus)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (define (wait-threads tp)
    (define size (thread-pool-size tp))
    (let loop ()
      (unless (= (thread-pool-idling-count tp) size)
	(thread-yield!)
	(thread-sleep! 0.1)
	(loop))))
  (wait-queue tp)
  (wait-threads tp))

(define (thread-pool-release! tp . opt)
  (define-syntax dovector
    (syntax-rules (->)
      ((_ vec -> v (i e) expr ...)
       (let ((v vec))
	 (do ((c (vector-length v)) (i 0 (+ i 1)))
	     ((= i c))
	   (let ((e (vector-ref v i)))
	     expr ...))))))
  (let ((type (if (null? opt) 'join (car opt))))
    (dovector (<thread-pool>-queues tp) -> v (i e)
      (shared-queue-put! e #f)
      ;; GC friendliness
      (vector-set! v i #f))
    (dovector (<thread-pool>-threads tp) -> v (i e)
      ;; default join
      (case type ((terminate) (thread-terminate! e)) (else (thread-join! e)))
      ;; GC friendliness
      (vector-set! v i #f))))

(define (thread-pool-thread-terminate! tp id)
  (define threads (<thread-pool>-threads tp))
  (define queues (<thread-pool>-queues tp))
  (define idlings (<thread-pool>-idlings tp))
  (let ((t (vector-ref threads id))
	(q (vector-ref queues id))
	(nq (make-shared-queue)))
    ;; we don't need old queue anymore
    (vector-set! queues id nq)
    ;; the operation may cause abondaned mutex so we need to handle
    ;; carefully.
    ;; on thread-pool, the lock is going like this:
    ;;   1. lock idlings -> release
    ;;   2. lock shared-queue -> release/wait (waiting is also released)
    ;; so what we need to do here is making sure 2 shared queues are
    ;; not locked. we do it like this:
    ;;   1. first clear the task queue
    ;;   2. lock idlings
    ;;   3. lock task queue
    ;;   4. terminate thread
    ;;   5. unlock idlings and task queue.
    ;;   6. remove thread id from idlings
    
    ;; clear all pending tasks.
    ;; the thread is terminated, so it's not interesting anymore
    (shared-queue-clear! q)
    (shared-queue-lock! idlings)
    (shared-queue-lock! q)

    ;; ok both mutex are held by this thread so terminate the
    ;; pooled thread.
    (thread-terminate! t) ;; this is dangerous, don't do it casually!
    
    ;; post termination
    (shared-queue-unlock! q)
    (shared-queue-unlock! idlings)
    (shared-queue-remove! idlings id)
    
    ;; prepare for next time
    (vector-set! threads id
		 (thread-start! 
		  (make-thread 
		   (make-executor idlings id nq
				  (<thread-pool>-error-handler tp)))))))

;; returns #t if the thread associated to given thread id 
;; is running.
;; it can take O(n) + blocking
;; NB: we can make it O(1) non blocking if thread pool has state per threads.
;;     as it was before but that requires extra storage. plus the result value
;;     would be inaccurate. not sure which is better...
(define (thread-pool-thread-task-running? tp id)
  (not (shared-queue-find (<thread-pool>-idlings tp) (lambda (o) (= o id)))))
;; TODO Should we add thread-pool-stop! ?

  )
