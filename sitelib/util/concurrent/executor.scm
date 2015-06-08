;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; util/concurrent/executor.scm - Concurrent executor
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

;; portable concurrent library
(library (util concurrent executor)
    (export <executor> executor? ;; interface
	    executor-state
	    executor-available?
	    shutdown-executor!
	    execute-future!

	    <thread-pool-executor> make-thread-pool-executor 
	    thread-pool-executor?
	    ;; below must be thread-pool-executor specific
	    thread-pool-executor-pool-size
	    thread-pool-executor-max-pool-size
	    thread-pool-executor-available?
	    thread-pool-executor-execute-future!
	    thread-pool-executor-shutdown!

	    abort-rejected-handler
	    terminate-oldest-handler
	    wait-finishing-handler
	    push-future-handler
	    
	    &rejected-execution-error rejected-execution-error?
	    rejected-future rejected-executor

	    ;; this condition should not be extended
	    duplicate-executor-registration?
	    duplicate-executor-rtd

	    ;; future
	    <executor-future> make-executor-future executor-future?

	    ;; for extension
	    register-executor-methods
	    )
    (import (rnrs)
	    (only (srfi :1) remove!)
	    (srfi :18)
	    (srfi :19)
	    (srfi :117)
	    (util concurrent shared-queue)
	    (util concurrent future)
	    (util concurrent thread-pool))

  (define (not-started dummy)
    (assertion-violation 'executor-future "Future is not started yet" dummy))
  (define-record-type (<executor-future> make-executor-future executor-future?)
    (parent <future>)
    ;; (fields (mutable worker future-worker future-worker-set!))
    (protocol (lambda (n)
		(lambda (thunk)
		  ((n thunk not-started))))))

  ;; executor and worker
  (define-condition-type &rejected-execution-error &error 
    make-rejected-execution-error rejected-execution-error?
    (future rejected-future)
    (executor rejected-executor))
  ;; TODO some other reject policy

  ;; aboring when pool is full
  (define (abort-rejected-handler future executor)
    (raise (condition (make-rejected-execution-error future executor)
		      (make-who-condition 'executor)
		      (make-message-condition "Failed to add task"))))
  ;; assume the oldest one is negligible
  (define (terminate-oldest-handler future executor)
    (let ((oldest (list-queue-remove-front! (executor-pool-ids executor))))
      (thread-pool-thread-terminate! (executor-pool executor) (car oldest))
      ;; now remove
      (cleanup executor (cdr oldest) 'terminated))
    ;; retry
    (execute-future! executor future))

  ;; wait for trivial time until pool is available
  ;; if it won't then raise an error.
  (define (wait-finishing-handler wait-retry)
    (lambda (future executor)
      (thread-sleep! 0.1) ;; trivial time waiting
      (let loop ((count 0))
	(cond ((executor-available? executor)
	       (execute-future! executor future))
	      ((= count wait-retry)
	       ;; now how should we handle this?
	       ;; for now raises an error...
	       (abort-rejected-handler future executor))
	      ;; bit longer waiting
	      (else (thread-sleep! 0.5) (loop (+ count 1)))))))

  ;; default is abort
  (define default-rejected-handler abort-rejected-handler)

  ;; For now only dummy
  (define-record-type (<executor> %make-executor executor?)
    (fields (mutable state executor-state executor-state-set!))
    (protocol (lambda (p) (lambda () (p 'running)))))

  (define-record-type (<thread-pool-executor> 
		       make-thread-pool-executor 
		       thread-pool-executor?)
    (parent <executor>)
    (fields (immutable pool-ids executor-pool-ids)
	    (immutable pool executor-pool)
	    (immutable rejected-handler executor-rejected-handler)
	    (immutable mutex executor-mutex))
    (protocol (lambda (n)
		(lambda (max-pool-size . rest)
		  ;; i don't see using mtqueue for this since
		  ;; mutating the slot is atomic
		  ((n) (make-list-queue '())
		   (make-thread-pool max-pool-size)
		   (if (null? rest)
		       default-rejected-handler
		       (car rest))
		   (make-mutex))))))

  (define (thread-pool-executor-pool-size executor)
    (length (list-queue-list (executor-pool-ids executor))))
  (define (thread-pool-executor-max-pool-size executor)
    (thread-pool-size (executor-pool executor)))

  ;; this is basically not a good solution...
  ;; there are couple of points that with-atomic is called more than
  ;; twice in the same thread.
  ;;  e.g. shutdown-executor! calls future-cancel and it's cancler is
  ;;       calling with-atomic ...
  ;; so the lock should be recursive lock.
  (define (mutex-lock-recursively! mutex)
    (if (eq? (mutex-state mutex) (current-thread))
	(let ((n (mutex-specific mutex)))
	  (mutex-specific-set! mutex (+ n 1)))
	(begin
	  (mutex-lock! mutex)
	  (mutex-specific-set! mutex 0))))

  (define (mutex-unlock-recursively! mutex)
    (let ((n (mutex-specific mutex)))
      (if (= n 0)
	  (mutex-unlock! mutex)
	  (mutex-specific-set! mutex (- n 1)))))
  (define-syntax with-atomic
    (syntax-rules ()
      ((_ executor expr ...)
       (dynamic-wind
	   (lambda () (mutex-lock-recursively! (executor-mutex executor)))
	   (lambda () expr ...)
	   (lambda () (mutex-unlock-recursively! (executor-mutex executor)))))))
  (define (cleanup executor future state)
    (define (remove-from-queue! proc queue)
      (list-queue-set-list! queue (remove! proc (list-queue-list queue))))
    (with-atomic executor
      (remove-from-queue! (lambda (o) (eq? (cdr o) future))
			  (executor-pool-ids executor))
      (future-state-set! future state)))

  (define (thread-pool-executor-available? executor)
    (< (thread-pool-executor-pool-size executor) 
       (thread-pool-executor-max-pool-size executor)))
  
  ;; shutdown
  (define (thread-pool-executor-shutdown! executor)
    (with-atomic executor
     (let ((state (executor-state executor)))
       (guard (e (else (executor-state-set! executor state)
		       (raise e)))
	 ;; to avoid accepting new task
	 ;; chenge it here
	 (when (eq? state 'running)
	   (executor-state-set! executor 'shutdown))
	 ;; we terminate the threads so that we can finish all
	 ;; infinite looped threads.
	 ;; NB, it's dangerous
	 (thread-pool-release! (executor-pool executor) 'terminate)
	 (for-each (lambda (future) (future-cancel (cdr future)))
		   (list-queue-remove-all! (executor-pool-ids executor)))))))

  (define (thread-pool-executor-execute-future! executor future)
    (or (with-atomic executor
	  (let ((pool-size (thread-pool-executor-pool-size executor))
		(max-pool-size (thread-pool-executor-max-pool-size executor)))
	    (and (< pool-size max-pool-size)
		 (eq? (executor-state executor) 'running)
		 (push-future-handler future executor))))
	;; the reject handler may lock executor again
	;; to avoid double lock, this must be out side of atomic
	(let ((reject-handler (executor-rejected-handler executor)))
	  (reject-handler future executor))))

  ;; We can push the future to thread-pool
  (define (push-future-handler future executor)
    (define (canceller future) (cleanup executor future 'terminated))
    (define (task-invoker thunk)
      (lambda ()
	(let ((q (future-result future)))
	  (guard (e (else
		     (future-canceller-set! future #t) ;; kinda abusing
		     (cleanup executor future 'finished)
		     (shared-queue-put! q e)))
	    (let ((r (thunk)))
	      ;; first remove future then put the result
	      ;; otherwise future-get may be called before.
	      (cleanup executor future 'finished)
	      (shared-queue-put! q r))))))
    ;; in case this is called directly
    (unless (eq? (executor-state executor) 'running)
      (assertion-violation 'push-future-handler
			   "executor is not running" executor))
    (future-result-set! future (make-shared-queue))    
    (let* ((thunk (future-thunk future))
	   (id (thread-pool-push-task! (executor-pool executor)
				       (task-invoker thunk))))
      (future-canceller-set! future canceller)
      (list-queue-add-back!
       (executor-pool-ids executor) (cons id future))
      executor))

  ;; fork-join-executor
  ;; this is more or less an example of how to implement 
  ;; custom executors. the executor doesn't manage anything
  ;; but just creates a thread and execute it.
  (define-record-type (<fork-join-executor> 
		       make-fork-join-executor 
		       fork-join-executor?)
    (parent <executor>)
    (protocol (lambda (n) (lambda () ((n))))))

  (define (fork-join-executor-available? e) 
    (unless (fork-join-executor? e)
      (assertion-violation 'fork-join-executor-available? 
			   "not a fork-join-executor" e))
    #t)
  (define (fork-join-executor-execute-future! e f)
    ;; the same as simple future
    (define (task-invoker thunk)
      (lambda ()
	(let ((q (future-result f)))
	  (guard (e (else (future-canceller-set! f #t)
			  (future-state-set! f 'finished)
			  (shared-queue-put! q e)))
	    (let ((r (thunk)))
	      (future-state-set! f 'finished)
	      (shared-queue-put! q r))))))
    (unless (fork-join-executor? e)
      (assertion-violation 'fork-join-executor-available? 
			   "not a fork-join-executor" e))
    ;; we don't manage thread so just create and return
    (thread-start! (make-thread (task-invoker (future-thunk f))))
    f)
  (define (fork-join-executor-shutdown! e)
    (unless (fork-join-executor? e)
      (assertion-violation 'fork-join-executor-available? 
			   "not a fork-join-executor" e))
    ;; we don't manage anything
    (executor-state-set! executor 'shutdown))

  (define *registered-executors* '())
  (define *register-lock* (make-mutex))
  (define-condition-type &duplicate-executor-registration
    &error make-duplicate-executor-registration duplicate-executor-registration?
    (rtd duplicate-executor-rtd))

  (define (%register-executor-methods rtd pred available? execute shutdown)
    (mutex-lock! *register-lock*)
    ;; hope record-type-descriptor returns the defined rtd not
    ;; newly created one
    (when (assq rtd *registered-executors*)
      (mutex-unlock! *register-lock*)
      (raise (make-duplicate-executor-registration rtd)
	     (make-who-condition 'register-executor-methods)
	     (make-message-condition 
	      "specified predicate is already registered")))
    (set! *registered-executors*
	  (cons (list rtd pred available? execute shutdown)
		*registered-executors*))
    (mutex-unlock! *register-lock*))

  (define-syntax register-executor-methods
    (syntax-rules ()
      ((_ type available? execute shutdown)
       (define dummy
	 (let ((rtd (record-type-descriptor type)))
	   (%register-executor-methods 
	    rtd
	    (record-predicate rtd) available? execute shutdown))))))

  (define-syntax invoke-method
    (syntax-rules ()
      ((_ who pos fallack e args ...)
       (let loop ((methods *registered-executors*))
	 (cond ((null? methods) 
		(if (executor? e)
		    fallback
		    (assertion-violation 'who "not an executor" e)))
	       (((cadar methods) e)
		((pos (cdar methods)) e args ...))
	       (else (loop (cdr methods))))))))
  (define (not-supported e)
    (error #f "given executor is not supported" e))

  (define (executor-available? e)
    (invoke-method executor-available? cadr #f e))
  (define (execute-future! e f)
    (invoke-method execute-future! caddr (not-supported e) e f))
  (define (shutdown-executor! e)
    (invoke-method shutdown-executor! cadddr (not-supported e) e))

  ;; pre-defined executor
  (register-executor-methods <thread-pool-executor>
			     thread-pool-executor-available?
			     thread-pool-executor-execute-future!
			     thread-pool-executor-shutdown!)

  (register-executor-methods <fork-join-executor>
			     fork-join-executor-available?
			     fork-join-executor-execute-future!
			     fork-join-executor-shutdown!)
)
