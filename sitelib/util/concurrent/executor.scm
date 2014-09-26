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

;; almost portable concurrent library
(library (util concurrent executor)
    (export future? make-future future
	    future-get future-cancel
	    future-done? future-cancelled?

	    executor? make-executor
	    executor-state executor-pool-size
	    executor-max-pool-size
	    executor-available?

	    shutdown-executor!
	    execute-future!

	    abort-rejected-handler
	    terminate-oldest-handler
	    waiting-next-handler
	    
	    &rejected-execution-error
	    rejected-execution-error?

	    )
    (import (rnrs)
	    (srfi :18)
	    (srfi :19)
	    (util queue))
  ;; future
  (define-record-type (<future> make-future future?)
    (fields (immutable thunk future-thunk)
	    (mutable worker future-worker future-worker-set!)
	    (mutable state future-state future-state-set!))
    (protocol (lambda (p)
		(lambda (thunk)
		  (p thunk #f 'created)))))

  ;; for convenience
  (define-syntax future
    (syntax-rules ()
      ((_ expr ...)
       (make-future (lambda () expr ...)))))

  (define (future-get future) 
    (let ((w (future-worker future)))
      (unless w (error 'future-get "Future didn't run yet"))
      (thread-join! (worker-thread w))))

  (define (future-cancel future)
    (let ((w (future-worker future)))
      (unless w (error 'future-get "Future didn't run yet"))
      (thread-terminate! (worker-thread w))
      (cleanup (worker-executor w) w 'terminated)))

  (define (future-done? future) (eq? (future-state future) 'done))
  (define (future-cancelled? future) (eq? (future-state future) 'terminated))

  ;; executor and worker
  (define-condition-type &rejected-execution-error &error 
    make-rejected-execution-error rejected-execution-error?
    (future rejected-future)
    (executor rejected-executor))
  ;; TODO some other reject policy
  (define (abort-rejected-handler future executor)
    (raise (condition (make-rejected-execution-error future executor)
		      (make-who-condition 'executor)
		      (make-message-condition "Failed to add task"))))
  (define (terminate-oldest-handler future executor)
    (let ((oldest (dequeue! (executor-workers executor))))
      (thread-terminate! (worker-thread oldest))
      ;; now remove
      (cleanup executor oldest 'terminated))
    ;; retry
    (execute-future! executor future))
  (define (waiting-next-handler wait-retry)
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
  (define default-rejected-handler abort-rejected-handler)

  (define-record-type (<thread-pool-executor> make-executor executor?)
    (fields (mutable pool-size executor-pool-size executor-pool-size-set!)
	    (mutable state executor-state executor-state-set!)
	    (immutable max-pool-size executor-max-pool-size)
	    (immutable workers executor-workers)
	    (immutable rejected-handler executor-rejected-handler)
	    (immutable mutex executor-mutex))
    (protocol (lambda (p)
		(lambda (max-pool-size . rest)
		  ;; i don't see using mtqueue for this since
		  ;; mutating the slot is atomic
		  (p 0 'running max-pool-size (make-queue)
		     (if (null? rest)
			 default-rejected-handler
			 (car rest))
		     (make-mutex))))))

  ;; it's also defined in (sagittarius thread)
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

  (define-syntax with-atmoic
    (syntax-rules ()
      ((_ executor expr ...)
       (dynamic-wind
	   (lambda () (mutex-lock-recursively! (executor-mutex executor)))
	   (lambda () expr ...)
	   (lambda () (mutex-unlock-recursively! (executor-mutex executor)))))))
  (define (cleanup executor worker state)
    (with-atmoic executor
     (executor-pool-size-set! executor (- (executor-pool-size executor) 1))
     ;; reduce pool count
     ;; remove from workers
     (remove-from-queue! (lambda (w) (eq? worker w))
			 (executor-workers executor))
     (future-state-set! (worker-task worker) state)))

  (define-record-type (<worker> make-worker worker?)
    (fields (mutable thread worker-thread worker-thread-set!)
	    (immutable executor worker-executor)
	    (immutable create-time worker-create-time)
	    (immutable future worker-task))
    (protocol (lambda (p)
		(lambda (executor future) 
		  (p #f executor (current-time) future)))))

  (define (executor-available? executor)
    (< (executor-pool-size executor) (executor-max-pool-size executor)))
  
  ;; shutdown
  ;; shutdown given executor. We need to first finish all
  ;; workers then change the state.
  (define (shutdown-executor! executor)
    (with-atmoic executor
     (let ((state (executor-state executor))
	   (workers (executor-workers executor)))
       (guard (e (else (executor-state-set! executor state)
		       (raise e)))
	 ;; to avoid accepting new task
	 ;; chenge it here
	 (when (eq? state 'running)
	   (executor-state-set! executor 'shutdown))
	 (let loop ((workers workers))
	   (unless (queue-empty? workers)
	     (thread-terminate! (worker-thread (queue-front workers)))
	     (cleanup executor (dequeue! workers) 'terminated)
	     (loop workers)))))))

  (define (execute-future! executor future)
    (define (worker-thunk worker)
      (lambda ()
	(let ((future (worker-task worker)))
	  (guard (e (else (cleanup executor worker 'error) 
			  (raise e)))
	    (let* ((thunk (future-thunk future))
		   (r (thunk)))
	      (cleanup executor worker 'done)
	      r)))))
    
    (with-atmoic executor
     (let ((pool-size (executor-pool-size executor))
	   (max-pool-size (executor-max-pool-size executor))
	   (reject-handler (executor-rejected-handler executor)))
       (if (and (< pool-size max-pool-size)
		(eq? (executor-state executor) 'running))
	   ;; add
	   (let* ((worker (make-worker executor future))
		  (thread (make-thread (worker-thunk worker))))
	     (enqueue! (executor-workers executor) worker)
	     (executor-pool-size-set! executor (+ pool-size 1))
	     (future-worker-set! future worker)
	     (worker-thread-set! worker thread)
	     ;; thread must be started *after* we do above
	     ;; otherwise may get something weird condition
	     (thread-start! thread)
	     executor)
	   (reject-handler future executor)))))

)
